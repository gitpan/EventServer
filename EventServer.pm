package EventServer;
require Config;
use strict qw(vars subs);

############################################################
# Author: Jack Shirazi, Copyright 1995,1996
#
# All singing, all dancing server.
# Handles i/o driven clients, timer driven clients,
# and interrupt driven clients. If there are any more
# types, please tell me.
#
# ---------------------------------------
# SEE THE DOCUMENTATION AFTER THE __END__
# ---------------------------------------
# 
# (The documentation to this module is in pod format. This means
#  that it is readable as is, and can be generated into man,
#  html and other formats using the pod2* programs supplied
#  with perl. Both the podhtml and pod2man programs should
#  be available under the perl source tree in the 'pod' directory.
#  Executing 'perl pod2html EventServer.pm' will produce the file
#  'EventServer.pm.html'.)
############################################################

$EventServer::VERSION = 2.3;
sub Version {$EventServer::VERSION}

############################################################
# 
#         GLOBALS
# 
############################################################

#$VERSION; 	# the version.

$EventServer::DEBUG = 0;

#$HAS_POSIX - Boolean whether POSIX module is available
#$FULL_MASK - a filled signal mask (if POSIX is available)
eval {require POSIX};
if ($@) {
    $EventServer::HAS_POSIX = 0;
} else {
    $EventServer::HAS_POSIX = 1;
    $EventServer::FULL_MASK = POSIX::SigSet->new();
    $EventServer::FULL_MASK->fillset();
#    $EventServer::FULL_MASK->delset(POSIX::SIGALRM());
    # The SIG_UNBLOCK is because on IRIX a die in the handler
    # leaves the signal blocked (IRIX bug I think).
    $EventServer::SIG_UNBLOCK = POSIX::SIG_UNBLOCK()
}

#$CHLD_SIG - the name of the SIGCHLD signal.

#$SERVER_CLASS; # The name of the EventServer class to use.
# Variable holding the name of the server class.
# Set this before require'ing this file if you are subclassing
# - then the initialization is called from the subclass.
# i.e.
#     $EventServer::SERVER_CLASS ||= MyClass;
#     require EventServer;
#

$EventServer::SERVER_CLASS ||= 'EventServer';

#%EXPORTS;	# function wrappers for public methods.

# The vector of filenumbers to test for non-blocking reads
$EventServer::READVEC = '';
# The vector of filenumbers to test for non-blocking writes
$EventServer::WRITEVEC = '';

#%REGISTRY;	# Holds registered objects
#%IO_OBJS;	# Hash of objects registered as i/o driven objects
#%TIMED_OBJS;	# Hash of objects registered as interval/timer driven objects
#%SIGNALLED_OBJS;# Hash of objects registered as signal driven objects
#%SIGNAL_*;	# A global for each signal
#@SIGNALS;	# Array of all trappable signals
#%PID_WAIT_OBJS	# Hash of objects registered to wait for terminating children
#%IMMEDIATELY_SIGNALLED_OBJS;# Under development
#%EVENT_OBJS    # Hash of objects registered as event objects
#@EVENT         # Array of received events

# The maximum time to allow before timing out client calls.
#$MAX_INACTIVE_SERVER_TIME;

#$EINTR, $EBADF, $EINVAL, $EFAULT, $WNOHANG; # System constants
#$TIME_CLASS	# The class to use for timings.

#$ALARM_ON - boolean used in alarm handling
#$LAST_TIME - A time object used to store times for the server loop

#The order for clients to be triggered in.
$EventServer::ORDERED_KEYS = 
  EventServer::OrderedKeys->new();

#%SIGNAL_VALUES # Signal values for POSIX

############################################################
# 
#         Initialization
# 
############################################################

$EventServer::SERVER_CLASS->init();

sub init {
    my($self) = @_;

    # The maximum amount of time allowed before the server must do something.
    $self->setMaximumInactiveServerTime(60);

    $self->_setSignals();

    $self->_setConstantsAndTimeClass();
}

sub setMaximumInactiveServerTime {
    $EventServer::MAX_INACTIVE_SERVER_TIME = $_[1];
}

sub maximumInactiveServerTime {
    $EventServer::MAX_INACTIVE_SERVER_TIME;
}

sub hasPosix {$EventServer::HAS_POSIX}

sub _setSignals {
    my($self) = @_;
    
    my ($sig,@c_sigs,@sigs,$sig_mod,$sig_val);
    @c_sigs = split(' ',$Config::Config{'sig_name'});

    if ( $self->hasPosix() ) {
	$EventServer::CHLD_SIG = 'CHLD';

	if ($c_sigs[0] eq 'ZERO') {
	    $sig_val = 0;
	} else {
	    for ($sig_val = $#c_sigs; $sig_val >= 0; $sig_val--) {
		if ($c_sigs[$sig_val] eq 'ALRM') {
		    $sig_val = &POSIX::SIGALRM() - $sig_val;
		    last;
		}
	    }
	}

	for (;$sig_val <= $#c_sigs; $sig_val++) {
	    $self->_notTrappable($c_sigs[$sig_val]) && next;
	    if ($c_sigs[$sig_val] =~ /^C(HI?)?LD$/) {
		$c_sigs[$sig_val] = $EventServer::CHLD_SIG;
	    }
	    $EventServer::SIGNAL_VALUES{$c_sigs[$sig_val]} = 
		$sig_val;
	    push(@sigs,$c_sigs[$sig_val]);
	}
    } else {

	# All trappable signals.
	foreach $sig ( @c_sigs ) {
	    $self->_notTrappable($sig) && next;
	    if ($sig =~ /^C(HI?)?LD$/) {
		$EventServer::CHLD_SIG = $sig;
	    }
	    push(@sigs,$sig);
	}
    }

    $self->_setAllSignals(@sigs);
}

sub _notTrappable {
    my($self,$signal) = @_;
    $signal eq 'KILL' || $signal eq 'STOP' || $signal eq 'ZERO';
}

sub canonicalSignalName {
    my($self,$signal) = @_;
    $signal =~ /^C(HI?)?LD$/ ? $EventServer::CHLD_SIG:$signal;
}

sub isValidSignal {
    my($self,$signal) = @_;
    my($sig);
    $signal = $self->canonicalSignalName($signal);

    foreach $sig ( $self->allSignals() ) {
	$sig eq $signal && return 1;
    }
    0;
}

sub _setAllSignals {shift;@EventServer::SIGNALS = @_;}

sub allSignals {@EventServer::SIGNALS}

sub _setConstantsAndTimeClass {
    my($self) = @_;
    $self->_setConstants();

    my $os_class = 'EventServer::' . $Config::Config{'osname'};
    my $timeClass = eval {$os_class->timeClass()};
    $self->_setTimeClass(
	     $@ ? ('EventServer::Time') : @{$timeClass});
    return;

}

sub _setConstants {
    my($self) = @_;

    # Initialize constants.
    # EINTR EBADF EINVAL EFAULT WNOHANG
    my($include);

    # Is POSIX available?
    if ( $self->hasPosix() ) {
	$self->_setEINTR(POSIX::EINTR());
    	$self->_setEBADF(POSIX::EBADF());
    	$self->_setEINVAL(POSIX::EINVAL());
    	$self->_setEFAULT(POSIX::EFAULT());
    	$self->_setWNOHANG(POSIX::WNOHANG());
    } else {
	# Will have to set Errno and Wait constants from different modules
	if (eval {require Errno; 1;} && !$@) {
	    $self->_setEINTR(Errno::EINTR());
	    $self->_setEBADF(Errno::EBADF());
	    $self->_setEINVAL(Errno::EINVAL());
	    $self->_setEFAULT(Errno::EFAULT());
	} elsif (eval {require Sys::Errno; 1;} && !$@) {
	    $self->_setEINTR(Sys::Errno::EINTR());
	    $self->_setEBADF(Sys::Errno::EBADF());
	    $self->_setEINVAL(Sys::Errno::EINVAL());
	    $self->_setEFAULT(Sys::Errno::EFAULT());
	} elsif (eval {package main; require 'errno.ph'; 1;} && !$@) {
	    $self->_setEINTR(main::EINTR());
	    $self->_setEBADF(main::EBADF());
	    $self->_setEINVAL(main::EINVAL());
	    $self->_setEFAULT(main::EFAULT());
	} elsif (eval {package main; require 'sys/errno.ph'; 1;} && !$@) {
	    $self->_setEINTR(main::EINTR());
	    $self->_setEBADF(main::EBADF());
	    $self->_setEINVAL(main::EINVAL());
	    $self->_setEFAULT(main::EFAULT());
	} else {
	    if (-e '/usr/include/errno.h') {
	    	$include = 'errno.h';
	    } elsif (-e '/usr/include/sys/errno.h') {
	    	$include = 'sys/errno.h';
	    } else {
	    	die "Error - unable to find the Errno constants required.\n",
	    }
	    die "Error - unable to find the Errno constants required.\n",
	        "You could try running h2ph on errno.h (or sys/errno.h), or\n",
	    	"you could try creating a restricted Errno.pm file - \n",
	    	"the following C program will do that: (save it into\n",
	    	"file errno.c, compile using 'cc -o errno errno.c' and then\n",
	    	"execute './errno > Errno.pm' to create the Errno module)\n",
	    	"\n#include <$include>\n\nmain(){\n\t",
	    	'printf("package Errno;\n");',"\n\t",
	    	'printf("sub EINTR  {%d}\n",EINTR);',"\n\t",
	    	'printf("sub EBADF  {%d}\n",EBADF);',"\n\t",
	    	'printf("sub EINVAL {%d}\n",EINVAL);',"\n\t",
	    	'printf("sub EFAULT {%d}\n",EFAULT);',"\n\t",
	    	'printf("1;\n");',
	    	"\n}\n";
	}
	if (eval {require Wait; 1;} && !$@) {
    	    $self->_setWNOHANG(Wait::WNOHANG());
	} elsif (eval {require Sys::Wait; 1;} && !$@) {
    	    $self->_setWNOHANG(Sys::Wait::WNOHANG());
	} elsif (eval {package main; require 'wait.ph'; 1;} && !$@) {
    	    $self->_setWNOHANG(main::WNOHANG());
	} elsif (eval {package main; require 'sys/wait.ph'; 1;} && !$@) {
    	    $self->_setWNOHANG(main::WNOHANG());
	} else {
	    if (-e '/usr/include/wait.h') {
	    	$include = 'wait.h';
	    } elsif (-e '/usr/include/sys/wait.h') {
	    	$include = 'sys/wait.h';
	    } else {
	    	die "Error - unable to find the Wait constants required.\n",
	    }
	    die "Error - unable to find the Wait constants required.\n",
	        "You could try running h2ph on wait.h (or sys/wait.h), or\n",
	    	"you could try creating a restricted Wait.pm file - \n",
	    	"the following C program will do that: (save it into\n",
	    	"file wait.c, compile using 'cc -o wait wait.c' and then\n",
	    	"execute './wait > Wait.pm' to create the Wait module)\n",
	    	"\n#include <$include>\n\nmain(){\n\t",
	    	'printf("package Wait;\n");',"\n\t",
	    	'printf("sub WNOHANG  {%d}\n",WNOHANG);',"\n\t",
	    	'printf("1;\n");',
	    	"\n}\n";
	}
    }
}

sub _setEINTR {$EventServer::EINTR = $_[1];}

sub _setEBADF {$EventServer::EBADF = $_[1];}

sub _setEINVAL {$EventServer::EINVAL = $_[1];}

sub _setEFAULT {$EventServer::EFAULT = $_[1];}

sub _setWNOHANG {$EventServer::WNOHANG = $_[1];}

sub _setTimeClass {
    shift;
    $EventServer::TIME_CLASS = shift;
    $EventServer::TIME_CLASS->initialize(@_);
}

sub timeClass {$EventServer::TIME_CLASS}

sub testSysCall {

    # To use for testing that the gettimeofday call works correctly
    # use as $SERVER->testSysCall($SYS_gettimeofday);
    # returns true if it works, else false. A wrong value for
    # $SYS_gettimeofday could cause a core dump.

    my($self,$const) = @_;
    my $buff =  " " x 1024;
    my $dbuff =  " " x 1024;
    my($t1,$t2);
    eval {syscall($const,$buff,$dbuff)};
    if($@){return 0};
    $t2 = time;
    $t1 = (unpack("l l",$buff))[0];
    ($t1 == $t2 || $t1 + 1 == $t2);
} 

############################################################
# 
#         Signal handling (including alarms and timeouts)
# 
############################################################

sub setSignalHandlers {
    my($self) = @_;
    my($sig,$subname);

    foreach $sig ($self->allSignals()) {
	$self->_notTrappable($sig) && next;
	$subname = $self->signalHandlerFor($sig);
	$self->installHandlerIfSpecial($sig,$subname);
    }
}

sub isSpecialSignalHandler {
    my($self,$signal) = @_;
    $signal eq 'ALRM' || $signal eq $EventServer::CHLD_SIG ||
	$signal eq 'ILL' || $signal eq 'SEGV' || $signal eq 'BUS' ||
	    $signal eq 'PIPE';
}

sub signalHandlerFor {
    my($self,$signal) = @_;
    $self->isValidSignal($signal) || 
	die "Error: Attempt to get handler for invalid signal $signal\n";

    if ( $self->isSpecialSignalHandler($signal) ) {
	return $self->signalHandlerForSpecialSignal($signal);
    } elsif ($self->hasPosix()) {
	return $self->posixSignalHandlerFor($signal);
    } else {
	return $self->defaultSignalHandlerFor($signal);
    }
}

sub signalHandlerForSpecialSignal {
    my($self,$signal) = @_;
    $signal =~ tr/A-Z/a-z/;
    $self->hasPosix() ? 
	'EventServer::Signal::posix_' . $signal :
	'EventServer::Signal::resetting_' . $signal;
}

sub posixSignalHandlerFor {
    my($self,$signal) = @_;

    my($handler);
    $handler = $self->_handlerPrefix() . $signal;

    unless ( defined(&{$handler}) ) {
	# sub handler {$handler++} $handler=0;
	eval sprintf('sub %s {$%s++;} $%s=0;',$handler,$handler,$handler);
    	$@ && $self->unblock_all_signals();

#	$format = 'sub %s {$%s++;';
#	$format .= '$EventServer::SERVER_CLASS->';
#	$format .='triggerImmediatelySignalledClients(\'%s\');}$%s=0;';
#	@args = ($handler,$handler,$signal,$handler)
#       eval sprintf($format,@args);

    }
    $handler;
}

sub unblock_all_signals {
    my($self) = @_;

    # This is because on IRIX a die in the handler
    # leaves the signal blocked (IRIX bug I think).

    $self->hasPosix() && POSIX::sigprocmask(
			$EventServer::SIG_UNBLOCK,
			$EventServer::FULL_MASK);
}

sub defaultSignalHandlerFor {
    my($self,$signal) = @_;
    
    my $handler = $self->_handlerPrefix() . $signal;
    unless ( defined(&{$handler}) ) {
	# sub handler {$SIG{'$signal'} = \&handler ; $handler++ } $handler=0;
	eval sprintf('sub %s {$SIG{\'%s\'} = \&%s;$%s++;} $%s=0;',
		     $handler,$signal,$handler,$handler,$handler);
    }
    $handler;
}

sub triggerImmediatelySignalledClients {
    my($self,$signal) = @_;
    my $hash = 
	$EventServer::IMMEDIATELY_SIGNALLED_OBJS{$signal} ||
	    return;
    my(%hash) = %{$hash};	# Make a copy to work on
    my($obj,$i);
    my $ordered_keys = $self->orderedKeysRef();

    for ( $i = 0; $i <= $ordered_keys->size() ; $i++ ) {
	$obj = delete($hash{$ordered_keys->key_at($i)}) || next;
	$obj->trigger($self);
    }
    foreach $obj (values %hash) {
	$obj->trigger($self);
    }
}

sub installHandlerIfSpecial {
    my($self,$signal,$subname) = @_;
    if ( $self->isSpecialSignalHandler($signal) ) {
	$self->installHandler($signal,$self->signalHandlerFor($signal));
    }
}

sub installHandlerIfNotSpecial {
    my($self,$signal,$subname) = @_;
    unless ( $self->isSpecialSignalHandler($signal) ) {
	$self->installHandler($signal,$self->signalHandlerFor($signal));
    }
}

sub installHandler {
    my($self,$signal,$subname) = @_;

    if ( $self->hasPosix() ) {
      POSIX::sigaction( $EventServer::SIGNAL_VALUES{$signal},
		     POSIX::SigAction->new($subname,
			       $EventServer::FULL_MASK)
		        );
    } else {
	$SIG{$signal} = \&{$subname};
    }
}

sub installDefaultHandlerIfSpecial {
    my($self,$signal,$subname) = @_;
    if ( $self->isSpecialSignalHandler($signal) ) {
	$self->installDefaultHandler($signal,$self->signalHandlerFor($signal));
    }
}

sub installDefaultHandlerIfNotSpecial {
    my($self,$signal,$subname) = @_;
    unless ( $self->isSpecialSignalHandler($signal) ) {
	$self->installDefaultHandler($signal,$self->signalHandlerFor($signal));
    }
}

sub installDefaultHandler {
    my($self,$signal) = @_;
    $SIG{$signal} = 'DEFAULT';
}

sub _handlerPrefix {'EventServer::Signal::'}

sub executeInArrayContextWithTimeout {
    my($self,$timeout,$timeoutReturn,$errorReturn,$funcRef,@args) = @_;
    my(@return,$timeleft,$alarmTime,$t);
    $t = int($timeout);
    $timeout = $t + 1;
    $timeout > 0 || die "Invalid TIMEOUT parameter. Must be > 0";

    my $old_alarm = alarm(0);
#    defined($old_alarm) || ($old_alarm = 0);
    $EventServer::ALARM_ON = 0;
    if ( $old_alarm ) {
	# There was an old alarm, so set the new one to the smaller
	# of the given one and the old one.
	$alarmTime = ($timeout > $old_alarm) ? $old_alarm : $timeout;
	$timeleft = $self->timeClass()->now();
	eval { $EventServer::ALARM_ON = 1;alarm($alarmTime);
		@return = &$funcRef(@args); 
		alarm(0); $EventServer::ALARM_ON = 0; 
	     } ;
    	$@ && $self->unblock_all_signals();
	alarm(0);
	$EventServer::ALARM_ON = 0;
	if ($@) {
	    if($@ eq "T\n") {
		if ($timeout >= $old_alarm) {
		    # If we timed out, then just timeout the nested timer
		    $EventServer::ALARM_ON = 1;
		    kill 'ALRM',$$;
		} else {
		    @return = ($timeoutReturn);
		}
	    } else {
		@return = ($errorReturn,$@)
	    }
	}
	$timeleft = $self->timeClass()->now()->newFromDiff($timeleft);
	$timeleft = $old_alarm - $timeleft->wholeSecondsRoundedDown();
	$EventServer::ALARM_ON = 1;

	# Two options are available if somehow the '$timeleft' has dropped
	# to zero or less - either time out the nested call, or
	# give another second. I'll give another second for now.
	alarm(($timeleft > 0) ? $timeleft : 1);
    } else {
	# No old alarm
	eval { $EventServer::ALARM_ON = 1; alarm($timeout);
		@return = &$funcRef(@args); 
		alarm(0); $EventServer::ALARM_ON = 0; 
	     } ;
    	$@ && $self->unblock_all_signals();
	alarm(0);
	$EventServer::ALARM_ON = 0;
	if ($@) {
	    if($@ eq "T\n") {
		@return = ($timeoutReturn);
	    } else {
		@return = ($errorReturn,$@)
	    }
	}
    }

    @return;
}

############################################################
# 
#         Dealing with the registry
# 
############################################################
sub _addTimedObjectOnKey {
    my($self,$key,$obj) = @_;
    $EventServer::TIMED_OBJS{$key->key()} = $obj;
    $EventServer::REGISTRY{$key->key()} = $obj;
}

sub _removeTimedObjectOnKey {
    my($self,$key) = @_;
    delete($EventServer::TIMED_OBJS{$key->key()});
    delete($EventServer::REGISTRY{$key->key()});
}

sub _addIOObjectOnKey {
    my($self,$key,$obj) = @_;
    $EventServer::IO_OBJS{$key->key()} = $obj;
    $EventServer::REGISTRY{$key->key()} = $obj;
}

sub _removeIOObjectOnKey {
    my($self,$key) = @_;
    delete($EventServer::IO_OBJS{$key->key()});
    delete($EventServer::REGISTRY{$key->key()});
}

sub _addEventObject {
    my($self,$event,$key,$obj) = @_;
    my $eventHash = $EventServer::EVENT_OBJS{$event};
    unless ($eventHash) {
	$EventServer::EVENT_OBJS{$event} = $eventHash = {};
    }
    $eventHash->{$key->key()} = $obj;
    $EventServer::REGISTRY{$key->key()} = $obj;
}

sub _removeEventObject {
    my($self,$event,$key) = @_;
    delete($EventServer::REGISTRY{$key->key()});
    my $eventHash = $EventServer::EVENT_OBJS{$event} ||
	return 0;
    delete($eventHash->{$key->key()});
    unless (%{$eventHash}) {
	delete($EventServer::EVENT_OBJS{$event});
    }
    1;
}

sub _addSignalObject {
    my($self,$signal,$key,$obj) = @_;
    my $sigHash = $EventServer::SIGNALLED_OBJS{$signal};
    unless ($sigHash) {
	$EventServer::SIGNALLED_OBJS{$signal} = $sigHash = {};
	$self->installHandlerIfNotSpecial($signal);
    }
    $sigHash->{$key->key()} = $obj;
    $EventServer::REGISTRY{$key->key()} = $obj;
}

sub _removeSignalObject {
    my($self,$signal,$key) = @_;
    delete($EventServer::REGISTRY{$key->key()});
    my $sigHash = $EventServer::SIGNALLED_OBJS{$signal} ||
	return 0;
    delete($sigHash->{$key->key()});
    unless (%{$sigHash}) {
	delete($EventServer::SIGNALLED_OBJS{$signal});
    }

    $self->_possiblyRemoveSignalHandlerFor($signal);
    1;
}

sub _possiblyRemoveSignalHandlerFor {
    my($self,$signal) = @_;
    $EventServer::IMMEDIATELY_SIGNALLED_OBJS{$signal} ||
	$EventServer::SIGNALLED_OBJS{$signal} ||
	    $self->installDefaultHandlerIfNotSpecial($signal);
}

sub _addImmediateSignalObject {
    my($self,$signal,$key,$obj) = @_;
    my $sigHash = 
	$EventServer::IMMEDIATELY_SIGNALLED_OBJS{$signal};
    unless ($sigHash) {
	$EventServer::IMMEDIATELY_SIGNALLED_OBJS{$signal} =
	                                           $sigHash = {};
	$self->installHandlerIfNotSpecial($signal);
    }
    $sigHash->{$key->key()} = $obj;
    $EventServer::REGISTRY{$key->key()} = $obj;
}
sub _removeImmediateSignalObject {
    my($self,$signal,$key) = @_;
    delete($EventServer::REGISTRY{$key->key()});
    my $sigHash = 
	$EventServer::IMMEDIATELY_SIGNALLED_OBJS{$signal} ||
	    return 0;
    delete($sigHash->{$key->key()});
    unless (%{$sigHash}) {
	delete($EventServer::IMMEDIATELY_SIGNALLED_OBJS{
	                               $signal});
    }
    $self->_possiblyRemoveSignalHandlerFor($signal);
    1;
}

sub _addPIDWaitObject {
    my($self,$pid,$key,$obj) = @_;
    my $sigHash = $EventServer::PID_WAIT_OBJS{$pid};
    unless ($sigHash) {
	$EventServer::PID_WAIT_OBJS{$pid} = $sigHash = {};
    }
    $sigHash->{$key->key()} = $obj;
    $EventServer::REGISTRY{$key->key()} = $obj;
}

sub _removePIDWaitObject {
    my($self,$pid,$key) = @_;
    delete($EventServer::REGISTRY{$key->key()});
    my $sigHash = $EventServer::PID_WAIT_OBJS{$pid} ||
	return 0;
    delete($sigHash->{$key->key()});
    unless (%{$sigHash}) {
	delete($EventServer::PID_WAIT_OBJS{$pid});
    }
    1;
}

sub _registerObject {
    my($self,$class,$obj,@args) = @_;
    my($registryKey,$registryKeyObj,$registryEntry,$i);

    if (ref($obj) ne 'EventServer::RegistryKey') {
	$registryKey = join('|',$class,$obj,map(defined($_)?$_:'',@args));
	#Don't register if the entry is identical to one which exists.
	unless ($registryKeyObj = $EventServer::REGISTRY{$registryKey}) {
	    $registryKeyObj = EventServer::RegistryKey->new($registryKey);
	    $EventServer::REGISTRY{$registryKey} = 
		$class->new($self,$registryKeyObj,$obj,@args) || return undef;
    	}
    } else {
	# Already registered, so change entry.
	$registryEntry = $EventServer::REGISTRY{$obj->key()} ||
				return undef;
	$obj = $registryEntry->object();

	# Calls to alter an existing entry are allowed to just
	# specify the the first few arguments, and have the
	# others taken from the original entry.
	my @oldArgs = $registryEntry->extraPassedArgs();
	for ($i = 0; $i <= $#args; $i++) {$oldArgs[$i] = $args[$i];}

	$registryEntry->deregisterFrom($self);
	$registryKeyObj = $self->_registerObject($class,$obj,@oldArgs);
    }

    $registryKeyObj;
}

sub cancelRegistration {
    my($self,$registryKeyObj) = @_;
    my $registryEntry = 
	$EventServer::REGISTRY{$registryKeyObj->key()} ||
				 return 0;
    $registryEntry->deregisterFrom($self);
    1;
}

sub registerIntervalClient {
    my($self,$obj,$interval,$funcRef,$arg) = @_;
    $self->_registerObject(
	     'EventServer::IntervalClientWrapper',
			$obj,$interval,$funcRef,$arg);
}

sub registerTimedClient {
    my($self,$obj,$interval,$funcRef,$arg) = @_;
    $self->_registerObject('EventServer::TimedClientWrapper',
			$obj,$interval,$funcRef,$arg);
}

sub registerIOClient {
    my($self,$obj,$mode,$socket,$rFunc,$wFunc,$rwFunc,$arg) = @_;
    $self->_registerObject('EventServer::IOClientWrapper',
			   $obj,$mode,$socket,$rFunc,$wFunc,$rwFunc,$arg);
}

sub registerSignalClient {
    my($self,$obj,$signal,$funcRef,$arg) = @_;
    $signal = $self->canonicalSignalName($signal);
    $self->_registerObject('EventServer::SignalClientWrapper',
			       $obj,$signal,$funcRef,$arg);
}

sub registerChildTerminationClient {
    my($self,$obj,$pid,$funcRef,$arg) = @_;
    $self->_registerObject(
             'EventServer::ChildTerminationClientWrapper',
			       $obj,$pid,$funcRef,$arg);
}
sub alwaysReturnChildTerminationStatus {
    $EventServer::ChildTerminationClientWrapper::RETURN_STATUS = $_[1];
}

sub registerEventClient {
    my($self,$obj,$event,$funcRef,$arg) = @_;
    $self->_registerObject(
             'EventServer::EventClientWrapper',
			       $obj,$event,$funcRef,$arg);
}

sub registerImmediateSignalClient {
    die "registerImmediateSignalClient is NOT IMPLEMENTED\n";
    my($self,$obj,$signal,$funcRef,$arg) = @_;
    $signal = $self->canonicalSignalName($signal);
    $self->_registerObject(
	      'EventServer::ImmediateSignalClientWrapper',
			       $obj,$signal,$funcRef,$arg);
}

sub triggerOnDeregistering {
    my($self,$registryKeyObj,$funcRef) = @_;
    my $registryEntry = 
	$EventServer::REGISTRY{$registryKeyObj->key()} ||
					 return undef;
    $registryEntry->triggerOnDeregistering($funcRef) ? $registryEntry : undef;
}

############################################################
# 
#         The server loop
# 
############################################################

sub forkWithChildRetainingClients {
    my($self,@registryKeys) = @_;
    my $pid = fork();
    if (defined($pid) && $pid == 0) {
	# The child
	my(%registry,$key,$obj);
	foreach $key (@registryKeys) {
	    $registry{$key->key()} = 
		delete($EventServer::REGISTRY{$key->key()});
	}
	foreach $obj (values %EventServer::REGISTRY) {
	    $obj->deregisterFrom($self);
	}
	%EventServer::REGISTRY = %registry;
    }
    $pid;
}

sub startServer {$EventServer::SERVER_CLASS->_startServer}

sub _startServer {
    my($self) = @_;

    $self->hasClients() || return $self->noClients();

    $self->setSignalHandlers();

    $EventServer::LAST_TIME = $self->timeClass()->now();

    for (;;) {
	eval { $self->serverLoop() };
	if ($@) {
	    $self->unblock_all_signals();
	    print STDERR $@;
    	} else {
	    last;
	}
    }

    # Remove all signal handlers before we exit.
    my($sig);
    foreach $sig ( $self->allSignals() ) {
	$self->installDefaultHandler($sig);
    }

}

sub _print_all_clients {
    my($self,$obj) = @_;
    print STDERR "These are the clients registered on entering the loop\n";
    foreach $obj (values %EventServer::REGISTRY) {
	print STDERR '  ',$obj->_as_String(),"\n";
    }
    print STDERR "\n";
}

sub serverLoop {
    my($self) = @_;
    my($obj,$objs,$ordered_keys,$i,%seen);

    $ordered_keys = $self->orderedKeysRef();

    for(;;) {

	# Are there any clients registered?
	$self->hasClients() || $self->noClients() || last;

	$EventServer::DEBUG && $self->_print_all_clients();

	$objs = {};
	%seen = ();

	$self->IOPendingClients($objs);
	$self->signalledClients($objs);
	$self->timedOutClients($objs);
	$self->terminatedPIDClients($objs);
	$self->eventClients($objs);

	# First do any objects in order from the ordered list.
	for ( $i = 0; $i <= $ordered_keys->size() ; $i++ ) {
	    $obj = delete($objs->{$ordered_keys->key_at($i)}) || next;
	    if ( $obj->isIOObject() ) {
		# Just make sure that any i/o objects which are multiply
		# registered on the same fileno are really triggerble
		if ( $seen{$obj->filenum()} ) {
		    $obj->ioPendingTest($self) || next;
		} else {
		    $seen{$obj->filenum()} = 1;
		}
	    }
	    $obj->trigger($self);
	}

	# Then do any left in whatever order.
        foreach $obj (values %{$objs}) {
	    if ( $obj->isIOObject() ) {
		# Just make sure that any i/o objects which are multiply
		# registered on the same fileno are really triggerble
		if ( $seen{$obj->filenum()} ) {
		    $obj->ioPendingTest($self) || next;
		} else {
		    $seen{$obj->filenum()} = 1;
		}
	    }
	    $obj->trigger($self);
	}
    }
}

sub orderedKeysRef {$EventServer::ORDERED_KEYS}

sub hasClients {scalar(%EventServer::REGISTRY)}

sub noClients {$_[0]->_noClients();}

sub addEvent {
    my($self,$event) = @_;
    push(@EventServer::EVENTS,$event);
}

sub eventClients {
    my($self,$objs) = @_;
    $objs ||= {};
    my($event,$eventHash,$key,$obj);

    while($event = shift(@EventServer::EVENTS)) {
	$eventHash = $EventServer::EVENT_OBJS{$event} ||
	    next;
	while ( ($key,$obj) = each %{$eventHash} ) {
	    $objs->{$key} = $obj;
	}
    }
    $objs;
}

sub terminatedPIDClients {
    my($self,$objs) = @_;
    $objs ||= {};
    my($pid,$key,$obj,$sigHash);

    #Accumulate the dead children
    for(;;) {
	$pid = waitpid(-1,$EventServer::WNOHANG);
	last if (!defined($pid) || $pid < 1);
	$sigHash = $EventServer::PID_WAIT_OBJS{$pid} || next;
	while ( ($key,$obj) = each %{$sigHash} ) {
	    $obj->setTerminationStatus($?);
	    $objs->{$key} = $obj;
	}
    }

    $objs;
}

sub signalledClients {
    my($self,$objs) = @_;
    $objs ||= {};
    my($sig,$sigHash,$key,$obj,$handler);

    foreach $sig ($self->allSignals()) {
	$handler = $self->_handlerPrefix() . $sig;
	if (${$handler}) {
	    $sigHash = $EventServer::SIGNALLED_OBJS{$sig};
	    unless ($sigHash){
		${$handler} = 0;
		next;
	    }
	    while ( ($key,$obj) = each %{$sigHash} ) {
		$obj->setRepeat(${$handler});
		$objs->{$key} = $obj;
	    }
            ${$handler} = 0;
	}
    }
    $objs;
}

sub timedOutClients {
    my($self,$objs) = @_;
    $objs ||= {};
    my($time_now,$time_diff,$obj);
    $time_now = $self->timeClass()->now();
    $time_diff = $time_now->newFromDiff(
			    $EventServer::LAST_TIME);
    $EventServer::LAST_TIME = $time_now;
    foreach $obj (values %EventServer::TIMED_OBJS) {
	if ($obj->registryKey()) {
	    if ( $obj->isTimedOut($time_diff) ) {
		$objs->{$obj->registryKey()->key()} = $obj;
	    }
	} else {
	    $obj->deregisterFrom($self);
	}
    }
    $objs;
}

sub IOPendingClients {
    my($self,$objs) = @_;
    $objs ||= {};
    my($temp,$selectTimeout,$time_now,$diff,$obj,$rout,$wout);
    my($reset,$nfound,$sig);

    # Set the select timeout.

    # Start at the maximum allowed - maximumInactiveServerTime.
    $temp = $self->timeClass()->newFromSeconds(
		  $self->maximumInactiveServerTime() );
    $selectTimeout = \$temp;
    $time_now = $self->timeClass()->now();
    $diff = $time_now->newFromDiff($EventServer::LAST_TIME);

    # And use minimum left for all timeouts if that is smaller.
    foreach $obj (values %EventServer::TIMED_OBJS) {
	if ($obj->registryKey()) {
	    $obj->decrementTimeAndSetMin($diff,$selectTimeout);
	} else {
	    $obj->deregisterFrom($self);
	}
    }
    $temp = $$selectTimeout->isPositive() ? $$selectTimeout->time() : 0;

    # If any events pending, set 0 timeout
    if (@EventServer::EVENTS) {$temp = 0}

    if ($temp > 0) {
	# Or if any signals have been received, timeout is 0
	foreach $sig ($self->allSignals()) {
	    if (${$self->_handlerPrefix() . $sig}) {
	        $temp = 0;
	        last;
	    }
        }
    }

    # Check for io until the timeout is up or a signal is received
    $nfound = select($rout = $EventServer::READVEC,
		 $wout = $EventServer::WRITEVEC,
		 undef,$temp);
    if ($nfound == -1) {
	$self->_selectError($!+0,"$!");
    } elsif ($nfound > 0) {
	$reset = 0;
	foreach $obj (values %EventServer::IO_OBJS) {
	    if ($obj->registryKey()) {
		if ( $obj->isReady($self,$rout,$wout) ) {
		    $objs->{$obj->registryKey()->key()} = $obj;
		}
	    } else {
		$obj->deregisterFrom($self);
		$reset = 1;
	    }
	}
	if ($reset) {
	    $self->_resetVectors();
	}
    }
    $objs;
}

sub _noClients {
    print STDERR "Error: No clients are registered with the server\n";
    exit;
}
sub _addFilenoToReadVector {
    my($self,$f) = @_;vec($EventServer::READVEC,$f,1) = 1;
}
sub _removeFilenoFromReadVector {
    my($self,$f) = @_;vec($EventServer::READVEC,$f,1) = 0;
}
sub _addFilenoToWriteVector {
    my($self,$f) = @_;vec($EventServer::WRITEVEC,$f,1) = 1;
}
sub _removeFilenoFromWriteVector {
    my($self,$f) = @_;vec($EventServer::WRITEVEC,$f,1) = 0;
}

sub _selectError {
    my($self,$errno,$errStr) = @_;
    # no real need to even test for interrupts, could just ignore them
    if ($errno == $EventServer::EINTR) {
	return;
    } elsif ($errno == $EventServer::EBADF) {
	$self->_resetVectors(); return;
    } elsif ($errno == $EventServer::EINVAL) {
	die "Internal Error (EINVAL)";
    } elsif ($errno == $EventServer::EFAULT) {
	die "Internal Error (EFAULT)";
    }
}

sub _resetVectors {
    my($self) = @_;
    $EventServer::READVEC = "";
    $EventServer::WRITEVEC = "";
    EventServer::IOClientWrapper->_resetVectors($self,
			values %EventServer::IO_OBJS);
}

############################################################
#
# Exporting. This class allows the public methods to be exported
# as functions, since they can be used that way. The export
# mechanism is inherited by subclasses. Exported functions are
# listed in the EventServer::Functions package
#
############################################################

sub import {
    # Pass the imports from EventServer::Functions
    require Exporter;

    my $this = shift;
    local $Exporter::ExportLevel = 1;
    Exporter::import('EventServer::Functions',@_);
}

############################################################
# 
#               Functions space
# 
############################################################
package EventServer::Functions;

@EventServer::Functions::EXPORT = 
(
 'register_interval_client',
 'register_timed_client',
 'register_io_client',
 'register_signal_client',
 'register_child_termination_client',
 'trigger_on_deregistering',
 'cancel_registration',
 'fork_with_child_retaining_clients',
 'start_server',
 'execute_in_array_context_with_timeout',
 'register_event_client',
 'add_event',
 'maximum_inactive_server_time',
 'set_maximum_inactive_server_time',
 'ordered_keys_ref',
 'always_return_child_termination_status',
 );

sub always_return_child_termination_status {
    $EventServer::ChildTerminationClientWrapper::RETURN_STATUS = $_[0];
}

sub ordered_keys_ref {
    $EventServer::SERVER_CLASS->
	orderedKeysRef(@_);
}

sub maximum_inactive_server_time {
    $EventServer::SERVER_CLASS->
	maximumInactiveServerTime(@_);
}

sub set_maximum_inactive_server_time {
    $EventServer::SERVER_CLASS->
	setMaximumInactiveServerTime(@_);
}

sub add_event {
    $EventServer::SERVER_CLASS->
	addEvent(@_);
}

sub register_interval_client {
    $EventServer::SERVER_CLASS->
	registerIntervalClient(@_);
}

sub register_timed_client {
    $EventServer::SERVER_CLASS->
	registerTimedClient(@_);
}

sub register_io_client {
    $EventServer::SERVER_CLASS->
	registerIOClient(@_);
}

sub register_signal_client {
    $EventServer::SERVER_CLASS->
	registerSignalClient(@_);
}

sub register_child_termination_client {
    $EventServer::SERVER_CLASS->
	registerChildTerminationClient(@_);
}

sub register_event_client {
    $EventServer::SERVER_CLASS->
	registerEventClient(@_);
}

sub trigger_on_deregistering {
    $EventServer::SERVER_CLASS->
	triggerOnDeregistering(@_);
}

sub cancel_registration {
    $EventServer::SERVER_CLASS->
	cancelRegistration(@_);
}

sub fork_with_child_retaining_clients {
    $EventServer::SERVER_CLASS->
	forkWithChildRetainingClients(@_);
}

sub start_server {
    $EventServer::SERVER_CLASS->
	startServer(@_);
}

sub execute_in_array_context_with_timeout {
    $EventServer::SERVER_CLASS->
	executeInArrayContextWithTimeout(@_);
}

############################################################
# 
#               Signal handler space
# 
############################################################
package EventServer::Signal;
sub posix_alrm {
    $EventServer::Signal::ALRM++;
    $EventServer::ALARM_ON && die "T\n";
}
sub resetting_alrm {
    $SIG{'ALRM'} = \&EventServer::Signal::resetting_alrm;
    $EventServer::Signal::ALRM++;
    $EventServer::ALARM_ON && die "T\n";
}
sub posix_cld {
    $EventServer::Signal::CLD++;
}
sub resetting_cld {
    $SIG{'CLD'} = \&EventServer::Signal::resetting_cld;
    $EventServer::Signal::CLD++;
}
sub posix_chld {
    $EventServer::Signal::CHLD++;
}
sub resetting_chld {
    $SIG{'CHLD'} = \&EventServer::Signal::resetting_chld;
    $EventServer::Signal::CHLD++;
}
sub posix_child {
    $EventServer::Signal::CHILD++;
}
sub resetting_child {
    $SIG{'CHILD'} = \&EventServer::Signal::resetting_child;
    $EventServer::Signal::CHILD++;
}

sub posix_ill {
    $EventServer::Signal::ILL++;
    die "Illegal instruction caught\n";
}
sub resetting_ill {
    $SIG{'ILL'} = \&EventServer::Signal::resetting_ill;
    $EventServer::Signal::ILL++;
    die "Illegal instruction caught\n";
}
sub posix_segv {
    $EventServer::Signal::SEGV++;
    die "Segmentation violation caught\n";
}
sub resetting_segv {
    $SIG{'SEGV'} = \&EventServer::Signal::resetting_segv;
    $EventServer::Signal::SEGV++;
    die "Segmentation violation caught\n";
}
sub posix_bus {
    $EventServer::Signal::BUS++;
    die "Bus error caught\n";
}
sub resetting_bus {
    $SIG{'BUS'} = \&EventServer::Signal::resetting_bus;
    $EventServer::Signal::BUS++;
    die "Bus error caught\n";
}
sub posix_pipe {
    $EventServer::Signal::PIPE++;
    die "Write on pipe/socket with no one to read it\n";
}
sub resetting_pipe {
    $SIG{'PIPE'} = \&EventServer::Signal::resetting_pipe;
    $EventServer::Signal::PIPE++;
    die "Write on pipe/socket with no one to read it\n";
}

############################################################
# 
#               OrderedKeys
# 
############################################################
package EventServer::OrderedKeys;
sub new {bless []}
sub size {$#{$_[0]}}
sub key_at {$_[0]->[$_[1]]}
sub push_keys {push(@{shift(@_)},@_)}
sub pop_key {pop(@{shift(@_)})}
sub shift_key {shift(@{shift(@_)})}
sub unshift_keys {unshift(@{shift(@_)},@_)}
sub insert_keys_before {
    my($self,$index,@keys) = @_;
    splice(@{$self},$index,0,@keys);
}
sub delete_key_at {
    my($self,$index) = @_;
    splice(@{$self},$index,1);
}
############################################################
# 
#               RegistryKey
# 
############################################################
package EventServer::RegistryKey;
sub new {bless \$_[1]}
sub key {${$_[0]}}
############################################################
# 
#               Time
# 
############################################################
package EventServer::Time;
sub initialize {}
sub now {bless \time()}
sub newFromSeconds {bless \$_[1];}
sub copy {bless \${$_[0]};}
sub newFromDiff {bless    \(${$_[0]} - ${$_[1]}) }
sub original {${$_[0]}}
sub isPositive {${$_[0]} > 0}
sub smallerTime { (${$_[0]} > ${$_[1]}) ? $_[1] : $_[0] }
sub time {${$_[0]}}
sub wholeSecondsRoundedDown {${$_[0]}}
############################################################
# 
#               Gettimeofday & os specific classes
# 
############################################################
package EventServer::Gettimeofday;
sub initialize {
    $EventServer::Gettimeofday::SYS_gettimeofday = $_[1];
    $EventServer::Gettimeofday::BUFF = ' ' x 1024;
    $EventServer::Gettimeofday::DUMMY_BUFF = ' ' x 1024;
}
sub now {
    syscall($EventServer::Gettimeofday::SYS_gettimeofday,
		$EventServer::Gettimeofday::BUFF,
		$EventServer::Gettimeofday::DUMMY_BUFF);
    bless ['now',
	   unpack('l l',$EventServer::Gettimeofday::BUFF)];
}
sub newFromSeconds {
    if ($_[1] =~ /^(\d+)$/) {
	return bless [$_[1],$1,0];
    } elsif ($_[1] =~ /^(\d*).(\d+)$/) {
	return bless [$_[1],$1,substr($2,0,6) . "0"x(6-length($2))];
    } else {
	die "Error unrecognized time";
    }
}
sub copy {bless [@{$_[0]}]}
sub newFromDiff {bless ['diff',$_[0]->[1]-$_[1]->[1], $_[0]->[2]-$_[1]->[2]]}
sub original {$_[0]->[0]}
sub isPositive {($_[0]->[1] > 0) || ( ($_[0]->[1] == 0) && ($_[0]->[2] > 0)) }
sub smallerTime {
    my($self,$other,$smaller) = @_;
    if ($self->[1] > $other->[1]) {
	$smaller = $other;
    } elsif ($self->[1] == $other->[1]) {
	if ($self->[2] > $other->[2]) {
	    $smaller = $other;
	} else {
	    $smaller = $self;
	}
    } else {
	$smaller = $self;
    }
    $smaller;
}
sub time {$_[0]->[1] + ($_[0]->[2]/1000000)}
sub wholeSecondsRoundedDown { ($_[0]->[2] < 0) ? $_[0]->[1] - 1 : $_[0]->[1] }

############################################################
package EventServer::sunos;

sub timeClass {
    $Config::Config{'osvers'} =~ /^4/ || die;
    $EventServer::SERVER_CLASS->testSysCall(116) || die;
    ['EventServer::Gettimeofday',116];
}

############################################################
package EventServer::irix;

sub timeClass {
    $Config::Config{'osvers'} =~ /^5/ || die;
    $EventServer::SERVER_CLASS->testSysCall(1128) || die;
    ['EventServer::Gettimeofday',1128];
}

    
############################################################
package EventServer::linux;

sub timeClass {
    $EventServer::SERVER_CLASS->testSysCall(78) || die;
    ['EventServer::Gettimeofday',78];
}

############################################################
# 
#               ClientWrapper
# 
############################################################
package EventServer::ClientWrapper;
use Carp qw(croak);

# ClientWrapper's are arrays with the first item the registryKey,
# the second item the object being registered, and other
# items specific to the subclass. Subclasses need to
# define all the methods containing 'subclassResponsibility()'
sub _as_String {
    substr($_[0]->registryKey()->key(),29);
}

sub subclassResponsibility {
    die "subclassResponsibility called at ",join(":",caller),"\n";
}

sub isIOObject {0;}

sub new {
    my($self,$server,$registryKey,$obj,@args) = @_;
    $self->testArgsValid($server,@args) || 
	croak("Arguments for registering '$self' client are invalid");
    my $inst = bless [$registryKey,$obj],$self;
    $inst->setArgs($server,@args);
    $inst->addToServerLoop($server);
    $inst;
}

# Test the extra args passed to subclasses to see if they are valid.
# Returns true/false
sub testArgsValid {
    my($self,$server,@args) = @_;
    subclassResponsibility();
}

# Set the extra args passed to subclasses into the receiver (self)
sub setArgs {
    my($self,$server,@args) = @_;
    subclassResponsibility();
}

# Add the receiver (self) into the server's loop by adding to the
# appropriate server section
sub addToServerLoop {
    my($self,$server) = @_;
    subclassResponsibility();
}

# Remove the receiver (self) from the server's loop by reversing
# the 'addToServerLoop' actions
sub removeFromServerLoop {
    my($self,$server) = @_;
    subclassResponsibility();
}
sub registryKey {$_[0]->[0];}
sub object {$_[0]->[1];}
sub deregisterFunc {$_[0]->[2];}
sub OFFSET {3;}

# The reference to the function to be called when this object is triggered
sub funcRef {subclassResponsibility()}

# The argument that was passed from the client at registration
sub funcArg {subclassResponsibility()}

# Extra arguments to be included when the 'funcRef' is called
sub serverArg {subclassResponsibility()}

# The extra arguments that are passed to subclasses of this
# class on creation of a new object (this is used to allow
# existing objects to have their registered entry altered,
# and allowing the registration to use any previously defined
# arguments).
sub extraPassedArgs {subclassResponsibility()}

sub trigger {shift->tellClient(@_);}

sub tellClient {
    my($self,$server) = @_;
    $self->registryKey() || return $self->deregisterFrom($server);
    my($err) = $server->executeInArrayContextWithTimeout(
	$server->maximumInactiveServerTime(),0,1,
	  \&EventServer::ClientWrapper::_makeClientCall,$self);
    $err && $self->deregisterFrom($server);
}

sub _makeClientCall {
    my($self) = @_;
    &{$self->funcRef()}($self->object(),$self->registryKey(),
			$self->serverArg(),$self->funcArg()
			);
    ();
}

sub deregisterFrom {
    my($self,$server) = @_;
    $self->registryKey() || return;
    $self->removeFromServerLoop($server);
    $self->[0] = 0;
    $self->deregisterFunc() && $server->executeInArrayContextWithTimeout(
	$server->maximumInactiveServerTime(),0,0,$self->deregisterFunc(),
	$self->object(),$self->serverArg(),$self->funcArg());
}

sub triggerOnDeregistering {
    my($self,$funcRef) = @_;
    ref($funcRef) eq 'CODE' || return 0;
    $self->[2] = $funcRef;
}

############################################################
# 
#               IntervalClientWrapper
# 
############################################################
package EventServer::IntervalClientWrapper;

@EventServer::IntervalClientWrapper::ISA = 
    qw(EventServer::ClientWrapper);

sub testArgsValid {
    my($self,$server,$interval,$funcRef,$arg) = @_;
    ($interval > 0) && ($interval < 10**8) && (ref($funcRef) eq 'CODE');
}

sub addToServerLoop {
    my($self,$server) = @_;
    $server->_addTimedObjectOnKey($self->registryKey(),$self);
}
sub removeFromServerLoop {
    my($self,$server) = @_;
    $server->_removeTimedObjectOnKey($self->registryKey());
}

sub setArgs {
    my($self,$server,$interval,$funcRef,$arg) = @_;
    $self->setInterval($server->timeClass()->newFromSeconds($interval));
    $self->setTimeLeft($self->interval()->copy());
    $self->setFuncRef($funcRef);
    $self->setFuncArg($arg);
    $self;
}

sub interval {$_[0]->[3];}
sub setInterval {$_[0]->[3] = $_[1];}
sub timeLeft {$_[0]->[4];}
sub setTimeLeft {$_[0]->[4] = $_[1];}
sub funcRef {$_[0]->[5];}
sub setFuncRef {$_[0]->[5] = $_[1];}
sub funcArg {$_[0]->[6];}
sub setFuncArg {$_[0]->[6] = $_[1];}
sub serverArg {$_[0]->interval()->original()}
sub extraPassedArgs {($_[0]->interval(),$_[0]->funcRef(),$_[0]->funcArg())}

sub decrementTimeAndSetMin {
    my($self,$time,$minRef) = @_;
    $self->setTimeLeft($self->timeLeft()->newFromDiff($time));
    $$minRef = $$minRef->smallerTime($self->timeLeft());
}

sub isTimedOut {
    my($self,$time) = @_;
    $self->setTimeLeft($self->timeLeft()->newFromDiff($time));
    !$self->timeLeft()->isPositive();
}

sub trigger {
    my($self,$server) = @_;
    $self->setTimeLeft($self->interval()->copy());
    shift->tellClient($server);
}

sub timeElapsed {
    my($self,$server,$time) = @_;
    $self->isTimedOut($time) && $self->trigger($server);
}

############################################################
# 
#               TimedClientWrapper
# 
############################################################
package EventServer::TimedClientWrapper;

@EventServer::TimedClientWrapper::ISA =
    qw(EventServer::IntervalClientWrapper);

sub trigger {
    my($self,$server) = @_;
    $self->tellClient($server);
    $self->deregisterFrom($server);
}

############################################################
# 
#               IOClientWrapper
# 
############################################################
package EventServer::IOClientWrapper;

@EventServer::IOClientWrapper::ISA =
    qw(EventServer::ClientWrapper);
#%RFILENO;	# Fileno's and number of read objects with that fileno
#%WFILENO;	# Fileno's and number of write objects with that fileno

sub isIOObject {1;}

sub testArgsValid {
    my($self,$server,$mode,$socket,$rFunc,$wFunc,$rwFunc,$arg) = @_;
no strict 'refs';
    return defined(fileno($socket)) && 
	($mode eq 'r' || $mode eq 'w' || $mode eq 'rw')
	&& (ref($rFunc) eq 'CODE') && (ref($wFunc) eq 'CODE')
	&& (ref($rwFunc) eq 'CODE');
use strict 'refs';
}
sub setArgs {
    my($self,$server,$mode,$socket,$rFunc,$wFunc,$rwFunc,$arg) = @_;
    $self->[3] = $socket;
no strict 'refs';
    $self->[4] = fileno($socket);
use strict 'refs';
    $self->[5] = $mode;
    $self->[6] = $rFunc;
    $self->[7] = $wFunc;
    $self->[8] = $rwFunc;
    $self->[9] = $arg;
    if ($mode eq 'r') {
	$self->setFuncRef($rFunc);
    } elsif ($mode eq 'w') {
	$self->setFuncRef($wFunc);
    } else {
	$self->setFuncRef($rwFunc);
    }
    $self;
}
sub extraPassedArgs {
    ($_[0]->mode(),$_[0]->socket(),$_[0]->rFunc(),
	$_[0]->wFunc(),$_[0]->rwFunc(),$_[0]->funcArg())
}
sub socket {$_[0]->[3];}
sub filenum {$_[0]->[4];}
sub mode {$_[0]->[5];}
sub rFunc {$_[0]->[6];}
sub wFunc {$_[0]->[7];}
sub rwFunc {$_[0]->[8];}
sub funcArg {$_[0]->[9];}
sub setFuncRef {$_[0]->[10] = $_[1];}
sub funcRef {$_[0]->[10];}
sub serverArg {$_[0]->socket()}

sub addToServerLoop {
    my($self,$server) = @_;
    $server->_addIOObjectOnKey($self->registryKey(),$self);

    my $mode = $self->mode();
    my $fileno = $self->filenum();
    my $num;
    if ($mode =~ /r/) {
	$num = $EventServer::IOClientWrapper::RFILENO{$fileno};
    	if (!defined($num) || $num <= 0) {
	    $server->_addFilenoToReadVector($fileno);
	    $num = 1;
	} else {
	    $num++;
	}
	$EventServer::IOClientWrapper::RFILENO{$fileno} = $num;
    }
    if ($mode =~ /w/) {
	$num = $EventServer::IOClientWrapper::WFILENO{$fileno};
    	if (!defined($num) || $num <= 0) {
	    $server->_addFilenoToWriteVector($fileno);
	    $num = 1;
	} else {
	    $num++;
	}
	$EventServer::IOClientWrapper::WFILENO{$fileno} = $num;
    }
}
sub removeFromServerLoop {
    my($self,$server) = @_;
    $server->_removeIOObjectOnKey($self->registryKey());

    my $mode = $self->mode();
    my $fileno = $self->filenum();
    my $num;
    if ($mode =~ /r/) {
	$num = $EventServer::IOClientWrapper::RFILENO{$fileno};
    	if ($num > 1) {
	    $num--;
	} else {
	    $server->_removeFilenoFromReadVector($fileno);
	    $num = 0;
	}
	$EventServer::IOClientWrapper::RFILENO{$fileno} = $num;
    }
    if ($mode =~ /w/) {
	$num = $EventServer::IOClientWrapper::WFILENO{$fileno};
    	if ($num > 1) {
	    $num--;
	} else {
	    $server->_removeFilenoFromWriteVector($fileno);
	    $num = 0;
	}
	$EventServer::IOClientWrapper::WFILENO{$fileno} = $num;
    }
}

sub _resetVectors {
    my($self,$server,@IO_Objs) = @_;
    my($fileno,$obj);
    %EventServer::IOClientWrapper::RFILENO = ();
    %EventServer::IOClientWrapper::WFILENO = ();

    foreach $obj (@IO_Objs) {
no strict 'refs';
	$fileno = fileno($obj->socket());
use strict 'refs';
	if (defined($fileno) && ($obj->filenum() == $fileno) ) {
	    $obj->addToServerLoop($server);
	} else {
	    $obj->deregisterFrom($server);
	}
    }
}

sub trigger {
    my($self,$server) = @_;
    $self->tellClient($server);
}

sub ioNonblocking {
    my($self,$server,$rout,$wout) = @_;
    $self->isReady($server,$rout,$wout) && $self->tellClient($server);
}

sub isReady {
    my($self,$server,$rout,$wout) = @_;
    my $mode = $self->mode();
    
    $mode eq 'r' ? 
	$self->r_is_ready($rout,$wout) :
	($mode eq 'w' ? $self->w_is_ready($rout,$wout) :
			$self->rw_is_ready($rout,$wout) );
}

sub rw_is_ready {
    my($self,$rout,$wout) = @_;
    my $fileno = $self->filenum();
    if ( vec($rout,$fileno,1) ) {
	if (vec($wout,$fileno,1)) {
	    $self->setFuncRef($self->rwFunc());
	    return 1;
	} else {
	    $self->setFuncRef($self->rFunc());
	    return 1;
	}
    } elsif(vec($wout,$fileno,1)) {
	$self->setFuncRef($self->wFunc());
	return 1;
    }
    0;
}

sub r_is_ready {
    my($self,$rout,$wout) = @_;
    if ( vec($rout,$self->filenum(),1) ) {
	$self->setFuncRef($self->rFunc());
	return 1;
    }
    0;
}

sub w_is_ready {
    my($self,$rout,$wout) = @_;
    if ( vec($wout,$self->filenum(),1) ) {
	$self->setFuncRef($self->wFunc());
	return 1;
    }
    0;
}

sub ioPendingTest {

    # Does a single select i/o poll to test for any i/o.
    # This is in case there are multiple clients registered
    # on a socket - the extra test may be needed in this case.

    my($self,$server) = @_;
    my($r,$w,$mode,$n);
    $mode = $self->mode();

    if ($mode eq 'r') {
	vec($r,$self->filenum(),1) = 1;
	if (select($r,undef,undef,0) > 0) {
	    $self->setFuncRef($self->rFunc());
	    return 1;
	}
    } elsif ($mode eq 'w') {
	vec($w,$self->filenum(),1) = 1;
	if (select(undef,$w,undef,0) > 0) {
	    $self->setFuncRef($self->wFunc());
	    return 1;
	}
    } else {
	vec($r,$self->filenum(),1) = 1;
	vec($w,$self->filenum(),1) = 1;
	if (select($r,$w,undef,0) > 0) {
	    return $self->isReady($server,$r,$w);
	}
    }
    0;
}


############################################################
# 
#               SignalClientWrapper
# 
############################################################
package EventServer::SignalClientWrapper;

@EventServer::SignalClientWrapper::ISA = 
    qw(EventServer::ClientWrapper);

sub testArgsValid {
    my($self,$server,$signal,$funcRef,$arg) = @_;
    $server->isValidSignal($signal) && ref($funcRef) eq 'CODE';
}
sub setArgs {
    my($self,$server,$signal,$funcRef,$arg) = @_;
    $self->[3] = $signal;
    $self->[4] = $funcRef;
    $self->[5] = $arg;
}
sub signal {$_[0]->[3]}
sub funcRef {$_[0]->[4]}
sub funcArg {$_[0]->[5]}
sub serverArg {($_[0]->signal(),$_[0]->repeat())}
sub setRepeat {$_[0]->[6] = $_[1];}
sub repeat {$_[0]->[6];}
sub extraPassedArgs {($_[0]->signal(),$_[0]->funcRef(),$_[0]->funcArg())}

sub addToServerLoop {
    my($self,$server) = @_;
    $server->_addSignalObject($self->signal(),$self->registryKey(),$self);
}
sub removeFromServerLoop {
    my($self,$server) = @_;
    $server->_removeSignalObject($self->signal(),$self->registryKey());
}

sub trigger {
    my($self,$server) = @_;
    $self->tellClient($server);
    $self->setRepeat(0);
}

sub signalled {
    my($self,$server,$signal,$sigRepeat) = @_;
    $self->setRepeat($sigRepeat);
    $self->trigger($server);
}

############################################################
# 
#               ImmediateSignalClientWrapper
# 
############################################################
package EventServer::ImmediateSignalClientWrapper;

@EventServer::ImmediateSignalClientWrapper::ISA = 
    qw(EventServer::SignalClientWrapper);
sub serverArg {$_[0]->signal()}

sub testArgsValid {
    my($self,$server,$signal,$funcRef,$arg) = @_;
    $server->hasPosix() && $signal ne 'ALRM' && 
	$server->isValidSignal($signal) && ref($funcRef) eq 'CODE';
}

sub addToServerLoop {
    my($self,$server) = @_;
    $server->_addImmediateSignalObject($self->signal(),$self->registryKey(),
				       $self);
}

sub removeFromServerLoop {
    my($self,$server) = @_;
    $server->_removeImmediateSignalObject($self->signal(),
					  $self->registryKey());
}

sub trigger {
    my($self,$server) = @_;
    $self->tellClient($server);
}

sub signalled {
    my($self,$server,$signal) = @_;
    $self->trigger($server);
}

############################################################
# 
#               ChildTerminationClientWrapper
# 
############################################################
package EventServer::ChildTerminationClientWrapper;
$EventServer::ChildTerminationClientWrapper::RETURN_STATUS = 0;

@EventServer::ChildTerminationClientWrapper::ISA = 
    qw(EventServer::SignalClientWrapper);

sub testArgsValid {
    my($self,$server,$pid,$funcRef,$arg) = @_;
    $pid > 0 && ref($funcRef) eq 'CODE';
}
sub setArgs {
    my($self,$server,$signal,$funcRef,$arg) = @_;
    $self->[3] = $signal;
    $self->[4] = $funcRef;
    $self->[5] = $arg;
}
sub setTerminationStatus {$_[0]->[6] = $_[1];}
sub terminationStatus {$_[0]->[6];}
sub serverArg {
    $EventServer::ChildTerminationClientWrapper::RETURN_STATUS ? 
	[$_[0]->signal(), $_[0]->terminationStatus()] : $_[0]->signal();
}

sub addToServerLoop {
    my($self,$server) = @_;
    $server->_addPIDWaitObject($self->signal(),$self->registryKey(),$self);
}
sub removeFromServerLoop {
    my($self,$server) = @_;
    $server->_removePIDWaitObject($self->signal(),$self->registryKey());
}

sub trigger {
    my($self,$server) = @_;
    $self->tellClient($server);
    $self->deregisterFrom($server);
}

sub signalled {
    my($self,$server,$signal) = @_;
    $self->trigger($server);
}

############################################################
# 
#               EventClientWrapper
# 
############################################################
package EventServer::EventClientWrapper;

@EventServer::EventClientWrapper::ISA = 
    qw(EventServer::ClientWrapper);

sub testArgsValid {
    my($self,$server,$event,$funcRef,$arg) = @_;
    $event =~ /./ && ref($funcRef) eq 'CODE';
}
sub setArgs {
    my($self,$server,$event,$funcRef,$arg) = @_;
    $self->[3] = $event;
    $self->[4] = $funcRef;
    $self->[5] = $arg;
}
sub event {$_[0]->[3]}
sub funcRef {$_[0]->[4]}
sub funcArg {$_[0]->[5]}
sub serverArg {$_[0]->event()}
sub extraPassedArgs {($_[0]->event(),$_[0]->funcRef(),$_[0]->funcArg())}

sub addToServerLoop {
    my($self,$server) = @_;
    $server->_addEventObject($self->event(),$self->registryKey(),$self);
}
sub removeFromServerLoop {
    my($self,$server) = @_;
    $server->_removeEventObject($self->event(),$self->registryKey());
}

sub trigger {
    my($self,$server) = @_;
    $self->tellClient($server);
}
############################################################
# 
#               End of ClientWrapper's
# 
############################################################
package EventServer;


1;
__END__

=head1 NAME

EventServer - the all singing, all dancing server.
Handles i/o driven clients, timer driven clients, and interrupt
driven clients, all at the same time. Allows user defined events
also. If there are any more types of clients, please tell me.

=head1 SYNOPSIS

Functions can be imported and used:

    use EventServer;
    
    $r1 = register_timed_client($obj1,$time1,$coderef1);
    $r2 = register_interval_client($obj2,$time2,$coderef2);
    $r3 = register_signal_client($obj3,$signal3,$coderef3);
    $r4 = register_io_client($obj4,$mode4,$coderef4_r,
		$coderef4_w,$coderef4_rw);
    $r5 = register_child_termination_client($obj5,$pid5,$coderef5);
    $r6 = register_event_client($obj6,$eventName6,$coderef6);
    
    trigger_on_deregistering($r1,$coderef7);
    cancel_registration($r3);
    $ordered_keys_ref = ordered_keys_ref();
    
    $time = maximum_inactive_server_time();
    set_maximum_inactive_server_time($time);
    execute_in_array_context_with_timeout($timeout,$timeout_retcode,
		$error_retcode,$coderef,@args);
    
    fork_with_child_retaining_clients($r1,$r2,...);
    
    sub something {add_event($eventName)}
    
    start_server();


Or the class can be used with methods:

    require EventServer;
    $S = EventServer;
    
    $r1 = $S->registerTimedClient($obj1,$time1,$coderef1);
    $r2 = $S->registerIntervalClient($obj2,$time2,$coderef2);
    $r3 = $S->registerSignalClient($obj3,$signal3,$coderef3);
    $r4 = $S->registerIOClient($obj4,$mode4,$coderef4_r,
		$coderef4_w,$coderef4_rw);
    $r5 = $S->registerChildTerminationClient($obj5,$pid5,$coderef5);
    $r6 = $S->registerEventClient($obj6,$eventName6,$coderef6);
    
    $S->triggerOnDeregistering($r1,$coderef7);
    $S->cancelRegistration($r3);
    $ordered_keys_ref = $S->orderedKeysRef();
    
    $time = $S->maximumInactiveServerTime();
    $S->setMaximumInactiveServerTime($time);
    $S->executeInArrayContextWithTimeout($timeout,$timeout_retcode,
		$error_retcode,$coderef,@args);
    
    $S->forkWithChildRetainingClients($r1,$r2,...);
    
    sub something {$S->addEvent($eventName)}
    
    $S->startServer();


=head1 IMPORTANT

The 'ALRM' signal is used extensively, as is alarm().
You should NOT call the function alarm() as this will
corrupt the internal logic of the server. Similarly sleep()
should not be used either, as this is often implemented
in terms of alarm().

Instead use execute_in_array_context_with_timeout() which is better
anyway since it allows multiple clients to set alarms simultaneously
and allows nested alarms. However, for this reason, registering
a client to receive 'ALRM' signals is probably of no use.

Also, if you assign to the %SIG hash, or install signal
handlers through POSIX yourself, then you may corrupt
the logic of the server. If you need to do this for something
other than a signal (e.g. __WARN__), that should be okay,
otherwise you should probably create a subclass to install
the handlers you want (see L<"The SIG hash and signals">
and L<"Creating Subclasses">).

=head1 CONTENTS

L<"NAME">

L<"SYNOPSIS">

L<"IMPORTANT">

L<"CONTENTS">

L<"Function and Method Summary">

L<"Including the server in your program">

L<"Starting the server">

L<"Registering clients (general)">

L<"Registering clients (methods)">

L<"Client order for simultaneous events">

L<"Deregistering clients">

L<"Timeouts within client code">

L<"Forking child processes">

L<"Times and Timing">

L<"The SIG hash and signals">

L<"Example">

L<"Creating Subclasses">

L<"Example subclasses">

L<"Possible problems">

L<"Questions and Answers">

L<"AUTHOR">

L<"COPYRIGHT">

L<"MODIFICATION HISTORY">

=head1 Function and Method Summary

There are 15 public functions/methods:

8 dealing with registering clients;

1 to add user defined events

3 dealing with executing code and timeouts;

1 to fork the process;

and 1 to start the server.

Functions are:

 register_interval_client(O/R,INTERVAL,FUNCREF,ARG)
 register_timed_client(O/R,TIMEOUT,FUNCREF,ARG)
 register_io_client(O/R,MODE,HANDLE,RFUNCREF,WFUNCREF,RWFUNCREF,ARG)
 register_signal_client(O/R,SIGNAL,FUNCREF,ARG)
 register_child_termination_client(O/R,PID,FUNCREF,ARG)
 register_event_client(O/R,EVENT,FUNCREF,ARG)
 
 trigger_on_deregistering(REGISTRY_KEY,FUNCREF)
 cancel_registration(REGISTRY_KEY)
 
 add_event(EVENT)
 
 maximum_inactive_server_time()
 set_maximum_inactive_server_time(TIME)
 execute_in_array_context_with_timeout(TIMEOUT,TRET,ERET,FUNCREF,ARGS)
 
 fork_with_child_retaining_clients(LIST_OF_REGISTRY_KEYS)
 
 start_server();

And defined as methods:

 $SERVER->registerIntervalClient(O/R,INTERVAL,FUNCREF,ARG)
 $SERVER->registerTimedClient(O/R,TIMEOUT,FUNCREF,ARG)
 $SERVER->registerIOClient(O/R,MODE,HANDLE,RFUNCREF,WFUNCREF,RWFUNCREF,ARG)
 $SERVER->registerSignalClient(O/R,SIGNAL,FUNCREF,ARG)
 $SERVER->registerChildTerminationClient(O/R,PID,FUNCREF,ARG)
 $SERVER->registerEventClient(O/R,EVENT,FUNCREF,ARG)
 
 $SERVER->triggerOnDeregistering(REGISTRY_KEY,FUNCREF);
 $SERVER->cancelRegistration(REGISTRY_KEY);
 
 $SERVER->addEvent(EVENT)
 
 $SERVER->maximumInactiveServerTime()
 $SERVER->setMaximumInactiveServerTime(TIME)
 $SERVER->executeInArrayContextWithTimeout(TIMEOUT,TRET,ERET,FUNCREF,ARGS)
 
 $SERVER->forkWithChildRetainingClients(LIST_OF_REGISTRY_KEYS)
 
 $SERVER->startServer();


=head1 Including the server in your program

The server is included in your program with the line

 use EventServer;

to import the functions, or

 require EventServer;

if used as a class.

=head1 Starting the server

The server is started by executing the function or method

=over 4

=item start_server();

=item EventServer->startServer();

In either case, if a subclass has been defined correctly,
then the server will be started using that subclass.

=back

=head1 Registering clients (general)

Clients are registered with the server using any of the 6 registering
methods listed in the next section. They all have various points in common:

1. $SERVER is assumed to be EventServer or a subclass;

2. All registration methods return a RegistryKey object on success
which holds the registration key, and false on failure. (Note
previous versions returned a string - the current version should
be fully compatible with previous versions). The registration key
is unique to the registration, depending on all the parameters passed
to the registration method - i.e a single object can be registered
multiple times using different parameters or registration
methods (multiple *identical* registrations will return the same key,
and will result in only one registration). To alter the parameters
of an existing registration, pass the registration key to the
registration method instead of the object (see 'O/R' below).
But note that this generates a new RegistryKey object since
the registration parameters are now different (the old RegistryKey
object is deregistered, and is essentially useless). Reregistering
an existing registration so that it is identical to another
registration will just derigister the first registration,
returning the existing identical RegistryKey object (i.e.
as stated above, there will only be one registry entry
for identical parameters regardless of how you register them).

3. 'O/R' is the object being registered or the registration key
of an already registered object. The object can be anything
(previous versions restricted it to be class names or objects
that returned true ref() values). This object is passed to
FUNCREF (see below) as the first argument.

4. 'ARG' is anything. It is passed to FUNCREF (see below) as the last argument.
If nothing is passed, then ARG is defaulted to undef();

5. At least one 'FUNCREF' argument is required. All FUNCREF arguments are
CODE references to the function which is executed when the client
is triggered. Where there is more than one FUNCREF to be specified,
the one called will depend on the trigger type. When triggered,
the FUNCREF is called as:

 &FUNCREF(OBJECT,REGISTRY_KEY,some method specific args,ARG);

where:

 OBJECT is the object registered (the 'O' in 'O/R' above);
 REGISTRY_KEY is the registration key for that registration
    (the 'R' in 'O/R' above, returned by registration methods);
 ARG is the last argument passed to the registration method
    ('ARG' above);

This call to FUNCREF takes place within a timeout. The current
maximum timeout value can be retrieved using
maximum_inactive_server_time(), and can be set using
set_maximum_inactive_server_time(). (These access and set the
global $EventServer::MAX_INACTIVE_SERVER_TIME.)
The default value is 60 seconds. Any fatal errors caused by
executing FUNCREF are trapped, and cause the client to be deregistered.
A timeout will also cause the client to be deregistered.

NOTE however that a call to exit() cannot be trapped and will
cause the server process to exit. Similarly, a call to dump() also
cannot be trapped and will cause the server process to core dump.

=head1 Registering clients (methods)

=over 4

=item register_interval_client (O/R,INTERVAL,FUNCREF,ARG)

=item $SERVER->registerIntervalClient(O/R,INTERVAL,FUNCREF,ARG)

INTERVAL is a time (see L<"Times and Timing">). The client is triggered
after every INTERVAL seconds. Triggering effects the function call

 &FUNCREF(OBJECT,REGISTRY_KEY,INTERVAL,ARG);

=item register_timed_client (O/R,TIMEOUT,FUNCREF,ARG)

=item $SERVER->registerTimedClient(O/R,TIMEOUT,FUNCREF,ARG)

TIMEOUT is a time (see L<"Times and Timing">). The client is triggered
after TIMEOUT seconds and then deregistered. Triggering effects the
function call

 &FUNCREF(OBJECT,REGISTRY_KEY,TIMEOUT,ARG);

=item register_io_client(O/R,MODE,HANDLE,RFUNCREF,WFUNCREF,RWFUNCREF,ARG)

=item $SERVER->registerIOClient(O/R,MODE,HANDLE,RFUNCREF,WFUNCREF,RWFUNCREF,ARG)

MODE is 'r', 'w' or 'rw' depending on whether the trigger
should be for input pending (read won't block), output
possible (write won't block) or both. 
HANDLE is the fully qualified package name of the filehandle
which has already been opened, on which i/o is tested.
RFUNCREF, WFUNCREF and RWFUNCREF are three 'FUNCREF's (see above).
If input is pending on HANDLE, this triggers the call

 &RFUNCREF(OBJECT,REGISTRY_KEY,HANDLE,ARG);

if output is possible on HANDLE, this triggers the call

 &WFUNCREF(OBJECT,REGISTRY_KEY,HANDLE,ARG);

and if both input and output won't block, then this triggers the call

 &RWFUNCREF(OBJECT,REGISTRY_KEY,HANDLE,ARG);

If MODE 'r' has been specified, then obviously only RFUNCREF
can ever get called, and similarly if MODE 'w' has been specified,
then only WFUNCREF can ever get called. However, if MODE 'rw'
has been specified, then any of the three functions could be
called depending on what becomes non-blocking first.

In all cases of MODE, all three FUNCREF's must be CODE references.

Note, unlike previous versions, now if you make multiple registrations
for a specific filehandle, then client functions are still only
triggered when they are guaranteed to be non-blocking. To paraphrase,
if any FUNCREF is called, you are guaranteed to be able
to do a sysread(), syswrite() or accept() (whichever is appropriate).

=item register_signal_client (O/R,SIGNAL,FUNCREF,ARG)

=item $SERVER->registerSignalClient(O/R,SIGNAL,FUNCREF,ARG)

SIGNAL is a valid trappable signal. The signals are obtained
from the Config module. (Previous versions specified them
explicitly in subroutines). The 'allSignals' method
retuns the list of signals.

The client is triggered after the signal is trapped (and after
the signal handler has exited). Triggering effects the function
call

 &FUNCREF(OBJECT,REGISTRY_KEY,SIGNAL,NSIGS,ARG);

where

  NSIGS is the number of times the signal was
      received since this function was last called.
  and SIGNAL is the canonical name for the signal
  (which may be different from what was passed in the
  case of 'CHLD'/'CLD'. You can always use either - the
  correct signal name for the system will be used.)

Note that 'ALRM' and 'CLD' (or 'CHLD' or 'CHILD') are specially
handled, and registering for these signals is of little use.
For alarms, use execute_in_array_context_with_timeout(),
and to find out when a child process has died, register
with register_child_termination_client().

Signals which have no clients registered for them will
cause the default action to occur (i.e. they will not 
be trapped).

Signals are not passed to the clients immediately, they
are put into the queue and clients are triggered when
the signal queue is checked. If you need some action to occur
IMMEDIATELY on receipt of the signal, you will need to
create a subclass to handle this. (This is because setting up
an 'immediately signalled' type of client is fraught with
difficulties, and is likely to lead to an unstable
process - I tried it. And that was even without having signal
handlers stacked through recursive calls to it. Mind you,
it should be doable with POSIX signals, and is almost,
but some bug that I haven't tracked down yet seems to
propagate a die past an eval if called from within the
handler, so its not yet implemented for POSIX signals
in the server.)

Signal handlers are NOT installed until the server has
been started (see L<"Starting the server">).

All signal handlers are reset to default if the server
loop exits (see L<"Questions and Answers">).

See also L<"The SIG hash and signals">.

=item register_child_termination_client (O/R,PID,FUNCREF,ARG)

=item $SERVER->registerChildTerminationClient(O/R,PID,FUNCREF,ARG)

PID is the process id of the child process. When that
child dies this triggers the function call

 &FUNCREF(OBJECT,REGISTRY_KEY,DATA,ARG);

Where data is either: the process id of the terminated
child; or an array reference with two items in the
array - the process id and the child termination status
as given by '$?' . The choice of which is returned is set
by calling always_return_child_termination_status() with
a boolean argument - true means return the array reference,
false means return the pid only. The default is false
for backward compatibility.

Note that if forking the server, you should use
fork_with_child_retaining_clients() rather than
just a fork().

=item register_event_client (O/R,EVENT,FUNCREF,ARG)

=item $SERVER->registerEventClient(O/R,EVENT,FUNCREF,ARG)

EVENT is any string. If any client adds the event EVENT into
the server's event loop (using add_event(EVENT)) then this
will trigger the call

 &FUNCREF(OBJECT,REGISTRY_KEY,EVENT,ARG);

for this client. This allows clients for user defined events

=item add_event (EVENT)

=item $SERVER->addEvent(EVENT)

Simply adds the string EVENT to the end of the event queue.
Any clients waiting for this event (registered using the
register_event_client() function) are triggered.

=item always_return_child_termination_status(BOOLEAN)

=item $SERVER->alwaysReturnChildTerminationStatus(BOOLEAN)

Sets whether the register_child_termination_client() call
will trigger a callback with just the child's pid as the third
argument (BOOLEAN true), or a reference to an array holding the
pid and the termination status (BOOLEAN false). Note that this
affects the call dynamically - the trigger checks as its
triggering to see what type of argument it should pass.

The default is false for backward compatibility.

=back

=head1 Client order for simultaneous events

If two events occur simultaneously, or an event occurs
for which more than one client is registered, then more
than one client will be triggered in the same server loop.
You may want to ensure that for any pair of clients, a specific
client is always called before another in this situation.

This can be achieved using the following function:

=over 4

=item ordered_keys_ref()

=item $SERVER->orderedKeysRef()

This method/function returns a reference to an ARRAY type object.
This object holds RegistryKey objects in whatever order you
want to specify. In cases where more than one client is
to be triggered within a single server loop, the order
of the keys within this array determines the ordering of
client activation. For example, this

 $r1 = register_...;
 $r2 = register_...;
 push(@{ordered_keys_ref()},$r2,$r1);

will ensure that in such a case, the client registered on key '$r2'
will always be called before the client registered on key '$r1'.

The object returned by ordered_keys_ref() is actually an object
of class EventServer::OrderedKeys, and there
are several methods in this class which may make it easier
for you to manipulate the array (though just treating it as
an array reference is absolutely fine):

 $order = ordered_keys_ref();
 $order->push_keys(LIST_OF_KEYS);
 $order->pop_key();
 $order->shift_key();
 $order->unshift_keys(LIST_OF_KEYS);
 $order->insert_keys_before(INDEX,LIST_OF_KEYS);
 $order->delete_key_at(INDEX);

=back

=head1 Deregistering clients

There are two methods for deregistering clients. One is to use the
fact that FUNCREF calls have fatal die() errors trapped - which means
that a client can die() when it is triggered, and this will cause that
client to be deregistered. (Timing out will have the same effect,
but is a silly way to do it since all other clients may be blocked
until the timeout is finished).

NOTE that generating an 'ALRM' signal (e.g. with "kill 'ALRM,$$")
will produce a die() since the alarm handler dies. This means
that if you produce an ALRM signal, you are effectively timing
out the client, and hence deregistering it.

The second method is to use the function/method provided:

=over 4

=item cancel_registration (REGISTRY_KEY);

=item $SERVER->cancelRegistration(REGISTRY_KEY);

This deregisters the client that was registered on the key
REGISTRY_KEY.

=back

The server will deregister a client if there are any problems
with it. You can find out when a client is deregistered by
setting a function to be triggered when the client is deregistered
using the function/method:

=over 4

=item trigger_on_deregistering (REGISTRY_KEY,FUNCREF);

=item $SERVER->triggerOnDeregistering(REGISTRY_KEY,FUNCREF);

This returns true (REGISTRY_KEY) on success, false (undef)
on failure. On success, the code reference FUNCREF has
been added to the clients registration such that when
the client is deregistered, this triggers the call:

 &FUNCREF(OBJECT,REGISTRY_KEY,method specific args,ARG);

where the 'method specific args' are determined by the type
of registration used (as specified in the
section L<"Registering clients (methods)">), and the other terms
are as previously defined.

=back

=head1 Timeouts within client code

Note alarm() should not be used (see L<"IMPORTANT">). Instead,
a function/method has been provided which allows for nested timeouts.

=over 4

=item execute_in_array_context_with_timeout (TIMEOUT,TRET,ERET,FUNCREF,ARGS)

=item $SERVER->executeInArrayContextWithTimeout(TIMEOUT,TRET,ERET,FUNCREF,ARGS)

TIMEOUT is a time (see L<"Times and Timing">). This sets the timeout
for the call (note that times are rounded up to the next
integer number of seconds);

TRET is the value/object returned as the first element of the
return array if the call is timed out;

ERET is the value/object returned as the first element of the
return array if the call produces a fatal error;

FUNCREF is the CODE reference which is called;

ARGS are the arguments which are passed to FUNCREF when it is called.

This method calls FUNCREF in an array context (if you want to
make a call in a scalar context, wrap the function and pass
the wrapped function reference, e.g.

 sub wrapper { (scalar_call(@_)) }

and FUNCREF = \&wrapper), with arguments ARGS. i.e the call is

 @ret = &FUNCREF(ARGS);

If the call is not timed out, and does not produce an error,
then the array returned by the FUNCREF call (@ret) is returned.
If a timeout occured, then the array (TRET) is returned, and
if an error occurred during the FUNCREF call, then the
array (ERET, $@) is returned.

This method allows timeouts to be nested - i.e. you can call
this method within another function which is being timed
out by this method.

=item maximum_inactive_server_time()

=item $SERVER->maximumInactiveServerTime()

Returns the current value that this is set to. This determines
the maximum time before triggered clients are timed out. Default
is 60 (seconds).

=item set_maximum_inactive_server_time (TIME)

=item $SERVER->setMaximumInactiveServerTime(TIME)

Sets this value. It should be a positive value.

=back

=head1 Forking child processes

The call fork() works fine, but the resulting child is a copy of the
server with all the clients retained. If the fork is to be followed
by an exec, this is fine. But otherwise, you need to know which
clients are still registered, and which ones you don't want.

Instead of worrying about this, I provide a function/method to fork the
server retaining ONLY those clients you know you want. All other clients
are deregistered in the child.

=over 4

=item fork_with_child_retaining_clients (LIST_OF_REGISTRY_KEYS)

=item $SERVER->forkWithChildRetainingClients(LIST_OF_REGISTRY_KEYS)

This function/method works and returns as fork(): On failure,
undef is returned, on success the process is forked
and the child gets 0 returned while the parent gets the
process id of the child returned.

In addition, only those clients with registry keys specified
as arguments when this method is called, have their registration
retained in the child. (Note that if you are handling signals
in addition to whatever else, you may want to retain those
signal handling clients in the child).

This saves you from needing to think about which clients
need to be deregistered in the child - you only need to consider
which ones need to be kept.

=back

=head1 Times and Timing

Note that all times should be specified in seconds, and can
be fractional (e.g. 2.35). However the fractional part may
be of no use depending on where it is used.

Currently, timing-out code using execute_in_array_context_with_timeout()
has values rounded up to the next highest integer , e.g. '2.35'
will be used as '3', and '2' will be used as '3' (this
latter use is because alarm() can be up to one second less).
This is because alarm() is being used to time out code
in this function, and alarm() only has a 1 second resolution.

Timing in the Interval and Timer client registration is
dependent on the resolution available from a clock timer
used from Perl. If the default time() is used, then
fractional seconds are effectively rounded up to the
next integer, since the times can only be ticked down
in seconds. Resolutions will specify how many digits
after the decimal point are used. The maximum resolution
is one microsecond (six digits after the decimal point).
Non-significant digits may be rounded up or down.

The server specifies the timing method during initialization.
Currently, if syscall() and the gettimeofday() system call
are available, these are used, otherwise time() is used.

However, the availability of the gettimeofday() call
is established with a call to the method timeClass()
in the OS specific class given by the OS name as obtained
from Config, appended to 'EventServer::'.

For example, if this module is run on SunOS, Config
says that the OS name ('osname' parameter) is 'sunos',
in which case the call

 EventServer::sunos->timeClass()

is made. If this call produces a die(), that is trapped,
and the default time class (using time()) is used.
If this does not die, it is assumed to return a
reference to an array, with first element being
the time class to use, and the second any initialization.

For example, in the case of SunOS, this returns

 ['EventServer::Gettimeofday',116];

which specifies to use the Gettimeofday class, and
initializes this class with the syscall number required
to make the call to gettimeofday().

Please tell me what is best on any specific platform,
I'll try to include support for it. Currently automatically
supported are SunOS 4.*, IRIX 5.*, and Linux. You can
add specific OS support just be adding the package
and timeClass() method as shown.

Remember, you can always let it default to the plain Time
class - this is usually sufficient.

=head1 The SIG hash and signals

If you assign to the %SIG hash, or install signal
handlers through POSIX yourself, then you may corrupt
the logic of the server. If you need to do this for anything
other than a signal (e.g. __WARN__), that should be okay,
otherwise you should probably create a subclass to install
the handlers you want (see L<"Creating Subclasses">).

If you want to trap a signal, do it by registering a signal client.
If you want to trap a signal and need to have control during the 
signal handler, then subclass the EventServer class and set
the handler in the subclass. And note that any handler which
dies will deregister any client which sends a signal for
that handler. Its usually a bad idea to do too
much in a signal handler (see L<"Possible problems">. 

However, if you are definitely not going to register any clients
for a particular signal, you can assign your own signal handler
for that signal (though not for ALRM and CHLD).

Terminating children have their pid's removed from the process
list before clients receive the 'CLD' signal. For this reason
you should not wait() for terminating children. If you want to
be notified of this, use the register_child_termination_client()
registration method. For this reason, registering a client
to receive 'CLD' signals is probably of no use.

Signals which have no clients registered for them will
not be trapped.

See also L<"Timeouts within client code">, L<"IMPORTANT"> and
the entries for methods register_signal_client()
and register_child_termination_client().

=head1 Example

Note that you can execute this example with
C<perl5 -x EventServer.pm> assuming you are in
the perl lib directory where you installed this module.

The example program below registers all the various types of clients.

o A timer client (expiring after 3 seconds), which is also told
that it is being deregistered when it dies;

o an interval client (sending a SIGCONT every 4.3 seconds for 4 times,
then deregistering) - on the fourth triggering this client calls
a function to test nested timeouts. That should timeout after 3 seconds,
though an interrupt could terminate it quicker;

o a signal client which also tests re-registering (triggered on receiving
the first 'CONT' from the interval client, at which point it reregisters,
changing the function that is called to 'cont_test2' which makes it catch
the second SIGCONT from the interval client, and then deregister);

o an event client, which waits for the event 'CHECK' - that
event is sent on the third triggering of the interval client.
The Event client calls a nested timeout which tests the
functionality of nested timeouts. That should timeout after 3 seconds,
though an interrupt could terminate it quicker;

o an i/o client, which waits for some input on STDIN (requires a <RETURN>
to be triggered) and then deregisters;

o a child termination client (the process forks right at the beginning,
and the child sleeps for 10 seconds then terminates);

o and finally another signal client which will take two SIGINT's (usually
generated by typing cntrl-C) then deregisters, which means that the next
SIGINT will cause the default signal action to occur (program termination).

Note that the server will terminate when all clients are deregistered
so if you want to see everything you need to run this at least
twice - once you can terminate by giving three cntrl-C's BEFORE all the
other clients have deregistered (you can keep the io client registered
by not typing <RETURN>), and the second time you can let the program
terminate by letting all the clients deregister (two cntrl-C's and
a <RETURN> get rid of the SIGINT client and the io client - all other
clients get deregistered within the first 20 seconds).

#!perl5

 BEGIN {print "Initializing, process id is $$\n";}
 use EventServer;
 
 # Timer test client (after 3 seconds)
 $r = register_timed_client([],3,sub {print STDERR "Timed test\n"})
 	|| die "Timed test not registered";
 
 # Deregistering Trigger test
 trigger_on_deregistering($r,
   sub {print STDERR "Deregistering Trigger test\n"}) ||
 	 die "Deregistering Trigger test not registered";
 
 # Interval test client (every 4.3 seconds, 4 times)
 register_interval_client([],4.3,\&interval_test)
 	|| die "Interval test not registered";
 
 sub interval_test {
     $C++;print STDERR "Interval test $C\n";
     kill 'CONT',$$;
     if ($C == 3) {
	 add_event('CHECK');
     } elsif ($C > 3) {
	 $t=time;
	 execute_in_array_context_with_timeout(2.5,0,0,\&t4_test);
	 print STDERR 'Nested timeout returned after ',time-$t," secs\n";
	 die;
     }
 }
 
 sub t3_test {
     execute_in_array_context_with_timeout(2.5,0,0,
					   sub {select(undef,undef,undef,9)});
 }
 
 sub t4_test {
     execute_in_array_context_with_timeout(6.5,0,0,
					   sub {select(undef,undef,undef,9)});
 }
 
 sub t1_test {
     print STDERR "Event client test\n";
     $t=time;
     execute_in_array_context_with_timeout(6.5,0,0,\&t3_test);
     print STDERR 'Nested timeout returned after ',time-$t," secs\n";
     die;
 }
 
 register_event_client([],'CHECK',\&t1_test) ||
    die "Event test not registered";
 
 # Signal test client (once after first Interval test)
 $r = register_signal_client([],'CONT',\&cont_test)
 	|| die "Signal test not registered";
 
 # Reregistration test client (once after second Interval test)
 sub cont_test {
   print STDERR "Signal test\n";
   register_signal_client($r,'CONT',\&cont_test2)
 }
 sub cont_test2 {print STDERR "Reregistering test\n";die}
 
 # IO test client (once after user types <RETURN>)
 register_io_client([],'r',STDIN,\&io,\&io,\&io) || 
 	die "STDIN test not registered";
 sub io {$l=<STDIN>;print STDERR "IO test: $l";die}
 
 # Child Termination test client (after 10 seconds)
 defined($pid = fork) || die "Couldn't fork";
 if($pid==0){
   #Keep the child around for 10 seconds
   $SIG{'INT'} = 'IGNORE';sleep(10);warn "Child Died\n";exit(23);
 }
 print STDERR "Start child process pid = $pid\n";
 always_return_child_termination_status(1);
 register_child_termination_client([],$pid,
   sub {print STDERR "Child (pid=$_[2]->[0]) terminated with status ",$_[2]->[1]>>8,"\n"}) ||
 	die "Not registered";
 
 # Signal test client (catches 2 ^C, then uses default SIGINT)
 register_signal_client([],'INT',
   sub {$A++;print STDERR "INT caught $A\n";$A>1 && die})
 	|| die "Signal test not registered";
 
 print "Starting server now\n";
 start_server();
 
__END__


=head1 Creating Subclasses

The EventServer server is designed with subclassing in mind.
There is only so much generality that can be catered for in
any class, and specific applications will do much better
by subclassing and specializing.

In making a subclass of the server, the following points are of note:

1.  The server class is specified in the variable

 $EventServer::SERVER_CLASS.

To allow your subclass to handle ALL methods (including signal handling,
initialization and exporting of functions) you need to specify
this variable before require'ing the EventServer.
This is best done as

 package MyServer;
 BEGIN {$EventServer::SERVER_CLASS ||= MyServer;}
 @ISA = qw(EventServer);
 require EventServer;

Note that the @ISA call _MUST_ be before the 'require' since
the require contains initialization calls that need to do
method lookups on $EventServer::SERVER_CLASS.

Making the assignment conditional on the variable being false
allows your class to be subclassed as well.

2.  The initialization is a method called init().
Specifying the SERVER_CLASS variable above will
ensure that the init method is called in the subclass
rather than the EventServer class.

Initialization occurs when EventServer
is require'd.


3.  The initialization sets several system constants:

 EINTR EBADF EINVAL EFAULT WNOHANG

and will produce a fatal error if they cannot be set.

These are set when the method _setConstantsAndTimeClass()
is called from init(), which in turn calls _setConstants().
The constants are set using the
methods _setEINTR(), _setEBADF(), _setEINVAL(), _setEFAULT(),
and _setWNOHANG().

So, for example, to specify the values for SunOS 4, you could
declare the following method in a subclass:

 sub _setConstants {
    my($self) = @_;
    $self->_setEINTR(0x4);
    $self->_setEBADF(0x9);
    $self->_setEINVAL(0x16);
    $self->_setEFAULT(0xe);
    $self->_setWNOHANG(0x1);
 }

4.  The initialization sets and initializes the variable time
class to use. It does this by finding the OS name from Config
($Config{'osname'}) and making the call:

 EventServer::<osname>->timeClass()

where <osname> is the OS name as found from CONFIG.
If this call does not die() (any call to die() is trapped),
then it is assumed to return an array reference to an array
consisting of the time class to use as the first element, and
values to initialize the time class for subsequent elements.

Typically, this would be 'EventServer::Gettimeofday'
as the first element, and the syscall number for the
gettimeofday call as the second element (e.g. SYS_gettimeofday
from syscall.h on many systems). However, you could
explicitly specify the default 'EventServer::Time'
using this method, or a completely different class.

If you roll your own time class, it must have the following
methods implemented appropriately:

 initialize(?)		# Whatever
 now()			# Return an object representing the time now
 newFromSeconds(SECONDS)# Return an object representing SECONDS
 copy()			# Return new object representing the time in 'self'
 newFromDiff(OTHER)	# Return an object representing the time difference 
			# between 'self' and OTHER
 original()		# Return the time in its original format
 isPositive		# Is the time positive? Return boolean
 smallerTime(OTHER)	# Return object with smaller time, 'self' or OTHER
 time()			# Return the time as a number (a float if needed)
 wholeSecondsRoundedDown()# Return time as an integer, ignoring fractions

The method timeClass() gives the class being used to handle
times. Available are EventServer::Time using
the time() function in Perl (resolution 1 second)
and EventServer::Gettimeofday which uses
the gettimeofday() C system call using syscall.

5.  The init() sets the list of signals that can
be registered for. The list is obtained from the
Config module, minus the untrappable KILL and STOP
signals.

6.  The setSignalHandlers() method

The setSignalHandlers() method creates the signal handlers if
necessary, and installs those that are to be permanently
installed. All signals have a signal handler assigned.

Unlike previous versions, in order to elminate possible
reentrancy bugs, the signal handlers do not execute
in subclasses. They are functions in their own namespace
which do the absolute minimum possible (mostly just
incrementing a variable).

To reimplement a signal handler, you need to respecify
the signalHandlerFor() method. This method takes as
argument the signal name, and returns the name of the
handler. The handlers should increment the
global $EventServer::Signal::<SIGNAME>,
e.g. the 'TERM' signal handler should increment
the global $EventServer::Signal::TERM.
(This is all they do by default).

The ALRM handler is implemented slightly differently, and
should not be reimplemented unless you know what you're doing.

Handlers are normally only installed when a client registers
for that signal. However, ALRM and CHLD are permanently
registered. You can specify which handlers are permanently
registered by reimplementing the isSpecialSignalHandler()
method. This returns true for those signals which should
have permanently installed handlers. But note that if
you reimplement this, you should include ALRM and
CHLD (or CLD) among the set of signals which return true.

Note that any handler which is set to die on receipt of
a signal will deregister any client which sends a that
signal.

7.  The server can be started using

 start_server();

or

 EventServer->startServer();

or

 MyServer->startServer();

since startServer() actually starts the server using
the class specified in  $EventServer::SERVER_CLASS


=head1 Example subclasses

The SunOS example is not necessary, and is just here for
illustrative purposes (though can be used).

 ############################################################
 # Subclass for SunOS4. Speeds up initialization and
 # ensures the use of gettimeofday(2) system call.
 # Also SunOS doesn't need to have handlers reinstalled
 # when they are called.
 # NOTE that you can use EventServer
 # on SunOS or any other OS without this subclass.
 # 
 package EventServer_SunOS4;
 
 BEGIN {
   if (`/bin/uname -sr` =~ /^SunOS\s+4/i) {
      $EventServer::SERVER_CLASS ||= 
 	EventServer_SunOS4;
   } else {
      warn "Warning: system is not SunOS4 - using plain EventServer class\n";
   }
 }
 
 @ISA = qw(EventServer);
 require EventServer;
 
 sub _setConstantsAndTimeClass {
     my($self) = @_;
     $self->_setConstants();
     $self->_setTimeClass(EventServer::Gettimeofday,116);
 }

 sub _setConstants {
     my($self) = @_;
     $self->_setEINTR(0x4);
     $self->_setEBADF(0x9);
     $self->_setEINVAL(0x16);
     $self->_setEFAULT(0xe);
     $self->_setWNOHANG(0x1);
 }
 
 # No need to reset signal handlers within signal handlers for SunOS
 # Though this is redundant, since POSIX handlers will be used anyway.
 sub signalHandlerForSpecialSignal {
     my($self,$signal) = @_;
     $signal =~ tr/A-Z/a-z/;
     'EventServer::Signal::posix_' . $signal;
 }
 sub defaultSignalHandlerFor {
     my($self,$signal) = @_;
 
     my $handler = $self->_handlerPrefix() . $signal;
     unless ( defined(&{$handler}) ) {
	 eval sprintf('sub %s {$%s++;die "\n"} $%s=0;',
	 	     $handler,$handler,$handler);
     }
     $handler;
 }
 
 1;
 __END__

=head1 Possible problems

Posting from Todd Hoff

 >From: tmh@ictv.com (Todd Hoff)
 Newsgroups: comp.lang.perl
 Subject: Re: Perl 5: alarm BSD vs. SysV
 Date: 3 Apr 1995 10:38:35 -0700
 Organization: ICTV, Inc.
 Lines: 24
 Message-ID: <3lpbqr$gbm@anxious.ictv.com>
 
 In article <3lomapINN334@calvin.lif.icnet.uk>,
 >Have you guys tried re-setting the signal handler within the
 >handler. Some systems reset the signal handler to default
 >after it is called.
 >
 >sig handler {
 >   $SIG{'ALRM'} = 'handler';
 >   ...
 >}
 
 Each UNIX vendor has chosen which version of the "old" signal semantics
 to emulate, thus signal work is not very portable and bug prone.
 Setting the handler in the handler breaks miserably because an interrupt
 can occur before the handler is set. What sucks is that you are
 unlikley to see problems unless you have a loaded machine or
 high interrupt rate, both of which i usually have :-(
 
 The only solution is for perl to use POSIX signals which are safe 
 (but harder to understand). As an aside do not do anything in a signal 
 handler but set a flag which tells you if you should call a handler
 in the main line logic. Reentrancy bugs are intermitent and nasty.
 -- 
 Todd Hoff     | My words are my own.
 tmh@ictv.com  | And i have all this extra white space...


In addition, perl has the problem that signals can interrupt
a malloc - and this seems prone to causing a SIGSEGV.

The problems are decreased in this server because most of the time it
will probably be in the select call, in which case signals are likely
to hit it mostly during a select call, not a malloc. But you should be
prepared for your server to die, and have some automated procedure to
restart it - like a cron job. This is a general problem of signals and
perl (and C), not a specific problem of the server.

If you want the general problem illustrated in a simple way, the
following is nice and clear, and will give a core dump after a
few seconds:

 @a = qw(1, 2, 3, 4);
 $sig_happened = 0;
 
 $SIG{'ALRM'} = 'sig_handler';
 alarm(1);
 
 while (1)
 {
     foreach $z (@a)
     {
 	reset_handler() if ($sig_happened);
     }
 }
 
 sub reset_handler
 {
     print "Reset the handler\n";
     $sig_happened = 0;
     $SIG{'ALRM'} = 'sig_handler';
     alarm(1);
 }
 
 sub sig_handler
 {
     $sig_happened = 1;
 }
__END__


=head1 Questions and Answers

Q1. How do I exit the start_server loop.

A1. When there are no more clients registered with the
server, the method noClients() is called. If this method
returns a false value then the start_server loop terminates.
If this returns a true value, then the loop continues.

The default action is for the server to print the message

 Error: No clients are registered with the server ...

to STDERR and then exit.

To change the default behaviour, create a subclass
which redefines noClients, and use that subclass.
For example

 package MyServer;
 BEGIN {$EventServer::SERVER_CLASS ||= MyServer;}
 @ISA = qw(EventServer);
 require EventServer;
 sub noClients {0} # Just terminate the loop if no clients left.

Note that you don't need this to go into a separate module -
it can be in your main program as an initialization if this
is all you need, e.g.

 $EventServer::SERVER_CLASS ||= MyServer;
 @MyServer::ISA = qw(EventServer);
 require EventServer;
 sub MyServer::noClients {0}

=head1 AUTHOR

This software was developed by Jack Shirazi in the Biomedical
Informatics Unit at the Imperial Cancer Research Fund, and was partly
funded by the European Union Computer Executive Committee under
EP6708 `APPLAUSE: Application and Assessment of Parallel Programming
Using Logic'.

=head1 COPYRIGHT

Copyright 1995 Imperial Cancer Research Fund, UK. All rights reserved.

This software is distributed under the same terms as Perl.

This program is free software; you can redistribute it and/or modify
it under the terms of either:

a) the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any
later version, or

b) the "Artistic License" which comes with Perl.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either
the GNU General Public License or the Artistic License for more details.

=head1 MODIFICATION HISTORY

=over 4

=item Future Versions (to do)

Use setitimer if available. Make into SelfLoader class(es)
when SelfLoader comes on stream? Allow clients option
of executing in own process (psuedo-threaded). Global
store across processes. Mobile server. Shadow server.
Automatic termination on dregistration of specific clients.

=item Version 2.3, 5th February 1996

Added option to return child termination status. Fixed missing
assignment of arguments in EventServer::_resetVectors which
prevented closed handles from being deregistered. Made
into EventServer package only, with Server::Server::EventDriven
subclass for backward compatibility. Fixed docs. Fixed
posix_alarm and resetting_alarm to posix_alrm and resetting_alrm.
Altered registering semantics in-line with docs (was not returning
existing registry key for identical registration).

=item Version 2.2, 8th August 1995

Bug fix - isReady had wrong number of args.

=item Version 2.1, 10th July 1995

Altered signal installers to make SEGV, ILL, PIPE and BUS
special handlers which are permanently installed.
Added 'Free software' copyright.

=item Version 2.0, 14th June 1995

Added Andrew Wilcox patch to fix recursive includes in _tryH.
Invalid arguments to register methods now produce a croak.
Added Linux subclass (courtesy of Andrew Wilcox). Added
Exportable function wrappers to methods. Enabled server
loop to terminate. Podified, cleaned and added to documentation.
Removed any redundant %SIG 'tie' code. Added dummy buffer
for gettimeofday syscall to workaround perl 5.000 (and 5.001?)
bug - now works with any perl5 version.

Changed constants retrieval to not try so hard - now just
looks at POSIX, Errno and Wait (in perl4 & 5 versions) and
uses those - also uses classes to get gettimeofday syscall
value. Now uses signals listed in Config. Now asks the os
specific class (obtained from Config) for time class and any
time class initialization.

Rewrote sig handlers to mostly do nothing except set a
global. Rewrote and modularized server loop so that it
is easier to alter the behaviour in a subclass. Wrapper
objects now trigger on same 'trigger' method. Loop
goes through one iteration, then triggers all clients
in a user defined order (or random order for any not
in the user defined order). IO clients are guaranteed
to be triggered only if ready - even if multiple clients
are registered on the same handle.

Added support for POSIX signals - uses them if available.
Fixed leaked alarm time logic. Added nested alarm time tests
to example. Changed all classes to be nested under EventServer.
Changed registry keys to be RegistryKey objects. Added client
defined events. Altered documentation. Put server loop
in eval loop. Added signal unblocking to handle IRIX bug.

=item Version 1.2, 10th April 1995

Altered various internal methods (mainly associated with init)
to allow subclassing to be more straightforward. Provided
example subclasses for SunOS4 and IRIX5.
Altered signal handlers to reset signal handler after being
called to provide support for systems which need it.
Removed tie on %SIG due to flakiness (%SIG no longer read-only).
Moved the methods required by tie into a separate package
space. Altered _tryH and _tryCompiling. Fixed bug in 
executeInArrayContextWithTimeout (wasn't handling recursive
timeouts !). Made 'use strict' and C<-w> clean (though the filehandles
need 'no strict' in 3 places). Documentation altered.

=item Version 1.1, 5th April 1995

EventServer::Time::copy &
EventServer::Time::newFromDiff bugfixed,
EventServer::_noClients error message changed,
added triggerOnDeregistering method and support methods
and altered documentation appropriately.

=item Version 1.0, 10th March 1995

Base version.

=back

=cut
