package Server::Server::EventDriven;
BEGIN {$EventServer::SERVER_CLASS ||= Server::Server::EventDriven;}
@ISA = qw(EventServer);
require EventServer;
1;
__END__
