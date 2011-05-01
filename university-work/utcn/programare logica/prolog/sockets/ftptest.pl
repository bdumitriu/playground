:-use_module(library(sockets)).

make_socket(Socket,Host,Port):-
	socket('AF_INET',Socket),
	socket_bind(Socket,'AF_INET'(Host,Port)),
	socket_listen(Socket,10).

wait_for_connect(Socket,NewStreams):-
	socket_select([s1-Socket],NewStreams,30:0,[],_).

my_write([]).
my_write([H|T]):-
	put_code(H),
	my_write(T).

test:-
	make_socket(Socket,'localhost',22),
	wait_for_connect(Socket,Streams),
	Streams = [s1-connection(_,Stream)],
	socket_buffering(Stream,write,_,unbuf),
	write(Stream,'Scrie acolo ce vrei tu si da Enter.'),
	write(Stream,'\r\n'),
	write(Stream,'N-o sa vezi in telnet ce scrii, da scrie linistit.'),
	write(Stream,'\r\n'),
	write(Stream,'Dupa ce dai Enter, uita-te-n Sicstus.'),
	write(Stream,'\r\n'),
	socket_buffering(Stream,read,_,unbuf),
	read_line(Stream,Line),
	my_write(Line),
	nl,
	close(Stream),
	socket_close(Socket).