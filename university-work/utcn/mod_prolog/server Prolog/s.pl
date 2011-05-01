:- use_module(library(sockets)).

server:-
	server(22222).

server(Port):-
	current_host(Host), 
	socket('AF_INET',Socket),
	socket_bind(Socket,'AF_INET'(Host,Port)),
	format('Server address is ~q~n',[Host:Port]),
	socket_listen(Socket,1),
	socket_accept(Socket,_Client,Stream),
	%socket_buffering(Stream,read,_,unbuf),
	%socket_buffering(Stream,write,_,buf),
	set_input(Stream),
	assert(streem(Stream)),
	%set_output(Stream),
	set_prolog_flag(user_input,Stream).
	%set_prolog_flag(user_output,Stream),
	%set_prolog_flag(user_error,Stream).

a:-
	assert(user:term_expansion(
		(?-A), 
		(?-current_output(C),streem(S),set_output(S),user:A,nl,write('!#%&(@$^*)'),nl,flush_output(S),set_output(C))
		)),
	assert(user:term_expansion(
		(?-halt),
		(?-write(halt))
		)).

:- server.
:- a.