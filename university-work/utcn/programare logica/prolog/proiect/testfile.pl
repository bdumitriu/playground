barbat(ion).
barbat(vasile).
barbat(mihai).
barbat(mircea).

var_list([],[]).
var_list([H|T],[var|R]):-
	var(H),!,var_list(T,R).
var_list([_|T],[nonVar|R]):-
	var_list(T,R).

% transformare lista - LTV
l2ltv([],_).
l2ltv([H|T],[H|R]):-l2ltv(T,R).

% transformare LTV - lista
ltv2l(L,[]):-var(L),!.
ltv2l([H|T],[H|R]):-ltv2l(T,R).

% nr. elementelor al LTV
nrel(L,0):-var(L),!.
nrel([_|T],N):-nrel(T,Z),N is Z+1.

% vmember
vmember(_,T):-var(T),!,fail.
vmember(X,[X|_]).
vmember(X,[H|T]):-nonvar(H),vmember(X,T).

% vappend
vappend(X,Y,Y):-var(X),!.
vappend([H|T],L,[H|R]):-vappend(T,L,R).

% reverse
vreverse(X,_):-var(X),!.
vreverse([H|T],R):-vreverse(T,R1),vappend(R1,[H|_],R).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

% transformare arbore in ATV
a2atv(nil,_).
a2atv(t(R,St,Dr),t(R,Sm,Dm)):-a2atv(St,Sm),a2atv(Dr,Dm).

% transformare arbore in ATV
atv2a(X,nil):-var(X),!.
atv2a(t(R,St,Dr),t(R,Sm,Dm)):-atv2a(St,Sm),atv2a(Dr,Dm).

% numarul de noduri
noduri(X,0):-var(X),!.
noduri(t(_,St,Dr),N):-noduri(St,Ns),noduri(Dr,Nd),N is 1+Ns+Nd.

% adancimea maxima a arborelui
max(M1,M2,M1):-M1>M2,!.
max(_,M2,M2).

ad_max(X,0):-var(X),!.
ad_max(t(_,St,Dr),M):-ad_max(St,Ms),ad_max(Dr,Md),max(Ms,Md,Mi),M is Mi+1.

:-use_module(library(lists)).

% traversarea unui arbore in inordine
in_ord(X,[]):-var(X),!.
in_ord(t(R,St,Dr),Z):-in_ord(St,Ts),in_ord(Dr,Td),append(Ts,[R|Td],Z).

% oglindirea unui arbore
ogl(X,_):-var(X),!.
ogl(t(R,St,Dr),t(R,Do,So)):-ogl(St,So),ogl(Dr,Do).