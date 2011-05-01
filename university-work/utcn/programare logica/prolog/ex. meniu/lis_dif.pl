:-dynamic arb/1.
:-dynamic sir/1.
:-dynamic list/1.
:-dynamic sir_pr1/1.
:-dynamic sir_pr2/1.

sir([]).
list([]).
arb([]).
sir_pr1([]).
sir_pr2([]).

setup:-use_module(library(system)),
       use_module(library(lists)),
       nofileerrors,intro(0).   


intro(X):-nl,nl,nl,write('     Program pentru prezentare liste diferente'),
          nl,write('           Realizat de Kerekes Peter gr. 3233'),
          nl,nl,nl,meniu(0).

% Meniu general

meniu(X):-X=\=48,writemenu,get(C),!,exec(C),meniu(C).
writemenu:-
	write('0-Exit'),nl,
	write('1-Produs cartezian'),nl,
	write('2-Exemple cu arbori'),nl,
        write('3-Quicksort'),nl,
	write('4-Netezirea unei liste multinivel'),nl,
	write('5-Descompunere poligon in triunghiuri'),nl,
	write('6-Problema labirintului'),nl,
	nl,nl,nl,nl.

% meniu arbori

meniu_arb(X):-X=\=48,writem_arb,get(C),!,exec_arb(C),meniu_arb(C).
writem_arb:-nl,nl,nl,nl,
               write('Exemple cu arbori'),nl,nl,
	       write('0-Inapoi'),nl,
 	       write('1-Construire de la tastatura'),nl,	               
               write('2-Construire din fisier'),nl,
	       write('3-Traversare inordine'),nl,
               write('4-Traversare postordine'),nl,
               write('5-Traversare preordine'),nl.        


% meniu netezire

meniu_net(X):-X=\=48,writem_net,get(C),!,exec_net(C),meniu_net(C).
writem_net:-nl,nl,nl,nl,
	       write('Netezirea unei liste multinivel'),nl,nl,
               write('0-Inapoi'),nl,
               write('1-Introducere lista de la tastatura'),nl,
               write('2-Introducere lista din fisier'),nl,
               write('3-Afisare rezultat'),nl.

% meniu quicksort

meniu_qui(X):-X=\=48,writem_qui,get(C),!,exec_qui(C),meniu_qui(C).
writem_qui:-nl,nl,nl,nl,
	       write('Quicksort'),nl,nl,
               write('0-Inapoi'),nl,
               write('1-Introducere lista din fisier'),nl,
               write('2-Introducere sir de la tastatura'),nl,
               write('3-Afisare rezultat'),nl.


% meniu produs cartezian

meniu_pro(X):-X=\=48,writem_pro,get(C),!,exec_pro(C),meniu_pro(C).
writem_pro:-nl,nl,nl,nl,
	       write('Produs cartezian'),nl,nl,
               write('0-Inapoi'),nl,
               write('1-Introducere liste de la tastatura'),nl,
               write('2-Introducere liste din fisier'),nl,
               write('3-Afisare rezultat'),nl.

% meniu descompunere in triunghiuri

meniu_pol(X):-X=\=48,writem_pol,get(C),!,exec_pol(C),meniu_pol(C).
writem_pol:-nl,nl,nl,nl,
               write('Descompunere poligon in triunghiuri'),nl,nl,
               write('0-Inapoi'),nl,
               write('1-Introducere lista varfurilor de la tastatura'),nl,
               write('2-Introducere lista varfurilor din fisier'),nl,
               write('3-Afiseaza rezultat '),nl.

% meniu labirint

meniu_lab(X):-X=\=48,writem_lab,get(C),!,exec_lab(C),meniu_lab(C).
writem_lab:-nl,nl,nl,nl,
               write('Cautare drumuri in labirint'),nl,nl,
               write('0-Inapoi'),nl,
               write('1-Initializare labirint'),nl,
               write('2-Cautare'),nl.



%
%   parcurgeri arbore
%


% traversare inordine binari

inord(X,Y):-inord(X,Y,[]).
inord(nil,X,X).
inord(n(N,S,D),P,U):-inord(S,P,[N|T]),inord(D,T,U).

% traversare preordine binari

preord(X,Y):-preord(X,Y,[]).
preord(nil,X,X).
preord(n(N,S,D),[N|P],U):-preord(S,P,T),preord(D,T,U).

% travers postordine binari

postord(X,Y):-postord(X,Y,[]).
postord(nil,X,X).
postord(n(N,S,D),P,U):-postord(S,P,T),postord(D,T,[N|U]).

% travers arb n-ari

preordn(X,Y):-preordn(X,Y,[]).
preordn(nil,X,X).
preordn(n(N,F1,F2,F3,F4),[N|P],U):-preordn(F1,P,T1),
                                   preordn(F2,T1,T2),
				   preordn(F3,T2,T3),
				   preordn(F4,T3,U).


%Introducere arb binar
constr(n(N,S,D)):-get(K),K=\=48,!,N is K-48,constr(S),constr(D).
constr(nil).

%
%      Quicksort
%

read_sir:-retract(sir(_)),read(S),assert(sir(S)).

q(X,Y):-q(X,Y,[]).
q([H|T],P,U):-part(H,T,A,B),
              q(A,P,[H|D]),
	      q(B,D,U).
q([],X,X).

part(H,[H1|T],[H1|A],B):-H1<H,!,part(H,T,A,B).
part(H,[H1|T],A,[H1|B]):-part(H,T,A,B).
part(_,[],[],[]).

%
%     Netezirea unei liste multinivel
%

read_list:-retract(list(_)),read(S),assert(list(S)).

netezire(X,Y):-netezire(X,Y,[]).
netezire([],N,N).
netezire(H,[H|T],T):-atomic(H).
netezire([H|T],Np,Nu):-netezire(H,Np,NN),
                       netezire(T,NN,Nu).


%
%     Produs cartezian
%

read_pro1:-retract(sir_pr1(_)),read(S),assert(sir_pr1(S)).
read_pro2:-retract(sir_pr2(_)),read(S1),assert(sir_pr2(S1)).


ins1(X,L,L1,R1):-ins(X,L,R),append(L1,R,R1).

ins(X,[],[]):-!.
ins(X,[H|T],[[X|H]|R]):-ins(X,T,R).

produs_cart([],L,L1,L1).
produs_cart([H1|T1],L1,L,R):-ins1(H1,L1,L,R1),
				produs_cart(T1,L1,R1,R).
    

%
%     DESCOMPUNERE IN TRIUNGHIURI
%

afis_sol([]).
afis_sol([H|T]):-write('                              '),write(H),nl,afis_sol(T).

desc([A,B|T],[[A,B,X]|Pb],U):-append(Tb,[X|Ta],T),
                              desc([X,B|Tb],Pb,Ub),
                              desc([A,X|Ta],Ub,U).
desc([A,B],L,L). 

%
%  LABIRINT
%

init_labirint:-write('Numele fisierului cu descrierea labirintului '),read(S),
               (file_exists(S)->consult(S);write('Nu exista fiser')).

e_trecere(X,Y):-e_usa(X,Y);e_usa(Y,X).
cauta(X,Y,Drum):-incearca(X,Y,[X],Drum),reverse(Drum,R),
                 (e_obiectiv(Y)->write('O solutie '),write(R),nl;
                  write(Y),write(' nu este obiectiv, dar avem solutia '),
                  write(R),nl),cauta(X,Y,[]).

incearca(X,X,D,D).
incearca(X,Y,Fir,Drum):-e_trecere(X,Z),
                        (member(Z,Fir)->fail;incearca(Z,Y,[Z|Fir],Drum)).


%
%    FISIERE
%
open_fisier(St):-nl,write('Numele fisierului : '),read(S),
                 file_exists(S),!,open(S,read,St),
                 set_input(St).
open_fisier(St):-write('Nume inexistent in directorul actual').

close_fisier(St):-close(St).



%
%     MENIURI
%


% Meniul principal

exec(48):-nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
	  write('   Prezentare liste diferenta  v. 1.20'),nl,nl,nl,
          write('      Va multumesc pentru folosire').
exec(49):-meniu_pro(0).
exec(50):-meniu_arb(0).
exec(51):-meniu_qui(0).
exec(52):-meniu_net(0).
exec(53):-meniu_pol(0).
exec(54):-meniu_lab(0).


% Realizare meniu quicksort

exec_qui(48):-intro(0).
exec_qui(49):-open_fisier(R),retract(sir(_)),read(S),assert(sir(S)),close_fisier(R).
exec_qui(50):-nl,write('Introduceti sirul '),read_sir.
exec_qui(51):-retract(sir(S)),nl,
              write('Sirul original '),write(S),nl,q(S,R),
              write('Sirul sortat '),write(R),nl,assert(sir(S)).



% Realizare meniu netezire

exec_net(48):-intro(0).
exec_net(49):-nl,write('Introduceti lista multinivel '),read_list.
exec_net(50):-open_fisier(R),retract(list(_)),read(S),assert(list(S)),close_fisier(R).
exec_net(51):-retract(list(S)),netezire(S,R),nl,
              write('Lista originala '),write(S),nl,
              write('Lista netezita '),write(R),nl,assert(list(S)).

% Realizare meniu descomp

exec_pol(48):-intro(0).
exec_pol(49):-nl,write('Introduceti lista cu varfurile poligonului '),read_sir.
exec_pol(50):-open_fisier(R),retract(sir(_)),read(S),assert(sir(S)),close_fisier(R).
exec_pol(51):-retract(sir(S)),findall(R,desc(S,R,[]),L),nl,
              write('Poligon '),write(S),nl,
              write('Lista triunghiurilor obtinute '),nl,afis_sol(L),nl,assert(sir(S)).

% Realizare meniu labirint

exec_lab(48):-intro(0).
exec_lab(49):-init_labirint.
exec_lab(50):-write('De unde pornim '),read(S1),nl,
              write('Unde vrem sa ajungem '),read(S2),nl,
              findall(R,cauta(S1,S2,R),L).

% Realizare meniu produs cartezian

exec_pro(48):-intro(0).
exec_pro(49):-nl,write('Primul sir'),read_pro1,
	      nl,write('Al doilea sir'),read_pro2.
exec_pro(50):-open_fisier(R),
              read_pro1,read_pro2,
              close_fisier(R).
exec_pro(51):-retract(sir_pr2(S2)),retract(sir_pr1(S1)),
              produs_cart(S1,S2,[],R),nl,
              write('Listele '),write(S1),write(S2),nl,
              write('Produsul cartezian '),write(R),nl,
	      assert(sir_pr2(S2)),assert(sir_pr1(S1)).

%Realizare meniu arbori

exec_arb(48):-intro(0).
exec_arb(49):-retract(arb(_)),constr(S),assert(arb(S)).
exec_arb(50):-open_fisier(R),retract(arb(_)),read(S),assert(arb(S)),close_fisier(R).
exec_arb(51):-retract(arb(S)),inord(S,R),write(R),assert(arb(S)).
exec_arb(52):-retract(arb(S)),postord(S,R),write(R),assert(arb(S)).
exec_arb(53):-retract(arb(S)),preord(S,R),write(R),assert(arb(S)).