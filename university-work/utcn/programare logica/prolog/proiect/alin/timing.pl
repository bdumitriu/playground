%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module: timing
%% Defines useful predicates for monitoring the runtime and walltime of predicate calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(timing,[walltime/2,runtime/2,write_time/1,trace_walltime/1,trace_runtime/1]).

%% computes the walltime of predicate call A as T
walltime(A,T):- statistics(walltime,[X1|_]), user:A, statistics(walltime,[X2|_]), T is X2-X1.

%% computes the runtime of predicate call A as T
runtime(A,T):- statistics(runtime,[X1|_]), user:A, statistics(runtime,[X2|_]), T is X2-X1.

%% writes T miliseconds as H hours M minutes S seconds MS miliseconds
write_time(T):- T>3600000, !, H is T // 3600000, T1 is T mod 3600000, write(H), write(' hours '),write_time(T1).
write_time(T):- T>60000, !, M is T // 60000, T1 is T mod 60000, write(M), write(' minutes '),write_time(T1).
write_time(T):- T>1000, !, S is T // 1000, T1 is T mod 1000, write(S), write(' seconds '),write_time(T1).
write_time(MS):- write(MS), write(' miliseconds').

%% traces the walltime for all goals entered at the command prompt
trace_walltime(on):-assert(user:term_expansion((?-A), (?-walltime(A,T),write('Elapsed walltime is : '),write_time(T),nl))).
trace_walltime(off):-retract(user:term_expansion((?-A), (?-walltime(A,T),write('Elapsed walltime is : '),write_time(T),nl))).

%% traces the runtime for all goals entered at the command prompt
trace_runtime(on):-assert(user:term_expansion((?-A), (?-runtime(A,T),write('Elapsed runtime is : '),write_time(T),nl))).
trace_runtime(off):-retract(user:term_expansion((?-A), (?-runtime(A,T),write('Elapsed runtime is : '),write_time(T),nl))).