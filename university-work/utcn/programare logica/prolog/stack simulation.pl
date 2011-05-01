% stack operations:
% push(S,X) - adds element X in stack S.
% push(S,X) - removes element from the top of stack S.

pop(S,X):-nonvar(S),retract(stiva(S,X)),!.
push(S,X):-asserta(stiva(S,X)).