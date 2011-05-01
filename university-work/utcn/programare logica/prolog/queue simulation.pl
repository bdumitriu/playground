% queue operations:
% enQueue(Q,X) - adds element X in queue Q.
% deQueue(Q,X) - removes first element from queue Q.

deQueue(Q,X):-nonvar(Q),retract(coada(Q,X)),!.
enQueue(Q,X):-assertz(coada(Q,X)).
