user:term_expansion('end_of_file','end_of_file'):-
	nl,
	write('Done.'),
	nl,nl.

user:term_expansion(?-(A),?-(A)):-
	nl,
	write('Executing question: '),
	write(A),
	write('.'),
	nl.

user:term_expansion(:-(A),:-(A)):-
	nl,
	write('Executing directive: '),
	write(A),
	write('.'),
	nl.

user:term_expansion((A:-B),(A:-B)):-
	nl,
	write('Loading rule: '),
	write(A),
	write(':-'),
	write(B),
	write('.'),
	nl.

user:term_expansion((A),(A)):-
	nl,
	write('Loading fact: '),
	write(A),
	write('.'),
	nl.