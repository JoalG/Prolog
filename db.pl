mamon(chacon).

equals(X,Y) :- X=Y.



doble([],[]).
doble([X|Xs],[X,X|Ys]) :- dupli(Xs,Ys).



sustituir(_, _, [], []).
sustituir(X, Y, [X|Xs], [Y|Ys]) :- sustituir(X,Y,Xs,Ys) .
sustituir(X, Y, [Z|L1], [Z|L2]) :- Z \= X, sustituir(X,Y,L1,L2).


nodobles([], []).
nodobles([X|Xs],[X|Ys]) :- not(member(X,Ys)) , nodobles(Xs,Ys). 



mamapichas(joaquin).
mamapichas(jose).
mamapichas(mela).
mamapichas(pao).

mamaranElQuiz(X) :- mamapichas(X). 


my_length([],0).
my_length([_|L],N) :- my_length(L,N1), N is N1 + 1.









