

combine(L,0,_,L):-!.

combine([],N1,D1,L1):- 
    L1= [D1|L2],N2 is N1-1, 
    combine([],N2,D1,L2).

combine([X|Xs],N1,D1,L1):- 
    L1= [X,D1|L2],N2 is N1-1, 
    combine(Xs,N2,D1,L2).





parejas([ (h1,1), (m1,1), (h2,4), (m2,4) ] ).


valido([ (h1,Shh1), (m1,Shm1), (h2,Shh2), (m2,Shm2) ]):-
    not(validoAux([ (h1,Shh1), (m1,Shm1), (h2,Shh2), (m2,Shm2) ])).
validoAux([ (h1,Shh1), (m1,Shm1), (h2,Shh2), (m2,Shm2) ] ):-
    Shh1 = Shm2 ,
    Shh2 \= Shh1, 
    Shh2\= Shm1.
validoAux([ (h1,Shh1), (m1,Shm1), (h2,Shh2), (m2,Shm2) ] ):-
    Shh2 = Shm1 , 
    Shh1 \= Shh2, 
    Shh1\= Shm2.