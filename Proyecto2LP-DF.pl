% Bridge crossing   consult('Proyecto2LP-DF.pl').

/*
    printSol(problem)
    imprime linea por linea cada movimiento
*/
printSol(Problem):- test_dfs(Problem,Movidas),
                    (write('Found sol='),nl),
                    (write(Movidas),nl,nl),
                    forall(member(X,Movidas),
                    (write(X),nl)).         


/*
    solve_dfs(State,History,Moves)
    Moves es la secuencia de movidas requeridas para
    alcanzar un estado final deseado a partir de State.
    History contiene los estados previamente visitados.
*/
 
% Si el Estado actual es un estado final, no hay que moverse.
solve_dfs(Estado,_,[]) :- final_state(Estado).


/*
    Si el Estado actual no es un estado final, genera una movida
    para desplazarse a un nuevo estado, y continua la b�squeda a
    partir de ese nuevo estado.
*/
solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),               % generar una nueva Movida
      update(Estado,Movida,Estado2),     % calcula nuevo estado usando Movida
      legal(Estado2),                    % nuevo estado debe ser legal
      not(member(Estado2,Historia)),     % debe ser primera vez que se llega al nuevo estado
      solve_dfs(Estado2,[Estado2|Historia],Movidas).   % continuar a partir de nuevo estado


/*
    Inicializa un problema y lo resuelve.
    Problema: nombre del problema.
    Movidas: movidas requeridas para resolver el problema.
*/
test_dfs(Problema,Movidas) :-
      initial_state(Problema,Estado),      % obtener un Estado inicial dado Problema
      solve_dfs(Estado,[Estado],Movidas).  % inicia resoluci�n desde Estado


/*
    En el estado incial el tiempo es 0 , linterna esta en la posicion left,
    todos se encuentran en la lista izq y nadie a la der.
    initial_state( bcp, bcp( Time, Pos, PLeft, PRigth , Tmax, Pmax ) ).    
*/
initial_state( bcp, bcp( 0, left, [(a,1),(b,2),(c,5),(d,10),(e,15),(j,20)], [] , 42, 2 ) ).


/*
    En el estado final el tiempo es TMax , linterna esta en la posicion rigth,
    todos se encuentran en la lista der y nadie a la izq.
*/
final_state( bcp( Tmax, rigth, [], L1, Tmax, _ ) ):- perm([(a,1),(b,2),(c,5),(d,10),(e,15),(j,20)],L1).

% Permuta la lista para ver si es uan solucion
perm(List,[H|Perm]) :- delete(H,List,Rest),perm(Rest,Perm).
perm([],[]).

delete(X,[X|T],T).
delete(X,[H|T],[H|NT]) :- delete(X,T,NT).


/*
    Genera los posibles movimientos de un estado a otro. 
*/

% Movimientos de izq a derecha 
move( bcp( _ , left, LList, _, _, Pmax) , People) :- crossNL(People,LList,Pmax).

% Movimientos de derecha a izq
move( bcp( _ , rigth, _, RList, _, Pmax) , People) :- crossNR(People,RList,Pmax) .

% Combinacciones para cruzar de las personas

% De izquierda a derecha va de las combinaciones de la mayor cantidad de personas posibles a 1 
crossNL(Comb,List,N1):- comb(N1,List,Comb); (N2 is N1-1, N2 >= 1 , crossNL(Comb,List,N2)).
% De derecha a izquierda solo cruza una de las personas
crossNR(Comb,List,N1):-comb(1,List,Comb).

% Combinaciones de N elementos de una lista
comb(N,L,X):-length(X,N),mem1(X,L).

mem1([],_).
mem1([H|T],Y):-member(H,Y),rest(H,Y,New),mem1(T,New).

rest(A,L,R):- append(_,[A|R],L),!.


/*
    Actualiza un estado a otro 
*/
update(bcp(Time1,Pos1,LList1,RList1, Tmax, Pmax),People,bcp(Time2,Pos2,LList2,RList2, Tmax, Pmax)):- 
        opp(Pos1,Pos2),
        (   
            (   Pos1=left,
                takes(People,LList1,LList2),
                append(People,RList1,RList2),
                findtime(People,Time),
                Time2 is Time1+Time
            );
        
            (   Pos1=rigth,
                takes(People,RList1,RList2),
                append(People,LList1,LList2),
                findtime(People,Time),
                Time2 is Time1+Time
            )
        ).
      

% Cambia el lado de la linterna
opp(left,rigth).  
opp(rigth,left).         
    
% Busca el mayor tiempo entre las personas de una lista 
findtime([(_,T)],Tim):- Tim is T,!.
findtime([(_,T1) | Ls],Tim):-  findtime(Ls,T2), Tim is max(T1,T2).

% Remueve elementos de una lista.
takes(S,L,R):- findall(Z,(member(Z,L),not(member(Z,S))),R).


/*
    Determina si un movimiento es legal 
*/
legal(bcp( Time , _, PLeft, _, Tmax, _)) :- maxTime(PLeft,MaxTimeLeft) , (Time + MaxTimeLeft) < Tmax+1 . 

% Determina el tiempo maximo 
maxTime([],0).
maxTime([(_,T1)], T1) :- !.
maxTime([(P1,T1),(P2,T2)|Tail], N):-
    ( T1 < T2 ->
        maxTime([(P2,T2)|Tail], N)
    ;
        maxTime([(P1,T1)|Tail], N)
    ).




