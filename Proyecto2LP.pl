% Bridge crossing   consult('Proyecto2LP.pl').

% En el estado incial el tiempo es 0 , linterna esta en la posicion left,
% todos se encuentran en la lista izq y nadie a la der.
initial_state( bcp, bcp( 0, left, [a,b,c,d], [] ) ).

% En el estado final el tiempo es 17 , linterna esta en la posicion rigth,
% todos se encuentran en la lista der y nadie a la izq.
final_state( bcp( 17, rigth, [], [a,b,c,d] ) ).


move( bcp( _ , left, LList, _) , People) :- cross(People,LList).

move( bcp( _ , rigth, _, RList) , People) :- cross(People,RList) .


opp(left,rigth).  
opp(rigth,left).

tim(a,1).
tim(b,2).
tim(c,5).
tim(d,10).


update(bcp(Time1,Pos1,LList1,RList1),People,bcp(Time2,Pos2,LList2,RList2)):- opp(Pos1,Pos2),
        ((Pos1=left,
        takes(People,LList1,LList2),append(People,RList1,RList2),findtime(People,Time),Time2 is Time1+Time);
        (Pos1=rigth,
        takes(People,RList1,RList2),append(People,LList1,LList2),findtime(People,Time),Time2 is Time1+Time)).
    
    
    
/* we know just one or two persons cross the bridge */
findtime([X],Tim):- tim(X,Tim),!.
findtime([A,B],Tim):- tim(A,Ta),tim(B,Tb),Tim is max(Ta,Tb),!.



takes(S,L,R):- findall(Z,(member(Z,L),not(member(Z,S))),R).



/* take all the combinations of 1 person, and 2 persons from our group: [a,b,c,d] */
cross(Comb,List):- comb(1,List,Comb); comb(2,List,Comb).

/* mem1(Lr,L). For comb/3. Same as mem/2 but does not generate [a,b] and [b,a]. 	
	?- mem1([X,Y],[a,b,c]).
	[a,b][a,c][b,c]
*/
mem1([],Y).
mem1([H|T],Y):-member(H,Y),rest(H,Y,New),mem1(T,New).

rest(A,L,R):- append(_,[A|R],L),!.
/* comb(N,L,Res). Combinations. Arrangements without " order".	
	| ?- comb(2,[a,b,c],I).
	I = [a,b] ;	I = [a,c] ;	I = [b,c] ;
*/
comb(N,L,X):-length(X,N),mem1(X,L).



legal(bcp( Time , _, _, _)) :- Time < 18. 




/*
 * solve_dfs(State,History,Moves)
 *   Moves es la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de State.
 *   History contiene los estados previamente visitados.
 */
 
% Si el Estado actual es un estado final, no hay que moverse.
solve_dfs(Estado,_,[]) :- final_state(Estado).

/*
 * Si el Estado actual no es un estado final, genera una movida
 * para desplazarse a un nuevo estado, y continua la b�squeda a
 * partir de ese nuevo estado.
 */
solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),               % generar una nueva Movida
      update(Estado,Movida,Estado2),     % calcula nuevo estado usando Movida
      legal(Estado2),                    % nuevo estado debe ser legal
      not(member(Estado2,Historia)),     % debe ser primera vez que se llega al nuevo estado
      solve_dfs(Estado2,[Estado2|Historia],Movidas).   % continuar a partir de nuevo estado

/*
 * Inicializa un problema y lo resuelve.
 *   Pr
 oblema: nombre del problema.
 *   Movidas: movidas requeridas para resolver el problema.
 */
test_dfs(Problema,Movidas) :-
      initial_state(Problema,Estado),      % obtener un Estado inicial dado Problema
      solve_dfs(Estado,[Estado],Movidas).  % inicia resoluci�n desde Estado
