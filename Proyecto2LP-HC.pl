% Bridge crossing   consult('Proyecto2LP-HC.pl').

/*
 * solve_hill_climb(State,History,Moves)
 *   Moves es la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de State.
 *   History contiene los estados previamente visitados.
 */

% Si el Estado actual es un estado final, no hay que moverse.
solve_hill_climb(State,_,[]) :-
    final_state(State).

/*
 * Si el Estado actual no es un estado final, genera una movida
 * para desplazarse a un nuevo estado, y continua la b�squeda a
 * partir de ese nuevo estado.
 * Las movidas son intentadas en el orden establecido por la heur�stica
 * que eval�a la "bondad" de los estados que se alcanzan para cada movida.
 */
solve_hill_climb(State,History,[Move|Moves]) :-
    hill_climb(State,Move),      % generar una nueva Movida en el orden heur�stico
    update(State,Move,State1),   % calcula nuevo estado usando Movida
    legal(State1),               % nuevo estado debe ser legal
    not(member(State1,History)), % debe ser primera vez que se llega al nuevo estado
    solve_hill_climb(State1,[State1|History],Moves).   % continuar a partir de nuevo estado

/*
 *  A partir de un Estado devuelve una Movida.
 *  Primero genera todas las movidas, luego las eval�a usando una heur�stica,
 *  y finalmente las va usando en orden decreciente de la evaluaci�n obtenida.
 */
hill_climb(State,Move) :-
    findall(M,move(State,M),Moves),         % Encuentra todas las movidas posibles
    evaluate_and_order(Moves,State,[],MVs), % Eval�a con la heur�stica todas las movidas y las ordena.
    member((Move,_),MVs).                   % Escoge movidas en orden de heur�stica


/*
 * evaluate_and_order(Movidas,Estado,AcumuladorParcial,MovidasOrdenadas)
 *   Todas las Movidas del Estado actual
 *   son evaluadas y almacenadas en orden en MovidasOrdenadas
 *   Acumulador es donde se van acumulando parcialmente las movidas evaluadas.
 */

% Caso: procesar la primera movida y continuar recursivamente
evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-
    update(State,Move,State1),         % obtiene nuevo estado usando movida
    value(State1,Value),               % calcula el valor heur�sico del nuevo estado
    insertPair((Move,Value),MVs,MVs1), % inserta en orden el par (movida,valor) en lista de movidas
    evaluate_and_order(Moves,State,MVs1,OrderedMVs).  % procesa recursivamente el resto de movidas
    
% Caso base: no hay m�s movidas que evaluar. Se retorna el acumulador como resultado.
evaluate_and_order([],_,MVs,MVs).

insertPair(MV,[],[MV]).
insertPair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :-
    V >= V1.
insertPair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :-
    V < V1,insertPair((M,V),MVs,MVs1).


/*
 * Inicializa un problema y lo resuelve.
 *   Problem: nombre del problema.
 *   Moves: movidas requeridas para resolver el problema.
 */
test_hill_climb(Problem,Moves) :-
   initial_state(Problem,State),           % obtener un Estado inicial dado Problema
   solve_hill_climb(State,[State],Moves).  % inicia resoluci�n desde Estado


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
move( bcp( _ , rigth, _, RList, _, _) , People) :- crossNR(People,RList) .

% Combinacciones para cruzar de las personas

% De izquierda a derecha va de las combinaciones de la mayor cantidad de personas posibles a 1 
crossNL(Comb,List,N1):- comb(N1,List,Comb); (N2 is N1-1, N2 >= 1 , crossNL(Comb,List,N2)).
% De derecha a izquierda solo cruza una de las personas
crossNR(Comb,List):-comb(1,List,Comb).

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


/*
    Calcula el valor de un estado 
    Es la euristica 
*/
value(bcp( _, _, _, PRigth , _, _),Value):-length(PRigth,Value).


