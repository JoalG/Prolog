% Bridge crossing   consult('Proyecto2LP-BF.pl').

 /*
  * Si el mejor punto en la frontera corresponde a un estado final,
  * no hay que buscar m�s.
  * Se obtiene la secuencia de movidas que llevan del estado inicial a este
  * estado final simplemente revirtiendo el orden de movidas encontradas en
  * la ruta correspondiente a este estado.
  */
solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State),reverse(Path,Moves).

/*
 * Si el mejor punto en la frontera no corresponde a un estado final:
 *     * se generan todas las movidas posibles a partir del estado de ese punto
 *     * se obtienen los nuevos estados que se alcanzar�an con esas movidas
 *     * se calcular�an los valores heur�sticos de los nuevos estados
 *     * se introducen los nuevos estados como nuevo puntos en la frontera
 *     * se elimina el mejor punto de la frontera y se incluye en el historial.
 */
 
solve_best([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),     % obtiene movidas del mejor estado
    updates(Moves,Path,State,States),   % obtiene los nuevos estados usando movidas
    legals(States,States1),             % escoge los nuevos estados que son legales
    news(States1,History,States2),      % elimina nuevos estados ya incluidos en historial
    evaluates(States2,Values),          % calcula valores heur�sticos de los nuevos estados
    inserts(Values,Frontier,Frontier1), % inserta en orden los nuevos puntos en la frontera
    solve_best(Frontier1,[State|History],FinalPath). % continuar a partir de nueva frontera


/*
 * updates(Moves,Path,State,States)
 *   States es la lista de posibles estados accesables a partir
 *   de State usando la lista de posibles movidas (Moves).
 *   Path es la ruta de movidas que llevan del estado inicial a State.
 *   Las rutas de los nuevos estados se agregan al inicio su respectiva movida
 *   a la ruta de State.
 *   States es una lista de pares (NuevoEstado, NuevaRuta).
 */

updates([M|Ms],Path,S,[(S1,[M|Path])|Ss]) :-
    update(S,M,S1),         % obtiene el estado al que se llega por una movida
    updates(Ms,Path,S,Ss).  % procesa recursivamente las siguientes movidas
updates([],_,_,[]).


/*
 * legasls(States,States1)
 *   States1 es el subconjunto de la lista State que son estados legales.
 *   Maneja pares (Estado,Ruta).
 */

% el primer estado es legal, incluirlo en la nueva lista
legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
    
% primer estado ilegal, excluirlo de la nueva lista
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1).
    
legals([],[]).


/*
 * news(States,History,States1)
 *   States1 es el subconjunto de la lista States que consiste de estados
 *   que no aparecen en el historial.
 *   Maneja pares (Estado,Ruta).
 */

% primer estado ya aparece en historial, excluirlo de la nueva lista
news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).

% primer estado no aparece en historial, incluirlo en nueva lista
news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1).
    
news([],_,[]).


/*
 * evaluates(States,Values)
 *   Calcula el valor heur�stico de los estados en la lista States.
 *   Values is la lista resultante con los estados junto con sus valores.
 *   La lista State consiste de pares (Estado,Ruta).
 *   La lista Values consiste de estructuras punto(Estado,Ruta,Valor).
 */

evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                % calcula valor heur�stico del estado S
    evaluates(States,Values).  % procesa resto de estados
evaluates([],[]).


/*
 * inserts(Points,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar una lista de puntos (Points)
 *   en una frontera anterior (Frontier).
 *   Los puntos son insertados preservando el orden descendente en el
 *   valor heur�stico.
 */

inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0),  % inserta primer punto
    inserts(Puntos,Frontier0,Frontier1).    % recursivamente inserta los dem�s puntos
inserts([],Frontier,Frontier).


/*
 * insertPoint(Point,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar el punto Points en
 *   su posici�n correcta dentro de Frontier de acuerdo con el orden
 *   del valor heur�stico.
 *
 */
insertPoint(Point,[],[Point]).

% nuevo punto es mejor que el primero de la frontera,
% va de primero en nueva frontera
insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :-
    less_than(Point1,Point).

% nuevo punto es igual al primero de la frontera,
% nuevo punto se ignora y se deja la frontera sin cambios
insertPoint(Point,[Point1|Points],[Point|Points]) :-
    equals(Point,Point1).

% nuevo punto es peor que el primero de la frontera,
% el primero de la frontera de deja en el primer lugar y
% el nuevo punto se inserta recursivamente dentro del resto de la frontera
insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).

% nuevo punto no es igual a primero de la frontera pero tiene
% el mismo valor heur�stico, se pone al nuevo punto como primero
% en la nueva frontera
insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :-
    same(Point,Point1).


/*
 * relaciones de comparaci�n de puntos
 *
 * no se puede dar el caso de que dos puntos tengan el mismo estado
 * pero diferente valor
 */

% dos puntos son iguales si contienen el mismo estado y tienen el mismo valor;
% se ignoran las rutas: no importa c�mo se haya llegado al mismo estado
equals(punto(S,_,V),punto(S,_,V)).

% un punto es menor que otro, si contienen diferentes estados y si el
% valor del primero es menor que el valor del segundo;
% las rutas son ignoradas
less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.

% dos puntos tienen el mismo valor si contienen diferentes estados
% y si sus valores son iguales
same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.



/*
 * Inicializa un problema y lo resuelve.
 *   Problem: nombre del problema.
 *   Moves: movidas requeridas para resolver el problema.
 */
test_best_search(Problem,Moves) :-
   initial_state(Problem,State),   % obtener un Estado inicial dado Problema
   value(State,Value),             % calcula el valor heur�stico del estado incial
   solve_best([punto(State,[],Value)],[State],Moves). % inicializa frontera e historial,
                                                      % inicia resoluci�n



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
value(bcp( _, left, _, PRigth , _, _),Value):- sumTime(PRigth,Value) .

value(bcp( _, rigth, _, PRigth , _, _),Value):- 
    length(PRigth,Value1), 
    maxTime(PRigth,Value2), 
    Value is (Value1*1000)+Value2.

%   Calcula el tiempo de toda una lista 
%   sumTime(List,Time)

sumTime([],0).
sumTime([(_,Time)],Time).
sumTime([(_,Time1),(_,Time2)|Xs],TimeRes):- 
    sumTime([(_,Time2)|Xs],TimePar),
    TimeRes is Time1+TimePar .



