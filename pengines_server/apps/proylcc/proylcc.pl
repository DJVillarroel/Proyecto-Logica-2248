:- module(proylcc, 
	[  
		random_power_of_2/1,
		replace_zeros_grid/2,
		get_element/5,
		replace/4, 
		replace_element/6,
		replace_elements/5,
		next_power_of_two/2,
		sum_grid_elements/4,
		sum_grid_elements/5,
		replace_path/4,
		divide_grid/3,
		move_zeros/2,
		apply_gravity/3,
		join/4,
        to_generate/4,
        is_valid_pos/4,
        adjacents/6,
        adjacents_all/6,
        remove_groups/3,
        remove_groups/7,
        calculate_coordinates/3,
        translate_indexes/3,
        translate_indexes/4,
        booster_colapse/3,
		find_best_path/3
	]).

:- use_module(library(clpfd)).


random_power_of_2(P) :-
    random_between(1, 6, N),
    P is 2**N.

replace_zeros_grid([], []).
replace_zeros_grid([0|T], [P|T2]) :-
    random_power_of_2(P),
    replace_zeros_grid(T, T2).
replace_zeros_grid([H|T], [H|T2]) :-
    H \= 0,
    replace_zeros_grid(T, T2).

% Predicado para obtener un elemento de la grilla dado las coordenadas
get_element(Grid, NumOfColumns, X, Y, Element) :- % X=columnas, Y=filas
    % Calculamos el índice correspondiente a las coordenadas
    Index is (X * NumOfColumns) + Y,
    % Obtenemos el elemento en el índice calculado
    nth0(Index, Grid, Element).

% Predicado para sumar los elementos de la grilla Grid según los pares ordenados de Path
sum_grid_elements(Grid, Path, NumOfColumns, Sum) :-
    sum_grid_elements(Grid, Path, 0, NumOfColumns, Sum).

sum_grid_elements(_, [], Acc, _NumOfColumns, Acc).
sum_grid_elements(Grid, [[X, Y] | Rest], Acc, NumOfColumns, Sum) :-
    get_element(Grid, NumOfColumns, X, Y, Element),
    NewAcc is Acc + Element,
    sum_grid_elements(Grid, Rest, NewAcc, NumOfColumns, Sum).

% Predicado para obtener la próxima potencia de 2 mayor o igual a un número dado
next_power_of_two(Number, NextPower) :-
    NextPower is 2 ** ceil(log(Number) / log(2)).

replace(List, Index, Element, Result) :-
	nth0(Index, List, _, Temp),
	nth0(Index, Result, Element, Temp).

% Predicado para reemplazar un elemento de la grilla dado las coordenadas
replace_element(Grid, NumOfColumns, X, Y, NewElement, RGrids) :-
	% Calculamos el índice correspondiente a las coordenadas
	Index is X * NumOfColumns + Y,
	% Reemplazamos el elemento en el índice calculado
	replace(Grid, Index, NewElement, RGrids).

% Predicado para reemplazar múltiples elementos de la grilla
replace_elements(Grid, _NumOfColumns, [], _Element, Grid). % Caso base: no hay más elementos para reemplazar
replace_elements(Grid, NumOfColumns, [[X, Y] | Rest], Element, RGrids) :-
	replace_element(Grid, NumOfColumns, X, Y, Element, TempGrid),
	replace_elements(TempGrid, NumOfColumns, Rest, Element, RGrids).

% Predicado principal para reemplazar el último elemento del Path con la próxima potencia de 2, además reemplaza el resto del camino a 0
replace_path(Grid, NumOfColumns, Path, RGrids) :-
    sum_grid_elements(Grid, Path, NumOfColumns, Sum),
    next_power_of_two(Sum, NextPower), 
	replace_elements(Grid, NumOfColumns, Path, 0, GridAux),
    last(Path, [X, Y]), % Obtener las coordenadas del último par ordenado en Path
    replace_element(GridAux, NumOfColumns, X, Y, NextPower, RGrids). %Reemplaza el último elemento de Path con la potencia obtenida


% divide la lista en una lista de 8 sublistas de 5 elementos cada una
divide_grid(_, [], []).
divide_grid(N, List, [Sublist|Sublists]) :-
    length(Sublist, N),
    append(Sublist, Rest, List),
    divide_grid(N, Rest, Sublists).

% mueve los ceros al principio de cada columna
move_zeros([], []).
move_zeros([Column|Columns], [MovedColumn|MovedColumns]) :-
    include(=(0), Column, Zeros),
    include(\=(0), Column, NonZeros),
    append(Zeros, NonZeros, MovedColumn),
    move_zeros(Columns, MovedColumns).

% aplica la gravedad a la grilla y devuelve la grilla resultante
apply_gravity(Grid, NumOfColumns, RGrid) :-
    divide_grid(NumOfColumns, Grid, RColumns),
	transpose(RColumns, Transposed),
    move_zeros(Transposed, RColumnsFixed),
	transpose(RColumnsFixed, FixedGrid),
    flatten(FixedGrid, RGrid).



/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, NumOfColumns, Path, RGrids) :-
    replace_path(Grid, NumOfColumns, Path, Grid1),
    apply_gravity(Grid1, NumOfColumns, Grid2),
	replace_zeros_grid(Grid2, Grid3),
	append([], [Grid1], Tmp1),
    append(Tmp1, [Grid2], Tmp2),
    append(Tmp2, [Grid3], RGrids).

/**
 * to_generate(+Grid, +NumOfColumns, +Path, -RGrids) 
 * Este predicado calculará la potencia resultante que se esté por generar en todo momento según Path, se llamará
 * en todo momento que el path cambie, desde el front-end
 */ 

to_generate(_Grid, _NumOfColumns, [], _ToGenerate) .
to_generate(Grid, NumOfColumns, Path, ToGenerate):-
    sum_grid_elements(Grid, Path, NumOfColumns, Sum),
    next_power_of_two(Sum, ToGenerate).


%Chequea si la posición pasada pertenece a la Grilla
is_valid_pos(I, J, NumOfRows, NumOfColumns) :-
	I > 0, J > 0, I =< NumOfRows, J =< NumOfColumns.

%Devuelve una lista de adyacentes iguales
adjacents(Grid, NumOfColumns, NumOfRows, X, Y, List) :-
	is_valid_pos(X, Y, NumOfRows, NumOfColumns),
	Pos1 is (X-1) * NumOfColumns + Y,
	nth1(Pos1, Grid, Elem),
	findall(Pos, 
				(between(-1, 1, DIStep),
				between(-1, 1, DJStep),
				XAdj is X + DIStep,
				YAdj is Y + DJStep,
				is_valid_pos(XAdj, YAdj, NumOfRows, NumOfColumns),          
				Pos is (XAdj-1) * NumOfColumns + YAdj,          
				nth1(Pos, Grid, Elem2),
				Elem == Elem2), 
			List).

%Devuelve un camino RList de elementos adyacentes de igual valor
adjacents_all(_, [], _, _, _, []). %Caso base
adjacents_all(Grid, Path, Visited, NumOfColumns, NumOfRows, RList):- %Caso recursivo
	Path = [H|T],
	not(member(H, Visited)),
	append([H],Visited, VisitedAux),
	PosXAux is H div NumOfColumns,
	PosYAux is H mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	adjacents(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
	append(T, AdjList, NewPath),
	adjacents_all(Grid, NewPath, VisitedAux, NumOfColumns, NumOfRows, RList2),
	(last(RList2, LastElem), LastElem < H -> append(RList2, [H], RList) ; append([H], RList2, RList)),
	!
	;
	Path = [_|T],
	adjacents_all(Grid, T, Visited, NumOfColumns, NumOfRows, RList).

%Predicado cascara de remove_groups/7
remove_groups(Grid, NumOfColumns, RGrids):- 
	length(Grid, CantElem),
	NumOfRows is CantElem/NumOfColumns,	
	remove_groups(Grid, Grid, 1, [], NumOfColumns, NumOfRows, AuxRGrids),
	last(AuxRGrids, RGrids).

%Remueve todos los grupos de números de igual valor adyacentes entre sí, reemplaza todos los números de los caminos en 0 exceptuando
%al elemento del path de mayor valor
remove_groups(Grid, _, Index, _, _, _,[]):- %Caso base
	length(Grid, CantElem),
	Index > CantElem.
remove_groups(Grid, GridNew, Index, Visited, NumOfColumns, NumOfRows, RGrids):- %Caso recursivo
	not(member(Index, Visited)),
	Index2 is Index + 1,
	adjacents_all(Grid, [Index], [], NumOfColumns, NumOfRows, AdjPath),
	append(AdjPath, Visited, VisitedAux),
    translate_indexes(AdjPath, NumOfColumns, AdjPathAsCoord),
	replace_path(GridNew, NumOfColumns, AdjPathAsCoord, GridWithZeros),
	remove_groups(Grid, GridWithZeros, Index2, VisitedAux, NumOfColumns, NumOfRows, RGridsNew),	
	append([GridWithZeros], RGridsNew, RGrids),
	!
	;   
	Index2 is Index+1,
	remove_groups(Grid, GridNew, Index2, Visited, NumOfColumns, NumOfRows, RGrids).

% Predicado para calcular las coordenadas en base a un índice (índice base 1) 
calculate_coordinates(Index, NumOfColumns, [X, Y]) :-
    X is (Index - 1) // NumOfColumns,
    Y is (Index - 1) mod NumOfColumns.

%Predicado cascara de translate_indexes/4, al recibir una lista al reves, la idea es que vuelva a tener las posiciones en su lugar
translate_indexes(IndexList, NumOfColumns, CoordinatesList) :-
    translate_indexes(IndexList, NumOfColumns, CoordinatesListReversed, []),
    reverse(CoordinatesListReversed, CoordinatesList).

% Predicado que traduce una lista de indices a listas de coordenadas X,Y de la grilla
translate_indexes([], _, CoordinatesList, CoordinatesList).
translate_indexes([Index | Rest], NumOfColumns, CoordinatesList, Acc) :-
    calculate_coordinates(Index, NumOfColumns, Coordinates),
    translate_indexes(Rest, NumOfColumns, CoordinatesList, [Coordinates | Acc]).

/**
 * booster_colapse(+Grid, +NumOfColumns, -RGrids) 
 * este predicado eliminará todos los grupos de números adyacentes reemplazando en el elemento
 * más abajo a la derecha (mayor índice) de cada grupo, por la potencia de 2 mayor o igual a la sumatoria de todos los numeros del grupo
 */ 

booster_colapse(Grid, NumOfColumns, RGrid):-
    remove_groups(Grid, NumOfColumns, Grid1),
    apply_gravity(Grid1, NumOfColumns, Grid2),
	replace_zeros_grid(Grid2, Grid3),
    append([], [Grid1], Tmp1),
    append(Tmp1, [Grid2], Tmp2),
    append(Tmp2, [Grid3], RGrid).


% Predicado que chequea el valor del cuadrado resultante del Path pasado como parámetro
check_result_square(Grid, [H|T], Sum, RSquare):-
	T == [],
	nth1(H, Grid, Elem),
	AuxSum is Sum+Elem,
	sum_next_power(AuxSum, Pot),
	RSquare is 2**Pot.
check_result_square(Grid, Path,Sum, RSquare):-
	Path = [H|T],
	nth1(H, Grid, Elem),
	AuxSum is Sum+Elem,
	check_result_square(Grid, T, AuxSum, RSquare).
    
%Devuelve una lista de adyacentes de igual valor o igual a la siguiente potencia de 2 del elemento en cuestión (Modificación de adjacents)
adjacents_best(Grid, NumOfColumns, NumOfRows, X, Y, List) :-
	is_valid_pos(X, Y, NumOfRows, NumOfColumns),
	Pos1 is (X-1) * NumOfColumns + Y,
	nth1(Pos1, Grid, Elem),
	findall(Pos, 
				(between(-1, 1, DIStep),
				between(-1, 1, DJStep),
				XAdj is X + DIStep,
				YAdj is Y + DJStep,
				is_valid_pos(XAdj, YAdj, NumOfRows, NumOfColumns),          
				Pos is (XAdj-1) * NumOfColumns + YAdj,          
				nth1(Pos, Grid, Elem2),
				Elem3 is Elem*2,
				(Elem2 == Elem; Elem2 == Elem3)), 
			List).

%Devuelve el mejor camino RList posible a partir de determinado indice de la grilla
adjacents_all_best_path(_, [], _, _, _, 0, []).
adjacents_all_best_path(Grid, Path, Visited, NumOfColumns, NumOfRows, MaxSum, RList):-
	Path = [H|T],
	not(member(H, Visited)),
	append([H],Visited, VisitedAux),
	PosXAux is H div NumOfColumns,
	PosYAux is H mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	adjacents_best(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
	adjacents_all_best_path(Grid, AdjList, VisitedAux, NumOfColumns, NumOfRows, MaxSumAux, RList_1),
	adjacents_all_best_path(Grid, T, VisitedAux, NumOfColumns, NumOfRows, MaxSumAux2, RList_2),
	nth1(H, Grid, Elem),
	SumH is Elem + MaxSumAux,
	sum_next_power(SumH, Pot_1),
	sum_next_power(MaxSumAux2, Pot_2),
	(Pot_1 >= Pot_2 -> append([H], RList_1, RList), MaxSum is SumH;  
	RList = RList_2, MaxSum is MaxSumAux2)
	;   
	Path = [_|T],
	adjacents_all_best_path(Grid, T, Visited, NumOfColumns, NumOfRows, MaxSum, RList).

%Calcula la próxima potencia de 2 según la sumatoria
sum_next_power(Sum, 1) :- Sum=<2, !.
sum_next_power(Sum, Pot) :- 
        SumAux is Sum/2,
        sum_next_power(SumAux, PotAux),
        Pot is PotAux + 1,
        Sum =< 2**Pot,
        !.

%Predicado que encuentra el mejor camino posible RPath dentro de una grilla, siempre controla que el primer y segundo
%elemento del camino sean de igual valor. El camino es devuelto en una lista de indices, a ser pasados a coordenadas
best_path(Grid, OldPath, _, _, Index, RPath):-
    length(Grid, Length),
    Index > Length,
    RPath = OldPath.
best_path(Grid, OldPath, NumOfColumns, NumOfRows, Index, RPath):-
    PosXAux is Index div NumOfColumns,
	PosYAux is Index mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	adjacents(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList), %Si es el primer elemento, debe buscar un segundo elemento de igual valor
	length(AdjList, LengthAdj),
	LengthAdj > 1,
	adjacents_all_best_path(Grid, AdjList, [Index], NumOfColumns, NumOfRows, MaxSum, PathNew),
	nth1(Index, Grid, Elem),
	SumAux is Elem + MaxSum,
    check_result_square(Grid, OldPath, 0, RSquare),
	sum_next_power(SumAux, NextPow),
    (2**NextPow >= RSquare ->   
		append([Index], PathNew, PathNew2),
		IndexNew is Index+1,
		best_path(Grid,PathNew2, NumOfColumns, NumOfRows, IndexNew, RPath);
		IndexNew is Index+1,
		best_path(Grid, OldPath, NumOfColumns, NumOfRows, IndexNew , RPath))
	;   
	IndexNew is Index+1,
	best_path(Grid, OldPath, NumOfColumns, NumOfRows, IndexNew, RPath).

/**
 * find_best_path(+Grid, +NumOfColumns, -RPath) 
 * predicado que encontrará el mejor camino posible de una grilla en cuanto al valor numérico resultante
 * encontrará el mejor camino en índices, y luego los traducirá a coordenadas X,Y en base (0,0)
 */ 

find_best_path(Grid, NumOfColumns, RPath):- 
	length(Grid, CantElem),
	NumOfRows is CantElem/NumOfColumns,
	best_path(Grid, [1], NumOfColumns, NumOfRows, 1, BestPath),
	translate_indexes(BestPath, NumOfColumns, RPath).