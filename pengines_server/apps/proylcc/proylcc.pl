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
		check_result_square/4,
		adjacents_best/6,
		adjacents_all_best_path/7,
		best_path/6,
		sum_next_power/2,
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

%Predicado similar a translate_indexes/3, cambia de manera inversa las coordenadas a indices base 1
coord_to_index([], _, []).
coord_to_index(Coords, NumOfColumns, Indexes):-
	Coords = [H|T],
	H = [X|Y],
	Pos is X * NumOfColumns + Y + 1,
	coord_to_index(T, NumOfColumns, IndAux),
	append([Pos], IndAux, Indexes). 

%Predicado que chequea si existe un elemento adyacente igual a la posición pasada por parametro, 0 falso, 1 verdadero
check_if_adjacent(_, _, _, [], _, 1).
check_if_adjacent(Grid, NumOfColumns, NumOfRows, IndexList, [PosX,PosY], Check) :-
    IndexList = [H|_],
	adjacents(Grid, NumOfColumns, NumOfRows, PosX+1, PosY+1, AdjList), %adjacents se maneja con posiciones en indices base [1,1], se suma 1 a cada coordenada
    member(H, AdjList),
    Check is 0,
    !
    ;   
    IndexList = [_|T],
    check_if_adjacent(Grid, NumOfColumns, NumOfRows, T, [PosX,PosY], Check)
    .

%Predicado que cuenta los ceros después de una posición de la lista
count_zeros_after_pos(List, Position, Count) :-
    nth0(Position, List, _),
    sublist_after_index(List, Position, Sublist),
    include(=(0), Sublist, Zeros),
    length(Zeros, Count).

sublist_after_index(List, Index, Sublist) :-
    length(Prefix, Index),
    append(Prefix, Sublist, List).

%Cambia la posición de las coordenadas según el efecto de gravedad correspondiente.
gravity_to_coordinates(_,[],[]). %Caso base
gravity_to_coordinates(Columns, Coordinates, RList):- %Caso recursivo
	Coordinates = [H|T],
	H = [X,Y],
	nth0(Y, Columns, Column),
	count_zeros_after_pos(Column, X, Zeros),
	NewX is X+Zeros,
	gravity_to_coordinates(Columns, T, RListAux),
	append(RListAux, [[NewX, Y]], RList).

%Predicado que simula la gravedad de la grilla, y obtiene la nueva posición teórica de la posición pasada por parámetro
simulate_gravity(Grid, NumOfColumns, CoordinateList, LastElem, NewLastElem, CoordWithGravity, RGrid) :-
    divide_grid(NumOfColumns, Grid, RColumns),
	transpose(RColumns, Transposed),
	gravity_to_coordinates(Transposed, [LastElem], NewLastElem),
	gravity_to_coordinates(Transposed, CoordinateList, CoordWithGravityAux),
	reverse(CoordWithGravityAux, CoordWithGravity),
    move_zeros(Transposed, RColumnsFixed),
	transpose(RColumnsFixed, FixedGrid),
    flatten(FixedGrid, RGrid).

%Simula la gravedad y chequea si el último elemento tiene como adyacente el máximo valor
fake_gravity(Grid, Path, NumOfColumns, NumOfRows, IndexList, Check) :-
	translate_indexes(IndexList, NumOfColumns, CoordinateList),
	translate_indexes(Path, NumOfColumns, TPath),
    last(TPath, IndexPos),
    replace_path(Grid, NumOfColumns, TPath, FGridAux),
    simulate_gravity(FGridAux, NumOfColumns, CoordinateList, IndexPos, NewPos, CoordsWithGravity, RGrid),
    NewPos = [H|_],
	coord_to_index(CoordsWithGravity, NumOfColumns, IndexesWithGravity),
    check_if_adjacent(RGrid, NumOfColumns, NumOfRows, IndexesWithGravity, H, Check),
    !
    .

%Predicado que devuelve la lista de indices de la grilla (Base 1)
index_list([], _, []). %Caso Base: lista vacía
index_list(Grid, Index, IndexList) :- %Caso Recursivo
    Grid = [_|T],
    IndexList = [Index|H],
    NextIndex is Index + 1,
    index_list(T, NextIndex, H).

%Predicado que ordena una lista y otra lista auxiliar en terminos de la primera, algoritmo de ordenamiento bubble_sort
sort_list_and_indexes(List, IndexListAux, IndexList, Sorted1) :-
       	(sort(List, IndexListAux, IndexList1, Sorted)
       	-> sort_list_and_indexes(Sorted, IndexList1, IndexList, Sorted1)
       	; List = Sorted1, IndexListAux = IndexList).

sort([A,B|T], [X,Y|T1], IndexList, List) :-
        (A < B
        -> List = [B,A|T], IndexList = [Y,X|T1]
        ; List = [A|Ls], IndexList = [X|T2],
        sort([B|T], [Y|T1], T2, Ls)).

%Predicado que agrupa elementos de una lista en una sub-lista si los valores de sus elementos son iguales,
%Utiliza 2 listas, la lista de indices, y la lista de valores, para devolver solo los indices ordenados
equal_values_list(PrevIndexElem, Sorted, _, Index, [[PrevIndexElem]]):-
	length(Sorted, Length),
	Index > Length.
equal_values_list(PrevIndexElem, Sorted, IndexList, Index, RLists):-
	nth1(Index, Sorted, Elem),
    nth1(PrevIndexElem, Sorted, PrevElem),
	(PrevElem == Elem->
		Index2 is Index + 1,
		equal_values_list(Index, Sorted, IndexList, Index2, RListsAux),
		RListsAux = [H|T],
        nth1(PrevIndexElem, IndexList, ElemIndex),
		append([ElemIndex], H, ThisList),
		append([ThisList], T, RLists),
    !
	;
		Index2 is Index + 1,
		equal_values_list(Index, Sorted, IndexList, Index2, RListsAux), 
    	nth1(PrevIndexElem, IndexList, ElemIndex),
		append([[ElemIndex]], RListsAux, RLists)).

adjacents_all_best_path_next_to_value(_, [], _, _, _, _, _, []).
adjacents_all_best_path_next_to_value(Grid, Path, Visited, NumOfColumns, NumOfRows, Sum, BiggestAdj, RList):-
	Path = [H|T],
	not(member(H, Visited)),
	append([H],Visited, VisitedAux),
	PosXAux is H div NumOfColumns,
	PosYAux is H mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	nth1(H, Grid, Elem),
	AuxSum is Elem + Sum,
	sum_next_power(AuxSum, NextPow),
	NextSum is 2**NextPow,
	(NextSum < BiggestAdj ->
		adjacents_best(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
		(adjacents_all_best_path_next_to_value(Grid, AdjList, VisitedAux, NumOfColumns, NumOfRows,  AuxSum, BiggestAdj, RListAux)->
			(RListAux \==[] -> append([H], RListAux, RList); RList = [])
			;
			adjacents_all_best_path_next_to_value(Grid, T, VisitedAux, NumOfColumns, NumOfRows, Sum, BiggestAdj, RList))
		;
		NextSum == BiggestAdj,
		RList = [H]
	),
	!
	;  
	Sum < BiggestAdj,
	Path = [_|T],
	adjacents_all_best_path_next_to_value(Grid, T, Visited, NumOfColumns, NumOfRows, Sum, BiggestAdj, RList).

%Predicado que encuentra el mejor camino adyacente posible al valor BiggestAdj pasado como parametro
best_adjacent_path(Grid, _, _, Index, _, _, []):- %Caso Base
	length(Grid, Length),
	Index > Length,
    !.
best_adjacent_path(Grid, NumOfColumns, NumOfRows, Index, IndexList, BiggestAdj, RPath):- %Caso Recursivo
	PosXAux is Index div NumOfColumns,
	PosYAux is Index mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	nth1(Index, Grid, Elem),
	adjacents(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
	length(AdjList, LengthAdj),
	LengthAdj > 1,
	adjacents_all_best_path_next_to_value(Grid, AdjList, [Index], NumOfColumns, NumOfRows, Elem, BiggestAdj, RList),
	length(RList, PathLength),
	PathLength \== 0,
	append([Index], RList, Path),
	fake_gravity(Grid, Path, NumOfColumns, NumOfRows, IndexList, CheckAux),
	CheckAux == 0,
	RPath = Path,		
	!
	;
	Index2 is Index + 1,
	best_adjacent_path(Grid, NumOfColumns, NumOfRows, Index2, IndexList, BiggestAdj, RPath).

%Predicado cascara, toma valores por sus indices ordenados de mayor a menor, busca el mejor camino adyacente posible
%a dicho valor. 
max_adjacent(_, _, _, [], []).
max_adjacent(Grid, NumOfColumns, NumOfRows, RIndexLists, RPath):-
	RIndexLists = [List|_],
	List = [H|_],
	nth1(H, Grid, BiggestAdj),
	best_adjacent_path(Grid, NumOfColumns, NumOfRows, 1, List, BiggestAdj, RPathAux),
	length(RPathAux, Length),
	Length > 1,
	RPath = RPathAux
	;
	RIndexLists = [_|T],
	max_adjacent(Grid, NumOfColumns, NumOfRows, T, RPath).

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

/**
 * find_best_adjacent_path(+Grid, +NumOfColumns, -RPath) 
 * predicado que encuentra el mejor camino posible cuyo valor adyacente al bloque a generar sea el más alto posible
 * 
 */ 

find_best_adjacent_path(Grid, NumOfColumns, RPath):-
	length(Grid, Length),
	NumOfRows is Length div NumOfColumns,
	index_list(Grid, 1, IndexList), %Lista de indices (base 1)
	sort_list_and_indexes(Grid, IndexList, RIndexList, Sorted), %Ordenar indices y grilla en base a sus valores
	equal_values_list(1, Sorted, RIndexList, 2, RIndexLists), %Listar indices de igual valor en sub-listas
	max_adjacent(Grid, NumOfColumns, NumOfRows, RIndexLists, RPathAux), 
	translate_indexes(RPathAux, NumOfColumns, RPath), %Traducir indices en base 1 a pares ordenados
    !. %Corta si encuentra un camino correspondiente