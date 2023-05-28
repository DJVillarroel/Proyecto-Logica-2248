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
        to_generate/4
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

/**
 * booster_colapse(+Grid, +NumOfColumns, -RGrids) 
 * **** A IMPLEMENTAR **** este predicado eliminará todos los grupos de números adyacentes reemplazando en el elemento
 * más abajo a la derecha (mayor índice) de cada grupo, por la potencia de 2 mayor o igual a la sumatoria de todos los numeros del grupo
 */ 

booster_colapse(Grid, NumOfColumns, RGrid):-
    divide_grid(NumOfColumns, Grid, GridInRows),
    remove_groups(GridInRows, NumOfColumns, Grid1),
    apply_gravity(Grid1, NumOfColumns, Grid2),
	replace_zeros_grid(Grid2, Grid3),
    append([], [Grid1], Tmp1),
    append(Tmp1, [Grid2], Tmp2),
    append(Tmp2, [Grid3], RGrids).