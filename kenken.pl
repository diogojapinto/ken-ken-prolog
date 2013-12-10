:- use_module(library(clpfd)).
:- use_module(library(lists)).

testBoard([[cell(1, _), cell(2, _), cell(2, _), cell(3, _)],
		   [cell(1, _), cell(4, _), cell(5, _), cell(3, _)],
		   [cell(6, _), cell(4, _), cell(5, _), cell(5, _)],
		   [cell(6, _), cell(7, _), cell(7, _), cell(8, _)]]).

testBoardPrint([[cell(1, 5), cell(2, 5), cell(2, 5), cell(3, 5)],
		   [cell(1, 5), cell(4, 5), cell(5, 5), cell(3, 5)],
		   [cell(6, 5), cell(4, 5), cell(5, 5), cell(5, 5)],
		   [cell(6, 5), cell(7, 5), cell(7, 5), cell(8, 5)]]).

testFields([field(1, '-', 1),
			field(2, '/', 2),
			field(3, '-', 3),
			field(4, '-', 2),
			field(5, '*', 24),
			field(6, '-', 1),
			field(7, '+', 7),
			field(8, '=', 2)]).

cell(_FieldID, _Value).
field(_FieldID, _Op, _FinalValue).

solveBoard(Size, Board) :- /*length(Board, Size),*/
						   /*initBoard(Size, Board),*/
						   testBoard(Board),
						   testFields(Fields),
						   length(Board, Size),
						   imposeDomainConstrain(Board, Size),
						   imposeRowConstrain(Board),
						   imposeColumnConstrain(Board),
						   imposeFieldConstrain(Board, Fields),
						   getValsList(Board, List),
						   labeling([], List),
						   printBoard(Board).




/*********************************************************************************************
 *
 * Initialization
 *
 *********************************************************************************************/

initBoard(_Size, []).
initBoard(Size, [B | Bs]) :- length(B, Size),
							 initBoard(Size, Bs).

getFieldCells(_, [], []).
getFieldCells(FieldID, [B | Bs], RetList) :- getFieldCellsInRow(FieldID, B, RetList1), 
											 getFieldCells(FieldID, Bs, RetList2),
											 append(RetList1, RetList2, RetList).

getFieldCellsInRow(FieldID, [], []).
getFieldCellsInRow(FieldID, [R | Rs], RetList) :- cell(FieldID, Val) = R, 
												  getFieldCellsInRow(FieldID, Rs, RetList1),
												  append([Val], RetList1, RetList).

getFieldCellsInRow(FieldID, [R | Rs], RetList) :- getFieldCellsInRow(FieldID, Rs, RetList).

getValsList([B | Bs], L) :- getValsListInRow(B, L1),
							getValsList(Bs, L2),
							append(L1, L2, L).

getValsListInRow([], []).
getValsListInRow([R | Rs], L) :- getValsListInRow(Rs, L1),
								 append([R], L1, L).


/*********************************************************************************************
 *
 * Restrictions
 *
 *********************************************************************************************/

imposeDomainConstrain([], _).
imposeDomainConstrain([B | Bs], SupLim) :- imposeDomainConstrainInRow(B, SupLim),
										   imposeDomainConstrain(Bs, SupLim).

imposeDomainConstrainInRow([], _).
imposeDomainConstrainInRow([R | Rs], SupLim) :- cell(_, Value) = R,
												Value in 1..SupLim,
										 		imposeDomainConstrainInRow(Rs, SupLim).

imposeRowConstrain([]).
imposeRowConstrain([B | Bs]) :- imposeRowConstrain(B, []),
								imposeRowConstrain(Bs).

imposeRowConstrain([], List) :- all_distinct(List).
imposeRowConstrain([R | Rs], List) :- cell(_, Value) = R,
									  append([Value], List, NewList),
									  imposeRowConstrain(Rs, NewList).

imposeColumnConstrain(Board) :- transpose(Board, TBoard),
								imposeRowConstrain(TBoard).

imposeFieldConstrain(_, []).
imposeFieldConstrain(Board, [F | Fs]) :- field(FID, Op, Res),
										 getFieldCells(FID, Board, L),
										 applyOpConstrain(L, Op, Res),
										 imposeFieldConstrain(Board, Fs).

applyOpConstrain(L, '+', Res) :- sum(L, #=, Res).

applyOpConstrain([], '-', _).

applyOpConstrain(L, '-', Res) :- maximum(L, Max),
								 minimum(L, Min),
								 Res #= Max - Min.

applyOpConstrain([], '*', Acum, Res) :- Acum #= Res.

applyOpConstrain([L | Ls], '*', Res) :- applyOpConstrain(Ls, '*', L, Res).

applyOpConstrain([L | Ls], '*', Acum, Res) :- Acum1 = L * Acum,
											  applyOpConstrain(Ls, '*', Acum1, Res).

applyOpConstrain([], '/', Res).
applyOpConstrain(L, '/', Res) :- maximum(L, Max),
								 minimum(L, Min),
								 Res #= Max / Min.

/*********************************************************************************************
 *
 * Print
 *
 *********************************************************************************************/

printBoard :- testBoardPrint(Board),
			  printBoard(Board).

printBoard(Board) :- printTopBorder(Board), 
			  		 printBoard(Board), 
			  		 printBottomBorder(Board).

printTopBorder(Board) :- length(Board, Size), 
						 printHorizBorder(Size).

printBoard([]).
printBoard([B | Bs]) :- printRow(B),
						printBoard(Bs).

printRow([C1]) :- cell(_, Value1) = C1, 
					 write(Value1), 
					 write('║\n').

printRow([C1, C2 | Cs]) :- cell(FieldID1, Value1) = C1, 
						   cell(FieldID2, Value2) = C2, 
						   FieldID1 = FieldID2, 
						   write(Value1), 
						   write(' |'), 
						   printRow([C2 | Cs]).

printRow([C1 | Cs]) :- cell(_, Value1) = C1,
					   write(Value1), 
					   write('║'), 
					   printRow(Cs).

printBottomBorder(Board) :- length(Board, Size), 
							printHorizBorder(Size), printBottomNumbers(Size).

printHorizBorder(0):-  write('\n').

printHorizBorder(Size) :- write('═══'), 
						  Size1 is Size - 1, 
						  printHorizBorder(Size1).

printBottomNumbers(Size) :- printBottomNumbers(Size, 1).

printBottomNumbers(Size, Size) :- write(Size).

printBottomNumbers(Size, Number) :- write(' '), 
									write(Number), 
									write(' '), 
									Number1 is Number + 1, 
									printBottomNumbers(Size, Number1).