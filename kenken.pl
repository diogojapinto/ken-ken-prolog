:- use_module(library(clpfd)).
:- use_module(library(lists)).

testBoard([[cell(1, _), cell(2, _), cell(2, _), cell(3, _)],
		   [cell(1, _), cell(4, _), cell(5, _), cell(3, _)],
		   [cell(6, _), cell(4, _), cell(5, _), cell(5, _)],
		   [cell(6, _), cell(7, _), cell(7, _), cell(8, _)]]).

testBoardPrint([[cell(1, 5), cell(2, 5), cell(2, 555), cell(3, 5)],
		   [cell(1, 55), cell(4, 5), cell(5, 5), cell(3, 5)],
		   [cell(6, 5), cell(4, 55), cell(5, 5), cell(5, 5)],
		   [cell(6, 5), cell(7, 5), cell(7, 5), cell(8, 55)]]).

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

solveBoard :- /*length(Board, Size),*/
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
			  printBoard(Board),
			  write('\n\n'),
			  printFieldTable(Board, Fields). 

/*
createBoard :- length(Board, Size),
			  initBoard(Size, Board)....
 */


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

getFieldCellsInRow(_FieldID, [], []).
getFieldCellsInRow(FieldID, [cell(FieldID, Val) | Rs], RetList) :- getFieldCellsInRow(FieldID, Rs, RetList1),
												  				   append([Val], RetList1, RetList).

getFieldCellsInRow(FieldID, [_R | Rs], RetList) :- getFieldCellsInRow(FieldID, Rs, RetList).

getValsList([], []).
getValsList([B | Bs], L) :- getValsListInRow(B, L1),
							getValsList(Bs, L2),
							append(L1, L2, L).

getValsListInRow([], []).
getValsListInRow([cell(_, Val) | Rs], L) :- getValsListInRow(Rs, L1),
								 			append([Val], L1, L).


/*********************************************************************************************
 *
 * Restrictions
 *
 *********************************************************************************************/

imposeDomainConstrain([], _).
imposeDomainConstrain([B | Bs], SupLim) :- imposeDomainConstrainInRow(B, SupLim),
										   imposeDomainConstrain(Bs, SupLim).

imposeDomainConstrainInRow([], _).
imposeDomainConstrainInRow([cell(_, Value) | Rs], SupLim) :- Value in 1..SupLim,
										 					 imposeDomainConstrainInRow(Rs, SupLim).

imposeRowConstrain([]).
imposeRowConstrain([B | Bs]) :- imposeRowConstrain(B, []),
								imposeRowConstrain(Bs).

imposeRowConstrain([], List) :- all_distinct(List).

imposeRowConstrain([cell(_, Value) | Rs], List) :- append([Value], List, NewList),
									  			   imposeRowConstrain(Rs, NewList).

imposeColumnConstrain(Board) :- transpose(Board, TBoard),
								imposeRowConstrain(TBoard).

imposeFieldConstrain(_Board, []).
imposeFieldConstrain(Board, [field(FID, Op, Res) | Fs]) :- getFieldCells(FID, Board, L),
										 				   applyOpConstrain(L, Op, Res),
										 				   imposeFieldConstrain(Board, Fs).

applyOpConstrain([], _Op, _Res) :- fail.

applyOpConstrain(L, '+', Res) :- sum(L, #=, Res).

applyOpConstrain(L, '-', Res) :- L = [_, _],
								 maximum(Max, L),
								 minimum(Min, L),
								 Res #= Max - Min.

applyOpConstrain(L, '/', Res) :- L = [_, _],
								 maximum(Max, L),
								 minimum(Min, L),
								 Res #= Max / Min.

applyOpConstrain([L | Ls], '*', Res) :- applyOpConstrain(Ls, '*', L, Res).

applyOpConstrain([], '*', Acum, Res) :- Acum #= Res.

applyOpConstrain([L | Ls], '*', Acum, Res) :- Acum1 #= L * Acum,
											  applyOpConstrain(Ls, '*', Acum1, Res).

applyOpConstrain([L], '=', Res) :- L #= Res.


/*********************************************************************************************
 *
 * Print
 *
 *********************************************************************************************/

printBoard :- testBoardPrint(Board),
			  printBoard(Board).

printBoard(Board) :- printTopBorder(Board), 
			  		 length(Board, Size), 
			  		 printBoard(Board, Size, 1), 
			  		 printBottomBorder(Board).

printTopBorder(Board) :- length(Board, Size), asserta(size(Size)),
						 write('   '), write('╔'),
						 printHorizTopBorder(Size).

printBoard([], _, _).
printBoard([B1, B2 | Bs], Size, RowIt) :- printNumber(RowIt), write('║'), 
									 printRow(B1),
									 write('   ║'),
									 printHorizMidBorder(B1, B2),
									 RowIt1 is RowIt + 1,
									 printBoard([B2 | Bs], Size, RowIt1).

printBoard([B1 | Bs], Size, RowIt) :- printNumber(RowIt), write('║'), 
									 printRow(B1),
									 RowIt1 is RowIt + 1,
									 printBoard(Bs, Size, RowIt1).

printRow([C1]) :- cell(_, Value1) = C1,  
				  printNumber(Value1),
				  write('║\n').

printRow([C1, C2 | Cs]) :- cell(FieldID1, Value1) = C1, 
						   cell(FieldID2, _Value2) = C2, 
						   FieldID1 = FieldID2, 
						   printNumber(Value1),
						   write('|'), 
						   printRow([C2 | Cs]).

printRow([C1 | Cs]) :- cell(_, Value1) = C1,
					   printNumber(Value1),
					   write('║'), 
					   printRow(Cs).

printBottomBorder(Board) :- length(Board, Size), write('   ╚'), printHorizBotBorder(Size), write(' '), printBottomNumbers(Size).

printHorizTopBorder(1):-  write('═══╗'), write('\n').

printHorizTopBorder(Size) :- write('════'), Size1 is Size - 1, printHorizTopBorder(Size1).

printHorizBotBorder(1):-  write('═══╝'), write('\n').

printHorizBotBorder(Size) :- write('════'), Size1 is Size - 1, printHorizBotBorder(Size1).

printBottomNumbers(Size) :- write('   '), printBottomNumbers(Size, 1).

printBottomNumbers(Size, Size) :- printNumber(Size).

printBottomNumbers(Size, Number) :- printNumber(Number), write(' '), Number1 is Number + 1, printBottomNumbers(Size, Number1).


printHorizMidBorder([R1], [R2]) :- cell(FieldID1, _) = R1, 
								   cell(FieldID2, _) = R2, 
								   FieldID1 = FieldID2, 
								   write('───║\n'). 

printHorizMidBorder([_R1], [_R2]) :- write('═══║\n').

printHorizMidBorder([R1 | R1s], [R2 | R2s]) :- cell(FieldID1, _) = R1, 
											   cell(FieldID2, _) = R2, 
											   FieldID1 = FieldID2, 
											   write('───═'), 
											   printHorizMidBorder(R1s, R2s). 

printHorizMidBorder([_R1 | R1s], [_R2 | R2s]) :- write('════'), printHorizMidBorder(R1s, R2s).

printNumber(Number) :- Number > 99, write(Number).
printNumber(Number) :- Number > 9, write(' '), write(Number).
printNumber(Number) :- write(' '), write(Number), write(' ').

printFieldTable(_, []).
printFieldTable(Board, [F | Fs]) :- RowIt = 1, printFieldTableAux(Board, F, RowIt), printFieldTable(Board, Fs).


printFieldTableAux([], _, _).
printFieldTableAux([B | Bs], F, RowIt) :- ColIt = 1, findFieldInCell(B, F, ColIt, Col), field(FID, Op, Val) = F, 
										  printField(FID, Op, Val, RowIt, Col).

printFieldTableAux([B | Bs], F, RowIt) :- RowIt1 is RowIt + 1, printFieldTableAux(Bs, F, RowIt1).

findFieldInCell([], _, _, _) :- fail.
findFieldInCell([cell(FID, _) | Bs], field(FID, _, _), ColIt, ColIt).
findFieldInCell([B | Bs], F, ColIt, Col) :- ColIt1 is ColIt + 1, findFieldInCell(Bs, F, ColIt1, Col).

printField(FID, Op, Val, Row, Col) :- write('Field: '), write(FID), write(' '), 
									  write('Operation: '), write(Op), write(' '),
									  write('Result: '), write(Val), write(' '),
									  write('Row: '), write(Row), write(' '),
									  write('Column: '), write(Col), write(' '),
									  write('\n').