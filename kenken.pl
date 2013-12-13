:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

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

% for testing the domain constrains
testBoard2([[cell(1, _), cell(1, _)],
			[cell(1, _), cell(1, _)]]).

testFields2([field(1, +, 6)]).

cell(_FieldID, _Value).
field(_FieldID, _Op, _FinalValue).

solveBoard :- testBoard(Board),
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


createBoard(Size) :- length(Board, Size),
					 now(Now),
					 setrand(Now),
			   		 initBoard(Size, Board),
			   		 imposeDomainConstrain(Board, Size),
			   		 imposeRowConstrain(Board),
			   		 imposeColumnConstrain(Board),
			   		 generateRandomVals(Board, Size),
			   		 generateFields(Board, Fields),
			   		 write(Board), nl,
			   		 printBoard(Board),
			  		 write('\n\n'),
			 		 printFieldTable(Board, Fields). 



/*********************************************************************************************
 *
 * Generations
 *
 *********************************************************************************************/

initBoard(_Size, []).
initBoard(Size, [B | Bs]) :- length(B, Size),
							 initBoardRow(B),
							 initBoard(Size, Bs).

initBoardRow([]).
initBoardRow([cell(_FieldID, _Val) | Rs]) :- initBoardRow(Rs).

generateRandomVals(Board, Size) :- getValsList(Board, L), 
								   Sqrt is (Size * Size) + 1,
								   random(Size, Sqrt, X),
								   retractall(size(_)),
								   asserta(size(X)),
								   genBoard(L).

genBoard(L) :- labeling([], L), size(X), X1 is X - 1, asserta(size(X1)), X1 = 0.

not(X) :- X, !, fail.
not(_X).

generateFields(Board, Fields) :- generateFields(Board, Fields, 1).

generateFields(Board, [F | Fs], FieldIt) :- not(isBoardFilled(Board)), !,
								 			iterBoard(Board, F, FieldIt),
								 			FieldIt1 is FieldIt + 1,
								 			generateFields(Board, Fs, FieldIt1).

generateFields(_Board, [], _FieldIt).

isBoardFilled([]).
isBoardFilled([B | Bs]) :- isBoardFilledIterRow(B),
						   isBoardFilled(Bs).

isBoardFilledIterRow([]).
isBoardFilledIterRow([cell(FID, _) | Rs]) :- nonvar(FID),
											 isBoardFilledIterRow(Rs).

iterBoard(Board, F, FieldIt) :- field(FieldIt, _Op, _Res) = F,
								repeat,
								random(1, 6, OpIt),
								length(Board, BoardSize),
								initField(OpIt, F, BoardSize, FieldIt, FieldSize),
								getFstAvailCell(Board, Row, Col),
								getCell(Board, Row, Col, cell(FieldIt, Val)),
								random(1, 3, X),
								getNextCellPos(X, Row, Col, NewRow, NewCol),
								FieldSize1 is FieldSize - 1,
								%trace, % getFstAvailCell checked
								makeField(Board, NewRow, NewCol, F, FieldSize1, Val).

initField(1, field(FieldIt, '+', _Res), BoardSize, FieldIt, Size) :- MaxSize = BoardSize + 1, random(2, MaxSize, Size).
initField(2, field(FieldIt, '-', _Res), _BoardSize, FieldIt, 2).
initField(3, field(FieldIt, '*', _Res), BoardSize, FieldIt, Size) :- MaxSize = BoardSize + 1, random(2, MaxSize, Size).
initField(4, field(FieldIt, '/', _Res), _BoardSize, FieldIt, 2).
initField(5, field(FieldIt, '=', _Res), _BoardSize, FieldIt, 1).

makeField(_Board, _Row, _Col, field(_FieldIt, _Op, Acum), 0, Acum) :- !.
makeField(Board, Row, Col, field(FieldIt, Op, NewAcum), 1, Acum) :- getCell(Board, Row, Col, cell(FieldIt, Val)),
																	getNewAcum(Acum, Op, Val, NewAcum), !.
																		   
makeField(Board, Row, Col, field(FieldIt, Op, Res), FieldSize, Acum) :- getCell(Board, Row, Col, cell(_FieldID, Val)),
																		getNewAcum(Acum, Op, Val, NewAcum),
																		random(1, 3, X),
																		getNextCellPos(X, Row, Col, NewRow, NewCol),
																		getCell(Board, NewRow, NewCol, cell(FID, _)),
																		FID = FieldIt,	%se falhar, backtracking para new random
																  		FieldSize1 is FieldSize -1,
											  				 	  		makeField(Board, NewRow, NewCol, field(FieldIt, Op, Res), FieldSize1, NewAcum).


getNextCellPos(1, Row, Col, Row, NewCol) :- NewCol is Col + 1.
getNextCellPos(2, Row, Col, NewRow, Col) :- NewRow is Row + 1.

getCell([B | _Bs], 1, Col, Cell) :- getCellInRow(B, Col, Cell).
getCell([_B | Bs], Row, Col, Cell) :- Row1 is Row - 1,
								  	 getCell(Bs, Row1, Col, Cell).

getCellInRow([C | _Cs], 1, C).
getCellInRow([_C | Cs], Col, Cell) :- Col1 is Col - 1,
							  		 getCellInRow(Cs, Col1, Cell).

getNewAcum(Acum, '+', Val, NewAcum) :- NewAcum is Acum + Val.
getNewAcum(Acum, '-', Val, NewAcum) :- min_member(Min, [Acum, Val]),
									   max_member(Max, [Acum, Val]),
									   NewAcum is Max - Min.
getNewAcum(Acum, '*', Val, NewAcum) :- NewAcum is Acum * Val.
getNewAcum(Acum, '/', Val, NewAcum) :- min_member(Min, [Acum, Val]),
									   max_member(Max, [Acum, Val]),
									   NewAcum is Max / Min.
getNewAcum(_Acum, '=', Val, Val).

getFstAvailCell(Board, RetRow, RetCol) :- getFstAvailCell(Board, 1, RetRow, RetCol).

getFstAvailCell([], _RowIt, _RetRow, _RetCol) :- fail.
getFstAvailCell([B | _Bs], RowIt, RowIt, RetCol) :- getFstAvailCellInRow(B, 1, RetCol).
getFstAvailCell([_B | Bs], RowIt, RetRow, RetCol) :- RowIt1 is RowIt + 1,
													getFstAvailCell(Bs, RowIt1, RetRow, RetCol).

getFstAvailCellInRow([], _, _) :- fail.
getFstAvailCellInRow([cell(FID, _) | _Rs], ColIt, ColIt) :- var(FID).
getFstAvailCellInRow([_R | Rs], ColIt, RetCol) :- ColIt1 is ColIt + 1,
												  getFstAvailCellInRow(Rs, ColIt1, RetCol).



/*********************************************************************************************
 *
 * Getters
 *
 *********************************************************************************************/

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

applyOpConstrain([L], '=', Res) :- L #= Res.

applyOpConstrain([L | Ls], '*', Res) :- applyOpConstrain(Ls, '*', L, Res).

applyOpConstrain([], '*', Acum, Res) :- Acum #= Res.

applyOpConstrain([L | Ls], '*', Acum, Res) :- Acum1 #= L * Acum,
											  applyOpConstrain(Ls, '*', Acum1, Res).


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

printNumber(Number) :- var(Number), write('   ').
printNumber(Number) :- Number > 99, write(Number).
printNumber(Number) :- Number > 9, write(' '), write(Number).
printNumber(Number) :- write(' '), write(Number), write(' ').

printFieldTable(_, []).
printFieldTable(Board, [F | Fs]) :- RowIt = 1, printFieldTableAux(Board, F, RowIt), printFieldTable(Board, Fs).


printFieldTableAux([], _, _).
printFieldTableAux([B | _Bs], F, RowIt) :- ColIt = 1, findFieldInCell(B, F, ColIt, Col), field(FID, Op, Val) = F, 
										  printField(FID, Op, Val, RowIt, Col).

printFieldTableAux([_B | Bs], F, RowIt) :- RowIt1 is RowIt + 1, printFieldTableAux(Bs, F, RowIt1).

findFieldInCell([], _, _, _) :- fail.
findFieldInCell([cell(FID, _) | _Bs], field(FID, _, _), ColIt, ColIt).
findFieldInCell([_B | Bs], F, ColIt, Col) :- ColIt1 is ColIt + 1, findFieldInCell(Bs, F, ColIt1, Col).

printField(FID, Op, Val, Row, Col) :- write('Field: '), write(FID), write(' '), 
									  write('Operation: '), write(Op), write(' '),
									  write('Result: '), write(Val), write(' '),
									  write('Row: '), write(Row), write(' '),
									  write('Column: '), write(Col), write(' '),
									  write('\n').
