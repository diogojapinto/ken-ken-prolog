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

cell(FieldID, Value).
field(FieldID, Op, FinalValue).

solveBoard(Size, Board) :- /*length(Board, Size),*/
						   initBoard(Size, Board),
						   imposeDomainConstrain(Board, Size),
						   imposeRowConstrain(Board),
						   imposeColumnConstrain(Board),
						   
						   labeling([], Board),
						   printBoard(Board).




/*********************************************************************************************
 *
 * Initialization
 *
 *********************************************************************************************/

initBoard(Size, []).
initBoard(Size, [B | Bs]) :- length(B, Size),
							 initBoard(Size, Bs).

getFieldCells(_, [], []).
getFieldCells(FieldID, [B | Bs], RetList) :- getFieldCellsInRow(FieldID, B, RetList), 
											 getFieldCells(FieldID, Bs, RetList).

getFieldCellsInRow(FieldID, [], []).
getFieldCellsInRow(FieldID, [R | Rs], RetList) :- cell(FieldID, _), append([R], RetList, RetList), 
												  getFieldCellsInRow(FieldID, Rs, RetList).
getFieldCellsInRow(FieldID, [R | Rs], RetList) :- getFieldCellsInRow(FieldID, Rs, RetList).



/*********************************************************************************************
 *
 * Restrictions
 *
 *********************************************************************************************/

imposeDomainConstraint([], _).
imposeDomainConstraint([B | Bs], SupLim) :- imposeDomainConstraintInRow(B, SupLim),
											imposeDomainConstraint(Bs, SupLim).

imposeDomainConstraintInRow([R | Rs], SupLim) :- cell(_, Value),
												 domain(Value, 1, SupLim),
										 		 imposeDomainConstraintInRow(Rs, SupLim).

imposeRowConstrain([]).
imposeRowConstrain([B | Bs]) :- imposeRowConstrain(B, []),
								imposeRowConstrain(Bs).

imposeRowConstrain([], List) :- all_distinct(List).
imposeRowConstrain([R | Rs], List) :- cell(_, Value) = R,
									  append([Value], List, NewList),
									  imposeRowConstrain(Rs, NewList).

imposeColumnConstrain(Board) :- transpose(Board, TBoard),
								imposeRowConstrain(TBoard).

/*********************************************************************************************
 *
 * Print
 *
 *********************************************************************************************/
/*, printBoardLine(Board), printBottomBorder(Board)*/
printBoard :- testBoardPrint(Board)/*, printTopBorder(Board)*/, printBoardLine(Board)/*, printBottomBorder(Board)*/.

printTopBorder(Board) :- length(Board, Size), printTopBorder(Board, Size).

printTopBorder(Board, 0):-  write('\n').
printTopBorder(Board, Size) :- write('_'), Size1 is Size - 1, printTopBorder(Board, Size1).

printBoardLine(Board) :- length(Board, Size), printBoardLine(Board, Size, Size+1).

printBoardLine(Board, Size, Line) :- write('OLA'), Size = Line + 1, write('|'), Line1 is Line - 1/*, printBoardLine(Board, Size, Line1)*/.

printBoardLine(_,_,0) :- write('|').
printBoardLine([B | BS], Size, LineIt) :- ColIt is Size, printCell(B, ColIt), LineIt1 is LineIt - 1, printBoardLine(Bs, Size, LineIt1).

printCell(_, 0) :- write('||').

printCell([C1, C2 | Cs], ColIt) :- cell(FieldID1, Value1) = C1, cell(FieldID2, Value2) = C2, 
									FieldID1 = FieldID2, write('|'), write(Value1), ColIt1 is ColIt - 1, printCell([C2 | Cs], ColIt1).

printCell([C1, C2 | Cs], ColIt) :- cell(_, Value1) = C1, cell(_, Value2) = C2, 
									write('||'), write(Value1), ColIt1 is ColIt - 1, printCell([C2 | Cs], ColIt1).

printBottomBorder(Board) :- length(Board, Size), printBottomBorder(Board, Size).

printBottomBorder(Board, 0):-  write('\n'), length(Board, Size), printBottomNumbers(Size).

printBottomBorder(Board, Size) :- write('_'), Size1 is Size - 1, printBottomBorder(Board, Size1).

printBottomNumbers(Size) :- printBottomNumbers(Size, 1).

printBottomNumbers(Size, Size) :- write(Size).

printBottomNumbers(Size, Number) :- write(Number), Number1 is Number + 1, printBottomNumbers(Size, Number1).