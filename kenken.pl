testBoard([[cell(1, _), cell(2, _), cell(2, _), cell(3, _)],
		   [cell(1, _), cell(4, _), cell(5, _), cell(3, _)],
		   [cell(6, _), cell(4, _), cell(5, _), cell(5, _)],
		   [cell(6, _), cell(7, _), cell(7, _), cell(8, _)],]).

testFields([field(1, '-', 1),
			field(2, '/', 2),
			field(3, '-', 3),
			field(4, '-', 2),
			field(5, '*', 24),
			field(6, '-', 1),
			field(7, '+', 7),
			field(8, '=', 2),]).

cell(FieldID, Value).
field(FieldID, Op, FinalValue).

solveBoard(Size, Board) :- length(Board, Size),
						   initBoard(Size, Board),
						   imposeDomainConstrain(Board, Size),
						   imposeRowConstrain(Board),
						   imposeColumnConstrain(Board),

						   .




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
imposeRowConstrain([B | Bs]) :- imposeRowConstrain(B, []]),
								imposeRowConstrain(Bs).

imposeRowConstrain([], List) :- all_distinct(List).
imposeRowConstrain([R | Rs], List) :- cell(_, Value) = R,
									  append([Value], List, NewList),
									  imposeRowConstrain(Rs, NewList).

imposeColumnConstrain(Board) :- transpose(Board, TBoard),
								imposeRowConstrain(TBoard).