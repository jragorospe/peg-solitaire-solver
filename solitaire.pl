/*
* CPSC 449: PROGRAMMING PARADIGMS
* PROLOG PEG SOLITAIRE ASSIGNMENT
* JOSEPH GOROSPE - 10155928
* THIS PROGRAM GENERATES A LIST OF MOVES THAT CAN SOLVE VARIOUS ENGLISH PEG SOLITAIRE BOARDS
*/

% GOAL WEIGHTS - goal_wgt(Game,Pagoda,GoalWeight)
goal_wgt(crossbow,simple,0).
goal_wgt(longbow,simple,0).
goal_wgt(full,simple,1).
goal_wgt(notQuiteDead,simple,1).
goal_wgt(halfDead,simple,1).

goal_wgt(crossbow,asymmetric,0).
goal_wgt(longbow,asymmetric,0).
goal_wgt(full,asymmetric,2).
goal_wgt(notQuiteDead,asymmetric,2).
goal_wgt(halfDead,asymmetric,2).

goal_wgt(crossbow,asymmetric2,0).
goal_wgt(longbow,asymmetric2,0).
goal_wgt(full,asymmetric2,2).
goal_wgt(notQuiteDead,asymmetric2,2).
goal_wgt(halfDead,asymmetric2,2).


% PAGODA PREDICATES - pagoda(Pagoda,Position,Weight)
% SIMPLE
pagoda(simple,13,1).
pagoda(simple,31,1).
pagoda(simple,33,1).
pagoda(simple,35,1).
pagoda(simple,53,1).

% ASYMMETRIC
pagoda(asymmetric,13,1).
pagoda(asymmetric,20,-1).
pagoda(asymmetric,21,1).
pagoda(asymmetric,23,1).
pagoda(asymmetric,25,1).
pagoda(asymmetric,26,-1).
pagoda(asymmetric,31,2).
pagoda(asymmetric,33,2).
pagoda(asymmetric,35,2).
pagoda(asymmetric,40,-1).
pagoda(asymmetric,41,1).
pagoda(asymmetric,43,1).
pagoda(asymmetric,45,1).
pagoda(asymmetric,46,-1).
pagoda(asymmetric,53,1).

% ASYMMETRIC ROTATED 90 DEGREES
pagoda(asymmetric2,2,-1).
pagoda(asymmetric2,4,-1).
pagoda(asymmetric2,12,1).
pagoda(asymmetric2,13,2).
pagoda(asymmetric2,14,1).
pagoda(asymmetric2,31,1).
pagoda(asymmetric2,32,1).
pagoda(asymmetric2,33,2).
pagoda(asymmetric2,34,1).
pagoda(asymmetric2,35,1).
pagoda(asymmetric2,52,1).
pagoda(asymmetric2,53,2).
pagoda(asymmetric2,54,1).
pagoda(asymmetric2,62,-1).
pagoda(asymmetric2,64,-1).

% onboard(Position) - CHECKS IF POSITION IS ON THE BOARD
onboard(Pos) :- 2 =< Pos, Pos =< 4.
onboard(Pos) :- 12 =< Pos, Pos =< 14.
onboard(Pos) :- 20 =< Pos, Pos =< 26.
onboard(Pos) :- 30 =< Pos, Pos =< 36.
onboard(Pos) :- 40 =< Pos, Pos =< 46.
onboard(Pos) :- 52 =< Pos, Pos =< 54.
onboard(Pos) :- 62 =< Pos, Pos =< 64.

% CHECKS IF START, JUMP, AND END POSITION ARE IN BOUNDS
% JUMP RIGHT
jump(Start, Jumped, End) :-
    Jumped is Start + 1,
    End is Start + 2,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

% JUMP LEFT
jump(Start, Jumped, End) :-
    Jumped is Start - 1,
    End is Start - 2,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

% JUMP DOWN
jump(Start, Jumped, End) :-
    Jumped is Start + 10,
    End is Start + 20,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

% JUMP UP
jump(Start, Jumped, End) :-
    Jumped is Start - 10,
    End is Start - 20,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

% TAKES IN START BOARD, A MOVE AND RETURNS A NEW BOARD
solitaire_move(SB, (Start, End), [End|SB2]) :-
    select(Start, SB, SB1),
    jump(Start, Jumped, End),
    select(Jumped, SB1, SB2),
    not(member(End,SB2)). % if the space End is empty  

% MAIN PROGRAM. TAKES IN GAME, START BOARD, HISTORY OF MOVES AND GOAL POSITION.
% CAN EVALUATE A MOVE LIST FOR VALIDITY
% OR IN OUR CASE RETURN A VALID MOVE LIST
% CHECKS FOR INDEPENDENCE AND PAGODA FUNCTION AND RECURSES BACK INTO ITSELF
% UNTIL A MOVE LIST IS CREATED
% ONCE GOAL POSITION IS REACHED, END.
solitaire_steps(GameName, SB, [Mv|Moves], Hist, GB) :-
    solitaire_move(SB, Mv, SB1),
    independent(Mv, Hist),
    findall((P,W),(member(P,[simple,asymmetric,asymmetric2]),wgt(P,SB1,W)),Wgts),
    check_wgts(GameName,Wgts),
    solitaire_steps(GameName, SB1, Moves, [Mv|Hist], GB).
solitaire_steps(_,GB, [], _, GB).

% CHECKS IF ANY ELEMENT IN LIST 1 AND LIST 2 ARE THE SAME
% CALLS JUMP PREDICATE TO GRAB JUMPED POSITIONS OF BOTH "LISTS"
overlap((A,B),(C,D)) :- 
    jump(A,X,B),
    jump(C,Y,D),
    (A is C; A is Y; A is D;
    X is C; X is Y; X is D;
    B is C; B is Y; B is D).

% CHECKS IF FORMER MOVE IS HAS SMALLER START POINT
lexorder((A,_),(C,_)) :-
    A =< C.

% INDEPENDENT CHECKS IF THE MOVE IS OKAY TO BE ADDED TO THE LIST OF MOVES
% TAKES IN A NEW MOVE AND A HISTORY OF PAST MOVES
% OVERLAP CHECKS IF ANY MOVES HAS MATCHING ELEMENTS TO EXISTING MOVES BY 
% ITERATING THROUGH THE HISTORY LIST
% IF SO, THE MOVE IS DEPENDENT AND AUTOMATICALLY SUCCEEDS TO PREVENT BACKTRACKING
% OTHERWISE, THE MOVE IS INDEPENDENT IN WHICH CASE LEXORDER GIVES MOVES A RANKING
% AND RETURN MOVES THAT HAVE LESSER START POINTS.
% ie. IF MOVE [(2,4),(10,12)] EXISTS AND [(10,12),(2,4)] EXISTS TOO THE LATTER WILL BE IGNORED
% INDEPENDENT IS THEN CALLED AGAIN UNTIL YOU ITERATE THROUGH THE ENTIRE HISTORY OR WE FIND AN OVERLAP
independent(_, []).
independent(Mv, [H|_]) :-
  overlap(Mv, H),!.
independent(Mv, [H|T]) :-
  lexorder(Mv, H),
  independent(Mv, T).

% SPECIFIES THE WEIGHT OF PAGODA FUNCTION POSITIONS
wgt(_, [], 0).
wgt(P, [Pos|T], Weight) :-
    (pagoda(P, Pos, PWgt) ; PWgt = 0), !, % IF NO POSITION SPECIFIED IN PREDICATES, RETURN 0 WEIGHT
    wgt(P, T, Weight2),
    Weight is PWgt + Weight2.

% CHECKS WEIGHT GIVEN THE GAME NAME AND WEIGHT LIST SPECIFIED BY GAME NAME
check_wgts(_, []).
check_wgts(G, [(P, WgtP)| Rest]) :-
    goal_wgt(G,P,WgtGoal),
    WgtP >= WgtGoal,
    check_wgts(G, Rest).

% CHECKS IF POSITION IS MEMBER OF THE GAMEBOARD
% IF GOAL POSITION IS NOT OCCUPIED, WRITE AN 'o' IN IT
% OTHERWISE, PRINT 'x' OR IF EMPTY, ' '
checkPos(Pos, X, G) :-
    (member(Pos, X) -> write('x'); 
    member(Pos, G) -> write('o'); 
    write(' ')),
    write(' | ').

% DISPLAYS BOARD FOR EACH STEP
% CALLS SOLITAIRE_MOVES TO UPDATE THE GAMEBOARD
% AND FEED THE GAMEBOARD TO BE PRINTED
displayMoves(_, [], _).
displayMoves(SB, [Mv | T], G) :-
    solitaire_move(SB, Mv, NB),
    nl, writeln('            CONTINUE? [.]'), get(_),
    printboard(NB, G),
    displayMoves(NB, T, G).

% PRINTS THE GAMEBOARD BY CHECKING WHETHER EACH POSITION EXISTS IN THE CURRENT GAMESTATE
printboard(X,G) :-
    write('\n    | a | b | c | d | e | f | g |\n'),
    write('  1 |   |   | '), checkPos(2,X,G), checkPos(3,X,G), checkPos(4,X,G), write('  |   |\n'),
    write('  2 |   |   | '), checkPos(12,X,G), checkPos(13,X,G), checkPos(14,X,G), write('  |   |\n'),
    write('  3 | '), checkPos(20,X,G), checkPos(21,X,G), checkPos(22,X,G), checkPos(23,X,G), checkPos(24,X,G), checkPos(25,X,G), checkPos(26,X,G), write('\n'),
    write('  4 | '), checkPos(30,X,G), checkPos(31,X,G), checkPos(32,X,G), checkPos(33,X,G), checkPos(34,X,G), checkPos(35,X,G), checkPos(36,X,G), write('\n'),
    write('  5 | '), checkPos(40,X,G), checkPos(41,X,G), checkPos(42,X,G), checkPos(43,X,G), checkPos(44,X,G), checkPos(45,X,G), checkPos(46,X,G), write('\n'),
    write('  6 |   |   | '), checkPos(52,X,G), checkPos(53,X,G), checkPos(54,X,G), write('  |   |\n'),
    write('  7 |   |   | '), checkPos(62,X,G), checkPos(63,X,G), checkPos(64,X,G), write('  |   |\n').    

% PRINTS STARTING BOARD AND EVERY STEP BASED ON UPDATING GAMEBOARD
% ALSO RETURNS EXECUTION TIME FOR MOVE GENERATION
peg(full) :-
    statistics(walltime, [_|[]]),
    solitaire_steps(full,[2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64],X,[],[33]),
    statistics(walltime, [_|[ExecutionTime]]),
    printboard([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64],[33]),
    displayMoves([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64],X,[33]),
    nl, write('EXECUTION TIME: '), write(ExecutionTime), write(' ms.'), nl.

peg(notQuiteDead) :-
    statistics(walltime, [_|[]]),
    solitaire_steps(notQuiteDead,[2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64],X,[],[33]),
    statistics(walltime, [_|[ExecutionTime]]),
    printboard([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64],[33]),
    displayMoves([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64],X,[33]),
    nl, write('EXECUTION TIME: '), write(ExecutionTime), write(' ms.'), nl.
    
peg(halfDead) :-
    statistics(walltime, [_|[]]),
    solitaire_steps(halfDead,[20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64],X,[],[33]),
    statistics(walltime, [_|[ExecutionTime]]),
    printboard([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64],[33]),
    displayMoves([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64],X,[33]),
    nl, write('EXECUTION TIME: '), write(ExecutionTime), write(' ms.'), nl.

peg(crossbow) :-
    statistics(walltime, [_|[]]),
    solitaire_steps(crossbow,[31,32,34,35,41,42,43,44,45,53],X,[],[3]),
    statistics(walltime, [_|[ExecutionTime]]),
    printboard([31,32,34,35,41,42,43,44,45,53],[3]),
    displayMoves([31,32,34,35,41,42,43,44,45,53],X,[3]),
    nl, write('EXECUTION TIME: '), write(ExecutionTime), write(' ms.'), nl.
    
peg(longbow) :-
    statistics(walltime, [_|[]]),
    solitaire_steps(longbow,[20,26,30,31,33,35,36,41,43,45,52,53,54,63],X,[],[3]),
    statistics(walltime, [_|[ExecutionTime]]),
    printboard([20,26,30,31,33,35,36,41,43,45,52,53,54,63],[3]),
    displayMoves([20,26,30,31,33,35,36,41,43,45,52,53,54,63],X,[3]),
    nl, write('EXECUTION TIME: '), write(ExecutionTime), write(' ms.'), nl.

    