/*----------------------------------------------------------------------------------*/
/*----------------------------Parse Data and Print Data-----------------------------*/
/*----------------------------------------------------------------------------------*/

/* Clear the screen */
clear :-  write('\e[2J').

/* New Game Data */
getRawData([200,1, 
	[[], 0], 
	[[], 0], 
	[], 
	[[0, 0], [0, 1], [0, 2], [0, 3], [0, 4], [0, 5], [0, 6], 
	[1, 1], [1, 2], [1, 3], [1, 4], [1, 5], [1, 6], [2, 2], 
	[2, 3], [2, 4], [2, 5], [2, 6], [3, 3], [3, 4], [3, 5], [3, 6],
	[4, 4], [4, 5], [4, 6], [5, 5], [5, 6], [6, 6]], 
	false, computer
	]).

/* Get Game State of New or Loaded Game */
getGameState(State):-
	getRawData(Data),
	[TS|_] = Data,
	[ _, R | _ ] = Data,
	[ _, _, [_, HS] | _ ] = Data,
	[ _, _, _, [_, CS] | _ ] = Data,
	[ _, _, [HH, _] | _ ] = Data,
	[ _, _, _, [CH, _] | _ ] = Data,
	[ _, _, _, _, L | _ ] = Data,
	[ _, _, _, _, _, B  | _ ] = Data,
	[ _, _, _, _, _, _, P, _ ] = Data,
	[ _, _, _, _, _, _, _, T] = Data,
	State = [TS, R, HS, CS, HH, CH, L, B, P, T].

/* Print the Game State */
printGameDetails(State):-
	[TS, R, HS, CS, HH, CH, L, B, P, T] = State,
	format('~nRound: ~w ~nTournament Score: ~w ~nLayout: L- ~w -R ~nBoneyard: ~w ~nHuman Score: ~w ~nComputer Score: ~w ~nHuman Hand: ~w ~nComputer Hand: ~w ~nTurn: ~w ~nPrevious Player Passed: ~w ~n', 
			[R, TS, L, B, HS, CS, HH, CH, T, P]).







/*----------------------------------------------------------------------------------*/
/*---------------------------------Distribute Hand----------------------------------*/
/*----------------------------------------------------------------------------------*/

/* Distribue Hand for New Game */
getSixDominos([D1, D2, D3, D4, D5, D6 | Tail], SixDominos, NewBoneyard) :-
	SixDominos = [D1, D2, D3, D4, D5, D6],
	NewBoneyard = Tail.

distributeHandsToPlayers(State, NewState) :-
	[TS, R, HS, CS, _, _, L, B, P, T] = State,
	random_permutation(B, Boneyard),
	getSixDominos(Boneyard, SixDominos1, Boneyard1),
	HumanHand = SixDominos1,
	getSixDominos(Boneyard1, SixDominos2, Boneyard2),
	ComputerHand = SixDominos2,
	NewState = [TS, R, HS, CS, HumanHand, ComputerHand, L, Boneyard2, P, T].

/* Distribute Hand */
distributeHands(State, FinalState) :-
	getGameState(State),
	[_, _, _, _, _, _, L, _, _, _] = State,
	length(L, 0),
	distributeHandsToPlayers(State, NewState),
	printGameDetails(NewState),
	FinalState = NewState.





/*----------------------------------------------------------------------------------*/
/*---------------------------------Find and Place Engine----------------------------*/
/*----------------------------------------------------------------------------------*/

/* Add one Domino to each Player */
addOneDomino(State, NewState) :-
	[TS, R, HS, CS, HH, CH, L, B, P, T] = State,
	[Head1 | _] = B,
	append([Head1], HH, HumanHand),
	delete(B, Head1, Boneyard1),
	[Head2 | _] = Boneyard1,
	append([Head2], CH, ComputerHand),
	delete(Boneyard1, Head2, Boneyard2),
	NewState = [TS, R, HS, CS, HumanHand, ComputerHand, L, Boneyard2, P, T],
	write("Added Domino to each Player."), nl,
	printGameDetails(NewState).

/* Find and place Engine */
findAndPlaceEngine(_, Engine, Hand, Layout, NewHand, NewLayout) :-
	delete(Hand, [Engine, Engine], NewHand),
	append([[Engine, Engine]], Layout, NewLayout).

/* Find if Human has Engine */ 
placeEngine("human", State) :-
	[TS, R, HS, CS, HH, CH, L, B, P, _] = State,
	Engine is 7 - R,
	(member([Engine, Engine], HH) ->
		(
			findAndPlaceEngine("human", Engine, HH, L, HumanHand, Layout),
			NewState = [TS, R, HS, CS, HumanHand, CH, Layout, B, P, "computer"],
			write("Human placed the Engine."), nl,
			nextTurn(NewState)
		);
			placeEngine("computer", State)
	).

/* Find if Computer has Engine */
placeEngine("computer", State) :-
	[TS, R, HS, CS, HH, CH, L, B, P, _] = State,
	Engine is 7 - R,
	(member([Engine, Engine], CH) ->
		(
			findAndPlaceEngine("Computer", Engine, CH, L, ComputerHand, Layout),
			NewState = [TS, R, HS, CS, HH, ComputerHand, Layout, B, P, "human"],			
			write("Computer placed the Engine."), nl,
			nextTurn(NewState)
		);
			write("Engine Not Found. Drawing one domino for each player."),nl,
		addOneDomino(State, NewState),
		placeEngine("human", NewState)
	).





/*----------------------------------------------------------------------------------*/
/*---------------------------------Find Best Moves----------------------------------*/
/*----------------------------------------------------------------------------------*/

/* Find Possible Moves in the side with end Point - Pip */
hasPIP(Pip, Head) :-
	=([Pip,_], Head); 
	=([_, Pip], Head).

findPossibleMoves(Pip, Hand, PossibleMoves) :-
	include(hasPIP(Pip), Hand, PossibleMoves).

/* Find Best Moves from the Possible Moves */
findBestMove([], _, X, Y) :-
	Y = X.

findBestMove([Head|Tail], MaxSum, BestMove, ReturnValue) :-
	[X, Y] = Head,
	Sum is X+Y,
	(Sum > MaxSum ->
		NewMaxSum is Sum,
		NewBestMove = Head,
		findBestMove(Tail, NewMaxSum, NewBestMove, ReturnValue)
		;
		findBestMove(Tail, MaxSum, BestMove, ReturnValue)
	).
	
/* Find Best Possible Move for Human */
findBestPossibleMove(Left, Right, "human", Hand, Passed, BestMove) :-
	(=(Passed, "true") ->
		findPossibleMoves(Left, Hand, PossibleMovesLeft),
		findPossibleMoves(Right, Hand, PossibleMovesRight),
		union(PossibleMovesLeft, PossibleMovesRight, PossibleMoves),
		Moves = PossibleMoves
		;
		findPossibleMoves(Left, Hand, PossibleMovesLeft),
		(member([Right, Right], Hand) ->
			append([[Right, Right]], PossibleMovesLeft, NewPossibleMoves),
			Moves  = NewPossibleMoves
			;
			Moves = PossibleMovesLeft
		)
	),
	findBestMove(Moves, 0, [], Move),
	(=(Move, []) ->
		BestMove=[Move, "No Moves Possible. Please draw a domino from boneyard. If already drawn, pass."]
		;
		(member(Move, PossibleMovesLeft) ->
			BestMove = [Move, "left"],
			Position = "left"
			;
			BestMove = [Move, "right"],
			Position = "right"
		),
		format("~w has the maximum sum out of all possible moves. Hence, best Move is ~w on ~w.", [Move, Move, Position])
	).
	
	
/* Find Best Possible Move for Computer */
findBestPossibleMove(Left, Right, "computer", Hand, Passed, BestMove) :-
	(=(Passed, "true") ->
		findPossibleMoves(Left, Hand, PossibleMovesLeft),
		findPossibleMoves(Right, Hand, PossibleMovesRight),
		union(PossibleMovesLeft, PossibleMovesRight, PossibleMoves),
		Moves = PossibleMoves
		;
		findPossibleMoves(Right, Hand, PossibleMovesRight),
		(member([Left, Left], Hand) ->
			append([[Left, Left]], PossibleMovesRight, NewPossibleMoves),
			Moves  = NewPossibleMoves
			;
			Moves = PossibleMovesRight
		)
	),
	findBestMove(Moves, 0, [], Move),
	(=(Move, []) ->
		BestMove=[Move, "No Moves Possible"]
		;
		(member(Move, PossibleMovesRight) ->
			BestMove = [Move, "right"],
			Position = "right"
			;
			BestMove = [Move, "left"],
			Position = "left"
		),
		format("~w has the maximum sum out of all possible moves. Hence, best Move is ~w on ~w.", [Move, Move, Position])
	).





	

/*----------------------------------------------------------------------------------*/
/*---------------------------------Play Turns---------------------------------------*/
/*----------------------------------------------------------------------------------*/

/* Get Left and Right ends of Board */
getLeftRightPips(Layout, Left, Right) :-
	nth1(1, Layout, First),
	[Left, _] = First,
	last(Layout, Last),
	[_, Right] = Last.

/* Play the Turn */
nextTurn(State) :-
	[_, _, _, _, _, _, _, _, _, T] = State,
	printGameDetails(State),
	play(T, State).

/* Human Turn */
play("human", State) :-
	nl,
	write("--------------Human's Turn--------------"),nl,
	[TS, R, HS, CS, HH, CH, L, B, P, T] = State,
	getLeftRightPips(L, Left, Right),
	findBestPossibleMove(Left, Right, "human", HH, P, BestMove).

/* Computer Turn */
play("computer", State) :-
	nl,
	write("-------------Computer's Turn--------------"),nl,
	[TS, R, HS, CS, HH, CH, L, B, P, T] = State,
	getLeftRightPips(L, Left, Right),
	findBestPossibleMove(Left, Right, "computer", CH, P, BestMove).







/*----------------------------------------------------------------------------------*/
/*---------------------------------Start Game---------------------------------------*/
/*----------------------------------------------------------------------------------*/

startGame() :-
	getGameState(State),
	distributeHands(State, NewState),
	placeEngine("human", NewState).









	
