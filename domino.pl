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
	getGameState(State),
	[TS, R, HS, CS, _, _, L, B, P, T] = State,
	random_permutation(B, Boneyard),
	getSixDominos(Boneyard, SixDominos1, Boneyard1),
	HumanHand = SixDominos1,
	getSixDominos(Boneyard1, SixDominos2, Boneyard2),
	ComputerHand = SixDominos2,
	NewState = [TS, R, HS, CS, HumanHand, ComputerHand, L, Boneyard2, P, T].

/* Distribute Hand */
distributeHands(FinalState) :-
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
findAndPlaceEngine(Player, Engine, Hand, Layout, NewHand, NewLayout) :-
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
	write("--------------Human's Turn--------------"),
	[TS, R, HS, CS, HH, CH, L, B, P, T] = State,
	getLeftRightPips(L, Left, Right).

/* Computer Turn */
play("computer", State) :-
	nl,
	write("-------------Computer's Turn--------------"),
	[TS, R, HS, CS, HH, CH, L, B, P, T] = State,
	getLeftRightPips(L, Left, Right).

/*----------------------------------------------------------------------------------*/
/*---------------------------------Start Game---------------------------------------*/
/*----------------------------------------------------------------------------------*/

startGame() :-
	distributeHands(State),
	placeEngine("human", State).









	