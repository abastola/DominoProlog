%------------------------------------------------------------------------------------
%   Name: Arjun Bastola
%	Class: OPL
% 	Project: Longana / Prolog
%	Date: December 9, 2017
%------------------------------------------------------------------------------------

/*----------------------------------------------------------------------------------*/
/*---------------------------------Start Game---------------------------------------*/
/*----------------------------------------------------------------------------------*/

%------------------------------------------------------------------------------------
%   Predicate: beginGame()
%	Purpose: Ask user if they want to play new game or load. Begin game accordigly.
% 	Parameters: N/A
%	Local Variables: Filename - User option 
% 					 TournamentScore: User input for TournamentScore
% 					 Data: Unparsed game data from File
% 					 State: Parsed game data from File
% 					 NewState: New State after placing hand.
% 					 NewRoundState: New State with updated Tournament Score.
%------------------------------------------------------------------------------------

/* start the game */
beginGame() :-
	working_directory(_, "C:/Users/ZDF6BJK/Desktop/Prolog"),
	write("Enter 'new' for new game or 'filename' to load file: "),
	read_line_to_codes(user_input, Raw_input),
	atom_string(Raw_input, Filename),
	(=(Filename, "new") ->
		write("Enter tournament score: "),
		read(TournamentScore),
		getRawData(Data),
		getGameState(Data, State),
		distributeHands(State, NewState),
		[_, Round, Human_Score, Computer_Score, Human_Hand, Computer_Hand, Layout, Boneyard, Passed, Turn, PassedCount] = NewState,
		NewRoundState = [TournamentScore, Round, Human_Score, Computer_Score, Human_Hand, Computer_Hand, Layout, Boneyard, Passed, Turn, PassedCount],
		placeEngine("human", NewRoundState)	
		;
		open(Filename, read, Str),
		read(Str, Data),
		close(Str),
		getGameState(Data, State),
		printGameDetails(State),
		[_, _, _, _, _, _, BoardLayout, _, _, _, _] = State,
		(length(BoardLayout, 0) -> placeEngine("human", State); 
		nextTurn(State))
	).



/*----------------------------------------------------------------------------------*/
/*----------------------------Parse Data and Print Data-----------------------------*/
/*----------------------------------------------------------------------------------*/

%------------------------------------------------------------------------------------
%   Predicate: getRawData(FileData)
%	Purpose: Open the file containing new Game data.
% 	Parameters: FileData - Return Variable for filedata
%	Local Variables: Str- Stream.
%------------------------------------------------------------------------------------

/* New Game Data */
getRawData(FileData) :-
	open("abc.txt", read, Str),
	read(Str, FileData),
	close(Str).

%------------------------------------------------------------------------------------
%   Predicate: getGameState(Data, State)
%	Purpose: Parse raw file data.
% 	Parameters: Data : raw file data; State: Return value for parsed game State.
%	Local Variables: Game Components
%------------------------------------------------------------------------------------

/* Get Game State of New or Loaded Game */
getGameState(Data, State):-
	[TournamentScore|_] = Data,
	[ _, Round | _ ] = Data,
	[ _, _, Human_Hand | _ ] = Data,
	[ _, _, _, Human_Score | _ ] = Data,
	[ _, _, _, _, Computer_Hand | _ ] = Data,
	[ _, _, _, _, _, Computer_Score | _ ] = Data,
	[ _, _, _, _, _, _, Layout | _ ] = Data,
	[ _, _, _, _, _, _, _, Boneyard | _ ] = Data,
	[ _, _, _, _, _, _, _, _, Passed | _ ] = Data,
	[ _, _, _, _, _, _, _, _, _, Turn ] = Data,
	atom_string(Turn, TurnStr),
	atom_string(Passed, PassedStr),
	State = [TournamentScore, Round, Human_Score, Computer_Score, Human_Hand, Computer_Hand, Layout, Boneyard, PassedStr, TurnStr, 0].


%------------------------------------------------------------------------------------
%   Predicate: printGameDetails(State)
%	Purpose: Print the Game State after each turn.
% 	Parameters: Data : State: parsed game State.
%	Local Variables: Game Components
%------------------------------------------------------------------------------------

/* Print the Game State */
printGameDetails(State):-
	[TournamentScore, Round, Human_Score, Computer_Score, Human_Hand, Computer_Hand, Layout, Boneyard, Passed, Turn, _] = State,
	format("~n-----------------------------------------------------------"),
	format("~n-----------------------------------------------------------"),
	format("~nRound: ~w~nTournament Score: ~w~nHuman Score: ~w~nComputer Score: ~w~nTurn: ~w~nPrevious Player Passed: ~w~n~nLayout: ~n", 
			[Round, TournamentScore, Human_Score, Computer_Score, Turn, Passed]),
	placeLayout(Layout, "  ", "L ", "  "),
	format("~nBoneyard:~n~w~n~nHuman Hand:~n~w~n~nComputer Hand:~n~w~n", [Boneyard, Human_Hand, Computer_Hand]),
	format("~n-----------------------------------------------------------"),
	format("~n-----------------------------------------------------------~n").


%------------------------------------------------------------------------------------
%   Predicate: placeLayout(Layout, FirstRow, SecondRow, ThirdRow)
%	Purpose: Print Layout crisscross
% 	Parameters: Layout - Layout of the Game; FirstRow,SecondRow,ThirdRow: Return values to store variable of each row.
%	Local Variables: Game Components
%------------------------------------------------------------------------------------

/* Format Layout to represent non-double as straight and double as cross */
placeLayout([], FirstRow, SecondRow, ThirdRow) :-
	string_concat(SecondRow, "R", SecondRowFinal),
	format("~w~n~w~n~w~n", [FirstRow, SecondRowFinal, ThirdRow]),
	true.

placeLayout([Head | Tail], FirstRow, SecondRow, ThirdRow) :-
	[Pip1, Pip2] = Head,
	number_string(Pip1, Pip1Str),
	number_string(Pip2, Pip2Str),
	concatDominos([Pip1Str, Pip2Str], FirstRow, SecondRow, ThirdRow, FirstRowConcat, SecondRowConcat, ThirdRowConcat),
	placeLayout(Tail, FirstRowConcat, SecondRowConcat, ThirdRowConcat).


%------------------------------------------------------------------------------------
%   Predicate: concatDominos(Head, FirstRow, SecondRow, ThirdRow, FirstRowConcat, SecondRowConcat, ThirdRowConcat)
%	Purpose: Print Layout crisscross
% 	Parameters: Head - Domino to be parsed.
%				FirstRow,SecondRow,ThirdRow - Exisiting values each row.
%				FirstRowConcat,SecondRowConcat,ThirdRowConcat - Return values each row after concat.
%	Local Variables: Pip1: Side one of Domino; Pip1: Side two of Domino.
%------------------------------------------------------------------------------------

/* concat double domino */
concatDominos([Pip1, Pip1], FirstRow, SecondRow, ThirdRow, FirstRowConcat, SecondRowConcat, ThirdRowConcat) :-
	string_concat(FirstRow, Pip1, A),
	string_concat(A, " ", FirstRowConcat),
	string_concat(SecondRow, "| ", SecondRowConcat),
	string_concat(ThirdRow, Pip1, C),
	string_concat(C, " ", ThirdRowConcat).

/* concat non double domino */
concatDominos([Pip1, Pip2], FirstRow, SecondRow, ThirdRow, FirstRowConcat, SecondRowConcat, ThirdRowConcat) :-
	string_concat(FirstRow, "    ", FirstRowConcat),
	string_concat(Pip1, "-", B1),
	string_concat(B1, Pip2, B2),
	string_concat(B2, " ", B3),
	string_concat(SecondRow, B3, SecondRowConcat),
	string_concat(ThirdRow, "    ", ThirdRowConcat).



/*----------------------------------------------------------------------------------*/
/*---------------------------------Distribute Hand----------------------------------*/
/*----------------------------------------------------------------------------------*/

%------------------------------------------------------------------------------------
%   Predicate: getSixDominos(Boneyard, SixDominos, NewBoneyard)
%	Purpose: Get first Six Dominos from Boneyard
% 	Parameters: Boneyard- Boneyard of Game
%				SixDominos- First Six Dominos
%				NewBoneyard- Return value for new Boneyard
%	Local Variables: Domino1, ... Domino6- First Six Domino; Tail: Rest of the Boneyard
%------------------------------------------------------------------------------------

/* Distribue Hand for New Game */
getSixDominos([Domino1, Domino2, Domino3, Domino4, Domino5, Domino6 | Tail], SixDominos, NewBoneyard) :-
	SixDominos = [Domino1, Domino2, Domino3, Domino4, Domino5, Domino6],
	NewBoneyard = Tail.

%------------------------------------------------------------------------------------
%   Predicate: distributeHands(State, NewState)
%	Purpose: Shuffle the Boneyard and distribute hands to player.
% 	Parameters: State - Old Game State
%				NewState - Return value for New Game State after drawing hands.
%	Local Variables: ShuffledBoneyard1, ShuffledBoneyard2 - Boneyard after removing 6 Dominos each. 
%------------------------------------------------------------------------------------

/* Distribute hands and get new State */
distributeHands(State, NewState) :-
	[TournamentScore, Round, Human_Score, Computer_Score, _, _, Layout, Boneyard, Passed, Turn, _] = State,
	random_permutation(Boneyard, ShuffledBoneyard1),
	getSixDominos(ShuffledBoneyard1, Human_Hand, ShuffledBoneyard2),
	getSixDominos(ShuffledBoneyard2, Computer_Hand, FinalBoneyard),
	NewState = [TournamentScore, Round, Human_Score, Computer_Score, Human_Hand, Computer_Hand, Layout, FinalBoneyard, Passed, Turn, 0],
	printGameDetails(NewState).

/*----------------------------------------------------------------------------------*/
/*---------------------------------Find and Place Engine----------------------------*/
/*----------------------------------------------------------------------------------*/

%------------------------------------------------------------------------------------
%   Predicate: addOneDomino(State, NewState)
%	Purpose: Add one domino to each player.
% 	Parameters: State - Old Game State
%				NewState - Return value for New Game State after drawing dominos.
%	Local Variables: Head1, Head2 - First Domino of Boneyard; Boneyard1 and Boneyard2: Boneyard after drawing Domino
%------------------------------------------------------------------------------------

/* Add one Domino to each Player */
addOneDomino(State, NewState) :-
	[TournamentScore, Round, Human_Score, Computer_Score, Human_Hand, Computer_Hand, Layout, Boneyard, Passed, Turn, E]= State,
	[Head1 | _] = Boneyard,
	append([Head1], Human_Hand, HumanHand),
	delete(Boneyard, Head1, Boneyard1),
	[Head2 | _] = Boneyard1,
	append([Head2], Computer_Hand, ComputerHand),
	delete(Boneyard1, Head2, Boneyard2),
	NewState = [TournamentScore, Round, Human_Score, Computer_Score, HumanHand, ComputerHand, Layout, Boneyard2, Passed, Turn, E],
	write("Added Domino to each Player."), nl,
	printGameDetails(NewState).


/* Find and place Engine */
findAndPlaceEngine(_, Engine, Hand, Layout, NewHand, NewLayout) :-
	delete(Hand, [Engine, Engine], NewHand),
	append([[Engine, Engine]], Layout, NewLayout).


%------------------------------------------------------------------------------------
%   Predicate: placeEngine(Player, State)
%	Purpose: Check if player has engine and place it to Layout
% 	Parameters: State - Return value for New State after placing engine
%				Player - name of player
%	Local Variables: Engine - Current Engine
%------------------------------------------------------------------------------------

/* Find if Human has Engine */ 
placeEngine("human", State) :-
	[TS, R, Human_Score, CS, HH, CH, L, B, P, _, _] = State,
	Engine is 7 - R,
	(member([Engine, Engine], HH) ->
		(
			findAndPlaceEngine("human", Engine, HH, L, HumanHand, Layout),
			NewState = [TS, R, Human_Score, CS, HumanHand, CH, Layout, B, P, "computer", 0],
			write("Human placed the Engine."), nl,
			nextTurn(NewState)
		);
			placeEngine("computer", State)
	).
/* Find if Computer has Engine */
placeEngine("computer", State) :-
	[TS, R, Human_Score, CS, HH, CH, L, B, P, _, E] = State,
	Engine is 7 - R,
	(member([Engine, Engine], CH) ->
		(
			findAndPlaceEngine("Computer", Engine, CH, L, ComputerHand, Layout),
			NewState = [TS, R, Human_Score, CS, HH, ComputerHand, Layout, B, P, "human", E],			
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

%------------------------------------------------------------------------------------
%   Predicate: play(Player, State, Drawn)
%	Purpose: Play turn according to value of Player
% 	Parameters: State - Current State
%				Player - name of player
%				Drawn - has drawn from card
%	Local Variables: Left - Left end of layout
%					 Right - Right end of layout	
%					 AllMoves - All Possible Moves
%					 BestMove - Best Move of all possible moves.
%------------------------------------------------------------------------------------

/* Human Turn */
play("human", State, Drawn) :-
	nl,	write("--------------------Human's Turn---------------------------"),nl,
	getHumanInput(State, Drawn).
	
/* Computer Turn */
play("computer", State, Drawn) :-
	nl,	write("--------------------Computer's Turn------------------------"),nl,
	[_, _, _, _, _, CH, L, _, P, _, _] = State,
	getLeftRightPips(L, Left, Right),
	findAllPossibleMoveComputer(Left, Right, "computer", CH, P, AllMoves),
	findBestPossibleMove(AllMoves, BestMove),
	format("Possible Moves are: ~w.~n", [AllMoves]),
	format("Best Move is ~w.~n", [BestMove]),
	performComputerCommand(State, BestMove, Drawn).

/*----------------------------------------------------------------------------------*/
/*---------------------------------Get Human Move-----------------------------------*/
/*----------------------------------------------------------------------------------*/

%------------------------------------------------------------------------------------
%   Predicate: getHumanInput(State, Drawn)
%	Purpose: Play turn according to value of Player
% 	Parameters: State - Current State
%				Drawn - has drawn from card
%	Local Variables: UserInput - user input for move
%					 SplitMove - Parsed Move
%					 Pip1, Pip2 - Pips of Domino for move
%------------------------------------------------------------------------------------
/* Get Input from User */
getHumanInput(State, Drawn) :-
	 write("Enter your move: "),
	 read_line_to_codes(user_input, UserInput),
	 split_string(UserInput, " ", "", SplitMove),
	 [Pip1 | Pip2] = SplitMove,
	 (member(SplitMove, [["draw"], ["left", _, _], ["right", _, _], ["pass"], ["help"], ["save"]]) ->
			performHumanCommand(State, Pip1, Pip2, Drawn)
	 		;
			write("Invalid Move. Your move must be one of these: \n\t1. left s1 s2\n\t2. right s1 s2 \n\t3. draw \n\t4. pass \n\t5. help \n\t6. save"), nl, nl,
			getHumanInput(State, Drawn)
	 ).


%------------------------------------------------------------------------------------
%   Predicate: performHumanCommand(State, Command, Domino)
%	Purpose: perform human command that could be insert, help, save, draw or pass
% 	Parameters: State - Current Game State
%				Command - command to be performed
%				Domino: Domino for the Move
%	Local Variables: State - Current Game State
%					Command - command to be performed
%					Domino: Domino for the Move
%------------------------------------------------------------------------------------

/* Human inserts to left */
performHumanCommand(State, "left", [Pip1, Pip2], Drawn) :-
	atom_number(Pip1, S1),
	atom_number(Pip2, S2),
	[_, _, _, _, HH, _, L, _, P, _, _] = State,
	getLeftRightPips(L, Left, Right),
	findAllPossibleMoveHuman(Left, Right, "human", HH, P, AllMoves),
	((member(["left", S1, S2], AllMoves);member(["left", S2, S1], AllMoves)) ->
		(=(Left, S2) -> placeOnLeftHuman(State, [S1, S2]); placeOnLeftHuman(State, [S2, S1]))
		;
		write("Your Move is Invalid."),
		play("human", State, Drawn)
	).

/* Human inserts to right */
performHumanCommand(State, "right", [Pip1, Pip2], Drawn) :-
	atom_number(Pip1, S1),
	atom_number(Pip2, S2),
	[_, _, _, _, HH, _, L, _, P, _, _] = State,
	getLeftRightPips(L, Left, Right),
	findAllPossibleMoveHuman(Left, Right, "human", HH, P, AllMoves),
	((member(["right", S1, S2], AllMoves);member(["right", S2, S1], AllMoves)) ->
		(=(Right, S2) -> placeOnRightHuman(State, [S2, S1]); placeOnRightHuman(State, [S1, S2]))
		;
		write("Your Move is Invalid."),
		play("human", State, Drawn)
	).

/* Human draws */
performHumanCommand(State, "draw", [], Drawn) :-
	(=(Drawn, "false") ->
		[TS, R, Human_Score, CS, HH, CH, L, B, P, T, E] = State,
		getLeftRightPips(L, Left, Right),
		findAllPossibleMoveHuman(Left, Right, "human", HH, P, AllMoves),
		findBestPossibleMove(AllMoves, BestMove),
		(=(BestMove, []) ->
			(=(B, []) ->
				write("Boneyard is Empty. So, passing Instead."),
				performHumanCommand(State, "pass", [], Drawn)
				;
				addOneDominoToHand(B, HH, NewHand, NewBoneyard),
				NewState = [TS, R, Human_Score, CS, NewHand, CH, L, NewBoneyard, P, T, E],
				format("Added one Domino to Human Hand.~n~n"),
				printGameDetails(NewState),
				play("human", NewState, "true")
			)
			;
			format("Moves Possible. Can't Draw. Type help to get possible Moves.~n"),
			play("human", State, Drawn)
		)
	;
		format("Can't Draw. Already drawn.~n"),
		play("human", State, Drawn)
	).

/* Human passes */
performHumanCommand(State, "pass", [], Drawn) :-
	[TS, R, Human_Score, CS, HH, CH, L, B, P, _, E] = State,
	getLeftRightPips(L, Left, Right),
	findAllPossibleMoveHuman(Left, Right, "human", HH, P, AllMoves),
	findBestPossibleMove(AllMoves, BestMove),
	(=(Drawn, "false") ->
		(=(B, []) ->
			NewE is E +1,
			NewState = [TS, R, Human_Score, CS, HH, CH, L, B, "true", "computer", NewE],
			nextTurn(NewState);
			format("Can't pass yet. You can draw since Boneyard is not empty.~n"),
			play("human", State, Drawn)
		)
		;
		(=(BestMove, []) ->
			NewState = [TS, R, Human_Score, CS, HH, CH, L, B, "true", "computer", 0],
			nextTurn(NewState);
			format("Can't pass yet. Moves possible.~n"),
			play("human", State, Drawn)
		)
	).

/* Human asks for help */
performHumanCommand(State, "help", [], Drawn) :-
	[_, _, _, _, HH, _, L, _, P, _, _] = State,
	getLeftRightPips(L, Left, Right),
	findAllPossibleMoveHuman(Left, Right, "human", HH, P, AllMoves),
	format("Possible Moves are: ~w.~n", [AllMoves]),
	findBestPossibleMove(AllMoves, BestMove),
	(=(BestMove, []) ->
		format("No Moves Possible. Draw a Domino if you haven't already or Pass.");
		format("~w has the maximum sum out of all possible moves. ~nHence, best Move is ~w.~n", [BestMove, BestMove])
	),
	play("human", State, Drawn).

/* Human asks for help */
performHumanCommand(State, "save", [], _) :-
	[TS, R, Human_Score, CS, HH, CH, L, B, P, T, _] = State,
	NewState = [TS, R, HH, Human_Score, CH, CS, L, B, P, T],
	write("Enter file name to save (.txt): "),
	read_line_to_codes(user_input, A),
	open(A, write, Stream),
	write(Stream, NewState), write(Stream, '.'),
	close(Stream),
	true.


%------------------------------------------------------------------------------------
%   Predicate:placeOnLeftHuman(State, Domino)
%	Purpose: place domino to left of layout
% 	Parameters: State - Current Game State
%				Domino: Domino to be placed
%	Local Variables: New State: New state of game after placement
%------------------------------------------------------------------------------------

/* Insert Domino to Left */
placeOnLeftHuman(State, [Pip1, Pip2]) :-
	[TS, R, Human_Score, CS, HH, CH, L, B, _, _, _] = State,
	(member([Pip1, Pip2], HH) -> delete(HH, [Pip1, Pip2], NewHand); delete(HH, [Pip2, Pip1], NewHand)),
	pushFront([Pip1, Pip2], L, NewLayout),
	format("Human placed ~w on left.~n", [[Pip1,Pip2]]),
	NewState = [TS, R, Human_Score, CS, NewHand, CH, NewLayout, B, "false", "computer", 0],
	nextTurn(NewState).

%------------------------------------------------------------------------------------
%   Predicate:placeOnRightHuman(State, Domino)
%	Purpose: place domino to right of layout
% 	Parameters: State - Current Game State
%				Domino: Domino to be placed
%	Local Variables: New State: New state of game after placement
%------------------------------------------------------------------------------------

/* Insert Domino to Right */
placeOnRightHuman(State, [Pip1, Pip2]) :-
	[TS, R, Human_Score, CS, HH, CH, L, B, _, _, _] = State,
	(member([Pip1, Pip2], HH) -> delete(HH, [Pip1, Pip2], NewHand); delete(HH, [Pip2, Pip1], NewHand)),
	pushEnd([Pip1, Pip2], L, NewLayout),
	format("Human placed ~w on Right.~n", [[Pip1,Pip2]]),
	NewState = [TS, R, Human_Score, CS, NewHand, CH, NewLayout, B, "false", "computer", 0],
	nextTurn(NewState).

%------------------------------------------------------------------------------------
%   Predicate:addOneDominoToHand(Boneyard, Hand, NewHand, NewBoneyard)
%	Purpose: draw one Domino from Boneyard to Hand
% 	Parameters: Boneyard - Old Boneyard before adding
%				Hand - Old Hand before Adding
%				NewBoneyard - New Boneyard after adding
%				NewHand - New hand after adding
%	Local Variables: Tail- Boneyard after removing one Domino
%------------------------------------------------------------------------------------

/* Draw one domino to hand */
addOneDominoToHand([Head|Tail], Hand, NewHand, NewBoneyard) :-
	append([Head], Hand, NewHand),
	NewBoneyard = Tail.

/* Push Element to Front of List */
pushFront(Element, List, NewList):-
	append([Element], List, NewList).

/* Push Element to End of List */
pushEnd(Element, List, NewList) :-
	append(List, [Element], NewList).

/*----------------------------------------------------------------------------------*/
/*---------------------------------Get Computer Move--------------------------------*/
/*----------------------------------------------------------------------------------*/



%------------------------------------------------------------------------------------
%   Predicate:performComputerCommand(State, Move)
%	Purpose: place domino to left of layout
% 	Parameters: State - Current Game State
%				Move - Move of the computer Format (sideOfLayout, pip1, pip2)
%	Local Variables: New State: New state of game after placement
%------------------------------------------------------------------------------------
/* Computer places on left */
performComputerCommand(State, ["left", Pip1, Pip2], _):-
	[TS, R, Human_Score, CS, HH, CH, L, B, _, _, _] = State,
	(member([Pip1, Pip2], CH) -> delete(CH, [Pip1, Pip2], NewHand); delete(CH, [Pip2, Pip1], NewHand)),
	getLeftRightPips(L, Left, _),
	(=(Left, Pip1) ->
		pushFront([Pip2, Pip1], L, NewLayout);
		pushFront([Pip1, Pip2], L, NewLayout)
	),
	format("Computer placed ~w on left since it has the maximum sum out all possible moves.~n", [[Pip1,Pip2]]),
	NewState = [TS, R, Human_Score, CS, HH, NewHand, NewLayout, B, "false", "human", 0],
	nextTurn(NewState).

%------------------------------------------------------------------------------------
%   Predicate:performComputerCommand(State, Move)
%	Purpose: place domino to right of layout
% 	Parameters: State - Current Game State
%				Move - Move of the computer Format (sideOfLayout, pip1, pip2)
%	Local Variables: New State: New state of game after placement
%------------------------------------------------------------------------------------

/* Computer places on right */
performComputerCommand(State, ["right", Pip1, Pip2], _):-
	[TS, R, Human_Score, CS, HH, CH, L, B, _, _, _] = State,
	(member([Pip1, Pip2], CH) -> delete(CH, [Pip1, Pip2], NewHand); delete(CH, [Pip2, Pip1], NewHand)),
	getLeftRightPips(L, _, Right),
	(=(Right, Pip1) ->
		pushEnd([Pip1, Pip2], L, NewLayout);
		pushEnd([Pip2, Pip1], L, NewLayout)
	),
	format("Computer placed ~w on right since it has the maximum sum out of all possible moves.~n", [[Pip1,Pip2]]),
	NewState = [TS, R, Human_Score, CS, HH, NewHand, NewLayout, B, "false", "human", 0],
	nextTurn(NewState).

%------------------------------------------------------------------------------------
%   Predicate:performComputerCommand(State, Move, Drawn)
%	Purpose: draw or pass the turn
% 	Parameters: State - Current Game State
%				Drawn - Previously drawn	
%	Local Variables: New State: New state of game after placement
%------------------------------------------------------------------------------------

/* Computer Draws or Passes */
performComputerCommand(State, [], Drawn) :-
	[TS, R, Human_Score, CS, HH, CH, L, B, P, T, E] = State,
	(=(Drawn, "false") ->
			(=(B, []) ->
					format("Computer Passed because Boneyard is empty.~n"),
					NewE is E + 1,
					NewState = [TS, R, Human_Score, CS, HH, CH, L, B, "true", "human", NewE],
					nextTurn(NewState)
				;
					addOneDominoToHand(B, CH, NewHand, NewBoneyard),
					NewState = [TS, R, Human_Score, CS, HH, NewHand, L, NewBoneyard, P, T, E],
					format("Computer Drew. Added one Domino to Computer Hand.~n~n"),
					printGameDetails(NewState),
					play("computer", NewState, "true")
				)
		;
			write("Computer Passed since it has already drawn from Boneyard."),
			NewState = [TS, R, Human_Score, CS, HH, CH, L, B, "true", "human", 0],
			nextTurn(NewState)
	).

/*----------------------------------------------------------------------------------*/
/*---------------------------------Find Best Moves----------------------------------*/
/*----------------------------------------------------------------------------------*/

/* Find Possible Moves in the side with end Point - Pip */
hasPIP(Pip, Head) :-
	=([Pip,_], Head); =([_, Pip], Head).

%------------------------------------------------------------------------------------
%   Predicate:addSideToPossibleMoves(Hand, Side, PossibleMoves, ReturnValue)
%	Purpose: parse moves and return final result
% 	Parameters: Hand - hand of the player
%				Side - side to be matched
%				PossibleMoves - all possible moves
%				ReturnValue - return value for all possible moves
%	Local Variables: NewHead - Domino with left/right side
%------------------------------------------------------------------------------------

addSideToPossibleMoves([], _, X, Y) :-
	Y = X.

addSideToPossibleMoves([Head | Tail], Side, PossibleMoves, ReturnValue) :-
	[X, Y] = Head,
	NewHead = [Side, X, Y],
	append([NewHead], PossibleMoves, NewPossibleMoves),
	addSideToPossibleMoves(Tail, Side, NewPossibleMoves, ReturnValue).


/* Find Possible Moves and construct a list */
findPossibleMoves(Pip, Hand, PossibleMoves, Side) :-
	include(hasPIP(Pip), Hand, X),
	addSideToPossibleMoves(X, Side, [], PossibleMoves).

/* Find Best Moves from the Possible Moves */
findBestMove([], _, X, Y) :-
	Y = X.


%------------------------------------------------------------------------------------
%   Predicate:findBestMove(PossibleMoves, MaxSum, BestMove, ReturnValue)
%	Purpose: find the best move and return it
% 	Parameters: PossibleMoves - all possible moves
%				MaxSum - maximum sum of domino yet
%				BestMove - best move yet
%				ReturnValue - return value for best possible move
%	Local Variables: Sum - sum of current domino; NewMaxSum- new max sum
%------------------------------------------------------------------------------------

findBestMove([Head|Tail], MaxSum, BestMove, ReturnValue) :-
	[_, X, Y] = Head,
	Sum is X+Y,
	(Sum > MaxSum ->
		NewMaxSum is Sum,
		NewBestMove = Head,
		findBestMove(Tail, NewMaxSum, NewBestMove, ReturnValue)
		;
		findBestMove(Tail, MaxSum, BestMove, ReturnValue)
	).

/* Find Best Possible Move out of all Moves */
findBestPossibleMove(Moves, BestMove):-
	findBestMove(Moves, 0, [], Move),
	(=(Move, []) ->	BestMove=[];BestMove=Move).


%------------------------------------------------------------------------------------
%   Predicate:findAllPossibleMoveHuman(Left, Right, Player, Hand, Passed, AllMoves)
%	Purpose: find all possible moves and return a list
% 	Parameters: Left, Right - End points of layout; Hand - hand of player; 
%               Passed - previous player passed; AllMoves - all possible moves
%	Local Variables: PossibleMovesRight - moves on right; PossibleMovesLeft - moves of left; 
%	                 moves - all moves
%------------------------------------------------------------------------------------

/* Find All Possible Move for Human */
findAllPossibleMoveHuman(Left, Right, "human", Hand, Passed, AllMoves) :-
	(=(Passed, "true") ->
		findPossibleMoves(Left, Hand, PossibleMovesLeft, "left"),
		findPossibleMoves(Right, Hand, PossibleMovesRight, "right"),
		union(PossibleMovesLeft, PossibleMovesRight, PossibleMoves),
		Moves = PossibleMoves
		;
		findPossibleMoves(Left, Hand, PossibleMovesLeft, "left"),
		(member([Right, Right], Hand) ->
			append([["right", Right, Right]], PossibleMovesLeft, NewPossibleMoves),
			Moves  = NewPossibleMoves
			;
			Moves = PossibleMovesLeft
		)
	),
	AllMoves = Moves.

/* Find Best Possible Move for Computer */
findAllPossibleMoveComputer(Left, Right, "computer", Hand, Passed, AllMoves) :-
	(=(Passed, "true") ->
		findPossibleMoves(Left, Hand, PossibleMovesLeft, "left"),
		findPossibleMoves(Right, Hand, PossibleMovesRight, "right"),
		union(PossibleMovesLeft, PossibleMovesRight, PossibleMoves),
		Moves = PossibleMoves
		;
		findPossibleMoves(Right, Hand, PossibleMovesRight, "right"),
		(member([Left, Left], Hand) ->
			append([["left", Left, Left]], PossibleMovesRight, NewPossibleMoves),
			Moves  = NewPossibleMoves
			;
			Moves = PossibleMovesRight
		)
	),
	AllMoves = Moves.

%------------------------------------------------------------------------------------
%   Predicate:getLeftRightPips(Layout, Left, Right)
%	Purpose: find all possible moves and return a list
% 	Parameters: Left, Right - Return value for left and right pips of layout
%               Layout - layout of the game
%	Local Variables: None
%------------------------------------------------------------------------------------

/* Get Left and Right Pips of Layout */
getLeftRightPips(Layout, Left, Right) :-
	nth1(1, Layout, First),
	[Left, _] = First,
	last(Layout, Last),
	[_, Right] = Last.

/*----------------------------------------------------------------------------------*/
/*------------------------Check if Round or Tournament has Ended--------------------*/
/*-------------------------------Calculate Round Score------------------------------*/

/* Determine if the round or tournament ended */
nextTurn(State) :-
	checkIfRoundEnded(State).


%------------------------------------------------------------------------------------
%   Predicate:checkIfRoundEnded(State)
%	Purpose: check if round has ended
% 	Parameters: State: Current State of the Game
%	Local Variables: EmptyPassed - Number of times passed because stock is empty.
%                    HumanScore, ComputerScore - Scores of Players
%------------------------------------------------------------------------------------

/* Determine if round has ended */
checkIfRoundEnded(State) :-
	[TS, R, Human_Score, CS, HH, CH, _, _, _, _, EmptyPassed] = State,
	calculateRoundScores(CH, 0, HumanScore),
	calculateRoundScores(HH, 0, ComputerScore),
	(=(EmptyPassed, 2) ->
		(=(HumanScore, ComputerScore) ->
			format("Same Score. Round has tied. No points for anyone.~n"),
			printRoundScore(TS, R, Human_Score, CS);
			(>(HumanScore, ComputerScore) ->
				format("Both player passed and Boneyard is Empty. Round Ends.~nHuman won since it has less Sum.~n"),
				NewHuman_Score is Human_Score + HumanScore,
				printRoundScore(TS, R, NewHuman_Score, CS);
				format("Both player passed and Boneyard is Empty. Round Ends.~nComputer won since it has less Sum.~n"),
				NewCS is CS + ComputerScore,
				printRoundScore(TS, R, Human_Score, NewCS)
			)
		);
		(=(HH, []) ->
				format("Human's Hand is Empty. Human wins the Round"),
				format("~nHuman gets ~w points.", [HumanScore]),
				NewHuman_Score is Human_Score + HumanScore,
				printRoundScore(TS, R, NewHuman_Score, CS)
			;
				(=(CH, []) ->
					format("Computer's Hand is Empty. Computer wins the Round"),
					format("~nComputer gets ~w points.", [ComputerScore]),
					NewCS is CS + ComputerScore,
					printRoundScore(TS, R, Human_Score, NewCS)
					;
					[_, _, _, _, _, _, _, _, _, T, _] = State,
					printGameDetails(State),
					play(T, State, "false")
				)
			)
	).


%------------------------------------------------------------------------------------
%   Predicate:calculateRoundScores(Hand, Sum, Score)
%	Purpose: calculate sum of a hand
% 	Parameters: Hand - player hand; Sum - sum of hands; Score - return value of sum 
%	Local Variables: NewSum - sum after adding dominos sum
%------------------------------------------------------------------------------------

/* Calculate Score of each Player */
calculateRoundScores([], Sum, Score) :-
	Score = Sum.

calculateRoundScores([Head | Tail], Sum, Score) :-
	[Pip1, Pip2] = Head,
	NewSum is Pip1 + Pip2 + Sum,
	calculateRoundScores(Tail, NewSum, Score).

/* Print Score of Each Player */
printRoundScore(TS, Round, Human_Score, CS) :-
	nl,	write("--------------------Updated Scores-------------------------"),nl,
	format("~nRound: ~w ~nHuman Score: ~w~nComputer Score: ~w~n----------------------------------------~n", [Round, Human_Score, CS]),
	format("~nPress any key to continue: "),
	get_single_char(_),
	(=(Round, 7) -> NewRound is 1; NewRound is Round + 1),
	checkIfTournamentEnded(TS, NewRound, Human_Score, CS).


%------------------------------------------------------------------------------------
%   Predicate:checkIfTournamentEnded(TS, Round, HS, CS)
%	Purpose: check if tournament has ended if not start another round
% 	Parameters: TS, Round, HS, CS - Game Components
%	Local Variables: None
%------------------------------------------------------------------------------------

/* Check if Tournament has Ended */
checkIfTournamentEnded(TS, Round, HS, CS) :-
	(>=(HS, TS) ->
		format("Human wins the Tournament.");
		(>=(CS, TS) ->
			format("Computer wins the Tournament.");
			startRound(TS, Round, Human_Score, CS)
		)
	).

%------------------------------------------------------------------------------------
%   Predicate: startRound(TournamentScore, Round, HumanScore, ComputerScore)
%	Purpose: start a new round
% 	Parameters: TournamentScore, Round, HumanScore, ComputerScore - Updated Game Components
%	Local Variables: Data - raw new round data, State - parsed new game data; 
%                    NewState - updated state; NewRoundState - updated state with round and scores.
%------------------------------------------------------------------------------------

/* start new round */
startRound(TournamentScore, Round, HumanScore, ComputerScore) :-
	getRawData(Data),
	getGameState(Data, State),
	distributeHands(State, NewState),
	[_, _, _, _, HH, CH, L, B, P, T, E] = NewState,
	NewRoundState = [TournamentScore, Round, HumanScore, ComputerScore, HH, CH, L, B, P, T, E],
	placeEngine("human", NewRoundState).

