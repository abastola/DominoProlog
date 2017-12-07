/*----------------------------------------------------------------------------------*/
/*----------------------------Parse Data and Print Data-----------------------------*/
/*----------------------------------------------------------------------------------*/
/* New Game Data */
getRawData(FileData) :-
	open("abc.txt", read, Str),
	read(Str, FileData),
	close(Str).

/* Get Game State of New or Loaded Game */
getGameState(Data, State):-
	[TS|_] = Data,
	[ _, R | _ ] = Data,
	[ _, _, HH | _ ] = Data,
	[ _, _, _, HS | _ ] = Data,
	[ _, _, _, _, CH | _ ] = Data,
	[ _, _, _, _, _, CS | _ ] = Data,
	[ _, _, _, _, _, _, L | _ ] = Data,
	[ _, _, _, _, _, _, _, B | _ ] = Data,
	[ _, _, _, _, _, _, _, _, P | _ ] = Data,
	[ _, _, _, _, _, _, _, _, _, T ] = Data,
	atom_string(T, T1),
	atom_string(P, Pip1),
	State = [TS, R, HS, CS, HH, CH, L, B, Pip1, T1, 0].


/* Print the Game State */
printGameDetails(State):-
	[TS, R, HS, CS, HH, CH, L, B, P, T, _] = State,
	format("~n-----------------------------------------------------------"),
	format("~n-----------------------------------------------------------"),
	format("~nRound: ~w~nTournament Score: ~w~nHuman Score: ~w~nComputer Score: ~w~nTurn: ~w~nPrevious Player Passed: ~w~n~nLayout: ~n", [R, TS, HS, CS, T, P]),
	placeLayout(L, "  ", "L ", "  "),
	format("~nBoneyard:~n~w~n~nHuman Hand:~n~w~n~nComputer Hand:~n~w~n", [B, HH, CH]),
	format("~n-----------------------------------------------------------"),
	format("~n-----------------------------------------------------------~n").

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

/* Distribue Hand for New Game */
getSixDominos([D1, D2, D3, D4, D5, D6 | Tail], SixDominos, NewBoneyard) :-
	SixDominos = [D1, D2, D3, D4, D5, D6],
	NewBoneyard = Tail.

/* DIstribute hands and get new State */
distributeHandsToPlayers(State, NewState) :-
	[TS, R, HS, CS, _, _, L, B, P, T, E] = State,
	random_permutation(B, Boneyard),
	getSixDominos(Boneyard, SixDominos1, Boneyard1),
	HumanHand = SixDominos1,
	getSixDominos(Boneyard1, SixDominos2, Boneyard2),
	ComputerHand = SixDominos2,
	NewState = [TS, R, HS, CS, HumanHand, ComputerHand, L, Boneyard2, P, T, E].

/* Distribute Hand */
distributeHands(State, FinalState) :-
	distributeHandsToPlayers(State, NewState),
	printGameDetails(NewState),
	FinalState = NewState.

/*----------------------------------------------------------------------------------*/
/*---------------------------------Find and Place Engine----------------------------*/
/*----------------------------------------------------------------------------------*/

/* Add one Domino to each Player */
addOneDomino(State, NewState) :-
	[TS, R, HS, CS, HH, CH, L, B, P, T, E] = State,
	[Head1 | _] = B,
	append([Head1], HH, HumanHand),
	delete(B, Head1, Boneyard1),
	[Head2 | _] = Boneyard1,
	append([Head2], CH, ComputerHand),
	delete(Boneyard1, Head2, Boneyard2),
	NewState = [TS, R, HS, CS, HumanHand, ComputerHand, L, Boneyard2, P, T, E],
	write("Added Domino to each Player."), nl,
	printGameDetails(NewState).

/* Find and place Engine */
findAndPlaceEngine(_, Engine, Hand, Layout, NewHand, NewLayout) :-
	delete(Hand, [Engine, Engine], NewHand),
	append([[Engine, Engine]], Layout, NewLayout).

/* Find if Human has Engine */ 
placeEngine("human", State) :-
	[TS, R, HS, CS, HH, CH, L, B, P, _, _] = State,
	Engine is 7 - R,
	(member([Engine, Engine], HH) ->
		(
			findAndPlaceEngine("human", Engine, HH, L, HumanHand, Layout),
			NewState = [TS, R, HS, CS, HumanHand, CH, Layout, B, P, "computer", 0],
			write("Human placed the Engine."), nl,
			nextTurn(NewState)
		);
			placeEngine("computer", State)
	).

/* Find if Computer has Engine */
placeEngine("computer", State) :-
	[TS, R, HS, CS, HH, CH, L, B, P, _, E] = State,
	Engine is 7 - R,
	(member([Engine, Engine], CH) ->
		(
			findAndPlaceEngine("Computer", Engine, CH, L, ComputerHand, Layout),
			NewState = [TS, R, HS, CS, HH, ComputerHand, Layout, B, P, "human", E],			
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
	=([Pip,_], Head); =([_, Pip], Head).

addSideToPossibleMoves([], _, X, Y) :-
	Y = X.

addSideToPossibleMoves([Head | Tail], Side, PossibleMoves, ReturnValue) :-
	[X, Y] = Head,
	NewHead = [Side, X, Y],
	append([NewHead], PossibleMoves, NewPossibleMoves),
	addSideToPossibleMoves(Tail, Side, NewPossibleMoves, ReturnValue).

findPossibleMoves(Pip, Hand, PossibleMoves, Side) :-
	include(hasPIP(Pip), Hand, X),
	addSideToPossibleMoves(X, Side, [], PossibleMoves).

/* Find Best Moves from the Possible Moves */
findBestMove([], _, X, Y) :-
	Y = X.

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

/* Get Left and Right Pips of Layout */
getLeftRightPips(Layout, Left, Right) :-
	nth1(1, Layout, First),
	[Left, _] = First,
	last(Layout, Last),
	[_, Right] = Last.

/*----------------------------------------------------------------------------------*/
/*---------------------------------Get Human Move-----------------------------------*/
/*----------------------------------------------------------------------------------*/

/* Get Input from User */
getHumanInput(State, Drawn) :-
	 write("Enter your move: "),
	 read_line_to_codes(user_input, UserInput),
	 split_string(UserInput, " ", "", A),
	 [T1 | T2] = A,
	 (member(A, [["draw"], ["left", _, _], ["right", _, _], ["pass"], ["help"], ["save"]]) ->
			performHumanCommand(State, T1, T2, Drawn)
	 		;
			write("Invalid Move. Your move must be one of these: \n\t1. left s1 s2\n\t2. right s1 s2 \n\t3. draw \n\t4. pass \n\t5. help \n\t6. save"), nl, nl,
			getHumanInput(State, Drawn)
	 ).

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
		[TS, R, HS, CS, HH, CH, L, B, P, T, E] = State,
		getLeftRightPips(L, Left, Right),
		findAllPossibleMoveHuman(Left, Right, "human", HH, P, AllMoves),
		findBestPossibleMove(AllMoves, BestMove),
		(=(BestMove, []) ->
			(=(B, []) ->
				write("Boneyard is Empty. So, passing Instead."),
				performHumanCommand(State, "pass", [], Drawn)
				;
				addOneDominoToHand(B, HH, NewHand, NewBoneyard),
				NewState = [TS, R, HS, CS, NewHand, CH, L, NewBoneyard, P, T, E],
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
	[TS, R, HS, CS, HH, CH, L, B, P, _, E] = State,
	getLeftRightPips(L, Left, Right),
	findAllPossibleMoveHuman(Left, Right, "human", HH, P, AllMoves),
	findBestPossibleMove(AllMoves, BestMove),
	(=(Drawn, "false") ->
		(=(B, []) ->
			NewE is E +1,
			NewState = [TS, R, HS, CS, HH, CH, L, B, "true", "computer", NewE],
			nextTurn(NewState);
			format("Can't pass yet. You can draw since Boneyard is not empty.~n"),
			play("human", State, Drawn)
		)
		;
		(=(BestMove, []) ->
			NewState = [TS, R, HS, CS, HH, CH, L, B, "true", "computer", 0],
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
	[TS, R, HS, CS, HH, CH, L, B, P, T, _] = State,
	NewState = [TS, R, HH, HS, CH, CS, L, B, P, T],
	write("Enter file name to save (.txt): "),
	read_line_to_codes(user_input, A),
	open(A, write, Stream),
	write(Stream, NewState), write(Stream, '.'),
	close(Stream),
	true.

/* Insert Domino to Left */
placeOnLeftHuman(State, [Pip1, Pip2]) :-
	[TS, R, HS, CS, HH, CH, L, B, _, _, _] = State,
	(member([Pip1, Pip2], HH) -> delete(HH, [Pip1, Pip2], NewHand); delete(HH, [Pip2, Pip1], NewHand)),
	pushFront([Pip1, Pip2], L, NewLayout),
	format("Human placed ~w on left.~n", [[Pip1,Pip2]]),
	NewState = [TS, R, HS, CS, NewHand, CH, NewLayout, B, "false", "computer", 0],
	nextTurn(NewState).

/* Insert Domino to Right */
placeOnRightHuman(State, [Pip1, Pip2]) :-
	[TS, R, HS, CS, HH, CH, L, B, _, _, _] = State,
	(member([Pip1, Pip2], HH) -> delete(HH, [Pip1, Pip2], NewHand); delete(HH, [Pip2, Pip1], NewHand)),
	pushEnd([Pip1, Pip2], L, NewLayout),
	format("Human placed ~w on Right.~n", [[Pip1,Pip2]]),
	NewState = [TS, R, HS, CS, NewHand, CH, NewLayout, B, "false", "computer", 0],
	nextTurn(NewState).

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

/* Computer places on left */
performComputerCommand(State, ["left", Pip1, Pip2], _):-
	[TS, R, HS, CS, HH, CH, L, B, _, _, _] = State,
	(member([Pip1, Pip2], CH) -> delete(CH, [Pip1, Pip2], NewHand); delete(CH, [Pip2, Pip1], NewHand)),
	getLeftRightPips(L, Left, _),
	(=(Left, Pip1) ->
		pushFront([Pip2, Pip1], L, NewLayout);
		pushFront([Pip1, Pip2], L, NewLayout)
	),
	format("Computer placed ~w on left since it has the maximum sum out all possible moves.~n", [[Pip1,Pip2]]),
	NewState = [TS, R, HS, CS, HH, NewHand, NewLayout, B, "false", "human", 0],
	nextTurn(NewState).

/* Computer places on right */
performComputerCommand(State, ["right", Pip1, Pip2], _):-
	[TS, R, HS, CS, HH, CH, L, B, _, _, _] = State,
	(member([Pip1, Pip2], CH) -> delete(CH, [Pip1, Pip2], NewHand); delete(CH, [Pip2, Pip1], NewHand)),
	getLeftRightPips(L, _, Right),
	(=(Right, Pip1) ->
		pushEnd([Pip1, Pip2], L, NewLayout);
		pushEnd([Pip2, Pip1], L, NewLayout)
	),
	format("Computer placed ~w on right since it has the maximum sum out of all possible moves.~n", [[Pip1,Pip2]]),
	NewState = [TS, R, HS, CS, HH, NewHand, NewLayout, B, "false", "human", 0],
	nextTurn(NewState).

/* Computer Draws or Passes */
performComputerCommand(State, [], Drawn) :-
	[TS, R, HS, CS, HH, CH, L, B, P, T, E] = State,
	(=(Drawn, "false") ->
			(=(B, []) ->
					format("Computer Passed because Boneyard is empty.~n"),
					NewE is E + 1,
					NewState = [TS, R, HS, CS, HH, CH, L, B, "true", "human", NewE],
					nextTurn(NewState)
				;
					addOneDominoToHand(B, CH, NewHand, NewBoneyard),
					NewState = [TS, R, HS, CS, HH, NewHand, L, NewBoneyard, P, T, E],
					format("Computer Drew. Added one Domino to Computer Hand.~n~n"),
					printGameDetails(NewState),
					play("computer", NewState, "true")
				)
		;
			write("Computer Passed since it has already drawn from Boneyard."),
			NewState = [TS, R, HS, CS, HH, CH, L, B, "true", "human", 0],
			nextTurn(NewState)
	).

/*----------------------------------------------------------------------------------*/
/*------------------------Check if Round or Tournament has Ended--------------------*/
/*-------------------------------Calculate Round Score------------------------------*/

/* Determine if the round or tournament ended */
nextTurn(State) :-
	checkIfRoundEnded(State).

/* Determine if round has ended */
checkIfRoundEnded(State) :-
	[TS, R, HS, CS, HH, CH, _, _, _, _, E] = State,
	calculateRoundScores(CH, 0, HumanScore),
	calculateRoundScores(HH, 0, ComputerScore),
	(=(E, 2) ->
		(=(HumanScore, ComputerScore) ->
			format("Same Score. Round has tied. No points for anyone.~n"),
			printRoundScore(TS, R, HS, CS);
			(>(HumanScore, ComputerScore) ->
				format("Human won since it has less Sum.~n"),
				NewHS is HS + HumanScore,
				printRoundScore(TS, R, NewHS, CS);
				format("Computer won since it has less Sum.~n"),
				NewCS is CS + ComputerScore,
				printRoundScore(TS, R, HS, NewCS)
			)
		);
		(=(HH, []) ->
				format("Human's Hand is Empty. Human wins the Round"),
				format("~nHuman gets ~w points.", [HumanScore]),
				NewHS is HS + HumanScore,
				printRoundScore(TS, R, NewHS, CS)
			;
				(=(CH, []) ->
					format("Computer's Hand is Empty. Computer wins the Round"),
					format("~nComputer gets ~w points.", [ComputerScore]),
					NewCS is CS + ComputerScore,
					printRoundScore(TS, R, HS, NewCS)
					;
					[_, _, _, _, _, _, _, _, _, T, _] = State,
					printGameDetails(State),
					play(T, State, "false")
				)
			)
	).

/* Calculate Score of each Player */
calculateRoundScores([], Sum, Score) :-
	Score = Sum.

calculateRoundScores([Head | Tail], Sum, Score) :-
	[Pip1, Pip2] = Head,
	NewSum is Pip1 + Pip2 + Sum,
	calculateRoundScores(Tail, NewSum, Score).

/* Print Score of Each Player */
printRoundScore(TS, Round, HS, CS) :-
	nl,	write("--------------Updated Score--------------"),nl,
	format("~nRound: ~w ~nHuman Score: ~w~nComputer Score: ~w~n----------------------------------------~n", [Round, HS, CS]),
	(=(Round, 7) -> NewRound is 1; NewRound is Round + 1),
	checkIfTournamentEnded(TS, NewRound, HS, CS).

/* Check if Tournament has Ended */
checkIfTournamentEnded(TS, Round, HS, CS) :-
	(>=(HS, TS) ->
		format("Human wins the Tournament.");
		(>=(CS, TS) ->
			format("Computer wins the Tournament.");
			startRound(TS, Round, HS, CS)
		)
	).

/*----------------------------------------------------------------------------------*/
/*---------------------------------Play Turns---------------------------------------*/
/*----------------------------------------------------------------------------------*/

/* Human Turn */
play("human", State, Drawn) :-
	nl,	write("--------------Human's Turn--------------"),nl,
	getHumanInput(State, Drawn).
	
/* Computer Turn */
play("computer", State, Drawn) :-
	nl,	write("-------------Computer's Turn--------------"),nl,
	[_, _, _, _, _, CH, L, _, P, _, _] = State,
	getLeftRightPips(L, Left, Right),
	findAllPossibleMoveComputer(Left, Right, "computer", CH, P, AllMoves),
	findBestPossibleMove(AllMoves, BestMove),
	format("Possible Moves are: ~w.~n", [AllMoves]),
	format("Best Move is ~w.~n", [BestMove]),
	performComputerCommand(State, BestMove, Drawn).

/*----------------------------------------------------------------------------------*/
/*---------------------------------Start Game---------------------------------------*/
/*----------------------------------------------------------------------------------*/

/* start new round */
startRound(TournamentScore, Round, HumanScore, ComputerScore) :-
	getRawData(Data),
	getGameState(Data, State),
	distributeHands(State, NewState),
	[_, _, _, _, HH, CH, L, B, P, T, E] = NewState,
	NewRoundState = [TournamentScore, Round, HumanScore, ComputerScore, HH, CH, L, B, P, T, E],
	placeEngine("human", NewRoundState).

/* start the game */
beginGame() :-
	working_directory(_, "C:/Users/ZDF6BJK/Desktop/Prolog"),
	write("Enter 'new' for new game or 'filename' to load file: "),
	read_line_to_codes(user_input, A),
	atom_string(A, Filename),
	(=(Filename, "new") ->
		write("Enter tournament score: "),
		read(TournamentScore),
		getRawData(Data),
		getGameState(Data, State),
		distributeHands(State, NewState),
		[_, R, HS, CS, HH, CH, L, B, P, T, E] = NewState,
		NewRoundState = [TournamentScore, R, HS, CS, HH, CH, L, B, P, T, E],
		placeEngine("human", NewRoundState)	
		;
		open(Filename, read, Str),
		read(Str, Data),
		close(Str),
		getGameState(Data, State),
		printGameDetails(State),
		[_, _, _, _, _, _, L, _, _, _, _] = State,
		(length(L, 0) -> placeEngine("human", State); nextTurn(State))
	).
