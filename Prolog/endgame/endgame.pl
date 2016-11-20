%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% King  + 2 Bishops vs. King chess endgame, (c) Lukas Kudela, 2007. %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MAIN ROUTINES                                      %%%
%%% -------------------------------------------------- %%%
%%% start/0, mate/1, eog/2, get_move/2, execute_move/3 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------
% start/0 :-
% 	Executes the main program.
% ----------------------------

start :-
	write( 'King + 2 Bishops vs. King chess endgame, (c) Lukas Kudela, 2007.'),
	nl,
	write( 'Please enter the initial position in the following format: p( w, WK, B1, B2, BK).'),
    nl,
    read( P),
    display_position( P),
    nl,
    P = p( w, WK, B1, B2, BK),
    threatened_by_W( WK, B1, B2, Tfin),
    (member( BK, Tfin) ->
		write( 'Illegal initial position: Black king in check (or checkmate)!')
	; (
		eog( P) ->
			write( 'Black king checkmated. Game over!')
		;
			mate( P) 
	)),
	!.

% -------------------------------------------------------------------
% mate/1( +P) :-
% 	Given initial (non-terminal) position, checkmates the black king.
% P ... initial (or root) non-terminal position	 
% -------------------------------------------------------------------

mate( P) :-
    % --- Uncomment one of the following options: --- %
	% --- 1st --------------------------------------- %
	% move( 4, P, O),                                 %
	% O = o( N, M),                                   %
	% execute_move( P, M, P1),                        %
	% --- 2nd --------------------------------------- %
	% minimax( 4, P, P1, N),                          %
	% --- 3rd --------------------------------------- %
	alphabeta( 6, P, -32767, 32767, P1, N),           %
	% ----------------------------------------------- %
	display_position( P1),
	nl,
	(eog( P1) ->
		write( 'Black king checkmated. Game over!')
	; (
		get_move( P1, Move),
		execute_move( P1, Move, P2),
		mate( P2)
	)).

% ------------------------------------------------------------------------------------------------------------
% eog/1( +P) :-
%	Evaluates to true if (and only if) the game is over (black king checkmated), otherwise evaluates to false.
% P ... posiiton to be checked for the "end of game" (eog) status
% ------------------------------------------------------------------------------------------------------------

eog( P) :-
	P = p( _, WK, B1, B2, BK),
	threatened_by_W( WK, B1, B2, Tfin),
	member( BK, Tfin),
    \+move_K( BK, Tfin, _).
 
% ----------------------
% get_move/2( +P, -M) :-  
% ----------------------
      
get_move( P, M) :-
	P = p( b, WK, B1, B2, BKi),
	write( 'Please enter your move in the following format: C-R.'),
	nl,
	read( Mtmp),
	threatened_by_W( WK, B1, B2, Tfin),
	(member( Mtmp, Tfin) -> (
		write( 'Illegal move: Cannot move into check!'),
		nl,
		get_move( P, M)
	) ; 
		M = 'k':BKi:Mtmp
	),
	!.
	
% ------------------------------------------------
% execute_move/3( +Pi, +M, -Po) :-
% 	Executes a move (by either side) on the board.
% Pi ... the position before the move
% M  ... the move executed
% Po ... the position after the move
% ------------------------------------------------

execute_move( p( b, WK,  B1,  B2,  BKi), _:BKi:BKo, p( w, WK,  B1,  B2,  BKo)) :-
	!.

execute_move( p( w, WKi, B1,  B2,  BK),  _:WKi:WKo, p( b, WKo, B1,  B2,  BK )) :-
	!.

execute_move( p( w, WK,  B1i, B2,  BK),  _:B1i:B1o, p( b, WK,  B1o, B2,  BK )) :-
	!.

execute_move( p( w, WK,  B1,  B2i, BK),  _:B2i:B2o, p( b, WK,  B1,  B2o, BK )) :-
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MINIMAX (WITH ALPHA-BETA PRUNING) ROUTINES  %%%
%%% ------------------------------------------- %%%
%%% move/3, minimax/4, alphabeta/6              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------------------------------------------------------------------- 
% move/3( +D, +P, -O) :-
% 	Searches the state-space from the initial (or root) position for an optimal move to a given depth.
% D ... the depth at which to terminate the calculation and evaluate using the static_evaluation function
% P ... the position: p(side_to_move{w|b}, WKc-WKr, B1c-B1r, B2c-B2r, BKc-BKr)
% O ... o( N, M), where
% 	N ... the best game equity, that a side to play can obtain (+32767 for "white wins" and 0 for draw)
% 	M ... the best move!
% -------------------------------------------------------------------------------------------------------

% ------------------------------------------------------------------------------------------------------------------------------------------------ %
% P is a terminal position if and only if (white to move && either bishop captured) || (black to move && stalemated) || (black to move and mated). %
% ------------------------------------------------------------------------------------------------------------------------------------------------ %

move( _, P, O) :-									% white to move && either bishop captured	
	(
		P = p( w, _, BK, _, BK);					% 1st white bishop captured
	 	P = p( w, _, _, BK, BK)						% 2nd white bishop captured
	),
	O = o( 0, ['1/2']),								% game drawn
	!.

move( D, P, O) :-									% (black to move && mated) || (black to move && stalemated)																			
	P = p( b, WK, B1, B2, BK),
	threatened_by_W( WK, B1, B2, Tfin),
	\+move_K( BK, Tfin, _),						% black king unable to move
	(member( BK, Tfin) ->	(						% black king's square threatened?		
		N is 10000 + D,								% IMPORTANT! 10000 because the highest value static_evaluation can assign is 9999; the deeper the mate the less favourable it is
		O = o( N, ['1-0'])							% yes => black mated								
	) ; 										
		O = o( 0, ['1/2'])							% no => black stalemated
	),
	!.

% ---------------------------------------------------------------------------------------------------------------------------- %	
% The terminal depth has been reached, but no terminal position => the position is evaluated using static evaluation function. %
% ---------------------------------------------------------------------------------------------------------------------------- %

move( 0, P, o( N, ['...'])) :-					
	static_evaluation( P, N),
	!.												

% -------------------------------------------------------------------------------------- %	
% WARNING! No position unifiable with previous three rules ought to get past this point. %
% -------------------------------------------------------------------------------------- %

move( D, P, O) :-
	P = p( S, _, _, _, _),							% whose turn is it?
	D_m1 is D - 1,									% searching deeper: D_m1 is the remaining depth
	findall( 
		o( N1, M),
		(
			generate_position( P, M, P1), 							
			move( D_m1, P1, O1),
			O1 = o( N1, _)
		), 
		L),
	(S = w ->										% white player's turn => minimizing N
		find_max( L, O)
	;												% black player's turn => maximizing N
		find_min( L, O)
	).
	
% -------------------------------------------------------------------------------------------------------
% minimax/4( D, P, B, V) :-
%	Searches the state-space from the initial (or root) position for an optimal move to a given depth.
% D ... the depth at which to terminate the calculation and evaluate using the static_evaluation function
% P ... the position: p(side_to_move{w|b}, WKc-WKr, B1c-B1r, B2c-B2r, BKc-BKr)
% B ... the best successor (a position)
% V ... the value (or equity) of the best successor
% -------------------------------------------------------------------------------------------------------

% ------------------------------------------------------------------------------------------------------------------------------------------------ %
% P is a terminal position if and only if (white to move && either bishop captured) || (black to move && stalemated) || (black to move and mated). %
% ------------------------------------------------------------------------------------------------------------------------------------------------ %

minimax( _, P, B, V) :-								% white to move && either bishop captured	
	(
		P = p( w, _, BK, _, BK);					% 1st white bishop captured
	 	P = p( w, _, _, BK, BK)						% 2nd white bishop captured
	),
	B = ['1/2'],
	V = 0,											% game drawn
	!.

minimax( D, P, B, V) :-								% (black to move && mated) || (black to move && stalemated)																			
	P = p( b, WK, B1, B2, BK),
	threatened_by_W( WK, B1, B2, Tfin),
	\+move_K( BK, Tfin, _),						% black king unable to move
	(member( BK, Tfin) ->	(						% black king's square threatened? 	
		B = ['1-0'],								% yes => black mated
		V is 10000 + D								% IMPORTANT! 10000 because the highest value static_evaluation can assign is 9999; the deeper the mate the less favourable it is
	) ; (
		B = ['1/2'],								% no => black stalemated		
		V = 0							
	)),
	!.

% ---------------------------------------------------------------------------------------------------------------------------- %	
% The terminal depth has been reached, but no terminal position => the position is evaluated using static evaluation function. %
% ---------------------------------------------------------------------------------------------------------------------------- %

minimax( 0, P, ['...'], N) :-					
	static_evaluation( P, N),
	!.

% -------------------------------------------------------------------------------------- %	
% WARNING! No position unifiable with previous three rules ought to get past this point. %
% -------------------------------------------------------------------------------------- %	

minimax( Depth, Pos, BestSucc, Val) :-
	moves( Pos, PosList), !, 						% Legal moves in Pos
	Depth_m1 is Depth - 1,
	best( Depth_m1, PosList, BestSucc, Val).
%   ;staticval( Pos, Val).							% Pos has no successors
		
best( Depth, [Pos], Pos, Val) :-
	minimax( Depth, Pos, _, Val), !.

best( Depth, [Pos1 | PosList], BestPos, BestVal) :-
	minimax( Depth, Pos1, _, Val1),
	best( Depth, PosList, Pos2, Val2),
	betterof( Pos1, Val1, Pos2, Val2, BestPos, BestVal).
	
betterof( Pos0, Val0, Pos1, Val1, Pos0, Val0) :-
	min_to_move( Pos0), Val0 > Val1, !;				% MAX prefers the greater value
	max_to_move( Pos0), Val0 < Val1, !. 			% MIN prefers the lesser value
	
betterof( Pos0, Val0, Pos1, Val1, Pos1, Val1).		% Otherwise Pos1 better than Pos0

% -------------------------------------------------------------------------------------------------------
% alphabeta/6( +D, +P, +A, +B, -G, -V) :-
%	Searches the state-space from the initial (or root) position for an optimal move to a given depth.
% D ... the depth at which to terminate the calculation and evaluate using the static_evaluation function
% P ... the position: p(side_to_move{w|b}, WKc-WKr, B1c-B1r, B2c-B2r, BKc-BKr)
% A ... alpha = the minimum score that the maximizing player is assured of
% B ... beta  = the maximum score that the minimizing player is assured of  
% G ... the best successor (a position)
% V ... the value (or equity) of the best successor
% -------------------------------------------------------------------------------------------------------

% ------------------------------------------------------------------------------------------------------------------------------------------------ %
% P is a terminal position if and only if (white to move && either bishop captured) || (black to move && stalemated) || (black to move and mated). %
% ------------------------------------------------------------------------------------------------------------------------------------------------ %

alphabeta( _, P, _, _, G, V) :-						% white to move && either bishop captured	
	(
		P = p( w, _, BK, _, BK);					% 1st white bishop captured
	 	P = p( w, _, _, BK, BK)						% 2nd white bishop captured
	),
	G = ['1/2'],
	V = 0,											% game drawn
	!.

alphabeta( D, P, _, _, G, V) :-						% (black to move && mated) || (black to move && stalemated)																			
	P = p( b, WK, B1, B2, BK),
	threatened_by_W( WK, B1, B2, Tfin),
	\+move_K( BK, Tfin, _),						% black king unable to move
	(member( BK, Tfin) ->	(						% black king's square threatened? 	
		G = ['1-0'],								% yes => black mated
		V is 10000 + D								% IMPORTANT! 10000 because the highest value static_evaluation can assign is 9999; the deeper the mate the less favourable it is
	) ; (
		G = ['1/2'],								% no => black stalemated		
		V = 0							
	)),
	!.

% ---------------------------------------------------------------------------------------------------------------------------- %	
% The terminal depth has been reached, but no terminal position => the position is evaluated using static evaluation function. %
% ---------------------------------------------------------------------------------------------------------------------------- %

alphabeta( 0, P, _, _, ['...'], N) :-					
	static_evaluation( P, N),
	!.

% -------------------------------------------------------------------------------------- %	
% WARNING! No position unifiable with previous three rules ought to get past this point. %
% -------------------------------------------------------------------------------------- %	

alphabeta( Depth, Pos, Alpha, Beta, GoodPos, Val) :-
	moves( Pos, PosList), !,						% Legal moves in Pos
	Depth_m1 is Depth - 1,
	boundedbest( Depth_m1, PosList, Alpha, Beta, GoodPos, Val).
%	;staticval( Pos, Val).	

boundedbest( Depth, [Pos | PosList], Alpha, Beta, GoodPos, GoodVal) :-
	alphabeta( Depth, Pos, Alpha, Beta, _, Val),
	goodenough( Depth, PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).
	
goodenough( _, [], _, _, Pos, Val, Pos, Val) :- !.	% No other candidate
	
goodenough( _, _, Alpha, Beta, Pos, Val, Pos, Val) :-
	min_to_move( Pos), Val > Beta, !; 				% Maximizer attained upper bound
	max_to_move( Pos), Val < Alpha, !. 				% Minimizer attained lower bound

goodenough( Depth, PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal) :-
	newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),			% Refine bounds
	boundedbest( Depth, PosList, NewAlpha, NewBeta, Pos1, Val1),
	betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).
	
newbounds( Alpha, Beta, Pos, Val, Val, Beta) :-
	min_to_move( Pos), Val > Alpha, !.				% Maximizer increased lower bound

newbounds( Alpha, Beta, Pos, Val, Alpha, Val) :-
	max_to_move( Pos), Val < Beta, !.				% Minimizer decreased upper bound
	
newbounds( Alpha, Beta, _, _, Alpha, Beta).	
		
% --------------------------------------------------------------------------
% moves/2( +P, -L) :-
% 	Finds all possible continuations (positions) given the initial position.
% P ... the initial position
% L ... the list containing all possible continuations (positions)
% --------------------------------------------------------------------------

moves( P, L) :-
	findall( P1, generate_position( P, _, P1), L).
	
% ------------------------------------------------------------------------------------------------------
% min_to_move/1( +P) :-
%	Evaluates to true if (and only if) it is the minimizing player's turn, otherwise evaluates to false.
% P ... the position to be checked
% ------------------------------------------------------------------------------------------------------

min_to_move( p( b, _, _, _, _)).

% ------------------------------------------------------------------------------------------------------
% max_to_move/1( +P) :-
%	Evaluates to true if (and only if) it is the maximizing player's turn, otherwise evaluates to false.
% P ... the position to be checked
% ------------------------------------------------------------------------------------------------------

max_to_move( p( w, _, _, _, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% STATIC POSITION EVALUATION ROUTINES                                                                         %%%
%%% ----------------------------------------------------------------------------------------------------------- %%%
%%% static_evaluation/2, corner_proximity/2, opposition_efficiency/3, evasion_potential/5, bishop_cooperation/3 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
			
% -----------------------------------------	
% static_evaluation/2( +P, -N) :-
% 	Evaluates the leaf position statically.
% P ... the position to be evaluated
% N ... the equity of the position	
% -----------------------------------------

static_evaluation( P, N) :-
	P = p( _, WK, B1, B2, BK),
	corner_proximity( BK, C1),						% 1st criterion: the proximity of either corner to the black king 
	opposition_efficiency( WK, BK, C2),				% 2nd criterion: the efficiency of the opposition held by the white king
	evasion_potential( WK, B1, B2, BK, C3), 		% 3rd criterion: the black king's potential to evade forcing moves
	bishop_cooperation( B1, B2, C4),				% 4th criterion: the cooperation between white bishops
	N is C1 * 1000 + C2 * 100 + C3 * 10 + C4.
	
% ------------------------------------------------------------------------------------------------------------------------------
% corner_proximity/2( + BK, -C1) :-
% 	Evaluates to 0-3 depending on the proximity of either corner to the black king: the nearer the corner the higher the equity.
% BK ... the black king's position
% C1 ... the equity of the position
% ------------------------------------------------------------------------------------------------------------------------------

corner_proximity( BK, 0) :-
	BK = C-R,
	(
		C = 4; C = 5;
		R = 4; R = 5
	),
	!.

corner_proximity( BK, 1) :-
	BK = C-R,
	(
		(C = 3; C = 6), R \= 4, R \= 5;
		(R = 3; R = 6), C \= 4, C \= 5
	),
	!.
	
corner_proximity( BK, 2) :-
	BK = C-R,
	(
		(C = 2; C = 7), R \= 3, R \= 4, R \= 5, R \= 6;
		(R = 2; R = 7), C \= 3, C \= 4, C \= 5, C \= 6
	),
	!.
	
corner_proximity( BK, 3) :-
	(
		BK = 1-1;
		BK = 8-1;
		BK = 1-8;
		BK = 8-8
	),
	!.
% -----------------------------------------------------------------------------------------------------------------------------------------
% opposition_efficiency/3( +WK, +BK, -C2) :-
% 	Evaluates to 2 or 1 if (and only if) the white king maintains the "strong" or "weak" opposition respectively, otherwise evaluates to 0.
% WK ... the white king's position
% BK ... the black king's position
% C2 ... the equity of the position
% -----------------------------------------------------------------------------------------------------------------------------------------

opposition_efficiency( WK, BK, 2) :-
	WK = WC-WR,
	BK = BC-BR,
	(
		(											% vertical "strong" opposition
			WC = BC,								% the same column
			DR is WR - BR,							% adjacent row
			(DR = 2; DR = -2) 
		) ; (										% horizontal "strong" opposition
			WR = BR,								% the same row
			DC is WC - BC,							% adjacent column
			(DC = 2; DC = -2)
		)
	),
	!.
	
opposition_efficiency( WK, BK, 1) :-
	WK = WC-WR,
	BK = BC-BR,
	(
		(											% vertical "weak" opposition
			DC is WC - BC,							% adjacent column
			(DC = 1; DC = -1),
			DR is WR - BR,
			(DR = 2; DR = -2)
		) ; (										% horizontal "weak" opposition
			DR is WR - BR,							% adjacent row
			(DR = 1; DR = -1),
			DC is WC - BC,
			(DC = 2; DC = -2)
		)
	),
	!.
	
opposition_efficiency( _, _, 0).					% otherwise no score for opposition
	
% -------------------------------------------------------------------------------------------------------------------
% evasion potential/5( +WK, +B1, +B2, +BK, -C3) :-
% 	Evaluates to 0-8 depending on the black king's potential to evade: the lower the potential the higher the equity.
% WK ... the white king's position
% B1 ... the 1st white bishop's position
% B2 ... the 2nd white bishop's position
% BK ... the black king's position
% C3 ... the equity of the position
% -------------------------------------------------------------------------------------------------------------------

evasion_potential( WK, B1, B2, BK, C3) :-
	threatened_by_W( WK, B1, B2, Tfin),
	findall( BKo, move_K( BK, Tfin, BKo), L),
	length( L, Len),								% the black king can evade to a maximum of 8 squares
	C3 is 8 - Len.									% minimum points assigned for any criterion is 0 => subtraction from 8
	
% --------------------------------------------------------------------------------------------------------------------
% bishop_cooperation( +B1, +B2, -C4) :-
% 	Evaluates to 1 if (and only if) the bishops are neighbouring vertically or horizontally, otherwise evaluates to 0.
% B1 ... the 1st white bishop's position
% B2 ... the 2nd white bishop's position
% C4 ... the equity of the position
% --------------------------------------------------------------------------------------------------------------------

bishop_cooperation( B1, B2, 1) :-
	B1 = C1-R1,
	B2 = C2-R2,
	(
		(											% bishops neighbouring vertically
			C1 = C2,								% the same column
			D is R1 - R2,							% adjacent row
			(D = 1; D = -1)							
		) ; (										% bishops neighbouring horizontally
			R1 = R2,								% the same row
			D is C1 - C2,							% adjacent column
			(D = 1; D = -1)
		)
	),
	!.

bishop_cooperation( _, _, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% POSITION & MOVE GENERATION ROUTINES + "THREATENED_BY_*" ROUTINES                                                                             %%%
%%% -------------------------------------------------------------------------------------------------------------------------------------------- %%%
%%% generate_position/3, move_WK/5, move_BK/5, move_K/3, move_B/3, threatened_by_K/3, threatened_by_B/4, threatened_by_B1-4/4, threatened_by_W/4 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% ------------------------------------------------------------------------------------	
% generate_position/3( +Pi, +M, -Po) :-
% 	Given the input position generates a legal move and the resulting output position.
% Pi ... the position before the move
% M  ... a legal move
% Po ... the position after the move
% ------------------------------------------------------------------------------------

generate_position( Pi, M, Po) :-
	Pi = p( Si, WKi, B1i, B2i, BKi),				% input position decomposition
	Po = p( So, WKo, B1o, B2o, BKo),				% output position decomposition
	(Si = w -> (									% white player's turn
		So = b,
		BKo = BKi,
		(
			(										% white king to move
				B1o = B1i,
				B2o = B2i,
				move_WK( WKi, B1i, B2i, BKi, WKo),
				M = 'K':WKi:WKo
			) ; (									% white 1st bishop to move
				WKo = WKi,
				B2o = B2i,
				move_B( WKi, B1i, B1o),
				M = 'B':B1i:B1o
			) ; (									% white 2nd bishop to move
				WKo = WKi,
				B1o = B1i,
				move_B( WKi, B2i, B2o),
				M = 'B':B2i:B2o
			)
		)
	) ; (											% black player's turn
		So = w,
		WKo = WKi,
		B1o = B1i,
		B2o = B2i,												
		move_BK( WKi, B1i, B2i, BKi, BKo),
		M = 'k':BKi:BKo 
	)).

% -----------------------------------------------------------------------------------------------
% move_WK/5( +WKi, +B1i, +B2i, +BKi, -WKo) :-
%	With respect to the current postion determined by the input parameters, moves the white king.
% WKi ... the white king's initial position
% B1i ... the 1st white bishop's position 
% B2i ... the 2nd white bishop's position
% BKi ... the black king's position
% WKo ... the white king's final position 
% -----------------------------------------------------------------------------------------------

move_WK( WKi, B1i, B2i, BKi, WKo) :-
	T = [B1i, B2i],									% squares occupied by allied bishops		
	threatened_by_K( BKi, T, Tfin),					% squares threatened by the black king							
	move_K( WKi, Tfin, WKo).
	
% -----------------------------------------------------------------------------------------------------------------
% move_BK/5( +WKi, +B1i, +B2i, +BKi, -BKo) :-
%	With respect to the current postion determined by the input parameters, moves the black king.
% WKi ... the white king's position
% B1i ... the 1st white bishop's position 
% B2i ... the 2nd white bishop's position
% BKi ... the black king's initial position
% BKo ... the black king's final position  
% Description: 
% Black king cannot move to a square where a white piece is already standing, nor can it step into the check by any
% of the white pieces. Therefore, the procedure will firstly evaluate threatened squares.
% -----------------------------------------------------------------------------------------------------------------

move_BK( WKi, B1i, B2i, BKi, BKo) :-
	threatened_by_W( WKi, B1i, B2i, Tfin),				
	move_K( BKi, Tfin, BKo).

% ------------------------------------------------------------------------
% move_K/3( +Ki, +Tfin, -Ko) :-
%	Moves a king avoiding the squares threatened by the opponent's pieces.
% Ki     ... the king's initial position
% Tfin ... the squares threatened by the opponent's pieces
% Ko     ... the king's final position 
% ------------------------------------------------------------------------

move_K( Ki, T, Ko) :-
	Ki = Kic-Kir,									% king's input position decomposition 
	Ko = Koc-Kor,									% king's output position decomposition
	Kic_m1 is Kic - 1,
	Kic_p1 is Kic + 1,
	Kir_m1 is Kir - 1,
	Kir_p1 is Kir + 1,
	
	% o-o-o-o
	% |1|2|3|
	% o-o-o-o
	% |4|K|6|
	% o-o-o-o
	% |7|8|9|
	% o-o-o-o
	
	(
		(											% 1
			within_board( Kic_m1-Kir_p1),
			\+member( Kic_m1-Kir_p1, T),
			Koc = Kic_m1,
			Kor = Kir_p1
		) ; (										% 2
			within_board( Kic-Kir_p1),
			\+member( Kic-Kir_p1, T),
			Koc = Kic,
			Kor = Kir_p1
		) ; (										% 3
			within_board( Kic_p1-Kir_p1),
			\+member( Kic_p1-Kir_p1, T),
			Koc = Kic_p1,
			Kor = Kir_p1
		) ; (										% 4
			within_board( Kic_m1-Kir),
			\+member( Kic_m1-Kir, T),
			Koc = Kic_m1,
			Kor = Kir
		) ; (										% 6
			within_board( Kic_p1-Kir),
			\+member( Kic_p1-Kir, T),
			Koc = Kic_p1,
			Kor = Kir
		) ; (										% 7
			within_board( Kic_m1-Kir_m1),
			\+member( Kic_m1-Kir_m1, T),
			Koc = Kic_m1,
			Kor = Kir_m1
		) ; (										% 8
			within_board( Kic-Kir_m1),
			\+member( Kic-Kir_m1, T),
			Koc = Kic,
			Kor = Kir_m1
		) ; (										% 9
			within_board( Kic_p1-Kir_m1),
			\+member( Kic_p1-Kir_m1, T),
			Koc = Kic_p1,
			Kor = Kir_m1
		)
	).
	
% -------------------------------------------------------------------------------------------------
% move_B/3( +WKi, +Bi, -Bo) :-
%	With respect to the current postion determined by the input parameters, moves the white bishop.
% WKi ... the white king's position
% Bi  ... the white bishop's initial position
% Bo  ... the white bishop's final position
% Description: 
% The only piece that can stand in bishop's way is the allied king. Therefore it is the only piece
% that is reckoned with. The majority of work is done by the procedure threatened_by_B/4. 	
% -------------------------------------------------------------------------------------------------

move_B( WKi, Bi, Bo) :-
	threatened_by_B( Bi, WKi, [], B),
	member( Bo, B).									% the procedure returns all members from the list B one after another
	
% ----------------------------------------------------------------------------------------------------------
% threatened_by_K/3( +K, +Li, -Lo) :-
% 	Computes the squares threatened by a king and merges them with the input list, creating the output list.
% K  ... the king's position
% Li ... the input list
% Lo ... the output list
% ----------------------------------------------------------------------------------------------------------

threatened_by_K( K, Li, Lo) :-
	K = Kc-Kr,
	Kc_m1 is Kc - 1,
	Kc_p1 is Kc + 1,
	Kr_m1 is Kr - 1,
	Kr_p1 is Kr + 1,
	Ttmp = [Kc_m1-Kr_p1, Kc-Kr_p1, Kc_p1-Kr_p1, 
	        Kc_m1-Kr,    Kc-Kr,    Kc_p1-Kr,
	        Kc_m1-Kr_m1, Kc-Kr_m1, Kc_p1-Kr_m1],
	within_board( Ttmp, Ttmp1),
	merge_unique( Li, Ttmp1, Lo).
	
% ---------------------------------------
% threatened_by_B/4( +B, +K, +Li, -Lo) :-
%	Computes the squares threatened by a bishop and merges them with the input list, creating the output list.
% B  ... the white bishop's position
% K  ... the white king's position
% Li ... the input list
% Lo ... the output list
% ---------------------------------------	

threatened_by_B( B, K, Li, Lo) :-
	B = Bc-Br,
	Bc_m1 is Bc - 1,
	Bc_p1 is Bc + 1,
	Br_m1 is Br - 1,
	Br_p1 is Br + 1,
	threatened_by_B1( Bc_m1-Br_p1, K, [], B1),
	threatened_by_B2( Bc_p1-Br_p1, K, [], B2),
	threatened_by_B3( Bc_m1-Br_m1, K, [], B3),
	threatened_by_B4( Bc_p1-Br_m1, K, [], B4),
	append( B1,    B2, Btmp1),
	append( Btmp1, B3, Btmp2),
	append( Btmp2, B4, Btmp3),
	merge_unique( Li, Btmp3, Lo).

% -----------------------------------------
% threatened_by_B1/4( +B, +K, +Acc, -B1) :-
% -----------------------------------------	

threatened_by_B1( B, K, Acc, Acc) :-
	(B = K;											% white king in the way
	\+within_board( B)),							% not within board 
	!.												% accumulator is returned and the cut is applied

threatened_by_B1( Bc-Br, K, Acc, B1) :-
	Bc_m1 is Bc - 1,
	Br_p1 is Br + 1,
	threatened_by_B1( Bc_m1-Br_p1, K, [Bc-Br|Acc], B1).
		
% -----------------------------------------
% threatened_by_B2/4( +B, +K, +Acc, -B2) :-
% -----------------------------------------	

threatened_by_B2( B, K, Acc, Acc) :-
	(B = K;										
	\+within_board( B)),						 
	!.												

threatened_by_B2( Bc-Br, K, Acc, B2) :-
	Bc_p1 is Bc + 1,
	Br_p1 is Br + 1,
	threatened_by_B2( Bc_p1-Br_p1, K, [Bc-Br|Acc], B2).

% -----------------------------------------
% threatened_by_B3/4( +B, +K, +Acc, -B3) :-
% -----------------------------------------	

threatened_by_B3( B, K, Acc, Acc) :-
	(B = K;										
	\+within_board( B)),						 
	!.												

threatened_by_B3( Bc-Br, K, Acc, B3) :-
	Bc_m1 is Bc - 1,
	Br_m1 is Br - 1,
	threatened_by_B3( Bc_m1-Br_m1, K, [Bc-Br|Acc], B3).

% -----------------------------------------
% threatened_by_B4/4( +B, +K, +Acc, -B4) :-
% -----------------------------------------	

threatened_by_B4( B, K, Acc, Acc) :-
	(B = K;										
	\+within_board( B)),						 
	!.												

threatened_by_B4( Bc-Br, K, Acc, B4) :-
	Bc_p1 is Bc + 1,
	Br_m1 is Br - 1,
	threatened_by_B4( Bc_p1-Br_m1, K, [Bc-Br|Acc], B4).

% ---------------------------------------------
% threatened_by_W/4( +WK, +B1, +B2, -Tfin) :-
% ---------------------------------------------	
	
threatened_by_W( WK, B1, B2, Tfin) :-
	threatened_by_K( WK, [], Ttmp1),				% squares threatened by the white king
													% We pass-by-value the position of WK because it is relevant when calculating the squares threatened by B.	
	threatened_by_B( B1, WK, Ttmp1, Ttmp2),			% squares threatened by the 1st white bishop
	threatened_by_B( B2, WK, Ttmp2, Tfin).			% squares threatened by the 2nd white bishop	
													% Tfin now contains the list of all threatened squares

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AUXILLIARY ROUTINES                                                                            %%%
%%% ---------------------------------------------------------------------------------------------- %%%
%%% find_min/2, find_min/3, find_max/2, find_max/3, within_board/2, within_board/1, merge_unique/3 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------------------------------------
% find_min/2( +List, -Min) :-
% 	Finds the minimum member of the List.
% ---------------------------------------

find_min( [H|T], Min) :-
	find_min( H, T, Min).							% interface

% ----------
% find_min/3
% ----------

find_min( Min, [], Min).
find_min( o( N1, M1), [o( N2, M2)|T], Min) :-
	(N2 < N1 -> (									% new minimum found
		N = N2,
		M = M2
	) ; (											% old minimum remains
		N = N1,
		M = M1
	)),	
	find_min( o( N, M), T, Min).

% ---------------------------------------
% find_max/2( +List, -Min) :-	
% 	Finds the maximum member of the List.
% ---------------------------------------

find_max( [H|T], Max) :-
	find_max( H, T, Max).							% interface

% ----------
% find_max/3
% ----------

find_max( Max, [], Max).
find_max( o( N1, M1), [o( N2, M2)|T], Max) :-
	(N2 > N1 -> (									% new maximum found
		N = N2,
		M = M2
	) ; (											% old maximum remains
		N = N1,
		M = M1
	)),	
	find_max( o( N, M), T, Max).
	
% --------------------------------------------------------------
% within_board/2( +Li, -Lo) :-
%	Excludes the squares that do not lie within the chess-board.
% Li ... the input list
% Lo ... the output list
% --------------------------------------------------------------

within_board( [], []).
within_board( [H|T], Lo) :-
	(within_board( H) -> 
		Lo = [H|T1] 
	; 
		Lo = T1
	),
	within_board( T, T1).

% ----------------------------------------------------------------------------------------------------------
% within_board/1( +H) :-
% 	Evaluates to true if (and only if) the square lies within the chess-board, otherwise evaluates to false.
% H ... the square being checked
% ----------------------------------------------------------------------------------------------------------

within_board( H) :-
	H = C-R,
	C >= 1,
	C =< 8,
	R >= 1,
	R =< 8.
	
% -----------------------------------------------------------
% merge_unique/3( +L1, +L2, -L) :-
% 	Merges two lists into one; excluding duplicit occurences.
% L1 ... the 1st list
% L2 ... the 2nd list
% L  ... the resulting list (merge & unique)
% -----------------------------------------------------------	

merge_unique( [], L2, L2).
merge_unique( [H|T], L2, L) :-
	merge_unique( T, L2, LT),
	(member( H, L2) -> 
		L = LT 
	; 
		L = [H|LT]
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT ROUTINES                                                                %%%
%%% ------------------------------------------------------------------------------ %%%
%%% display_position/1, display_position_R/2, display_position_C/3, piece_lookup/3 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% --------------------------------------------------------------
% display_position( +P) :-
% 	Displays the position in the form of a simple ASCII diagram.
% P ... position to be displayed 
% --------------------------------------------------------------

display_position( P) :-
	P = p( S, WK, B1, B2, BK),
	Pieces = ['K':WK, 'B':B1, 'B':B2, 'k':BK],
	display_position_R( Pieces, 8), 
	(S = w -> 
		write( 'White to move.') 
	; 
		write( 'Black to move.')
	),
	!.

% -------------------------------------------------
% display_position_R( +Pieces, +R) :-
% Pieces ... the list of pieces and their locations
% R      ... the row being displayed
% -------------------------------------------------

display_position_R( _, 0) :-
	write( ' 12345678'),
	nl,
	!.												% necessary?
display_position_R( Pieces, R) :-
	write( R),
	display_position_C( Pieces, 1, R),
	nl,
	R_m1 is R - 1,
	display_position_R( Pieces, R_m1).
	
% -------------------------------------------------
% display_position_C( +Pieces, +C, +R) :-
% Pieces ... the list of pieces and their locations
% C      ... the column being displayed
% R      ... the row being displayed
% -------------------------------------------------

display_position_C( _, 9, _) :-
	!.												% necessary?
display_position_C( Pieces, C, R) :-
	piece_lookup( Pieces, C-R, Piece),
	write( Piece),
	C_p1 is C + 1,
	display_position_C( Pieces, C_p1, R).
	
% -------------------------------------------------------------------------------------------------------------------------------------------
% piece_lookup( +Pieces, +Sq, -Piece) :-
%	Returns the symbol of a piece standing on the square Sq (WK = 'K', B1/2 = 'B', BK = 'k'), otherwise returns '.' denoting a vacant square.
% Pieces ... the list of pieces and their locations
% Sq     ... the square being looked up
% Piece  ... the piece occupying suare Sq
% -------------------------------------------------------------------------------------------------------------------------------------------	

piece_lookup( [], _, '.').
piece_lookup( [Piece:Sq|_], Sq, Piece).
piece_lookup( [_|T], Sq, Piece) :-
	piece_lookup( T, Sq, Piece).