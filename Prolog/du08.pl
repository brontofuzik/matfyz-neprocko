% 1. (opraveno pùvodnì chybné zadání) Definujte predikát next/2, který k zadané permutaci množiny pøirozených èísel, reprezentované seznamem, vrátí permutaci lexikograficky následující. 

% ?- next([1,2,3,4],N).
% N=[1,2,4,3]

% ?- next([1,4,3,2],N).
% N=[2,1,3,4]

% ?- next([4,3,2,1],N).
% no

next(P,N) :-
	reverse(P,[H|R1]),
	next1([H],S1,R1,I1,R2),
	subst(S1,I1,S2,I2),
	append(S2,[I2|R2],NR),
	reverse(NR,N).
	
% next1(+Acc1,-Acc2,+Rest1,-I,-Rest2) :- Do akumulatora Acc1 postupne uklada prvky postupnosti Rest1 pokial narastaju,
%                                        vysledok vrati v akumulatore Acc2. I je prvy prvok, ktory je mensi ako jeho 
%                                        predchodca a Rest2 je zvysok postupnosti Rest1. 
next1([H1|T1],A,[H2|T2],I,R2) :-
	H1<H2,
	next1([H2,H1|T1],A,T2,I,R2).
next1([H1|T1],[H1|T1],[H2|T2],H2,T2) :-
	H1>H2.

% subst(+List1,+I1,-List2,-I2) :- V zozname List1 (zatriedenom klesajuco) nahradi posledny prvok vacsi ako I1,
%                                 vrati jeho hodnotu ako I2 a vysledny zoznam ako List2.
subst([H1,H2|T1],I1,[H1|HT2],I2) :-
	I1<H2,
	subst([H2|T1],I1,HT2,I2).
subst([H1,H2|T1],I1,[I1,H2|T1],H1) :-
	I1>H2.
subst([N],I1,[I1],N) :-
	number(N).

% 2. V jazyce Prolog navrhnìte reprezentaci obecného (nikoliv pouze binárního) stromu s  vrcholy ohodnocenými pøirozenými èísly. Nad touto reprezentací definujte predikát eq/2, která zjistí, zdali jsou dva zadané stromy ekvivalentní. Dva stromy jsou ekvivalentní, pokud lze jeden obdržet z druhého vhodnou permutací podstromù pro každý vrchol. 

% Reprezentacia obecneho stromu

strom1(t(0,[t(4,[t(2,[]),t(1,[])]),t(2,[]),t(1,[]),t(3,[])])).
strom2(t(0,[t(3,[]),t(1,[]),t(4,[t(1,[]),t(2,[])]),t(2,[])])).

% eq(+Strom1,+Strom2) :- Pravda ak su Strom1 a Strom2 ekvivalentne (podla definicie uvedenej vyssie).
eq(t(K,S1),t(K,S2)) :-
	eq1(S1,S2).

% eq1(+ZoznamSynov1,+ZoznamSynov2) :- Pravda ak su ZoznamSynov1 a ZoznamSynov2 permutacie tej istej mnoziny prir. cisel 1 az N.
eq1([],[]).
eq1([t(K,Ch1)|T1],S2) :- 
	retrieve(K,S2,KCh2,T2),
	eq(t(K,Ch1),KCh2),
	eq1(T1,T2).
	
% retrieve(+Kluc,+ZoznamPodstromov,-Podstrom,-UpravenyZoznamPodstromov) :- Zo ZoznamPodstromov vymaze podstrom s vrcholom Kluc, vrati ho ako Podstrom a upraveny zoznam vrati ako UpravenyZoznamPodstromov.
retrieve(K,[t(K,S1)|T],t(K,S1),T) :-
	!.
retrieve(K,[t(M,S1)|T],P,[t(M,S1)|S2]) :-
	K\=M,
	retrieve(K,T,P,S2).		