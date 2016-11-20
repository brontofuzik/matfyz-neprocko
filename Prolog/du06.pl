% Protože v tuto chvíli už známe podstatnou èást jazyka Prolog, mùžeme se pokusit o øešení ménì triviálních úloh, které mají smysl nejen jako jednoduché cvièení.

% 1. Vyhodnocování booleovských výrazù:

% Booleovské výrazy jsou utvoøeny obvyklým zpùsobem z konstant true, false, spojek and/2, or/2, xor/2, imp/2, ekv/2, non/1 a závorek.
% Spojky definujte jako operátory napø takto:

:- op(200,  fy, non).
:- op(210, yfx, and).
:- op(215, xfx, [nand,xor]).
:- op(220, yfx, or).
:- op(230, xfy, imp).
:- op(240, xfx, ekv).

% eval(+vyraz, ?hodnota) :- hodnota je pravdivostní hodnota booleovského vyrazu.

eval(V,X) :- eval([],V,X). % Riesime prevodom na obecnejsi pripad, aby sme nemuseli pisat "ten isty" kod dvakrat.

/* Tento kod je redundantny, preto je zakomentovany.
eval(true,true).
eval(false,false).

eval(non(B),X):-
	eval(B,V1), 	% rekurze
	eval_non(V1,X). % vyhodnocení spojky

eval((A)and(B),X) :- 
	eval(A,A1), 
	eval(B,B1), 
	eval_and(A1,B1,X).

eval((A)nand(B),X) :-
	eval(A,A1),
	eval(B,B1),
	eval_nand(A1,B1,X).

eval((A)or(B),X) :-
	eval(A,A1),
	eval(B,B1),
	eval_or(A1,B1,X).

eval((A)nor(B),X) :-
	eval(A,A1),
	eval(B,B1),
	eval_nor(A1,B1,X).

eval((A)xor(B),X) :-
	eval(A,A1),
	eval(B,B1),
	eval_xor(A1,B1,X).

eval((A)imp(B),X) :-
	eval(A,A1),
	eval(B,B1),
	eval_imp(A1,B1,X).

eval((A)ekv(B),X) :-
	eval(A,A1),
	eval(B,B1),
	eval_ekv(A1,B1,X).
*/

%% pomocné predikáty:

% eval_non - definice výètem

eval_non(true, false).
eval_non(false, true).

% eval_and - definicia vyctom

eval_and(true,  true,  true).
eval_and(true,  false, false).
eval_and(false, true,  false).
eval_and(false, false, false).

% eval_nand - definicia vyctom

eval_nand(true,  true,  false).
eval_nand(true,  false, true).
eval_nand(false, true,  true).
eval_nand(false, false, true).

% eval_or - definicia vyctom

eval_or(true,  true,  true).
eval_or(true,  false, true).
eval_or(false, true,  true).
eval_or(false, false, false).

% eval_nor - definicia vyctom

eval_nor(true,  true,  false).
eval_nor(true,  false, false).
eval_nor(false, true,  false).
eval_nor(false, false, true).

% eval_xor - definicia vyctom

eval_xor(true,  true,  false).
eval_xor(true,  false, true).
eval_xor(false, true,  true).
eval_xor(false, false, false).

% eval_imp - definicia vyctom

eval_imp(true,  true,  true).
eval_imp(true,  false, false).
eval_imp(false, true,  true).
eval_imp(false, false, true).

% eval_ekv - definicia vyctom

eval_ekv(true,  true,  true).
eval_ekv(true,  false, false).
eval_ekv(false, true,  false).
eval_ekv(false, false, true).

% a) Dokonèete definici predikátu eval/2 (alespoò pro spojky and/2 a or/2)

% b) Rozšiøte definici booleovského výrazu o promìnné (napø pomocí predikátu prom/1) a definujte predikát
% eval(+TabulkaProm, +Vyraz, -Hodnota) :- Hodnota je pravdivostní hodnota booleovského Vyrazu  pro pravdivostní hodnoty promìnných, které jsou uloženy v seznamu TabulkaProm.

% Pøíklad:
% ?- eval([a-true,b-false], prom(a) and non prom(b), V).
%   V = true

% Deklaracia potrebnych operatorov
:- op(100,fy,prom).
:- op(110,xfx,-).

% lookup(+TabulkaPremennych,+Premenna,-Hodnota) :- Vrati hodnotu premennej z tabulky premennych.
lookup([A-X|_],A,X).
lookup([_|T],A,X) :- lookup(T,A,X). 

% Boolsky vyraz bez premennych
% eval([],V,X) :- eval(V,X).

eval(_,true,true).
eval(_,false,false).

eval(TP,prom(A),X) :- lookup(TP,A,X).

eval(TP,non(A),X):-
	eval(TP,A,V1),
	eval_non(V1,X).

eval(TP,(A)and(B),X) :- 
	eval(TP,A,A1), 
	eval(TP,B,B1), 
	eval_and(A1,B1,X).

eval(TP,(A)or(B),X) :-
	eval(TP,A,A1),
	eval(TP,B,B1),
	eval_or(A1,B1,X).

eval(TP,(A)nand(B),X) :-
	eval(TP,A,A1),
	eval(TP,B,B1),
	eval_nand(A1,B1,X).

eval(TP,(A)xor(B),X) :-
	eval(TP,A,A1),
	eval(TP,B,B1),
	eval_xor(A1,B1,X).

eval(TP,(A)imp(B),X) :-
	eval(TP,A,A1),
	eval(TP,B,B1),
	eval_imp(A1,B1,X).

eval(TP,(A)ekv(B),X) :-
	eval(TP,A,A1),
	eval(TP,B,B1),
	eval_ekv(A1,B1,X).

% 2. Mùžete se pokusit i o definici predikátu tautologie/1

% tautologie(+Vyraz):- vyraz je tautologií (nabývá hodnoty pravda pro všechny možné pravdivostní hodnoty svých promìnných).
% Poznamka: Definicia vyuziva vstavany predikat rezu, ktory zaruci, ze prve false sposobi negativnu odpoved na otazku.

tautologie(V) :- 
	extract(V,TP),     % vyextrahuje premenne pouzite vo vyraze V (aj duplicitne) a ulozi ich do zoznamu TP
	unique(TP,TPU),    % odstrani viac-nasobne vyskyty premennych zo zoznamu TP, vznikne vysledny zoznam TPU
	generate(TPU,TPT), % pre zoznam premennych TPU vygeneruje vsetky mozne konfiguracie pravdivostnych hodnot
	eval(TPT,V,false), % spusti vyhodnotenie logickeho vyrazu pre konkretnu konfiguraciu
	!,                 % akonahle sa pravdivostna hodnota rovna "false", V nie je tautologiou
	fail.              % overovanie konci
tautologie(_).             % v pripade, ze "false" nenastalo ani raz, V je tautologiou

% extract(+Vyraz,-TabulkaPremennych) :- Z logickeho vyrazu Vyraz vyextrahuje logicke premenne a ulozi ich do zoznamu TabulkaPremennych.
extract(true,[]).
extract(false,[]).
extract(prom(X),[X]).
extract(non(A),TP) :- extract(A,TP).
extract((A)and(B),TP) :- extract(A,TPA),extract(B,TPB),append(TPA,TPB,TP).
extract((A)or(B),TP) :- extract(A,TPA),extract(B,TPB),append(TPA,TPB,TP).
extract((A)nand(B),TP) :- extract(A,TPA),extract(B,TPB),append(TPA,TPB,TP).
extract((A)xor(B),TP) :- extract(A,TPA),extract(B,TPB),append(TPA,TPB,TP).
extract((A)imp(B),TP) :- extract(A,TPA),extract(B,TPB),append(TPA,TPB,TP).
extract((A)ekv(B),TP) :- extract(A,TPA),extract(B,TPB),append(TPA,TPB,TP).

% unique(+TP,-TPU) :- Zo zoznamu TP odstrani viac-nasobne vyskyty premennych, vznikne vysledny zoznam TPU.
unique([],[]).
unique([H|T],[H|T2]) :- 
	delall(H,T,T1),
	unique(T1,T2).

% dellall(+X,+T,-T1) :- Zo zoznamu T odstrani vsetky vyskyty prvku X, vznikne vysledny zoznam T1.
delall(_,[],[]).
delall(X,[X|T],T1) :-
	delall(X,T,T1).
delall(X,[H|T],[H|T1]) :-
	X\=H,
	delall(X,T,T1).

% generate(+TPU,-TPT) :- Pre tabulku premennych TPU vygeneruje vsetky mozne konfiguracie pravdivostnych hodnot, vznikne vysledny zoznam TPT.
generate([],[]).
generate([H|T],[P|T1]) :- (P=H-true;P=H-false),generate(T,T1).

% 3. Bonus: Pøevod do konjunktivního normálního tvaru. Tento problém tematicky spadá do pøedmìtu Výroková a predikátová logika (NAIL062), vyuèovaného v LS. Øešení úlohy tedy pochopitelnì není povinné, pokud je však pošlete, bude poèítáno jako bonus, který nahradí libovolnou neodevzdanou úlohu.

% Formule výrokového poètu je v konjunktivním normálním tvaru (CNF), je-li konjunkcí klauzulí, kde klauzule je disjunkcí literálù, pøièemž literál je výroková promìnná nebo její negace. Každou formuli výrokového poètu lze pøevést (pomocí nahrazení ostatních logických spojek základními and,or,non, De Morganových pravidel, eliminace dvojité negace a distributivního zákona) na ekvivalentní formuli v CNF.

% Definujte predikát
% cnf(+F,?C) :- C je formule v CNF ekvivalentní k formuli F.

% Pøíklad:
% ?- cnf(prom(a) and (prom(b) or (prom(c) and non prom(d))), C).
%   C = prom(a) and (prom(b) or prom(c)) and (prom(b) or non prom(d))

% Pokuste se alespoò o formule vytvoøené pomocí spojek and/2, or/2 a non/1, závorek a výrokových promìnných (predikát prom/1).