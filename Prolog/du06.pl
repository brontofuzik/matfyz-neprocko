% Proto�e v tuto chv�li u� zn�me podstatnou ��st jazyka Prolog, m��eme se pokusit o �e�en� m�n� trivi�ln�ch �loh, kter� maj� smysl nejen jako jednoduch� cvi�en�.

% 1. Vyhodnocov�n� booleovsk�ch v�raz�:

% Booleovsk� v�razy jsou utvo�eny obvykl�m zp�sobem z konstant true, false, spojek and/2, or/2, xor/2, imp/2, ekv/2, non/1 a z�vorek.
% Spojky definujte jako oper�tory nap� takto:

:- op(200,  fy, non).
:- op(210, yfx, and).
:- op(215, xfx, [nand,xor]).
:- op(220, yfx, or).
:- op(230, xfy, imp).
:- op(240, xfx, ekv).

% eval(+vyraz, ?hodnota) :- hodnota je pravdivostn� hodnota booleovsk�ho vyrazu.

eval(V,X) :- eval([],V,X). % Riesime prevodom na obecnejsi pripad, aby sme nemuseli pisat "ten isty" kod dvakrat.

/* Tento kod je redundantny, preto je zakomentovany.
eval(true,true).
eval(false,false).

eval(non(B),X):-
	eval(B,V1), 	% rekurze
	eval_non(V1,X). % vyhodnocen� spojky

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

%% pomocn� predik�ty:

% eval_non - definice v��tem

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

% a) Dokon�ete definici predik�tu eval/2 (alespo� pro spojky and/2 a or/2)

% b) Roz�i�te definici booleovsk�ho v�razu o prom�nn� (nap� pomoc� predik�tu prom/1) a definujte predik�t
% eval(+TabulkaProm, +Vyraz, -Hodnota) :- Hodnota je pravdivostn� hodnota booleovsk�ho Vyrazu  pro pravdivostn� hodnoty prom�nn�ch, kter� jsou ulo�eny v seznamu TabulkaProm.

% P��klad:
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

% 2. M��ete se pokusit i o definici predik�tu tautologie/1

% tautologie(+Vyraz):- vyraz je tautologi� (nab�v� hodnoty pravda pro v�echny mo�n� pravdivostn� hodnoty sv�ch prom�nn�ch).
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

% 3. Bonus: P�evod do konjunktivn�ho norm�ln�ho tvaru. Tento probl�m tematicky spad� do p�edm�tu V�rokov� a predik�tov� logika (NAIL062), vyu�ovan�ho v LS. �e�en� �lohy tedy pochopiteln� nen� povinn�, pokud je v�ak po�lete, bude po��t�no jako bonus, kter� nahrad� libovolnou neodevzdanou �lohu.

% Formule v�rokov�ho po�tu je v konjunktivn�m norm�ln�m tvaru (CNF), je-li konjunkc� klauzul�, kde klauzule je disjunkc� liter�l�, p�i�em� liter�l je v�rokov� prom�nn� nebo jej� negace. Ka�dou formuli v�rokov�ho po�tu lze p�ev�st (pomoc� nahrazen� ostatn�ch logick�ch spojek z�kladn�mi and,or,non, De Morganov�ch pravidel, eliminace dvojit� negace a distributivn�ho z�kona) na ekvivalentn� formuli v CNF.

% Definujte predik�t
% cnf(+F,?C) :- C je formule v CNF ekvivalentn� k formuli F.

% P��klad:
% ?- cnf(prom(a) and (prom(b) or (prom(c) and non prom(d))), C).
%   C = prom(a) and (prom(b) or prom(c)) and (prom(b) or non prom(d))

% Pokuste se alespo� o formule vytvo�en� pomoc� spojek and/2, or/2 a non/1, z�vorek a v�rokov�ch prom�nn�ch (predik�t prom/1).