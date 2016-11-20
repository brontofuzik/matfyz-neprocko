% 1. Permutaci mno�iny n p�irozen�ch ��sel {1,2,...,n} lze zadat dv�ma zp�soby

% -jako seznam obraz�, nap�. [2,3,1,4,6,5]
% -jako seznam cykl�, [[1,2,3],[5,6]]

% Navrhn�te alespo� jeden z n�sleduj�c�ch predik�t�:

% permcyk(+N,+Permutace,-Cykly)
% Permutaci mno�iny {1,2,...,N} zadanou seznamem obraz� Permutace
% p�evede na odpov�daj�c� seznam cykl� Cykly.

:- op(200,xfx,'~'). 				% deklaracia operatora '~' ako bijekcie

permcyk(_,[],[]).				% osetrenie trivilaneho pripadu
permcyk(N,P,C) :-
	enumerate(1,P,PN),			% PN je zoznam P ocislovany od 1 ("tabulkovy zapis" permutacie)
	permcyk1(N,PN,C).			% prevedie tabulkovy zapis permutacie na zapis pomocou cyklov

% enumerate(+N,+P,-PN) :- Ocisluje prvky zoznamu P pocinajuc cislom N; vrati ako vystupny zoznam PN.
enumerate(_,[],[]).				% ostrenie trivialneho pripadu
enumerate(N,[H|T],[N~H|T1]) :-			% ocisluje hlavu
	N1 is N+1, 				% zvysi cislo od ktoreho sa
	enumerate(N1,T,T1).			% ocisluje telo zoznamu

% permcyk(+N,+PN,-C) :- Prvedie tabulkovy zapis permutacie PN dlzky N na zapis pomocou cyklov C.
permcyk1(_,[],[]).				% ostrenie trivialneho pripadu
permcyk1(N,[E~F|PN],C) :-
	N1 is N-1, 
	follow(N1,NR,E,F,PN,PNR,[E~F],CNR),
	denumerate(CNR,C1),
	permcyk1(NR,PNR,C2),
	C=[C1|C2].

% follow(N,NR,E,F,PN,PNR,CN,CNR) :- Extrahuje zo zoznamu PN dlzky N jeden cyklus zacinajuci cislom E; zo zoznamu tento cyklus odstrani - vznikne cyklus PNR dlzky NR; cyklus vrati ako CNR
follow(N,N,E,E,PN,PN,CN,CN).
follow(N,NR,E,F,PN,PNR,CN,CNR) :-
	retrieve(PN,F~G,PN1),
	N1 is N-1,
	append(CN,[F~G],CNT),			% vstavany predikat append
	follow(N1,NR,E,G,PN1,PNR,CNT,CNR).

% retrieve(+PN,X~Y,PN1) :- Zo zoznamu PN odstrani prvok X~Y, vrati ho mechanizmom unifikacie a vysledny zoznam vrati ako PN1.
retrieve([F~G|T],F~G,T).			% prvok X~Y sa nachadza v hlave; je vratene telo vst. zoznamu ako vys. zoznam
retrieve([H|T],F~G,[H|T1]) :-			% prvok sa nenachadza v hlave
	retrieve(T,F~G,T1).			% je teda vyzdvihnuty z tela

% denumerate(+CNR,-C1) :- Zo zoznamu CNR odstrani cislovanie; vysledny zoznam vracia ako C1.
denumerate([],[]).				% osetrenie trivialneho pripadu
denumerate([_~H|T],[H|T1]) :-			% odstrani cislovanie z hlavy a
 	denumerate(T,T1).			% pokracuje v odstranovani v tele

% cykperm(+N,+Cykly,-Permutace)
% Permutaci mno�iny {1,2,...,N} zadanou seznamem cykl� Cykly
% p�evede na odpov�daj�c� seznam obraz� Permutace.

cykperm(0,[],[]).
cykperm(N,C,P) :-
	enrich(C,CE),
	flatten(CE,CF),
	insert_sort(CF,CS),
	denumerate(CS,P).

% enrich(+C,-CE) :- CE je zoznam zoznamov, ktory vznikne zo zoznamu zoznamov C cyklickym ocislovanim prvkov.
enrich([],[]).
enrich([[H|B]|T],[HB1|T1]) :-
	enrich1(H,[H|B],HB1),
	enrich(T,T1).

% enrich1(+H,+HB,-HB1) :- HB1 je zoznam, ktory vznikne zo zoznamu HB cyklickym ocislovanim prvkov.
enrich1(_,[],[]).
enrich1(X,[L],[L~X]).
enrich1(X,[L,M|T],[L~M|T1]) :-
	enrich1(X,[M|T],T1).

% insert_sort(+[X|T],-S) :- Najprv sa zatriedi telo T vstupneho zoznamu, potom sa do neho vlozi na spravne miesto hlava X vstupneho zoznamu.
insert_sort([],[]).
insert_sort([X|T],S) :-
	insert_sort(T,S1),
	insert(X,S1,S).

insert(X~X1,[Y~Y1|S],[Y~Y1|S1]) :-
	X>Y ,                     % ak je X>Y musi byt vlozeny do tela,
	!,                        % ina moznost nie je, preto rez
	insert(X~X1,S,S1).
insert(X,S,[X|S]).

% 2. Operace nad bin�rn�m vyhled�vac�m stromem.

% Reprezentace bin�rn�ch strom�:
% nil     pr�zdn� strom
% t(Levy,Koren,Pravy)     Lev� podstrom, Ko�en, Prav� podstrom

% P��klad:
% t(t(nil,1,nil),3,t(nil,5,nil)) reprezentuje strom s ko�enem 3 a listy 1 a 5.

% Ve vrcholech bin�rn�ho vyhled�vac�ho stromu jsou ulo�ena ��sla tak, �e v lev�m podstromu jsou pouze ��sla men��, a v prav�m podstromu pouze ��sla v�t��, ne�li hodnota ulo�en� v ko�eni libovoln�ho podstromu. Definujte predik�ty

% pro vkl�d�n� ��sla do bin�rn�ho vyhled�vac�ho stromu, kter� usp�je i kdy� tam dan� ��slo ji� je (pak vr�t� p�vodn� strom)
% pro vypu�t�n� ��sla z bin�rn�ho vyhled�vac�ho stromu, kter� usp�je i kdy� tam dan� ��slo nebylo (pak vr�t� p�vodn� strom)

% prvok(?X,+Bvs) :- (+) Overuje, ci je X prvok bin. vyhl. stromu Bvs. (-) Generuje postupne vsetky prvky bin. vyhl. stromu Bvs.

prvok(X,t(_,X,_)).                % vrchol stromu je jeho prvkom
prvek(X,t(L,V,_)) :-
	X<V,                          % X je mensi nez vrchol V,
	prvok(X,L).        % moze teda byt len v lavom podstrome
prvek(X,t(_,V,R)) :- 
	X>V,                          % X je vacsi nez vrchol V,
	prvok(X,R).       % moze teda byt len v pravom podstrome

% vloz(+X,+IBvs,-OBvs) :- OBvs vznikne vlozenim prvku X do stromu IBvs. Ak tam uz bol, bude OBvs rovnaky ako IBvs.

vloz(X,nil,t(nil,X,nil)). % vlozenie prvku X do prazdneho stromu
vloz(X,t(L,X,R),t(L,X,R)). % X je uz vrcholom vstupujuceho stromu
vloz(X,t(L,V,R),t(L1,V,R)) :-
	X<V,                      % X je mensi nez vrchol stromu
	vloz(X,L,L1). % L1 vznikne vlozenim prvku X do laveho podstromu L
vloz(X,t(L,V,R),t(L,V,R1)) :-
	X>V,                      % X je vacsi nez vrchol stromu
	vloz(X,R,R1). % R1 vznikne vlozenim prvku X do praveho podstromu R

% vypust(+X,+IBvs,-OBvs) :- OBvs je strom, ktory vznikne zo vstupneho stromu Ibvs vypustenim prvku X.

vypust(_,nil,nil).               % vypustenie z prazdneho stromu
vypust(X,t(nil,X,R),R). % vypustenie prvku, ktory nema laveho syna
vypust(X,t(L,X,nil),L). % vypustenie prvku, ktory nema praveho syna
vypust(X,t(L,X,R),t(L1,Y,R)) :- % vypustany prvok X, ktory ma dvoch synov
	najprav(L,L1,Y). % bude nahradeny prvok, ktory je najpravejsim 
                                     % uzlom v lavom podstrome L
vypust(X,t(L,V,R),t(L1,V,R)) :- 
	X<V,               % prvok X je mensi nez je koren stromu V
	vypust(X,L,L1).        % vypusta sa z laveho podstromu L
vypust(X,t(L,V,R),t(L,V,R1)) :- 
	X>V,               % prvok X je vacsi nez je koren stromu V
	vypust(X,R,R1).       % vypusta sa z praveho podstromu P

% najprav(+IBvs,-Obvs,-Najprav) :- Obvs je strom, ktory vznikne zo vstupneho stromu IBvs vypustenim jeho najpravejsieho prvku Najprav.

najprav(t(L,X,nil),L,X).      % ak je pravy podstrom prazdny, je
                              % najpravejsim prvkom koren stromu
najprav(t(L,X,R),t(L,X,R1),Y) :-   % inak je najpravejsim prvkom
	najprav(R,R1,Y).   % najpravejsi prvok praveho podstromu

% Odlad�n� �e�en� probl�mu 1 nebo 2 ( sta�� jeden z nich) odeslat e-mailem nejpozd�ji do ned�le 18.11.

% 3. Rozpozn�v�n� regul�rn�ch v�raz�: Tento probl�m tematicky spad� do p�edm�tu Automaty a gramatiky (TIN071). �e�en� �lohy nen� povinn�, pokud je v�ak po�lete, bude po��t�no jako bonus, kter� nahrad� libovolnou neodevzdanou �lohu.

% Definujte predik�t
% regexp(+Text, +RegVyraz, ?Delka) :- prvn�ch Delka znak� Textu je rovno slovu pat��c�mu do mno�iny reprezentovan� regul�rn�m v�razem RegVyraz.

% P��klady:
% ?-regexp([a,b,b,a,c,d], *([a,b]):[c,d], D)
%   D = 5
% ?-regexp([a,b,b,a,c,d], *([a,d])+[b,c], D)
%   D = 0;
%   D = 1;
%   no

% Regul�rn� v�razy pro na�e ��ely definujeme takto:
% seznam znak� reprezentuje libovoln� znak tohoto seznamu
% *(RV) reprezentuje v�echna slova vznikl� z�et�zen�m libovoln�ho po�tu slov reprezentovan�ch regul�rn�m v�razem RV v�etn� pr�zdn�ho slova
% RV1 + RV2 reprezentuje sjednocen� mno�in reprezentovan�ch RV1 a RV2
% RV1 : RV2 reprezentuje mno�inu slov, kter� vznikla jako z�et�zen� slova z RV1 se slovem z RV2

% Znalci Automat� a gramatik �i UNIXu si mohou uvedenou definici samoz�ejm� libovoln� roz���it, prvn� argument Text tak� m��e b�t reprezentov�n vhodn�ji ne�li seznamem znak� apod.