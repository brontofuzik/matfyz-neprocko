% 1. Klasick� probl�m t�� mision��� a t�� lidojed� lze formulovat takto: Lze p�epravit 3 mision��e a 3 lidojedy z jednoho b�ehu �eky na druh�, ani� by na ��dn�m b�ehu v ��dn� chv�li
% nebylo v�ce lidojed� ne�li mision���? Pak jsou toti� mision��i bez milosti sn�zeni. K dipozici je jedna lo�ka pro dv� osoby. 

% Navrhn�te �e�en� (v jazyce Prolog) t�to �lohy v p��pad�, kdy se na vstupu zad�v� po�et m mision���, l lidojed�, r �ek. Na bli���m b�ehu ka�d� �eky je lo�ka pro 2 osoby.
% C�lem je ur�it, zda pro dan� vstup existuje �e�en� (p��padn� nal�zt nejkrat��).

% Reprezentacia stavu:
% Zoznam stavov na jednotlivych brehoch B = [b(M1,L1,K1), b(M2,L2,K2), ..., b(Mn,Ln,Kn)],
% kde Mi,Li a Ki su pocty misionarov, ludozrutov resp. lodiciek na brehu i.

%%%%% generuj_zac(+M,+L,+R,-Z) :- Vygeneruje zaciatocny stav pre M misionarov, L ludozrutov a R riek, vysledok vrati ako zoznam Z.
generuj_zac(M,L,R,[b(M,L,1)|T]) :-
	generuj_zac1(R,T).
generuj_zac1(1,[b(0,0,0)]).
generuj_zac1(R,[b(0,0,1)|T]) :-
	R > 1,
	R1 is R - 1,
	generuj_zac1(R1,T).
	
%%%%% generuj_kon(+M,+L,+R,-K) :- Vygeneruje koncovy stav pre M misionarov, L ludozrutov a R riek, vysledok vrati ako zoznam K.
generuj_kon(M,L,R,[b(0,0,0)|T]) :-
	generuj_kon1(M,L,R,T).
generuj_kon1(M,L,1,[b(M,L,1)]).
generuj_kon1(M,L,R,[b(0,0,1)|T]) :-
	R > 1,
	R1 is R - 1,
	generuj_kon1(M,L,R1,T).

%%%%% prevoz(S1,S2) :- Prevoz medzi stavom S1 a S2.
%%%%% Prevoz nastane vtedy, ak sa medzi lubovolnymi dvoma susednymi brehmi presunie bude jeden alebo dvaja ludia v lodke.
prevoz([H1,I1|T1],[H2,I2|T2]) :-
	(
		prevoz1(H1,I1,H2,I2),	% prevoz sa tyka aktualneho brehu - uskutocnil sa medzi brehmi H a I
		T2=T1
	);		
	(
		H1=H2,					% prevoz sa netyka aktualneho brehu
		T1\=[],					% zostavaju este aspon dva brehy, medzi ktorymi sa mohol uskutocnit prevoz
		prevoz([I1|T1],[I2|T2])	% preskumame tento prevoz
	).
prevoz1(b(M1,L1,K1),b(M2,L2,K2),b(M3,L3,K3),b(M4,L4,K4)) :-
	(
		(
			K1 > 0,
			K3 is K1 - 1,
			K4 is K2 + 1,
			M1 > 0,
			M3 is M1 - 1,
			M4 is M2 + 1,
			L3=L1,
			L4=L2
		);
		(
			K2 > 0,
			K3 is K1 + 1,
			K4 is K2 - 1,
			M2 > 0,
			M3 is M1 + 1,
			M4 is M2 - 1,
			L3=L1,
			L4=L2
		)
	);	% previezol sa jeden misionar
	(
		(
			K1 > 0,
			K3 is K1 - 1,
			K4 is K2 + 1,
			M3=M1,
			M4=M2,
			L1 > 0,
			L3 is L1 - 1,
			L4 is L2 + 1
		);
		(
			K2 > 0,
			K3 is K1 + 1,
			K4 is K2 - 1,
			M3=M1,
			M4=M2,
			L2 > 0,
			L3 is L1 + 1,
			L4 is L2 - 1
		)
	);	% previezol sa jeden ludozrut
	(
		(
			K1 > 0,
			K3 is K1 - 1,
			K4 is K2 + 1,
			M1 > 1,
			M3 is M1 - 2,
			M4 is M2 + 2,
			L3=L1,
			L4=L2
		);
		(
			K2 > 0,
			K3 is K1 + 1,
			K4 is K2 - 1,
			M2 > 1,
			M3 is M1 + 2,
			M4 is M2 - 2,
			L3=L1,
			L4=L2
		)
	);	% previezli sa dvaja misionari
	(
		(
			K1 > 0,
			K3 is K1 - 1,
			K4 is K2 + 1,
			M3=M1,
			M4=M2,
			L1 > 1,
			L3 is L1 - 2,
			L4 is L2 + 2
		);
		(
			K2 > 0,
			K3 is K1 + 1,
			K4 is K2 - 1,
			M3=M1,
			M4=M2,
			L2 > 1,
			L3 is L1 + 2,
			L4 is L2 - 2
		)
	);	% previezli sa dvaja ludozruti
	(
		(
			K1 > 0,
			K3 is K1 - 1,
			K4 is K2 + 1,
			M1 > 0,
			L1 > 0,
			M3 is M1 - 1,
			M4 is M2 + 1,
			L3 is L1 - 1,
			L4 is L2 + 1
		);
		(
			K2 > 0,
			K3 is K1 + 1,
			K4 is K2 - 1,
			M2 > 0,
			L2 > 0,
			M3 is M1 + 1,
			M4 is M2 - 1,
			L3 is L1 + 1,
			L4 is L2 - 1
		)
	).	% previezol sa jeden misionar a jeden ludozrut
		
%%%%% bezpecny(+S) :- Vyhodnoti bezpecnost stavu S.
bezpecny([]).		% prazdny zoznam je bezpecny
bezpecny([H|T]) :-	% zoznam je "bezpecny" ak
	bezpecny1(H),	% je bezpecna jeho hlava
	bezpecny(T).	% aj telo
bezpecny1(b(M,L,_)) :-
	M==0;
	(
		M=\=0,
		M >= L
	).
	
%%%%% dalsi(+S1,+S2) :- Zo stavu S1 je mozne prejst do (bezpecneho) stavu S2.
dalsi(S1,S2) :-
	prevoz(S1,S2),
	bezpecny(S2).

%%%%% hrana(+X,+Y) :- Interface pre grafovy algoritmus nejcesta (BFS).	
hrana(X,Y) :-
	dalsi(X,Y).
	
%%%%% nejcesta(Start,Cil,Cesta) :- najde nejkratsi cestu z vrcholu Start do vrcholu Cil
nejcesta(Start,Cil,Cesta) :-
	nejcesta1([[Start]],Cil,C),
	reverse(C,Cesta).
nejcesta1([X|_],Cil,X):- X=[Cil|_].
nejcesta1([[H|T]|Zbytek],Cil,Cesta):- 	
	findall([Z,H|T],(hrana(H,Z),\+member(Z,[H|T])),NoveCesty),
	append(Zbytek,NoveCesty,NovySeznam),!,
	nejcesta1(NovySeznam,Cil,Cesta).

%%%%% vypis() :- 
vypis([]) :-
	!.
vypis([H|T]) :-
	write(H),
	nl,
	vypis(T).
	
%%%%% uloha(+M,+L,+R,-C1) :- Riesi ulohu, vzsledok vracia ako zoznam vsetkych najkratsich ciest (postupnosti stavov) C1.	
uloha(M,L,R,C1) :-
	generuj_zac(M,L,R,Z),
	generuj_kon(M,L,R,K),
	bagof(C,nejcesta(Z,K,C),C1),
	tell('kudela.txt'),
	vypis(C1),
	told.

% Odlad�n� �e�en� probl�mu �.1 odeslat e-mailem nejpozd�ji do ned�le 9.12.
