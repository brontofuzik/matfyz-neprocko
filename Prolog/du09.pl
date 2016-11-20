%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Definujte predik·t most/2: %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% most(+Graf,?Hrana) 
% Vr·tÌ postupnÏ vöechny hrany, kterÈ jsou mosty danÈho (neorientovanÈho) Grafu (bez smyËek a n·sobn˝ch hran). 

% Hrana {x,y} je mostem grafu G, pokud kaûd· cesta v G mezi vrcholy x a y vede p¯es hranu {x,y}. 

% Pro zajiötÏnÌ nez·vislosti na konkrÈtnÌ reprezentaci grafu p¯edpokl·dejte, ûe je definov·n predik·t hrana/3: 
% hrana(X,Y,G):- vrcholy X a Y jsou v grafu G spojeny hranou. 

% a       b
% |\     /|
% | c - d |
% |/     \|
% e       f 

% Dve reprezentacie neorientovaneho, neohodnoteneho grafu G=(V,E), kde V je mnozina vrcholov a E je mnozina hran
% a ich prislusne pristupne procedury hrana/3 a susedia/3:

%%%%% A. Dva samostatne zoznamy: vrcholy a hrany %%%%%
graf_1a( g( [a,b,c,d,e,f], [e(a,c), e(a,e), e(c,e), e(b,d), e(b,f), e(d,f), e(c,d)])).

% hrana_1a/3(+Vrchol1,+Vrchol2,+Graf) :- Neorientovany, neohodnoteny graf v repr. A.
hrana_1a(V1,V2,g(Vs,Es)) :-
	member(e(V1,V2),Es);
	member(e(V2,V1),Es).
	
% susedia_1a/3(+Vrchol,+Graf,-Susedia) :- Neorientovany, neohodnoteny graf v repr. A.	
susedia_1a(V,G,Susedia) :-
	setof(S,hrana_1a(V,S,G),Susedia).

%%%%% B. Zoznam usporiadanych dvojic: vrchol a vrcholy s nim susediace %%%%%
graf_1b( [a-[c, e], b-[d, f], c-[a, d, e], d-[b, c, f], e-[a, c], f-[b, d]]).

% hrana_1b/3(+Vrchol1,+Vrchol2,+Graf) :- Neorientovany, neohodnoteny graf v repr. B.
hrana_1b(V1,V2,G) :-
	member(V1-Susedia,G),
	member(V2,Susedia).

% susedia_1b/3(+Vrchol,+Graf,-Susedia) :- Neorientovany, neohodnoteny graf v repr. B.
susedia_1b(V,G,Susedia) :-
	member(V-Susedia,G).	

% Riesenie predpoklada, ze uzivatel pouziva repr. A, v opacnom pripade je nutne nahradit vyskyty procedury "hrana_1a/3" za "hrana_1b/3".
% Toto by bolo mozne realizovat aj dynamickym rozpoznavanim aktualnej reprezentacie grafu.	
		
% cesta(Vrchol1,Vrchol2,Graf) :- Uspeje, ak existuje v Grafe cesta z Vrcholu1 do Vrcholu2.
cesta(X,Y,G) :-
	cesta(X,Y,G,[X]).		% interface
cesta(X,X,G,Navstivene) :-
	Navstivene=[_,_,_|_], 	% zoznam navstivenych vrcholov obsahuje aspon 3 vrcholy => najdena cesta X-Y nevedie po hrane X-Y (moste)
	!.						% nebacktrackujeme
cesta(X,Z,G,Navstivene) :-
	hrana_1a(X,Y,G),
	\+ member(Y,Navstivene),
	cesta(Y,Z,G,[Y|Navstivene]).

% most(+Graf,?Hrana) :- Vrati postupne vsetky hrany, ktore su mosty daneho (neorientovaneho) Grafu (bez smyciek a nasobnych hran).
most(G,X-Y) :-
	hrana_1a(X,Y,G),
	\+cesta(X,Y,G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. Definujte predik·t nejcesta/3: %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% nejcesta(Start,Cil,Cesta,Delka) :- Cesta je nejlevnÏjöÌ cesta v danÈm hranovÏ nez·pornÏ ohodnocenÈm orientovanÈm grafu z vrcholu Start do vrcholu Cil ceny Delka. 

% P¯epokl·dejme, ûe p¯Ìstup ke grafu zajiöùuje predik·t hrana3: 
% hrana(X,Y,C) :- z vrcholu X do vrcholu Y vede hrana s ohodnocenÌm C

% Dve reprezentacie orientovaneho, ohodnoteneho grafu G=(V,E), kde V je mnozina vrcholov a E je mnozina hran
% a ich prislusne pristupne procedury hrana/3 a susedia/3:

%%%%% A. Dva samostatne zoznamy: vrcholy a hrany s vahami %%%%%
graf_2a( g( [a,b,c,d,e,f], [e(a,c,3), e(c,a,3), e(a,e,1), e(e,a,1), e(c,e,1), e(e,c,1), e(b,d,3), e(d,b,3), e(b,f,1), e(f,b,1), e(d,f,1), e(f,d,1), e(c,d,2), e(d,c,2)])). 

% hrana_2a/3(+Vrchol1,+Vrchol2,+Vaha,+Graf) :- Orientovany, ohodnoteny graf v repr. A.
hrana_2a(V1,V2,Vaha,g(Vs,Es)) :-
	member(e(V1,V2,Vaha),Es).
	
% susedia_2a/3(+Vrchol,+Graf,-Susedia) :- Orientovany, Ohodnoteny graf v repr. A.	
susedia_2a(V,G,Susedia) :-
	setof(V2-Vaha,hrana_2a(V,V2,Vaha,G),Susedia).

%%%%% B. Zoznam usporiadanych dvojic: vrchol a zoznam usporiadanych dvojic: vrchol s nim susediaci a vaha hrany spajajucej tieto dva vrcholy %%%%%
graf_2b( [a-[c-3,e-1], b-[d-3,f-1], c-[a-3,d-2,e-1], d-[b-3,c-2,f-1], e-[a-1,c-1], f-[b-1,d-1], g-[]]).

% hrana_2b/3(+Vrchol1,+Vrchol2,+Vaha,+Graf) :- Orientovany, ohodnoteny graf v repr. B.
hrana_2b(V1,V2,Vaha,G) :-
	member(V1-Susedia,G),
	member(V2-Vaha,Susedia).
	
% susedia_2b/3(+Vrchol,+Graf,-Susedia) :- Neorientovany, neohodnoteny graf v repr. B.
susedia_2b(V,G,Susedia) :-
	member(V-Susedia,G).
	
% Riesenie predpoklada, ze uzivatel pouziva repr. B, v opacnom pripade je nutne nahradit vyskyty procedury "susedia_2b/3" za "susedia_2a/3".
% Toto by bolo mozne realizovat aj dynamickym rozpoznavanim aktualnej reprezentacie grafu.	
	
% najcesta(+Graf,+Start,+Ciel,-Cesta,-Dlzka)
najcesta(Graf,Start,Ciel,Cesta,Dlzka) :-
	dijkstrov_alg(Graf,[Start-0-u],[],MD),
	vrat_dlzku(Ciel,MD,Dlzka),
	vrat_cestu(Start,Ciel,MD,Cesta).

% dijkstrov_alg(+Graf,+OtvoreneVrcholy,+ZatvoreneVrcholy,-MinimialnaVzdialenost)	
dijkstrov_alg(G,[],MD,MD).
dijkstrov_alg(G,O,Z,MD) :-
	vyber_min(O,V-D-P,O1),	% V-D-P je vrchol s min. vzdialenostou od startu D a predchodcom P na min. ceste.
	susedia_2b(V,G,S),		% S je zoznam susedov prvku V v grafe G
	rozdiel(S,Z,S1),		% S1 su nezatvoreni (=otvoreni) susedia, moze byt aj []
	zlej(O1,S1,V,D,O2),		% zleje stare otvorene vrcholy O1 a otvorenych susedov S1 do zoznamu vsetkych otvorenych vrcholov O2
	dijkstrov_alg(G,O2,[V-D-P|Z],MD).
	
% vyber_min/3(+T,-Min,-T1) :- Zo zoznamu T vyberie vrchol s minimalnou vzdialenostou od pociatku Min a vysledny zoznam vrati ako T1.	
vyber_min([H|T],Min,T1) :-
	vyber_min(T,H,Min,T1).	% interface
vyber_min([],Min,Min,[]).
vyber_min([H|T],M,Min,[H1|T1]) :-
	H=_-D1-_,
	M=_-D2-_,
	(D1<D2 -> N=H,H1=M;
	          N=M,H1=H),
	vyber_min(T,N,Min,T1).
	
% rozdiel/3(+S,+Z,-S1) :- S1 := S - Z.
rozdiel([],_,[]).
rozdiel([H|T],Z,S1) :-
	H=V-_,
	(member(V-_-_,Z) -> S1=T1 ; S1=[H-u|T1]),	% 'u' symbolizuje nedefinovaneho predchodcu
	rozdiel(T,Z,T1).

% zlej/3(+O1,+S1,+V,+D,-O2)	
zlej(O1,[],_,_,O1).
zlej(O1,[V1-D1-_|T],V,D,O2) :-
	(odstran(O1,V1-D2-_,OZ) -> (DT is D+D1, DT<D2 -> DN=DT, PN=V ; DN=D2) ; OZ=O1, DN is D+D1, PN=V),
	zlej(OZ,T,V,D,OS),
	O2=[V1-DN-PN|OS].

% odstran(+T,+H,-T1) :- Zo zoznamu T odstrani prvok H, vysledok vrati ako zoznam T1.
odstran([H|T],H,T).
odstran([H|T],X,[H|T1]) :-
	H\=X,
	odstran(T,X,T1).

% vrat_dlzku(+Ciel,+MD,-Dlzka) :- Vrati dlzku najkratsej cesty do Ciela.	
vrat_dlzku(Ciel,[Ciel-Dlzka-_|_],Dlzka) :-
	!.										% nebacktrackujeme
vrat_dlzku(Ciel,[V1-_-_|T],Dlzka) :-
	Ciel\=V1,
	vrat_dlzku(Ciel,T,Dlzka).
	
% vrat_cestu(+Start,+Ciel,+MD,-Cesta) :- Vrati najkratsiu cestu zo Startu do Ciela podla spocitanej MD.
vrat_cestu(Start,Ciel,MD,Cesta) :-
	vrat_cestu(Start,Ciel,MD,[Ciel],Cesta).	% interface
	
vrat_cestu(Start,Start,_,Cesta,Cesta) :-
	!.
vrat_cestu(Start,Ciel,MD,Aku,Cesta) :-
	Start\=Ciel,
	najdi_ciel(Ciel-_-P,MD),
	vrat_cestu(Start,P,MD,[P|Aku],Cesta).
	
najdi_ciel(Ciel-_-P,[Ciel-_-P|_]) :-
	!.
najdi_ciel(Ciel-_-P,[V1-_-_|T]) :-
	Ciel\=V1,
	najdi_ciel(Ciel-_-P,T).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% On-line Guide to Prolog Programming by Roman Bartak %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% min_dist(+Graph,+Start,-MinDist)
min_dist(Graph,Start,MinDist) :-
	dijkstra(Graph,[],[Start-0],MinDist).
   
% dijkstra(+Graph,+ClosedVertices,+OpenVertices,-Distances)
dijkstra(_,MinDist,[],MinDist).
dijkstra(Graph,Closed,Open,MinDist) :-
	choose_v(Open,V-D,RestOpen),
	susedia_2b(V,Graph,NB),  % NB is a list of adjacent vertices+distance to V
	diff(NB,Closed,NewNB),
	merge(NewNB,RestOpen,D,NewOpen),
	dijkstra(Graph,[V-D|Closed],NewOpen,MinDist).
   
% choose_v(+OpenVertices,-VertexToExpand,-RestOpenVertices)
choose_v([H|T],MinV,Rest) :-
	choose_minv(T,H,MinV,Rest).			% H je vrchol so zatial najmnesou najdenou vzdialenostou od startu
choose_minv([],MinV,MinV,[]).			% presli sme cely zoznam => aktualne minimum je celkove minimum
choose_minv([H|T],M,MinV,[H2|Rest]) :-	
	H=V1-D1, M=V-D,
	(D1<D -> NextM=H,H2=M; NextM=M,H2=H),
	choose_minv(T,NextM,MinV,Rest).
   
% diff(+ListOfVertices,+Closed,-ListOfNonClosedVertices)
diff([],_,[]).
diff([H|T],Closed,L) :-
	H=V-D,
	(member(V-_,Closed) -> L=NewT ; L=[H|NewT]),
	diff(T,Closed,NewT).
   
% merge(+ListOfVertices,+OldOpenVertices,-AllOpenVertices)
merge([],L,_,L).
merge([V1-D1|T],Open,D,NewOpen) :-
	(remove(Open,V1-D2,RestOpen) -> VD is min(D2,D+D1) ; RestOpen=Open,VD is D+D1),
	NewOpen=[V1-VD|SubOpen],
	merge(T,RestOpen,D,SubOpen).
   
remove([H|T],H,T).
remove([H|T],X,[H|NT]) :-
	H\=X,
	remove(T,X,NT).

% OdladÏnÈ ¯eöenÌ jednoho z problÈm˘ odeslat e-mailem nejpozdÏji do pondÏlÌ 27.11.