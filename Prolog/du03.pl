% Su dane predikaty muz/1, zena/1 a rodina(Otec,Matka,Deti), kde Deti je zoznam deti od najstarsieho k najmladsiemu.
% Definujte predikaty starsibratr/2(Kdo,Koho), nejstarsibratr/2(Kdo,Koho) a bratri/2(Koho,Bratri)

muz(ivan).
muz(lukas).
muz(jakub).
muz(pali).

zena(libusa).
zena(katka).
zena(lucka).
zena(simonka).
zena(saska).

rodina(ivan,libusa,[lukas,simonka,pali,jakub,katka,lucka,saska]).

% prvok(X,S) :- X je prvkom zoznamu S.

prvok(X,[X|_]).			% X je hlavou S => X je prvkom S
prvok(X,[_|T]) :- prvok(X,T). 	% X nie je hlavou S => (X je prvkom T => X je prvkom S)

% del_zeny_okrem_koho(Koho,S,T):- Zo zoznamu S vyradi vsetky zeny okrem Koho; vytvori tak zoznam T.

del_zeny_okrem_koho(Koho,[X|T],T1) :- zena(X),					% hlava je zena a zaroven
				      Koho\=X,					% hlava nie je Koho
				      del_zeny_okrem_koho(Koho,T,T1).		% => do T1 (ktore spracujeme rekurzivne) ju nezarad
del_zeny_okrem_koho(Koho,[H|T],[H|T1]) :- (muz(H);Koho=H),			% hlava je Koho alebo nie je zena (=> je muz)
					  del_zeny_okrem_koho(Koho,T,T1).	% odpreparovanie hlavy a jej spatne prilepenie
del_zeny_okrem_koho(_,[],[]).							% predpoklady: 1. kazda osoba ma urcene pohlavie pomocou predikatov muz/1 resp. zena/1
                                                        			%              2. muz(H) => !zena(H)

% del_koho(Koho,S,T) :- Zo zonamu S vyradi Koho; vytvori tak zoznam T.

del_koho(X,[X|T],T).
del_koho(X,[H|T],[H|T1]) :- X\=H,
                            del_koho(X,T,T1).
del_koho(_,[],[]).

% bratia(Koho,Bratia) :- Bratia je zoznam bratov Koho (usporiadanych od najstarsieho po najmladsieho)
	
bratia(Koho,Bratia) :- rodina(_,_,S), 			% najde rodinu Koho
                       prvok(Koho,S),			% najde surodencov Koho 		
		       del_zeny_okrem_koho(Koho,S,T),	% vypusti vsetky sestry (okrem Koho) zo zoznamu S
		       del_koho(Koho,T,Bratia).		% vypusti Koho zo zoznamu

% najstarsibrat(Kto, Koho) :- Kto je najstarsim bratom Koho.                      

najstarsibrat(Kto,Koho) :- bratia(Koho,[Kto|_]).

% spracuj(Koho,T,U) :- Prvky T v intervale [Hlava,Koho) zaradi do zoznamu U.

spracuj(Koho,[Koho|_],[]).
spracuj(Koho,[H|T],[H,T1]) :- Koho\=H,spracuj(Koho,T,T1).

% starsibrat(Kto,Koho) :- Kto je starsim bratom Koho.

starsibrat(Kto,Koho) :- rodina(_,_,S),			% najde rodinu Koho
                        prvok(Koho,S),			% najde surodencov Koho
			del_zeny_okrem_koho(Koho,S,T),	% vypusti vstky sestry (okrem Koho) zo zoznamu S
			spracuj(Koho,T,Kto).  	        % do zoznamu Kto zaradi vsetkych starsich bratov		

% Predpokladajme, ze je definovany predikat modry/2(+X,-H), kde pre modre prvky plati modry(X,1) a pre ostatne modry(X,0).

% Bez pouzitia aritmetiky (tj. bez operatoru is) definujte predikaty pul/3 a tret/4
% pul(+S,-S1,-S2) :- zoznam S je zretazenim zoznamov S1 a S2, pricom pocet modrych prvkov v S1 a v S2 sa lisi o 1
% tret(+S,-T1,-T2,-T3) :- zoznam S je zretazenim zeoznamov T1,T2,T3, pricom pocet modrych prvkov v lubovolnych dvoch zo zoznamov
% T1,T2 a T3 sa lisi nanajvys o 1

