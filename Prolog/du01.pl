%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    DATABAZE PRIBUZENSKYCH VZTAHU     %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Nasledujici predikaty popiseme fakty

% muz(Muz)
% zena(Zena)
% rodic(Rodic,Dite)

% fakty

% muz(adam).
% muz(kain).
% zena(eva).
% muz(abel).
% rodic(adam,kain).
% rodic(adam,abel).
% rodic(eva,abel).

% personalizovane fakty

muz(tibor).
muz(tibino).
muz(ivan).
muz(palo).
muz(lukas).
muz(jakub).
muz(pali).

zena(marta).
zena(janka).
zena(hela).
zena(libusa).
zena(katka).
zena(lucka).
zena(simonka).
zena(saska).

rodic(tibor,tibino).
rodic(tibor,ivan).
rodic(tibor,janka).
rodic(marta,tibino).
rodic(marta,ivan).
rodic(marta,janka).

rodic(tibino,katka).
rodic(tibino,lucka).
rodic(hela,katka).
rodic(hela,lucka).

rodic(ivan,lukas).
rodic(ivan,jakub).
rodic(libusa,lukas).
rodic(libusa,jakub).

rodic(palo,pali).
rodic(palo,simonka).
rodic(palo,saska).
rodic(janka,pali).
rodic(janka,simonka).
rodic(janka,saska).

manzele(tibor,marta).
manzele(tibino,hela).
manzele(ivan,libusa).
manzele(palo,janka).

% Nove predikaty definujeme pomoci pravidel

bratr(Bratr,Osoba):- 
  rodic(Rodic,Bratr), 
  rodic(Rodic,Osoba), 
  muz(Bratr), 
  Bratr\=Osoba.

sestra(Sestra,Osoba):- 
  rodic(Rodic,Sestra), 
  rodic(Rodic,Osoba), 
  zena(Sestra), 
  Sestra\=Osoba.

sourozenec(Sourozenec,Osoba):- 
  bratr(Sourozenec,Osoba); 
  sestra(Sourozenec,Osoba).

% Definujte nasledujici predikaty:
% bratranec(Kdo,Ci) :- Kdo je bratranec osoby Ci
% neter(Kdo,Ci) :- Kdo je neteri osoby Ci
% svagr(Kdo,Ci) :- Kdo je svagrem osoby Ci

% bratranec = syn surodenca mojho rodica
bratranec(Bratranec,Osoba):- 
  muz(Bratranec), 
  rodic(Rodic_bratranec,Bratranec), 
  rodic(Rodic_osoba,Osoba), 
  sourozenec(Rodic_bratranec,Rodic_osoba).

% neter = dcera mojho surodenca
neter(Neter,Osoba):- 
  zena(Neter), 
  rodic(Rodic_neter,Neter), 
  sourozenec(Rodic_neter,Osoba).

% svagor = manzel mojej sestry OR brat mojho partnera OR manzel sestry mojho partnera OR surodenec partnera mojho surodenca  
svagr(Svagr,Osoba):- 
  muz(Svagr), 
  ((manzele(Osoba,Manzelka),bratr(Svagr,Manzelka)); 
  (sestra(Sestra,Osoba),manzele(Svagr,Sestra)); 
  (sourozenec(Sourozenec,Osoba),(manzele(Sourozenec,Partner);manzele(Partner,Sourozenec)),bratr(Svagr,Partner));
  ((manzele(Partner,Osoba);manzele(Osoba,Partner)),sourozenec(Sourozenec,Partner),manzele(Svagr,Sourozenec))).

% mechanizmus vyhodnoceni dotazu

% ?- bratr(abel,X).

% problemy s duplicitnimi resenimi