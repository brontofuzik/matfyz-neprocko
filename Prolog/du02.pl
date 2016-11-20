%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%          ARITMETIKA         %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Definice prirozeneho cisla

% nat(X) :- X je prirozene cislo.

nat(0).
nat(s(X)) :- nat(X).

% le(X,Y):-  X a Y jsou prirozena cisla takova, ze X je mensi nebo rovno Y

le(0,X) :- nat(X).
le(s(X),s(Y)) :- le(X,Y).

% lt(X,Y):-  X a Y jsou prirozena cisla takova, ze X je ostre mensi nez Y

lt(0,s(X)) :- nat(X).
lt(s(X),s(Y)) :- lt(X,Y).

% dotazy typu
% lt(+X,+Y)
% lt(+X,-Y)
% lt(-X,+Y)
% lt(-X,-Y)
% Problem: Je mozne definovat lt/2 tak, aby volani lt(-X,-Y) vratilo po konecnem 
% poctu kroku libovolnou dvojici prirozenych cisel n <= m ?

% soucet(+X,+Y,?Z) :- X,Y,Z jsou prirozena cisla takova, ze Z je souctem X a Y.

soucet(0,X,X) :- nat(X).
soucet(s(X),Y,s(Z)) :- soucet(X,Y,Z).

% soucin(+X,+Y,?Z) :- X,Y,Z jsou prirozena cisla takova, ze Z je soucinem X a Y.
% exp(+X,+N,?Y) :- X,N,Y jsou prirozena cisla takova, ze Y je N-tou mocninou X.
% mod(+X,+Y,?Z) :- X,Y,Z jsou prirozena cisla takova, ze Z je zbytek
% po celociselnem deleni cisla X cilem Y.

soucin(X,0,0) :- nat(X).
soucin(0,Y,0) :- nat(Y).
soucin(s(X),Y,S) :- soucin(X,Y,Z), soucet(Z,Y,S).

exp(s(X),0,s(0)) :- nat(X).
exp(X,s(0),X) :- nat(X).
exp(X,s(N),Y) :- exp(X,N,E),soucin(E,X,Y).

mod(X,s(0),0) :- nat(X).
mod(s(X),s(X),0) :- nat(X).
mod(X,Y,X) :- lt(X,Y).
mod(X,Y,Z) :- lt(Z,Y),soucet(H,Y,X),mod(H,Y,Z).

% Aky zmysel maju dotazy typu:
% ?- soucet(atom1,atom2,Premenna). Premenna := atom1 + atom2 
% ?- soucet(atom1,Premenna,atom2). Premenna := atom2 - atom1
% ?- soucet(Premenna,atom1,atom2). Premenna := atom2 - atom1
% ?- soucet(Premenna1,Premenna2,atom). Premenna1 := atom - Premenna2
