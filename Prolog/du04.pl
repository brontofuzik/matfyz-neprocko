% 1. Definujte predikat rozvin/2, ktory prevedie ciselny zoznam na uplny
%    a) nahradou prvkov tvaru a..b postupnostou a,a+1,...b ak je a<=b
%    b) ci a,a-1,...,b ak je a>b

% rozvin(+StrcucnyZoznam,-UplnyZoznam)
% ?- rozvin([10,1..4,13,14,8..6,5..5],X).
% X=[10,1,2,3,4,13,14,8,7,6,5] 

% Operator .. mozete definovat pomocou op/3 (napr. :-op(200,xfx,..).)
% ci nahradit lubovolnym inym binarnym operatorom

% Deklaracia

:- op(200,xfx,..).

% Definicia: interval(+A..+B,-Zoznam).

interval(A..A,[A]).
interval(A..B,Zoznam) :- A<B,A1 is A+1,interval(A1..B,Z),append([A],Z,Zoznam).
interval(A..B,Zoznam) :- A>B,A1 is A-1,interval(A1..B,Z),append([A],Z,Zoznam).

% Riesenie 1

rozvin([],[]).
rozvin([H|T],[H|T1]) :- number(H),rozvin(T,T1).
rozvin([A..B|T],UplnyZoznam) :- interval(A..B,Z),rozvin(T,T1),append(Z,T1,UplnyZoznam).

% Riesenie 2: Cast prace je presunuta z procedury rozvin do procedury interval.

interval2(A,[A]) :- number(A).
interval2(A..A,[A]).
interval2(A..B,Zoznam) :- A<B,A1 is A+1,interval2(A1..B,Z),append([A],Z,Zoznam).
interval2(A..B,Zoznam) :- A>B,A1 is A-1,interval2(A1..B,Z),append([A],Z,Zoznam). 

rozvin2([],[]).
rozvin2([H|T],UplnyZoznam) :- interval2(H,Z),rozvin2(T,T1),append(Z,T1,UplnyZoznam).

% 2. Na rozmyslenie: Co pocita predikat zahada/3?

zahada(_,0,[]).
zahada(Seznam,K,[H|T1]) :- K>0, K1 is K-1, append(_,[H|T],Seznam), zahada(T,K1,T1).

% Z rastucej (neklesajucej) postupnosti Seznam postupne generuje vsetky 
% (neklesajuce) postupnosti dlzky K.