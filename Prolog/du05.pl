% 1. Definujte predikat zplostenieZoznamu/2, ktory vrati zoznam "listov" zadaneho vstupneho zoznamu.

% zplostenieZoznamu(+Zoznam,-ListyZoznamu).
% ?- zplostenieZoznamu([a,[b,c],[[d]],[],[[[e]],f],L).
% L=[a,b,c,d,e,f].

% Uvedomte si, ze listy zoznamu nemusia byt nutne atomy, mozu to byt i zlozitejsie termy, napr.

% flatten([0,[s(0),s(s(0))],[s(s(s(0)))]],L).
% L=[0,s(0),s(s(0)),s(s(s(0)))].

% flatten(+Zoznam,-ListyZoznamu) :- ListyZoznamu je zoznam listov zoznamu Zoznam.

% is_list(+L) :- Predikat je pravdivy prave vtedy, ked L je zoznam.
is_list(L) :- 
	L=[];					
	L=[_|_].
	
% is_not_list(+L) :- Predikat je pravdivy prave vtedy, ked L nie je zoznam.
is_not_list(L) :-
	L\=[],
	L\=[_|_].

flatten([],[]).
flatten([H|T],L) :- 
	(is_list(H),flatten(H,H1)),
	(is_list(T),flatten(T,T1)),
	append(H1,T1,L).
flatten([H|T],L) :- 
	is_not_list(H),
	(is_list(T),flatten(T,T1)),
	append([H],T1,L).
flatten([H|T],L) :- 
	(is_list(H),flatten(H,H1)),
	is_not_list(T),
	append(H1,[T],L).