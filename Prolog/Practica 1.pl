% Ignacio de la Cruz Crespo

% 1.1

% por_encima_de(X,c). -> X = d
% por_encima_de(c,X). -> X = b
% por_arriba(b,X). -> X = [c, d]
% por_arriba(X,Y). -> X = d, Y = [] 
% poner_encima(X,f). -> false

% 1.2

% por_encima_de(X,Y), cima(Y). -> false
% cima(Y), pila_izquierda(X,Y), cima(X). -> Y = g, X = d 
% por_arriba(a,X), member(Y,X), por_encima_de(Z,Y). -> X = [b, c, d], Y = b, Z = c 

% 1.3
mas_por_encima_que(X,false) :- false.
mas_por_encima_que(false,Y) :- true.
mas_por_encima_que(true,Y) :- true.
mas_por_encima_que(X,Y) :- not(sobre(X,Z)),sobre(Y,W),mas_por_encima_que(Z,W).


% 2 

mezcla([], L2, L2).
mezcla(L1, [], L2).  
mezcla([X|L1], [Y|L2], [X,Y|L]) :- mezcla(L1, L2, L).

% 3.1

sublista([], _).
sublista(L1,L2) :- append([_,L1,_],L2).

% 3.2

contenida([],_) :- true.
contenida([X|L1], [X|L2]) :- contenida(L1,L2).
contenida([X|L1], L2) :- contenida(L1,L2).

% 4

contarNodos(nil,0).
contarNodos(arbol(_,I,D),C) :- contarNodos(I,C1), contarNodos(D,C2), C is C1+C2+1 .