% IGNACIO DE LA CRUZ CRESPO 

% 1.1 Usando igualdad sintactica:
elimina1([ ],X,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).

% elimina1([a,b,a,c],a,L). L = [b, c] .
% elimina1([a,b,a,c],X,L). L = [a, b, a, c] .

% 1.2 Usando unificacion:
elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).

% elimina2([a,b,a,c],a,L). L = [b, c] .
% elimina2([a,b,a,c],X,L). X = a, L = [b, c] .


% 1.3 Combinando las dos anteriores:
elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

% elimina3([a,b,a,c],a,L). L = [b, c] .
% elimina3([a,b,a,c],X,L). X = a, L = [b, c]

% Usando igualdad sintactica no se tienen en cuenta las repeticiones del elemento a eliminar,
% cuando usamos unificacion si, por eso 1.2 y 1.3 dan el mismo resultado

% 2.1 
tree1(tree(1,
            tree(2,
                tree(3,nil,nil),
                tree(4,nil,nil)),
            tree(5,
                tree(6,nil,nil),
                tree(7,nil,nil))
        )
    ).

sumatree(nil,0).
sumatree(arbol(X,I,D),N) :- sumatree(I,N1), sumatree(D,N2), N is N1+N2+X .

%2.2
maximo(nil,0).
maximo(arbol(Y,I,D),X) :- maximo(I,N1), maximo(D,N2), max(N1,N2,N),max(N,Y,Z), X is Z.
max(X,Y,Z):- (X>=Y->Z=X;Z=Y).

%3
sublistas([],[]).
sublistas([X|Y],[X|Z]):- sublist(Y,Z).
sublistas([_|Y],Z):-  sublist(Y,Z).

%4

move(1, A, _, C, [A, C]).
move(N, A, B, C, M) :-
    X is (N - 1),
    move(X, A, C, B, M1),
    move(1, A, B, C, M2),
    move(X, B, A, C, M3),
    append(M1, M2, U),
    append(U, M3, M).
