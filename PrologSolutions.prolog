% 1) Odd Multiple of 3
oddMultOf3(N) :-
   	not(integer(N)) ->  write("ERROR:  The given parameter is not an integer"), fail;
    1 is N mod 2,
    0 is N mod 3.


% 2) List Product
list_prod([], 0).
list_prod([X], X).
list_prod([N1|N2], Product) :-
    list_prod(N2, Rest), Product is Rest * N1.


% 3) Palindrome
palindrome(X) :- reverse(X, X).


% 4) Second Minimum
secondMin(L, M2) :- 
    (ground(L)
    ->   (is_list(L),
             maplist(number, L)
         ->  (sort(L, [_,S|_])
             ->  M2 = S
             ;   throw('ERROR: List has fewer than 2 uniqe elements.')
             )
         ;   throw('ERROR: "element" is not a number.')
         )
    ;   throw(error(error, _))
    ).


% 5) Classify
classify([], [], []).
classify([A|T], [A|X], Odd) :-
    0 is A mod 2, !, classify(T, X, Odd).
classify([A|T], Even, [A|Y]) :- classify(T, Even, Y).


% 6) Bookends
prefix([], _).
prefix([X|Y], [X|Z]) :- prefix(Y, Z).
suffix(X, Y) :- reverse(X, XR), reverse(Y, YR), prefix(XR, YR).

bookends(X, Y, Z) :- prefix(X, Z), suffix(Y, Z).


% 7) Subslice
subslice([], _).
subslice(Slice, [H|T]) :- prefix(Slice, [H|T]), 
    !; subslice(Slice, T).

% 8) Shift
leftrotate([H|T], L) :- append(T, [H], L).
shift(0, L, L) :- !.
shift(L1, N, L2) :-
    write(L1), N > 0, leftrotate(L1, L), N1 is N-1, shift(L, N1, L2).

% 9) Luhn Algorithm
% In the case that you attempt to add 0
nSum(0, S, R) :-
    R is S, !.

nSum(N, S, R) :-
    Remainder is mod(N, 100),
    Q is div(N, 100),
    altSum(Remainder, S2),
    NewSum is S + S2,
    nSum(Q, NewSum, R), !.

% Calculates sum of two digit number
sum(N, R) :-
    N1 is mod(N, 10),
    N2 is div(N, 10),
    R is N1 + N2, !.

% Calculate sum of two digit number, but accounts for remainder
altSum(N, R) :-
    N1 is mod(N, 10),
    N2 is div(N, 10),
    N3 is N2 * 2,
    N3 > 9 ->  sum(N3, X), R is X + N1, !;
    N1 is mod(N, 10),
    N2 is div(N, 10),
    N3 is N2 * 2,
    R is N1 + N3, !.

% Call to other facts to determine if the card number is correct
luhn(N) :-
    nSum(N, 0, R),
    Remainder is mod(R, 10),
    Remainder = 0.


% 10) Graph
edge(a,b).
edge(b,c).
edge(c,d).
edge(d,a).
edge(d,e).
edge(b,a). 

path(Node1, Node2) :-
    edge(Node1, Node2), 
    !.

path(Node1, Node2) :-
    edge(Node1, Z),
    path(Z, Node2), 
    !.

cycle(Node) :-
    path(Node, Node), 
    !.