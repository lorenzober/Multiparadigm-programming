alphabet(['A','B','C','D','E','F','G','H','I','J']).
alphabet_power(10).
numeric_series([134, 987, 543, 234, 765, 876, 453, 100, 321,
678]).
min_max(List, Min, Max) :-
min_list(List, Min),
max_list(List, Max).
build_intervals(Min, Max, Power, Intervals) :-
Step is (Max - Min + 1) / Power,
build_intervals_rec(Min, Step, Power, Intervals).
build_intervals_rec(_, _, 0, []) :- !.
build_intervals_rec(Start, Step, N, [[Start, End]|T]) :-
End is Start + Step,
N1 is N - 1,
build_intervals_rec(End, Step, N1, T).
find_interval(Number, [[Low, High]|_], 0) :-
Number >= Low, Number < High, !.
find_interval(Number, [_|Rest], Index) :-
find_interval(Number, Rest, I),
Index is I + 1.
convert_to_letters([], _, _, []).
convert_to_letters([N|Rest], Intervals, Alphabet, [L|Converted])
:-
find_interval(N, Intervals, Index),
nth0(Index, Alphabet, L),
convert_to_letters(Rest, Intervals, Alphabet, Converted).
build_matrix([], []).
build_matrix([_], []).
build_matrix([A,B|T], [[A,B]|Pairs]) :-
build_matrix([B|T], Pairs).
count_transitions(Pairs, Alphabet, Matrix) :-
findall([X,Y,Count],
(member(X, Alphabet), member(Y, Alphabet),
include(=([X,Y]), Pairs, Filtered),
length(Filtered, Count)),
Matrix).
run :-
numeric_series(Nums),
sort(Nums, Sorted),
min_max(Sorted, Min, Max),
alphabet_power(Power),
build_intervals(Min, Max, Power, Intervals),
alphabet(Alpha),
convert_to_letters(Nums, Intervals, Alpha, Letters),
format("Лінгвістичний ряд: ~w~n", [Letters]),
build_matrix(Letters, Pairs),
count_transitions(Pairs, Alpha, Matrix),
writeln('Матриця передування:'),
forall(member([A,B,C], Matrix), format('~w -> ~w: ~w~n',
[A,B,C])).