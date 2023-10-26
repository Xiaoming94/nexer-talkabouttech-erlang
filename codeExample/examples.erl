-module(examples).
-import(lists, [map/2, foldl/3, foldr/3]).
-export([add/2, fib/1, recsumlist/1, foldsum/2, qsort/1]).

% Comments are written using %

% Simple addition
add(Val1, Val2) -> Val1 + Val2. % Variables always start with capital letter, and functions end in "."

% Fibonaccis number
fib(1) -> 1; % Semicolon used to mark the end of function of this "pattern"/"case"
fib(2) -> 2; 
fib(Val) when is_integer(Val), Val > 2 -> fib(Val-1) + fib(Val-2); 
fib(_) -> 1.

% Sum over list using recursion
recsumlist([]) -> 0; % For quick return, function pattern match with an empty list
recsumlist(ListArg) -> recsumlist_helper(0, ListArg).

% Definition of helper function
recsumlist_helper(Res, []) -> Res; % Function match with an empty list
recsumlist_helper(Res, [Head|Tail]) -> recsumlist_helper(Res + Head, Tail).  % Essentially forloop that sums over a list 
% [Head|Tail] declaration of a list stands for [First Element in List| Rest of the List]

% Glorified sum function with higher-order function
% Func is a higher-order function
foldsum(List, Func) ->
    lists:foldl(fun(X, Acc) -> Acc + Func(X) end, 0, List). % lists:foldl function is pretty much equivalent to C++ std::accumulate

% Quicksort, this one is neat
qsort([]) -> [];
qsort([Head|Tail]) ->
    Left = [X || X <- Tail, X < Head], % This is called a list-comprehension, also exist in Python
    Right = [X || X <- Tail, X >= Head], % Does not exist in C++, is equivalent of minimum 5 lines of code using std::copy_if, and std::transform
    qsort(Left) ++ [Head] ++ qsort(Right). % ++ operator here is list concatenation operator