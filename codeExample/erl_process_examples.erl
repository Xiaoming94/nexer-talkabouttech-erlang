-module(erl_process_examples).
-export([fib/1, ping/1]).

ping(0) -> 
    io:format("Ping finished \n"),
    done_ping;

ping(N) when is_integer(N), N > 0 ->
    Pid = self(),
    spawn(fun() -> timer:sleep(1000), Pid ! pong end),
    receive
        pong ->
            io:format("I got a Pong \n"),
            ping(N-1)
    end.

% This is why Erlang is not considered a Pure language
fib(N) ->
    Pid = self(),
    spawn(fun() -> para_fib(N, Pid) end), % Creates a process that runs the para_fib function
    receive % Whites for a message that matches this pattern
        {done, Results} ->
            Results
    end.

para_fib(0, From) -> % Terminating case.
    From ! {done, 0},
    ok;

para_fib(1, From) -> % Terminating case.
    From ! {done, 1},
    ok;

para_fib(N, From) -> % Divides the two possible branches in two processes and have the join at the end.
    Pid = self(),
    spawn(fun() -> para_fib(N-1, Pid) end),
    spawn(fun() -> para_fib(N-2, Pid) end),
    receive
        {done, Results1} ->
            receive
                {done, Results2} ->
                    Results = Results1 + Results2,
                    From ! {done, Results}
            end
    end.