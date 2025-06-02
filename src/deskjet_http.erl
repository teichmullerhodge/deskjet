-module(deskjet_http).
-export([start/2]).

start(URL, RPS) when is_integer(RPS), RPS > 0, RPS =< 50 ->
    inets:start(),
    spawn(fun() -> run(URL, RPS) end).

run(URL, RPS) ->
    Duration = 10, 
    Interval = trunc(1000 / RPS), 
    StartTime = erlang:monotonic_time(seconds),  
    EndTime = StartTime + Duration,
    loop(URL, Interval, EndTime).

loop(URL, Interval, EndTime) ->
    case erlang:monotonic_time(seconds) >= EndTime of
        true ->
            io:format("✅ Ended~n"),
            ok;
        false ->
            spawn(fun() -> make_request(URL) end),
            timer:sleep(Interval),
            loop(URL, Interval, EndTime)
    end.

make_request(URL) ->
    case httpc:request(get, {URL, []}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            io:format("✓ Success: ~s~n", [URL]);
        {ok, {{_, Code, _}, _, _}} ->
            io:format("⚠️ Code: ~p~n", [Code]);
        Error ->
            io:format("❌ Error: ~p~n", [Error])
    end.
