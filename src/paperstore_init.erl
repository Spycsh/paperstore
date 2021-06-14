-module(paperstore_init).
-export([main/1]).

main(_) ->
    %% See: https://www.postgresql.org/docs/9.6/static/server-start.html
    ok = filelib:ensure_dir("postgres/data/.init-here"),
    io:format("initializing database structure...~n"),
    cmd("initdb -D postgres/data"),
    io:format("starting postgres instance...~n"),

    %% On windows this is synchronous and never returns until done
    StartCmd = "pg_ctl -D postgres/data -l logfile start",
    case os:type() of
        {win32, _} -> spawn(fun() -> cmd(StartCmd) end);
        {unix, _} -> cmd(StartCmd)
    end,
    timer:sleep(5000), % wait and pray!

    io:format("setting up 'paperstore_db' database...~n"),
    cmd("psql -h localhost -d template1 -c "
        "\"CREATE DATABASE paperstore_db;\""),
    io:format("OK.~n"),
    init:stop().

cmd(Str) -> io:format("~s~n", [os:cmd(Str)]).
