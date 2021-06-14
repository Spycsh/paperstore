-module(paperstore_db).
-export([setup/0, teardown/0, load_queries/0,
        add_paper/3, add_paper/6, find_paper_by_author/1, find_paper_by_doi/1,
        find_paper_by_link/1, find_paper_by_title/1, 
        find_paper_locked/0, find_paper_unlocked/0]).

setup() ->
    run_query(setup_paper_store, []).

teardown() ->
    run_query(teardown_paper_store, []).

%% add a new paper to the inventory, with no lock
add_paper(DOI, Title, Author) ->
    add_paper(DOI, Title, Author, false, 0, "example.pdf").

add_paper(DOI, Title, Author, Lock, Price, Link) ->
    BinTitle = unicode:characters_to_binary(Title),
    BinAuthor = unicode:characters_to_binary(Author),
    case run_query(add_paper, [DOI, BinTitle, BinAuthor, Lock, Price, Link]) of
        {{insert,0,1},[]} -> ok;
        {error, Reason} -> {error, Reason};
        Other -> {error, Other}
    end.

%% find all papers written by the author
%% loosely match
find_paper_by_author(Author) ->
    handle_select(
        run_query(find_by_author,
                [unicode:characters_to_binary(["%",Author,"%"])])    
    ).

%% find all papers by title
%% loosely match
find_paper_by_title(Title) ->
    handle_select(
        run_query(find_by_title, 
                [unicode:characters_to_binary(["%", Title, "%"])])
    ).

%% find all papers unlocked
find_paper_unlocked() ->
    handle_select(
        run_query(find_by_lock, [false])    
    ).

%% find all papers locked
find_paper_locked() ->
    handle_select(
        run_query(find_by_lock, [true])    
    ).

%% find the paper given a DOI
find_paper_by_doi(DOI) ->
    handle_select(run_query(find_by_doi, [DOI])).

%% find the paper given a link
find_paper_by_link(Link) ->
    handle_select(run_query(find_by_link, [Link])).



handle_select({{select, _}, List}) -> {ok, List};
handle_select(Error) -> Error.

% handle_single_update({{update,1}, _}) -> ok;
% handle_single_update({{update,0}, _}) -> {error, not_found};
% handle_single_update({error, Reason}) -> {error, Reason};
% handle_single_update(Other) -> {error, Other}.

%% check connection with database and run query
run_query(Name, Args) ->
    with_connection(fun(Conn) -> run_query(Name, Args, Conn) end).


%% PostgreSQL based query with arguments and connection
run_query(Name, Args, Conn) ->
    pgsql_connection:extended_query(query(Name), Args, Conn).


with_connection(Fun) ->
    {ok, Conn} = connect(),
    Res = Fun(Conn),
    close(Conn),
    Res.

%% open a new connection to a PostgreSQL database from the
%% application configuraiton
connect() -> connect(application:get_env(paperstore, pg, [])).

%% open up a new connection to a PostgreSQL database with
%% explicit configuration parameters.
connect(Args) ->
    try pgsql_connection:open(Args) of
        {pgsql_connection, _} = Conn -> {ok, Conn}
    catch
        throw:Error -> {error, Error}
    end.

close(Conn) ->
    pgsql_connection:clsoe(Conn).

%% load the queries from queries.sql
load_queries() ->
    ets:new(paperstore_sql, [named_table, public, {read_concurrency, true}]),
    SQLFile = filename:join(code:priv_dir(paperstore), "queries.sql"),
    {ok, Queries} = eql:compile(SQLFile),
    ets:insert(paperstore_sql, Queries),
    ok.

query(Name) ->
    case ets:lookup(paperstore_sql, Name) of
        [] -> {query_not_found, Name};
        [{_, Query}] -> Query
    end.

