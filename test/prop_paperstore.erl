-module(prop_paperstore).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

-define(ALL(Vars, Types, Exprs),
        ?SETUP(fun() ->
            {ok, Apps} = application:ensure_all_started(paperstore),
            fun() -> [application:stop(App) || App <- Apps], ok end
         end, 
        ?FORALL(Vars, Types, Exprs))).

prop_test() ->
   ?ALL(Cmds, commands(?MODULE),
        begin
            paperstore_db:setup(),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            paperstore_db:teardown(),
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                            [History, State, Result]), aggregate(command_names(Cmds), Result =:= ok))
        end)
        
    ).

prop_parallel() ->
    ?ALL(Cmds, commands(?MODULE),
        begin
            papaerstore_db:setup(),
            {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
            paperstore_db:teardown(),
            ?WHENFAIL(io:format("=======~n"
                                "Failing command sequence:~n~p~n"
                                "At state: ~p~n"
                                "=======~n"
                                "Result: ~p~n"
                                "History: ~p~n",
                                [Cmds, State,Result,History]),
                      aggregate(command_names(Cmds), Result =:= ok))
        end)
    ).

%% Initial model value at system start. Should be deterministic
initial_state() -> #{}.


%% TODO a paper should be locked if it has a price
command(State) ->
    AlwaysPossible = [
        % firstly add a paper 
        % and then find a random unknown paper which is not matched to the added paper
        {call, paper_shim, add_paper_new, [doi(), title(), author(), false, 0, "example.pdf"]},
        {call, paper_shim, find_paper_by_doi, [doi()]},
        {call, paper_shim, find_paper_by_title_unknown, [title()]},
        {call, paper_shim, find_paper_by_author_unknown, [author()]}
    ],
    ReliesOnState = case maps:size(State) of
        0 ->
            [];
        _ ->
            S = State,
            [{call, paper_shim, add_paper_existing,
                [doi(S), title(), author(), false, 0, "example.pdf"]},
             {call, paper_shim, find_paper_by_doi_exists, [doi(S)]},
             {call, paper_shim, find_paper_by_title_matching, title(S)},
             {call, paper_shim, find_paper_by_author_matching, author(S)}]
    end,
    oneof(AlwaysPossible ++ ReliesOnState).

%% whether a command should be valid under the current state
% before the call of add_paper_new, there is no doi
precondition(S, {call, _, add_paper_new, [DOI|_]}) ->
    not has_doi(S, DOI);
precondition(S, {call, _, find_paper_by_doi_unknown, [DOI]}) ->
    not has_doi(S, DOI);
precondition(S, {call, _, find_paper_by_author_unknown, [Author]}) ->
    not like_author(S, Author);
precondition(S, {call, _, find_paper_by_title_unknown, [Title]}) ->
    not like_title(S, Title);
precondition(S, {call, _, find_paper_by_author_matching, [Author]}) ->
    like_author(S, Author);
precondition(S, {call, _, find_paper_by_title_matching, [Title]}) ->
    like_title(S, Title);
precondition(S, {call, _Mod, _Fun, [DOI|_]})
    has_doi(S, DOI).

%% whether the result of the call {call, Mod, Fun, Args}
%% coming from the actual system makes sense
% postcondition(State, {call, _Mod, _Fun, _Args}, _Res) -> true.
postcondition(_, {_, _, add_paper_new, _}, ok) ->
    true;
postcondition(_, {_, _, add_paper_existing,_}, {error, _}) ->
    true;
postcondition(S, {_, _, find_paper_by_doi_exists, [DOI]}, {ok, [Res]}) ->
    paper_equal(Res, maps:get(DOI, S, undefined));
postcondition(_, {_, _, find_paper_by_doi_unknown, _}, {ok, []}) ->
    true;
postcondition(S, {_, _, find_paper_by_author_matching, [Auth]}, {ok,Res}) ->
    Map = maps:filter(fun(_, {_,_,A,_,_}) ->
                              nomatch =/= string:find(A, Auth)
                      end, S),
    papers_equal(lists:sort(Res), lists:sort(maps:values(Map)));
postcondition(_, {_, _, find_paper_by_author_unknown, _}, {ok, []}) ->
    true;
postcondition(S, {_, _, find_paper_by_title_matching, [Title]}, {ok,Res}) ->
    Map = maps:filter(fun(_, {_,T,_,_,_}) ->
                             nomatch =/= string:find(T, Title)
                      end, S),
    papers_equal(lists:sort(Res), lists:sort(maps:values(Map)));
postcondition(_, {_, _, find_paper_by_title_unknown, _}, {ok, []}) ->
    true;
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
    io:format("~nnon-matching postcondition: {~p,~p,~p} -> ~p~n",
              [_Mod, _Fun, _Args, _Res]),
    false.

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
has_doi(Map, DOI) ->
    map:is_key(DOI, Map).

like_author(Map, Auth) ->
    lists:any(fun({_,_,A,_,_}) -> nomatch =/= string:find(A, Auth) end,
              maps:values(Map)).

like_title(Map, Title) ->
    lists:any(fun({_,T,_,_,_}) -> nomatch =/= string:find(T, Title) end,
              maps:values(Map)).

papers_equal([], []) ->
    true;
papers_equal([A|As], [B|Bs]) ->
    paper_equal(A, B) andalso papers_equal(As, Bs);
papers_equal(_, _) ->
    false.

paper_equal({DOIA, TitleA, AuthorA, LockA, PriceA, LinkA},
            {DOIB, TitleB, AuthorB, LockB, PriceB, LinkB}) ->
    {DOIA, LockA, PriceA, LinkA} =:= {DOIB,LockB, PriceB, LinkB}
    andalso
    string:equal(TitleA, TitleB) andalso string:equal(AuthorA, AuthorB).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
title() -> friendly_unicode().

author() -> friendly_unicode().

friendly_unicode() ->
    ?LET(X, ?SUCHTHAT(S, string(),
                      not lists:member(0, S) andalso
                      nomatch =:= string:find(S, "\\") andalso
                      nomatch =:= string:find(S, "_") andalso
                      nomatch =:= string:find(S, "%") andalso
                      string:length(S) < 256),
         elements([X, unicode:characters_to_binary(X)])).

% 10.1000/123
doi() ->
    ?LET(DOI,
         ["10.",
          ?LET(X, range(1000,9999), integer_to_list(X)),
          "/",
          any()]
         iolist_to_binary(DOI)).

doi(State) ->
    elements(maps:keys(State)).

author(State) ->
    elements([partial(Author) || {_,_,Author,_,_} <- maps:values(State)]).

title(State) ->
    elements([partial(Title) || {_,Title,_,_,_} <- maps:values(State)]).

%% Create a partial string, built from a portion of a complete one.
partial(String) ->
    L = string:length(String),
    ?LET({Start, Len}, {range(0, L), non_neg_integer()},
         string:slice(String, Start, Len)).