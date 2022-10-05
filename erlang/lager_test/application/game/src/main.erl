%% -*- coding: latin-1 -*-
%% @author zouv
%% @doc @todo 服务器开/关接口

-module(main).

-define(GAME_APP_LIST, [
    inets,
    crypto,
    asn1,
    public_key,
    ssl,
    compiler,
    syntax_tools,
    goldrush,
    lager,
    game
]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, stop/1]).


%% -------------------------
%% 开启服务器
%% -------------------------
start([NodeFlag]) ->
    AppList =
        case NodeFlag of
            game -> ?GAME_APP_LIST
        end,
    %% 开启应用
    SuccList = start_apps(AppList, []),
    if
        length(SuccList) == length(AppList) ->
            io:format("~n ___SERVER APP START SUCCESSFUL !!!");
        true ->
            io:format("server app start Error! info = ~p~n", [AppList -- SuccList])
    end,
    io:format("~n~n~n"),
    ok.

%% 开启应用
start_apps([], AccList) ->
    AccList;
start_apps([E | L], AccList) ->
    case application:start(E) of
        ok ->
            io:format("~n ___start application : ~p ", [E]),
            start_apps(L, [E | AccList]);
        {error, {already_started, App}} ->
            io:format("error! application = ~p, info = ~p~n", [E, {already_started, App}]),
            AccList;
        {error, Reason} ->
            io:format("~p~p~n", [E, Reason]),
            stop_app(AccList),
            throw({error, {E, Reason}}),
            AccList
    end.

%% 停止应用
stop_app([]) ->
    ok;
stop_app([E | L]) ->
    application:stop(E),
    io:format("~n ___stop application : ~p", [E]),
    stop_app(L).

%% -------------------------
%% 停止服务器
%% -------------------------
stop(game) ->
%%    log_writer:flush(),
    application:stop(game),
    application:stop(lager).
