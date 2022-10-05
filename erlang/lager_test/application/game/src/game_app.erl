%% -*- coding: latin-1 -*-
%% @author zouv
%% @doc game应用，游戏主应用入口

-module(game_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> start(game).

start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

start(_StartType, _StartArgs) ->
%%    erlang:system_flag(backtrace_depth, 12),
    io:format("before lager_test ========================================================.~n"),
%%    lager_test:test(),
    {ok, self()}.

stop(_State) ->
    ok.
