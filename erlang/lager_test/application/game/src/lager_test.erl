-module(lager_test).

%%API
-export([test/0]).

test() ->
	application:ensure_all_started(lager),
	%%aaa:bbb("error msg"),
	lager:error("error msg"),
	lager:warning("warning msg"),
	lager:debug("debug msg"),
	lager:info("info msg"),
	lager:critical("critical msg"),
	erlang:spawn_link(fun() -> erlang:throw("this is a test.") end).