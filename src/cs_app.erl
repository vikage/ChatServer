-module(cs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	application:start(cs).

start(_StartType, _StartArgs) ->
	lager:start(),
	ssl:start(),
	mnesia:start(),
	cs_db:create_ets(),
	cs_db:create_db(),
	cs_config:load_config(chat),
    cs_sup:start_link().

stop(_State) ->
    ok.
