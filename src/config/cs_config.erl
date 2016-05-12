%% @author thanhvu
%% @doc @todo Add description to cs_config.


-module(cs_config).
-include("cs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([load_config/1,get_config/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

get_config(Key, Default) ->
	case catch ets:lookup(?T_CONFIG, Key) of
		[{Key, Val}] -> Val;
		_ -> Default
	end.

load_config(FileName) ->
	FilePath = lists:flatten(io_lib:format("priv/~p.config", [FileName])),
	case catch file:consult(FilePath) of
		{error, Reason} -> lager:error("Read file ~p error, reason ~p",[FilePath, Reason]);
		{ok,Term} ->
			[ets:insert(?T_CONFIG, {K,V}) || {K,V} <- Term]
	end,
	ok.
	
