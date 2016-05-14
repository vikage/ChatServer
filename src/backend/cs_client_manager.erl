%% @author thanhvu
%% @doc @todo Add description to cs_client_manager.


-module(cs_client_manager).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([add_client/3,remove_client/1,find_client/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


add_client(UserName,Token,Pid) when is_pid(Pid) ->
	lager:debug("New client online with name \"~p\" at ~p~n",[UserName, Pid]),
	cs_client:change_state_online(UserName, Token, Pid),
	ets:insert(tbl_user_onl, #tbl_user_onl{username = UserName, pid = Pid}).

find_client(UserName) ->
	case ets:lookup(tbl_user_onl, UserName) of
		[] -> {error, user_offline};
		[U = #tbl_user_onl{}] -> {ok, U};
		_ -> {error, system_fail}
	end.
remove_client(UserName) ->
	lager:debug("Client remove with name \"~p\"", [UserName]),
	ets:delete(tbl_user_onl, UserName).