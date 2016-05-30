%% @author thanhvu
%% @doc @todo Add description to cs_client_manager.


-module(cs_client_manager).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([add_client/4,remove_client/1,find_client/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


add_client(UserName,FullName,Token,Pid) when is_pid(Pid) ->
	lager:debug("New client online with name \"~p\" at ~p~n",[UserName, Pid]),
	cs_client:change_state_online(UserName,FullName, Token, Pid),
	cs_process_friend:notice_friend_online(UserName),
	case find_client(UserName) of
		{ok, #tbl_user_onl{pid = OldPid}} ->
			if
				OldPid == Pid ->
					ok;
				true -> 
					lager:info("Kick username ~p~n", [UserName]),
					OldPid ! {kill}
			end;
		A -> lager:debug("Find user kick result ~p~n",[A])
	end,
	ets:insert(tbl_user_onl, #tbl_user_onl{username = UserName, pid = Pid, fullname = FullName}).

find_client(UserName) ->
	case ets:lookup(tbl_user_onl, UserName) of
		[] -> {error, user_offline};
		[User = #tbl_user_onl{}] -> {ok, User};
		_ -> {error, system_fail}
	end.
remove_client(UserName) ->
	ets:delete(tbl_user_onl, UserName).