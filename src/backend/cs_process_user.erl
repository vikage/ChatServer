%% @author ThanhVu
%% @doc @todo Add description to cs_process_user.


-module(cs_process_user).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([get_user_info/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================


get_user_info(TargetUserName, MeUserName) ->
	case cs_user_db:user_info(TargetUserName) of
		#db_res{error = ?DB_NOT_FOUND} ->
			{?API_USER_NOT_FOUND, undefined};
		#db_res{error = ?DB_DONE, result = _U = #tbl_users{fullname = FullName, avatar = Avatar}} ->
			% Check friend
			Status = case cs_friend_db:get(TargetUserName, MeUserName) of
						 #db_res{error = ?DB_DONE, result = #tbl_friend{}} ->
							 % Already friend
							 3;
						 _ -> 
							 % Check me request
							 case cs_friend_request_db:get_request(<<MeUserName/binary,<<",">>/binary,TargetUserName/binary>>) of
								 #db_res{error = ?DB_DONE, result = #tbl_friend_request{}} ->
									 % Me already request
									 1;
								 _ ->
									 % Check TargetUserName request
									 case cs_friend_request_db:get_request(<<TargetUserName/binary,<<",">>/binary,MeUserName/binary>>) of
										 #db_res{error = ?DB_DONE, result = #tbl_friend_request{}} ->
											 2;
										 _ ->
											 0
									 end
							 end
					 end,
			{?API_DONE, #res_user_info_with_status{username = TargetUserName, fullname = FullName, avatar = Avatar, status = Status}}
	end.