%% @author ThanhVu
%% @doc @todo Add description to cs_process_friend.


-module(cs_process_friend).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([accept_friend_request/3,add_request_friend/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

accept_friend_request(OurName, RId, From) ->
	% Check friend request
	case cs_friend_request_db:get_request(RId) of
		#db_res{error = ?DB_NOT_FOUND} ->
			{?API_FRIEND_REQUEST_NOT_FOUND, undefined};
		#db_res{error = ?DB_DONE} ->
			FriendRecord = #tbl_friend{friend_id = RId,
									   user1 = OurName,
									   user2 = From},
			cs_friend_request_db:remove_request(RId),
			case cs_friend_db:new_friend(FriendRecord) of
				#db_res{error = ?DB_DONE} ->
					%% Find and Send notify to Requester
					case cs_client_manager:find_client(From) of
						{error,Reason} -> lager:debug("Find user ~p fail with reason ~p~n", [From,Reason]);
						{ok, #tbl_user_onl{pid = U_Pid}} ->
							Body = list_to_binary(lists:flatten(io_lib:format("User ~s accept your friend request", [binary_to_list(OurName)]))),
							DataRev = #res_send_notification{title = <<"FRIEND ACCEPT">>,
															 body = Body},
							ResponseRecord = #response{group = ?GROUP_NOTIFICATION,
																type = ?TYPE_SEND_NOTIFICATION,
																req_id = 0,
																result = 0,
																data = DataRev},
							cs_client:send_data_res(U_Pid, ResponseRecord)
					end,
					{?API_DONE, undefined};
				#db_res{error = ?DB_ITEM_EXIST} ->
					{?API_ALREADY_FRIEND, undefined};
				{error, _Reason} ->
					{?API_SYSTEM_FAIL, undefined}
			end
	end.

add_request_friend(OurName, ToUser) ->
	%% Check friend status
	case cs_friend_db:get(OurName, ToUser) of
		#db_res{error = ?DB_DONE} ->
			{?API_ALREADY_FRIEND, undefined};
		#db_res{error = ?DB_NOT_FOUND} ->
			Req = #tbl_friend_request{request_id = <<OurName/binary,<<",">>/binary,ToUser/binary>>, from_user = OurName, to_user = ToUser},
			case cs_friend_request_db:new_request(Req) of
				#db_res{error = ?DB_DONE} -> 
					% Send req to user
					case cs_client_manager:find_client(ToUser) of
						{error,Reason} -> lager:debug("Find user ~p fail with reason ~p~n", [ToUser,Reason]);
						{ok, #tbl_user_onl{pid = U_Pid}} ->
							lager:info("Detect user ~p at Pid: ~p~n", [ToUser, U_Pid]),
							DataRev = #res_received_friend_request{request_id = <<OurName/binary,<<",">>/binary,ToUser/binary>>,
																   from_user = OurName},
							ResponseRecord = #response{group = ?GROUP_FRIEND,
																type = ?TYPE_RECEIVED_FRIEND_REQUEST,
																req_id = 0,
																result = 0,
																data = DataRev},
							cs_client:send_data_res(U_Pid, ResponseRecord)
					end,
					{?API_DONE, undefined};
				#db_res{error = ?DB_ITEM_EXIST} ->
					{?API_FRIEND_REQUEST_EXIST, undefined};
				_ -> {?API_SYSTEM_FAIL, undefined}
			end
	end.