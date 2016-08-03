%% @author ThanhVu
%% @doc @todo Add description to cs_process_friend.


-module(cs_process_friend).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([accept_friend_request/4,add_request_friend/3,get_list_friend/2,notice_friend_online/1]).
-export([notice_friend_offline/1,search_user/3,get_friend_request/2,unfriend/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

accept_friend_request(OurName,FullName, RId, From) ->
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
							DataRev = #res_other_user_accept_friend_request{username = OurName, fullname = FullName},
							ResponseRecord = #response{group = ?GROUP_NOTIFICATION,
													   type = ?TYPE_OTHER_USER_ACCEPT_FRIEND_REQUEST,
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

add_request_friend(OurName,FullName, ToUser) ->
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
																   from_user = OurName,
																   fullname = FullName},
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


get_list_friend(OurName, Page) ->
	case cs_friend_db:get_list_friend(OurName, Page) of
		#db_res{error = ?DB_EMPTY} ->
			%% EMPTY
			{?API_DONE, undefined};
		#db_res{error = ?DB_DONE, result = List} ->
			Fun = fun(Hd, Accu) ->
						  #mysql_friend_item{user1 = U1, user2 = U2, avatar = Avatar, fullname = FullName} = Hd,
						  FriendName = case U1 of
										   OurName -> U2;
										   _ -> U1
									   end,
						  % Check user F online
						  Result = case cs_client_manager:find_client(FriendName) of
									   {ok, #tbl_user_onl{}} ->
										   [{<<"username">>, FriendName},{<<"avatar">>, Avatar},{<<"status">>,1}, {<<"fullname">>, FullName}];
									   _ ->
										   [{<<"username">>, FriendName},{<<"avatar">>, Avatar},{<<"status">>,0}, {<<"fullname">>, FullName}]
								   end,
						  [Result|Accu]
				  end,
			ListF = lists:foldl(Fun, [], List),
			{?API_DONE, #res_list_friend{list_encoded = ListF}}
	end.

notice_friend_online(OurName) ->
	case cs_friend_db:get_list_friend_all(OurName) of
		#db_res{error = ?DB_EMPTY} ->
			%% EMPTY
			ok;
		#db_res{error = ?DB_DONE, result = List} ->
			Fun = fun(Hd) ->
						  #mysql_friend_item{user1 = U1, user2 = U2} = Hd,
						  FriendName = case U1 of
										   OurName -> U2;
										   _ -> U1
									   end,
						  % Check user F online
						  case cs_client_manager:find_client(FriendName) of
									   {ok, #tbl_user_onl{pid = Pid, username = FriendName}} ->
										   Record = #res_notice_friend_online{username = OurName},
										   ResponseRecord = #response{group = ?GROUP_FRIEND,
													   type = ?TYPE_NOTICE_FRIEND_ONLINE,
													   req_id = 0,
													   result = 0,
													   data = Record},
											cs_client:send_data_res(Pid, ResponseRecord);
									   _ -> ok
							end
				  end,
			lists:foreach(Fun, List),
			ok
	end.

notice_friend_offline(OurName) ->
	case cs_friend_db:get_list_friend_all(OurName) of
		#db_res{error = ?DB_EMPTY} ->
			ok;
		#db_res{error = ?DB_DONE, result = List} ->
			Fun = fun(Hd) ->
						  #mysql_friend_item{user1 = U1, user2 = U2} = Hd,
						  FriendName = case U1 of
										   OurName -> U2;
										   _ -> U1
									   end,
						  % Check user F online
						  case cs_client_manager:find_client(FriendName) of
									   {ok, #tbl_user_onl{pid = Pid, username = FriendName}} ->
										   Record = #res_notice_friend_offline{username = OurName},
										   ResponseRecord = #response{group = ?GROUP_FRIEND,
													   type = ?TYPE_NOTICE_FRIEND_OFFLINE,
													   req_id = 0,
													   result = 0,
													   data = Record},
											cs_client:send_data_res(Pid, ResponseRecord);
									   _ -> ok
							end
				  end,
			lists:foreach(Fun, List),
			ok
	end.

search_user(UserName,Keyword,Page) ->
	case cs_user_db:search(UserName, Keyword, Page) of
		#db_res{error = ?DB_EMPTY} ->
			{?API_DONE, undefined};
		#db_res{error = ?DB_DONE, result = List} ->
			{?API_DONE, #res_search_user{list = List}}
	end.


get_friend_request(UserName,Page) ->
	case cs_friend_request_db:get_list_request(UserName, Page) of
		#db_res{error = ?DB_EMPTY} ->
			{?API_DONE, undefined};
		#db_res{error = ?DB_DONE, result = List} ->
			{?API_DONE, #res_get_list_friend_request{list = List}}
	end.

unfriend(UserName,FriendUserName) ->
	case cs_friend_db:unfriend(UserName, FriendUserName) of
		_ -> {?API_DONE, undefined}
	end.