%% @author thanhvu
%% @doc @todo Add description to cs_process_data.


-module(cs_process_data).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([process_data/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================

process_data(Bin,Socket,StateData) ->
	Req = cs_decode:decode(Bin),
	Data = Req#request.data,
	{ResultCode,ResultData} = process(Data,StateData),
	ResponseObj = #response{group = Req#request.group,
							type = Req#request.type,
							req_id = Req#request.req_id,
							result = ResultCode,
							data = ResultData},
	lager:debug("Response tuple ~p~n",[ResponseObj]),
	ResponseBin = cs_encode:encode(ResponseObj),	
	Len = byte_size(ResponseBin),
	ResponseTCP = <<Len:16,ResponseBin/binary>>,
	gen_tcp:send(Socket, ResponseTCP).

process(#cmd_login{username = UserName, password = Password}, _StateData) ->
	case cs_user_db:user_info(UserName) of
		#db_res{result = #tbl_users{password = Password,
									fullname = FullName}} ->
			case cs_token_db:new_token(UserName) of
				#db_res{result = Token} -> 
					% add new client
					cs_client_manager:add_client(UserName,FullName,Token#tbl_token.token_string, self()),
					{?API_DONE, #res_login{token = Token#tbl_token.token_string}};
				_ -> {?API_SYSTEM_FAIL, undefined}
			end;
		_ -> {?API_USER_LOGIN_FAIL, undefined}
	end;
process(#cmd_register{username = Username,
					  password = Password,
					  fullname = FullName,
					  email = Email,
					  phone = Phone}, _StateData) ->
	case cs_user_db:user_info(Username) of
		#db_res{result = #tbl_users{}} ->
			{?API_USER_EXIST, undefined};
		#db_res{error = ?DB_NOT_FOUND} ->
			U = #tbl_users{username = Username,
						   password = Password,
						   fullname = FullName,
						   email = Email,
						   phone = Phone},
			case cs_user_db:new_user(U) of
				#db_res{error = ?DB_DONE} ->
					case cs_token_db:new_token(Username) of
						#db_res{result = Token} -> 
							% add new client
							cs_client_manager:add_client(Username,FullName,Token#tbl_token.token_string, self()),
							{?API_DONE, #res_login{token = Token#tbl_token.token_string}};
						_ -> {?API_SYSTEM_FAIL, undefined}
					end;
				_ -> {?API_SYSTEM_FAIL, undefined}
			end
	end;
process(#cmd_user_info{username = TargetUserName, token = Token}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			cs_process_user:get_user_info(TargetUserName, UserName)
	end;
process(#cmd_me_info{token = Token}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			case cs_user_db:user_info(UserName) of
				#db_res{result = #tbl_users{username = UserName,
											fullname = FullName,
											phone = Phone,
											email = Email,
											avatar = Avatar}} ->
					{?API_DONE, #res_user_info{username = UserName,
											   fullname = FullName,
											   phone 	= Phone,
											   email 	= Email,
											   avatar 	= Avatar}};
				#db_res{error = ?DB_NOT_FOUND} -> {?API_USER_NOT_FOUND, undefined};
				_ -> {?API_USER_LOGIN_FAIL, undefined}
			end
	end;
process(#cmd_update_avatar{token = Token, avatar = Avatar}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} ->
			case cs_user_db:update_avatar(UserName, Avatar) of
				#db_res{error = ?DB_DONE} ->
					{?API_DONE, undefined};
				_ -> {?API_SYSTEM_FAIL, undefined}
			end
	end;
process(#cmd_update_device_token{token = Token, device_token = DT}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} ->
			cs_user_db:update_device_token(UserName, DT),
			{?API_DONE, undefined}
	end;
process(#cmd_search_user{token = Token, keyword = Keyword, page = Page}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			cs_process_friend:search_user(UserName, Keyword, Page)
	end;
process(#cmd_user_auth{token = Token}, _StateData) ->
	case cs_client:auth(Token, self()) of
		ok -> {?API_DONE, #res_user_auth{}};
		_ -> {?API_USER_AUTH_TOKEN_FAIL, undefined}
	end;
% send message
process(#cmd_send_message{token = Token, message = Message, to_user_name = ToUserName}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			%Save message
			MessageObj = #tbl_message{to_user = ToUserName, from_user = UserName, message = Message, sender_fullname = FullName},
			cs_process_message:send_message(MessageObj)
	end;
process(#cmd_confirm_received_message{message_id = MessageId, token = Token}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			case cs_message_db:get_message(MessageId) of
				#db_res{result = #tbl_message{to_user = UserName}} ->								  
					case cs_message_db:delete_message(MessageId) of
						#db_res{error = ?DB_DONE} ->
							{?API_DONE, #res_confirm_received_message{}};
						_ -> {?API_SYSTEM_FAIL, #res_confirm_received_message{}}
					end;
				_ -> {?API_CONFIRM_MESSAGE_NOT_OF_YOU, undefined}
			end
	end;
process(#cmd_confirm_received_offline_message{token = Token}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			case cs_message_db:get_message_of_user(UserName) of
				#db_res{error = ?DB_DONE, result = ListMessage} ->
					[cs_message_db:delete_message(MessageId) || #tbl_message{message_id = MessageId} <- ListMessage],
					{?API_DONE, undefined};
				_ -> {?API_DONE, undefined}
			end
	end;
process(#cmd_add_friend{token = Token, to_user = ToUser}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			cs_process_friend:add_request_friend(UserName,FullName, ToUser)
	end;
process(#cmd_reject_friend_request{token = Token, from_user = FromUser}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			cs_friend_request_db:remove_request(<<FromUser/binary,<<",">>/binary, UserName/binary>>),
			{?API_DONE, undefined}
	end;
process(#cmd_accept_friend_request{token = Token, from_user = From}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			RId = <<From/binary,<<",">>/binary, UserName/binary>>,
			cs_process_friend:accept_friend_request(UserName,FullName, RId, From)
	end;
process(#cmd_get_list_friend{token = Token, page = Page}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			cs_process_friend:get_list_friend(UserName, Page)
	end;
process(#cmd_get_list_friend_request{token = Token, page = Page}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			cs_process_friend:get_friend_request(UserName, Page)
	end;
process(#cmd_unfriend{token = Token, friend_username = FriendUserName}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> 
			lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
			{?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName,_FullName} -> 
			lager:info("Check token success with Token: ~p~n",[Token]),
			cs_process_friend:unfriend(UserName,FriendUserName)
	end;
process(_Req, _StateData) ->
	{?API_BAD_MATCH,undefied}.


