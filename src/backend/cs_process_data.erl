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
	case cs_user_db:user_info_username(UserName) of
		#db_res{result = #tbl_users{password = Password, uid = Uid}} ->
			case cs_token_db:new_token(Uid) of
				#db_res{result = Token} -> 
					% add new client
					cs_client_manager:add_client(UserName,Token#tbl_token.token_string, self()),
					{?API_DONE, #res_login{token = Token#tbl_token.token_string}};
				_ -> {?API_SYSTEM_FAIL, undefined}
			end;
		_ -> {?API_USER_LOGIN_FAIL, undefined}
	end;
process(#cmd_user_info{username = UserName}, _StateData) ->
	case cs_user_db:user_info_username(UserName) of
		#db_res{result = #tbl_users{username = UserName,
									fullname = FullName,
									phone = Phone,
									email = Email}} ->
			{?API_DONE, #res_user_info{username = UserName,
									   fullname = FullName,
									   phone 	= Phone,
									   email 	= Email}};
		#db_res{error = ?DB_NOT_FOUND} -> {?API_USER_NOT_FOUND, undefined};
		_ -> {?API_USER_LOGIN_FAIL, undefined}
	end;
process(#cmd_user_auth{token = Token}, _StateData) ->
	case cs_client:auth(Token, self()) of
		ok -> {?API_DONE, #res_user_auth{}};
		_ -> {?API_USER_AUTH_TOKEN_FAIL, undefined}
	end;
% send message
process(#cmd_send_message{token = Token, message = Message, to_user_name = ToUserName}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
						  {?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName} -> lager:info("Check token success with Token: ~p~n",[Token]),
						  %Save message
						  MessageObj = #tbl_message{to_user = ToUserName, from_user = UserName, message = Message},
						  case cs_message_db:new_message(MessageObj) of
							  #db_res{result = #tbl_message{message_id = MessageId, datetime = DateTime}} ->
								  % Find user from cs_client_manager
								  case cs_client_manager:find_client(ToUserName) of
									  {error,Reason} -> lager:debug("Find user ~p fail with reason ~p~n", [ToUserName,Reason]);
									  {ok, #tbl_user_onl{pid = U_Pid}} ->
										  lager:info("Detect user ~p at Pid: ~p~n", [ToUserName, U_Pid]),
										  DataRev = #res_received_message{from_user_name = UserName,
																		  message = Message,
																		  message_id = MessageId,
																		  datetime = DateTime},
										  ResponseReceivedMessage = #response{group = ?GROUP_CHAT,
																			  type = ?TYPE_RECEIVED_MESSAGE,
																			  req_id = 0,
																			  result = 0,
																			  data = DataRev},
										  ResponseBin = cs_encode:encode(ResponseReceivedMessage),	
										  Len = byte_size(ResponseBin),
										  cs_client:received_message(U_Pid, <<Len:16,ResponseBin/binary>>)
								  
								  end,
								  {?API_DONE, #res_send_message{}};
							  _ -> {?API_SYSTEM_FAIL, undefined}
						  end
	end;
process(#cmd_config_received_message{message_id = MessageId, token = Token}, StateData) ->
	case cs_client:check_token(Token, StateData) of
		{error,Reason} -> lager:error("Check token fail with Token: ~p Reason: ~p~n",[Token, Reason]),
						  {?API_USER_AUTH_TOKEN_FAIL, undefined};
		{ok, UserName} -> lager:info("Check token success with Token: ~p~n",[Token]),
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
process(_Req, _StateData) ->
	undefied.


