%% @author thanhvu
%% @doc @todo Add description to cs_process_message.


-module(cs_process_message).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([send_message_offline/2,send_message/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


send_message_offline(UserName, Socket) ->
	case cs_message_db:get_message_of_user(UserName) of
		#db_res{result = List, error = ?DB_DONE} ->
			Result = ?API_DONE,
			Data = #res_message_offline{list_message = List},
			ResponseObj = #response{group = ?GROUP_CHAT,
									type = ?TYPE_SEND_OFFLINE_MESSAGE,
									req_id = 0,
									result = Result,
									data = Data},
			lager:debug("Response tuple ~p~n",[ResponseObj]),
			ResponseBin = cs_encode:encode(ResponseObj),	
			Len = byte_size(ResponseBin),
			ResponseTCP = <<Len:16,ResponseBin/binary>>,
			lager:info("Send message offline to ~p~n",[UserName]),
			gen_tcp:send(Socket, ResponseTCP);
		_ -> ok
	end,
	
	ok.

send_message(MessageObj = #tbl_message{to_user = ToUserName, from_user = UserName, message = Message, sender_fullname = TargetFullName}) ->
	case cs_message_db:new_message(MessageObj) of
		#db_res{result = #tbl_message{message_id = MessageId, datetime = DateTime}} ->
			% Find user from cs_client_manager
			case cs_client_manager:find_client(ToUserName) of
				{error,Reason} -> lager:debug("Find user ~p fail with reason ~p~n", [ToUserName,Reason]);
				{ok, #tbl_user_onl{pid = U_Pid}} ->
					lager:info("Detect user ~p at Pid: ~p~n", [ToUserName, U_Pid]),
					DataRev = #res_received_message{from_user_name = UserName,
													from_fullname = TargetFullName,
													message = Message,
													message_id = MessageId, 
													datetime = DateTime},
					ResponseReceivedMessage = #response{group = ?GROUP_CHAT,
														type = ?TYPE_RECEIVED_MESSAGE,
														req_id = 0,
														result = 0,
														data = DataRev},
					cs_client:send_data_res(U_Pid, ResponseReceivedMessage)
			end,
			{?API_DONE, #res_send_message{time = DateTime}};
		_ -> {?API_SYSTEM_FAIL, undefined}
	end.