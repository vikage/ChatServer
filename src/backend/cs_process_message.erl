%% @author thanhvu
%% @doc @todo Add description to cs_process_message.


-module(cs_process_message).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([send_message_offline/2]).



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