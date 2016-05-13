%% @author thanhvu
%% @doc @todo Add description to cs_process_data.


-module(cs_process_data).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([process_data/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

process_data(Bin,Socket) ->
	Req = cs_decode:decode(Bin),
	Data = Req#request.data,
	{ResultCode,ResultData} = process(Data),
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

process(#cmd_login{username = UserName, password = Password}) ->
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
process(#cmd_user_info{username = UserName}) ->
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
process(#cmd_user_auth{token = Token}) ->
	case cs_client:auth(Token, self()) of
		ok -> {?API_DONE, #res_user_auth{}};
		_ -> {?API_USER_AUTH_TOKEN_FAIL, undefined}
	end;
process(_Req) ->
	undefied.


