%% @author thanhvu
%% @doc @todo Add description to cs_decode.


-module(cs_decode).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([decode/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


decode(<<Len:16,Packet/binary>>) ->
	<<Bin:Len/binary,_/binary>> = Packet,
	JsonMap = jsx:decode(Bin, [return_maps]),
	
	Group = maps:get(<<"group">>, JsonMap),
	Type = maps:get(<<"type">>, JsonMap),
	Req_id = maps:get(<<"req_id">>, JsonMap),
	Data = maps:get(<<"data">>, JsonMap),
	
	io:format("data req ~p~n",[Data]),

	R = #request{group = Group, type = Type, req_id = Req_id, data = decode_data(Group,Type,Data)},
	io:format("Decode ~p~n",[R]),
	R.

decode_data(?GROUP_USER, ?TYPE_LOGIN, Data) ->
	Username = maps:get(<<"username">>, Data),
	Password = maps:get(<<"password">>, Data),
	#cmd_login{username = Username, password = Password};
decode_data(?GROUP_USER, ?TYPE_USER_INFO, Data) ->
	UserName = maps:get(<<"username">>, Data),
	#cmd_user_info{username = UserName};
decode_data(?GROUP_USER, ?TYPE_USER_AUTH, Data) ->
	Token = maps:get(<<"token">>, Data),
	#cmd_user_auth{token = Token};
decode_data(_Group, _Type, _Data) ->
	undefined.