%% @author thanhvu
%% @doc @todo Add description to cs_encode.


-module(cs_encode).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([encode/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


encode(_Res = #response{group = Group, type = Type, req_id = ReqId, result = Result, data = Data}) ->
	JsonArray = [{<<"group">>,Group},
				 {<<"type">>, Type},
				 {<<"result">>, Result},
				 {<<"req_id">>, ReqId}],
	case Result of
		?API_DONE -> jsx:encode(JsonArray ++ [{<<"data">>, encode_data(Data)}]);
		_ -> jsx:encode(JsonArray ++ [{<<"data">>, <<"">>}])
	end.
encode_data(#res_login{token = Token}) ->
	validate([{<<"token">>,Token}]);
encode_data(#res_user_info{username = UserName,
						   fullname = FullName,
						   phone 	= Phone,
						   email	= Email}) ->
	validate([{<<"username">>, UserName},
			  {<<"fullname">>, FullName},
			  {<<"phone">>, Phone},
			  {<<"email">>, Email}]);
encode_data(#res_user_auth{}) ->
	[];
encode_data(#res_send_message{}) ->
	[];
encode_data(#res_received_message{from_user_name = FromUserName, message = Message}) ->
	[{<<"from_user_name">>, FromUserName}, {<<"message">>, Message}];
encode_data(_) ->
	undefined.

validate([]) ->
	[];
validate([H|T]) ->
	{_K,V} = H,
	if
		V == undefined -> validate(T);
		true -> [H|validate(T)]
	end.