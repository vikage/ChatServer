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
	[{<<"token">>,Token}];
encode_data(#res_user_info{username = UserName,
						   fullname = FullName,
						   phone 	= Phone,
						   email	= Email,
						   avatar	= Avatar}) ->
	validate([{<<"username">>, UserName},
			  {<<"fullname">>, FullName},
			  {<<"phone">>, Phone},
			  {<<"email">>, Email},
			  {<<"avatar">>, Avatar}]);
encode_data(#res_user_info_with_status{username = UserName, fullname = FullName, avatar = Avatar, status = Status}) ->
	[{<<"username">>, UserName},
	 {<<"fullname">>, FullName},
	 {<<"avatar">>, Avatar},
	 {<<"status">>, Status}];
encode_data(#res_search_user{list = List}) ->
	[[{<<"username">>,UserName},{<<"fullname">>, FullName},{<<"avatar">>,Avatar}] || #mysql_user_search{username = UserName,fullname = FullName, avatar = Avatar} <- List];

encode_data(#res_user_auth{}) ->
	[];
encode_data(#res_send_message{time = Time}) ->
	[{<<"time">>,Time}];
encode_data(#res_received_message{from_user_name = FromUserName, from_fullname = FromFullName, message = Message, datetime = DateTime, message_id = MessageId}) ->
	[{<<"from_user_name">>, FromUserName},
	 {<<"sender_fullname">>, FromFullName},
	 {<<"message">>, Message},
	 {<<"message_id">>, MessageId},
	 {<<"datetime">>, DateTime}];
encode_data(#res_confirm_received_message{}) ->
	[];
encode_data(#res_message_offline{list_message = ListMessage}) ->
	lists:foldl(fun(#tbl_message{from_user = FromUserName,
								 message = Message,
								 datetime = DateTime,
								 message_id = MessageId,
								 sender_fullname = FullName}, R) -> 
						[[{<<"from_user_name">>, FromUserName},
						  {<<"message">>, Message},
						  {<<"sender_fullname">>, FullName},
						  {<<"message_id">>, MessageId},
						  {<<"datetime">>, DateTime}] | R]
				end, [], ListMessage);
encode_data(#res_received_friend_request{request_id = RId,
										 from_user = FromUser}) ->
	[{<<"request_id">>, RId},{<<"from_user">>, FromUser}];
encode_data(#res_send_notification{title = Title, body = Body}) ->
	[{<<"body">>, Body}, {<<"title">>, Title}];
encode_data(#res_list_friend{list_encoded = List}) ->
	List;
encode_data(#res_notice_friend_online{username = UserName}) ->
	[{<<"username">>, UserName}];
encode_data(#res_notice_friend_offline{username = UserName}) ->
	[{<<"username">>, UserName}];
encode_data(#res_get_list_friend_request{list = List}) ->
	[[{<<"request_id">>, RId},
	  {<<"username">>,UserName},
	  {<<"fullname">>, FullName},
	  {<<"avatar">>, Avatar}] || #mysql_get_friend_request{request_id = RId, username = UserName, fullname = FullName, avatar = Avatar} <- List];
encode_data(_) ->
	[].

validate([]) ->
	[];
validate([H|T]) ->
	{_K,V} = H,
	if
		V == undefined -> validate(T);
		true -> [H|validate(T)]
	end.