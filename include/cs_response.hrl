-record(response,{group,type,result,req_id,data}).

-record(res_login, {token}).
-record(res_user_info,{username,fullname,email,phone, avatar}).
-record(res_user_auth,{}).
-record(res_search_user,{list}).
-record(res_get_list_friend_request,{list}).
%% status = 0(request yet), status = 1(me request friend), status = 2(not you request), status = 3(already friend)
-record(res_user_info_with_status, {username,fullname, avatar, status}). 

-record(res_send_message,{time}).
-record(res_received_message,{message_id, from_user_name,from_fullname, message, datetime}).
-record(res_confirm_received_message,{}).
-record(res_message_offline,{list_message}).
-record(res_received_friend_request,{request_id, from_user, fullname}).

-record(res_send_notification, {body, title}).
-record(res_other_user_accept_friend_request,{username,fullname}).
-record(res_list_friend,{list_encoded}).
-record(res_notice_friend_online,{username}).
-record(res_notice_friend_offline,{username}).

-define(API_DONE,							16#0000).
-define(API_PARAMETER_FAIL, 				16#0001).
-define(API_SYSTEM_FAIL, 					16#0002).
-define(API_BAD_MATCH,						16#0003).
-define(API_EMPTY, 							16#0004).

-define(API_USER_LOGIN_FAIL,				16#1001).
-define(API_USER_NOT_FOUND,					16#1002).
-define(API_USER_AUTH_TOKEN_FAIL, 			16#1003).
-define(API_USER_EXIST, 					16#1004).

-define(API_SEND_MESSAGE_USERNAME_FAIL,		16#2001).
-define(API_CONFIRM_MESSAGE_NOT_OF_YOU, 	16#2002).

-define(API_FRIEND_REQUEST_EXIST, 			16#3001).
-define(API_ALREADY_FRIEND,					16#3002).
-define(API_FRIEND_REQUEST_NOT_FOUND, 		16#3003).
