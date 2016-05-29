-record(response,{group,type,result,req_id,data}).

-record(res_login, {token}).
-record(res_user_info,{username,fullname,email,phone}).
-record(res_user_auth,{}).
-record(res_send_message,{time}).
-record(res_received_message,{message_id, from_user_name, message, datetime}).
-record(res_confirm_received_message,{}).
-record(res_message_offline,{list_message}).
-record(res_received_friend_request,{request_id, from_user}).

-define(API_DONE,16#0000).
-define(API_PARAMETER_FAIL, 16#0001).
-define(API_SYSTEM_FAIL, 16#0002).

-define(API_USER_LOGIN_FAIL,16#1001).
-define(API_USER_NOT_FOUND,	16#1002).
-define(API_USER_AUTH_TOKEN_FAIL, 16#1003).

-define(API_SEND_MESSAGE_USERNAME_FAIL,16#2001).
-define(API_CONFIRM_MESSAGE_NOT_OF_YOU, 16#2002).

-define(API_ADD_FIEND_EXIST, 16#3001).