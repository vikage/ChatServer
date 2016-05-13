-record(response,{group,type,result,req_id,data}).

-record(res_login, {token}).
-record(res_user_info,{username,fullname,email,phone}).
-record(res_user_auth,{}).

-define(API_DONE,16#0000).
-define(API_PARAMETER_FAIL, 16#0001).
-define(API_SYSTEM_FAIL, 16#0002).

-define(API_USER_LOGIN_FAIL,16#1001).
-define(API_USER_NOT_FOUND,	16#1002).
-define(API_USER_AUTH_TOKEN_FAIL, 16#1003).