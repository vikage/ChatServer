-record(request,{group, type, req_id, data}).
-record(cmd_login,{username,password}).
-record(cmd_user_info,{username}).
-record(cmd_user_auth,{token}).

-define(GROUP_USER,<<"user">>).
-define(TYPE_LOGIN,<<"login">>).
-define(TYPE_USER_INFO,<<"user_info">>).
-define(TYPE_USER_AUTH,<<"user_auth">>).