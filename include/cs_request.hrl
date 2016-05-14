-record(request,{group, type, req_id, data}).
-record(cmd_login,{username,password}).
-record(cmd_user_info,{username}).
-record(cmd_user_auth,{token}).
-record(cmd_send_message,{token, to_user_name, message}).

-define(GROUP_USER,<<"user">>).
-define(GROUP_CHAT,<<"chat">>).

-define(TYPE_LOGIN,<<"login">>).
-define(TYPE_USER_INFO,<<"user_info">>).
-define(TYPE_USER_AUTH,<<"user_auth">>).
-define(TYPE_SEND_MESSAGE,<<"send_message">>).
-define(TYPE_RECEIVED_MESSAGE,<<"received_message">>).