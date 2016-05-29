-record(request,{group, type, req_id, data}).
-record(cmd_login,{username,password}).
-record(cmd_user_info,{username}).
-record(cmd_user_auth,{token}).
-record(cmd_send_message,{token, to_user_name, message}).
-record(cmd_confirm_received_message,{token, message_id}).
-record(cmd_confirm_received_offline_message,{token}).

-record(cmd_add_friend, {token, to_user}).

-define(GROUP_USER,<<"user">>).
-define(GROUP_CHAT,<<"chat">>).
-define(GROUP_FRIEND, <<"friend">>).

-define(TYPE_LOGIN,<<"login">>).
-define(TYPE_USER_INFO,<<"user_info">>).
-define(TYPE_USER_AUTH,<<"user_auth">>).
-define(TYPE_SEND_MESSAGE,<<"send_message">>).
-define(TYPE_RECEIVED_MESSAGE,<<"received_message">>).
-define(TYPE_CONFIRM_RECEIVED_MESSAGE,<<"confirm_received_message">>).
-define(TYPE_SEND_OFFLINE_MESSAGE,<<"send_offline_message">>).
-define(TYPE_CONFIRM_RECEIVED_OFFLINE_MESSAGE,<<"confirm_received_offline_message">>).
-define(TYPE_ADD_FRIEND, <<"add_friend">>).
-define(TYPE_RECEIVED_FRIEND_REQUEST,<<"received_friend_request">>).