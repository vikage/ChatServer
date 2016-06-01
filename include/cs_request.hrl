-record(request,{group, type, req_id, data}).
-record(cmd_login,{username,password}).
-record(cmd_register,{username, password, email, phone, fullname}).
-record(cmd_user_info,{username, token}).
-record(cmd_me_info,{token}).
-record(cmd_update_avatar,{token,avatar}).
-record(cmd_user_auth,{token}).
-record(cmd_search_user,{token,keyword,page}).

-record(cmd_send_message,{token, to_user_name, message}).
-record(cmd_confirm_received_message,{token, message_id}).
-record(cmd_confirm_received_offline_message,{token}).

-record(cmd_add_friend, {token, to_user}).
-record(cmd_accept_friend_request,{token, from_user}).
-record(cmd_reject_friend_request,{token, from_user}).
-record(cmd_get_list_friend,{token, page}).
-record(cmd_get_list_friend_request,{token,page}).


-define(GROUP_USER,<<"user">>).
-define(GROUP_CHAT,<<"chat">>).
-define(GROUP_FRIEND, <<"friend">>).
-define(GROUP_NOTIFICATION,<<"notification">>).

-define(TYPE_LOGIN,<<"login">>).
-define(TYPE_REGISTER,<<"register">>).
-define(TYPE_USER_INFO,<<"user_info_status">>).
-define(TYPE_ME_INFO,<<"me_info">>).
-define(TYPE_USER_AUTH,<<"user_auth">>).
-define(TYPE_UPDATE_AVATAR,<<"update_avatar">>).
-define(TYPE_SEARCH_USER,<<"search_user">>).

-define(TYPE_SEND_MESSAGE,<<"send_message">>).
-define(TYPE_RECEIVED_MESSAGE,<<"received_message">>).
-define(TYPE_CONFIRM_RECEIVED_MESSAGE,<<"confirm_received_message">>).
-define(TYPE_SEND_OFFLINE_MESSAGE,<<"send_offline_message">>).
-define(TYPE_CONFIRM_RECEIVED_OFFLINE_MESSAGE,<<"confirm_received_offline_message">>).
-define(TYPE_ADD_FRIEND, <<"add_friend">>).
-define(TYPE_RECEIVED_FRIEND_REQUEST,<<"received_friend_request">>).
-define(TYPE_ACCEPT_FRIEND_REQUEST,<<"accept_friend_request">>).
-define(TYPE_REJECT_FRIEND_REQUEST,<<"reject_friend_request">>).
-define(TYPE_GET_LIST_FRIEND_REQUEST,<<"get_list_friend_request">>).
-define(TYPE_SEND_NOTIFICATION,<<"send_notification">>).
-define(TYPE_GET_LIST_FRIEND,<<"get_list_friend">>).
-define(TYPE_NOTICE_FRIEND_ONLINE,<<"notice_friend_online">>).
-define(TYPE_NOTICE_FRIEND_OFFLINE,<<"notice_friend_offline">>).

