-record(request,{group, type, req_id, data}).
-record(cmd_login,{username,password}).


-define(GROUP_USER,<<"user">>).
-define(TYPE_LOGIN,<<"login">>).