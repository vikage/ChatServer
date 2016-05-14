-record(tbl_users,{uid, username,password,fullname,phone,email, create_date}).
-record(tbl_config,{key,val}).
-record(tbl_token,{token_string,uid, create_date}).
-record(tbl_user_onl,{username,pid}).
-record(tbl_message,{message_id, message, from_user, to_user, datetime}).


-record(db_request, {name,data}).
-record(db_res,{error = 0,reason,result}).

-define(DB_DONE,				16#0).
-define(DB_NOT_FOUND,			16#0001).
-define(DB_ITEM_EXIST,			16#0002).
-define(DB_REQ_PARAMETER_FAIL, 	16#0003).
-define(DB_SYS_ERROR,			16#0004).

-record(db_user_new,{user}).
-record(db_user_info,{uid}).
-record(db_user_info_username,{username}).
-record(db_user_update,{user}).

-record(db_token_new,{token}).
-record(db_token_get,{token_string}).

-record(db_message_new, {messageObj}).
-record(db_message_delete,{message_id}).
-record(db_message_get,{message_id}).