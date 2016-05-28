%% @author thanhvu
%% @doc @todo Add description to cs_db.


-module(cs_db).
-include("db.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([create_ets/0,create_db/0,read/2,index_read/3,write/2,read_last/1,read_all/1,delete/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

create_db() ->
%% 	case catch mnesia:table_info(tbl_users, all) of
%% 		{'EXIT', _} ->		% If table not exists
%% 			{atomic, ok} = mnesia:create_table(tbl_users, [{disc_copies, [node()]},{index,[email]},{type,ordered_set},{attributes, record_info(fields, tbl_users)}]);
%% 		_ -> created
%% 	end,
	case catch mnesia:table_info(tbl_token, all) of
		{'EXIT', _} -> 
			{atomic, ok} = mnesia:create_table(tbl_token, [{disc_copies, [node()]},{attributes, record_info(fields, tbl_token)}]);
		_ -> created
	end,
	case catch mnesia:table_info(tbl_message, all) of
		{'EXIT', _} -> 
			{atomic, ok} = mnesia:create_table(tbl_message, [{disc_copies, [node()]},{index,[to_user]},{type,ordered_set},{attributes, record_info(fields, tbl_message)}]);
		_ -> created
	end,
	ok.

create_ets() ->
	ets:new(tbl_config, [named_table, public]),
	ets:new(tbl_user_onl, [named_table, public,{keypos, 2}]).

read(Tab, Id) ->
	case catch mnesia:dirty_read(Tab, Id) of
		{'EXIT', Reason} -> {error, Reason};
		R -> R
	end.

index_read(Tab, Id, IndexPos) ->
	case catch mnesia:dirty_index_read(Tab, Id, IndexPos) of
		{'EXIT', Reason} -> {error, Reason};
		R -> R
	end.

write(Tab,Id) ->
	case catch mnesia:dirty_write(Tab, Id) of
		{'EXIT', Reason} -> {error, Reason};
		ok -> ok
	end.

read_last(Tab) ->
	case catch mnesia:dirty_last(Tab) of
		{'EXIT', Reason} -> {error, Reason};
		R -> R
	end.

read_all(Tab) ->
	case catch ets:tab2list(Tab) of
		{'EXIT', Reason} -> {error, Reason};
		R -> R
	end.

delete(Tab, Id) ->
	case catch mnesia:dirty_delete(Tab, Id) of
		{'EXIT', Reason} -> {error, Reason};
		ok -> ok
	end.