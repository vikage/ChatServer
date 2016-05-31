%% @author ThanhVu
%% @doc @todo Add description to cs_friend_db.


-module(cs_friend_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,query_insert/1,new_friend/1,unfriend/2,get/2,get_list_friend/2,get_list_friend_all/1]).
-export([query_get/1]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({db_query, Request}, From, State) ->
	R = process(Request#db_request.data),
	{reply, R, State};
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


new_friend(FO) ->
	NFO = FO#tbl_friend{datetime = util:get_current_date_time()},
	call(#db_friend_add{friend_obj = NFO}).
	
unfriend(User1,User2) ->
	call(#db_friend_remove{user1 = User1, user2 = User2}).

get(User1,User2) ->
	call(#db_friend_get{user1 = User1, user2 = User2}).
  

get_list_friend(UserName, Page) ->
	call(#db_friend_get_list{username = UserName, page = Page}).

get_list_friend_all(UserName) ->
	call(#db_friend_get_all_friend{username = UserName}).

call(Data) ->
	Req = #db_request{data = Data},
	gen_server:call(?MODULE, {db_query,Req}).

%% ====================================================================
%% Internal functions
%% ====================================================================
process(#db_friend_add{friend_obj = FO = #tbl_friend{friend_id = FId}}) ->
	case query_get(FId) of
		#tbl_friend{} -> #db_res{error = ?DB_ITEM_EXIST, reason = <<"Already friend">>};
		not_found ->
			case query_insert(FO) of
				ok -> #db_res{result = FO};
				{error, Reason} -> #db_res{error = ?DB_SYS_ERROR, reason = Reason}
			end
	end;
process(#db_friend_remove{user1 = User1, user2 = User2}) ->
	case query_delete(User1,User2) of
		ok -> #db_res{};
		{error, Reason} -> #db_res{error = ?DB_SYS_ERROR, reason = Reason}
	end;
process(#db_friend_get{user1 = User1, user2 = User2}) ->
	case query_get2(User1, User2) of
		F = #tbl_friend{} -> #db_res{result = F};
		not_found ->
			#db_res{error = ?DB_NOT_FOUND}
	end;
process(#db_friend_get_list{username = UserName, page = Page}) ->
	case query_get_list_friend(UserName, Page) of
		not_found -> #db_res{error = ?DB_EMPTY};
		List = [_|_] ->
			#db_res{result = List}
	end;
process(#db_friend_get_all_friend{username = UserName}) ->
	case query_get_all_friend(UserName) of
		not_found -> #db_res{error = ?DB_EMPTY};
		List = [_|_] ->
			#db_res{result = List}
	end;
process(_Request) ->
	{error, badmatch}.

query_get(FriendId) ->
	case mysql:query(whereis(mysql), "SELECT * FROM tbl_friend WHERE friend_id = ?",[FriendId]) of
	{ok, _Fields, _Rows = [FO|_]} ->
		list_to_tuple([tbl_friend|FO]);
	_ -> not_found
	end.

query_get2(User1,User2) ->
	Id1 = <<User1/binary,<<",">>/binary, User2/binary>>,
	Id2 = <<User2/binary,<<",">>/binary, User1/binary>>,
	case mysql:query(whereis(mysql), "SELECT * FROM tbl_friend WHERE friend_id = ? OR friend_id = ?",[Id1,Id2]) of
	{ok, _Fields, _Rows = [FO|_]} ->
		list_to_tuple([tbl_friend|FO]);
	_ -> not_found
	end.

query_insert(FO) ->
	case mysql:query(whereis(mysql), "INSERT INTO tbl_friend VALUES(?,?,?,?)",[FO#tbl_friend.friend_id,
																		  FO#tbl_friend.user1,
																		  FO#tbl_friend.user2,
																		  FO#tbl_friend.datetime]) of
		ok -> ok;
		{error, Reason} -> {error, Reason};
		Error -> {error, Error}
	end.

query_delete(User1,User2) ->
	Id1 = <<User1/binary,<<",">>/binary, User2/binary>>,
	Id2 = <<User2/binary,<<",">>/binary, User1/binary>>,
%% 	io:format("Id1: ~p, ID2: ~p~n",[Id1,Id2]),
	case mysql:query(whereis(mysql), "DELETE FROM tbl_friend WHERE friend_id = ? OR friend_id = ?",[Id1,Id2]) of
		ok -> ok;
		{error, Reason} -> {error, Reason};
		Error -> {error, Error}
	end.

query_get_list_friend(UserName, Page) ->
	Limit = 20 * (Page - 1),
	case mysql:query(whereis(mysql), "SELECT user1,user2,avatar,fullname FROM (SELECT friend_id,user1,user2,avatar,fullname FROM tbl_friend INNER JOIN tbl_user ON `tbl_friend`.user2 = tbl_user.`username` 
										WHERE user1 = ?
										UNION
										SELECT friend_id,user1,user2,avatar,fullname FROM tbl_friend INNER JOIN tbl_user ON `tbl_friend`.user1 = tbl_user.`username` 
										WHERE user2 = ?) AS U LIMIT ?,20",[UserName,UserName, Limit]) of
	{ok, _Fields, Rows = [_|_]} ->
		[list_to_tuple([mysql_friend_item|R]) || R <- Rows];
	_ -> not_found
	end.

query_get_all_friend(UserName) ->
	case mysql:query(whereis(mysql), "SELECT user1,user2,avatar,fullname FROM (SELECT friend_id,user1,user2,avatar,fullname FROM tbl_friend INNER JOIN tbl_user ON `tbl_friend`.user1 = tbl_user.`username` 
										WHERE user2 = ?
										UNION
										SELECT friend_id,user1,user2,avatar,fullname FROM tbl_friend INNER JOIN tbl_user ON `tbl_friend`.user1 = tbl_user.`username` 
										WHERE user1 = ?) AS U",[UserName,UserName]) of
	{ok, _Fields, Rows = [_|_]} ->
		[list_to_tuple([mysql_friend_item|R]) || R <- Rows];
	_ -> not_found
	end.