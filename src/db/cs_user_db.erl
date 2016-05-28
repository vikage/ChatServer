%% @author thanhvu
%% @doc @todo Add description to cs_user_db.


-module(cs_user_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([new_user/1,user_info/1,update_info/1,query_update_user/1]).
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

%% ====================================================================
%% Interface
%% ====================================================================

new_user(U) ->
	Date = util:get_current_date_time(),
	NewU = U#tbl_users{create_date = Date},
	call(#db_user_new{user = NewU}).
user_info(Uid) ->
	call(#db_user_info{username = Uid}).
update_info(U) ->
	call(#db_user_update{user = U}).


call(Data) ->
	Req = #db_request{data = Data},
	gen_server:call(?MODULE, {db_query,Req}).

%% ====================================================================
%% DB Process
%% ====================================================================
process(#db_user_new{user = #tbl_users{username = undefined}}) ->
	#db_res{error = ?DB_REQ_PARAMETER_FAIL};
process(#db_user_new{user = #tbl_users{password = undefined}}) ->
	#db_res{error = ?DB_REQ_PARAMETER_FAIL};
process(#db_user_new{user = U = #tbl_users{username = Username}}) ->
	case query_username(Username) of
		#tbl_users{} -> #db_res{error = ?DB_ITEM_EXIST};
		not_found -> 
			NewU = U#tbl_users{},
			case query_new_user(NewU) of
				ok -> #db_res{result = NewU};
				{error, Reason} -> #db_res{reason = Reason}
			end
	end;
process(#db_user_info{username = Username}) ->
	case query_username(Username) of
		not_found -> #db_res{error = ?DB_NOT_FOUND};
		User = #tbl_users{} -> #db_res{result = User};
		{error,Reason} -> #db_res{reason = Reason, error = ?DB_SYS_ERROR}
	end;
process(#db_user_update{user = U = #tbl_users{username = Username}}) ->
	case query_username(Username) of
		not_found -> #db_res{error = ?DB_NOT_FOUND};
		CurrentU = #tbl_users{} ->
			NewU = list_to_tuple(util:merge(tuple_to_list(CurrentU), tuple_to_list(U))),
			case query_update_user(NewU) of
				ok -> #db_res{result = NewU};
				{error, Reason} ->
					#db_res{reason = Reason, error = ?DB_SYS_ERROR}
			end
	end;
process(Request) ->
	{error, badmatch}.

query_new_user(User) ->
	case mysql:query(whereis(mysql), "INSERT INTO tbl_user VALUES(?,?,?,?,?,?)",
							 [
								User#tbl_users.username,
								User#tbl_users.password,
								User#tbl_users.fullname,
								User#tbl_users.phone,
								User#tbl_users.email,
								User#tbl_users.create_date]) of
				ok -> ok;
				{error, Reason} -> {error,Reason}
	end.

query_update_user(User) ->
	case mysql:query(whereis(mysql), "UPDATE tbl_user SET password = ?, fullname = ?, phone = ?, email = ? WHERE username = ?",
					 [User#tbl_users.password,
					  User#tbl_users.fullname,
					  User#tbl_users.phone,
					  User#tbl_users.email,
					  User#tbl_users.username]) of
		ok -> ok;
		{error, Reason} -> {error, Reason}
	end.

query_username(UserName) ->
	case mysql:query(whereis(mysql), "SELECT * FROM tbl_user WHERE username = ?",[UserName]) of
	{ok, _Fields, _Rows = [U|_]} ->
		list_to_tuple([tbl_users|U]);
	_ -> not_found
	end.
