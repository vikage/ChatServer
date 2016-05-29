%% @author ThanhVu
%% @doc @todo Add description to cs_friend_request_db.


-module(cs_friend_request_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([query_insert/1,query_get/1,query_delete/1]).
-export([new_request/1, remove_request/1,get_request/1]).

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

new_request(FRO) ->
	NFRO = FRO#tbl_friend_request{datetime = util:get_current_date_time()},
	call(#db_friend_request_add{request = NFRO}).

remove_request(RId) ->
	call(#db_friend_request_remove{request_id = RId}).

get_request(RId) ->
	call(#db_friend_request_get{request_id = RId}).

call(Data) ->
	Req = #db_request{data = Data},
	gen_server:call(?MODULE, {db_query,Req}).

%% ====================================================================
%% Internal functions
%% ====================================================================

process(#db_friend_request_add{request = FRO = #tbl_friend_request{request_id = RId}}) ->
	case query_get(RId) of
		#tbl_friend_request{} -> #db_res{error = ?DB_ITEM_EXIST, reason = <<"Already request">>};
		not_found ->
			case query_insert(FRO) of
				ok -> #db_res{result = FRO};
				{error, Reason} -> #db_res{error = ?DB_SYS_ERROR, reason = Reason}
			end
	end;
process(#db_friend_request_remove{request_id = RId}) ->
	case query_delete(RId) of
		ok -> #db_res{};
		{error, Reason} -> #db_res{error = ?DB_SYS_ERROR, reason = Reason}
	end;
process(#db_friend_request_get{request_id = RId}) ->
	case query_get(RId) of
		FRO = #tbl_friend_request{} ->
			#db_res{result = FRO};
		not_found ->
			#db_res{error = ?DB_NOT_FOUND, reason = <<"NOT FOUND REQUEST FRIEND">>}
	end;
process(_Request) ->
	{error, badmatch}.

query_insert(FRO) ->
	case mysql:query(whereis(mysql), "INSERT INTO tbl_friend_request VALUES(?,?,?,?)",
					 [FRO#tbl_friend_request.request_id,
					  FRO#tbl_friend_request.from_user,
					  FRO#tbl_friend_request.to_user,
					  FRO#tbl_friend_request.datetime]) of
		ok -> ok;
		{error, Reason} -> {error, Reason};
		Error -> {error, Error}
	end.

query_get(RequestId) ->
	case mysql:query(whereis(mysql), "SELECT * FROM tbl_friend_request WHERE request_id = ?",[RequestId]) of
	{ok, _Fields, _Rows = [FO|_]} ->
		list_to_tuple([tbl_friend_request|FO]);
	_ -> not_found
	end.

query_delete(RId) ->
	case mysql:query(whereis(mysql), "DELETE FROM tbl_friend_request WHERE request_id = ?",[RId]) of
		ok -> ok;
		{error, Reason} -> {error, Reason};
		Error -> {error, Error}
	end.