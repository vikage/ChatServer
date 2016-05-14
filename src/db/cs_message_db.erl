%% @author thanhvu
%% @doc @todo Add description to cs_message_db.


-module(cs_message_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,new_message/1,delete_message/1,get_message/1]).



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

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE	, [], []).

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


new_message(MessageObj) ->
	Date = util:get_date_time(os:timestamp()),
	call(#db_message_new{messageObj = MessageObj#tbl_message{datetime = Date}}).
delete_message(MessageId) ->
	call(#db_message_delete{message_id = MessageId}).
get_message(MessageId) ->
	call(#db_message_get{message_id = MessageId}).
	
call(Data) ->
	Req = #db_request{data = Data},
	gen_server:call(?MODULE, {db_query,Req}).
%% ====================================================================
%% Internal functions
%% ====================================================================
process(#db_message_new{messageObj = MessageObj}) ->
	case cs_db:read_last(tbl_message) of
		'$end_of_table' ->
			NewMessage = MessageObj#tbl_message{message_id = 1},
			cs_db:write(tbl_message, NewMessage),
			#db_res{result = NewMessage};
		CurrentId ->
			NewMessage = MessageObj#tbl_message{message_id = CurrentId + 1},
			cs_db:write(tbl_message, NewMessage),
			#db_res{result = NewMessage}
	end;
process(#db_message_delete{message_id = Msg_id}) ->
	case cs_db:delete(tbl_message, Msg_id) of
		{error, Reason} -> #db_res{reason = Reason, error = ?DB_SYS_ERROR};
		ok -> #db_res{}
	end;
process(#db_message_get{message_id = Msg_id}) ->
	case cs_db:read(tbl_message, Msg_id) of
		{error, Reason} -> #db_res{reason = Reason, error = ?DB_SYS_ERROR};
		[] -> #db_res{error = ?DB_NOT_FOUND};
		[Message = #tbl_message{}] ->
			#db_res{result = Message}
	end;
process(_Request) ->
	{error, badmatch}.