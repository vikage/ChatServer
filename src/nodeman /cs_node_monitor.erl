%% @author thanhvu
%% @doc @todo Add description to cs_node_monitor.


-module(cs_node_monitor).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("cs.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,install_nodes/1,get_db_nodes/0]).
	
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {nodes=[], db_node=[], cs_node=[], disconnected=[], timer_ref}).


install_nodes(NodeList) ->
	gen_server:cast(?MODULE, {handle_install_node,NodeList}).

get_db_nodes() ->
	gen_server:call(?MODULE, {get_db_node}).
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
	Nodes = cs_config:get_config(ping, []),
	gen_server:cast(?MODULE, {handle_install_node, Nodes}),
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
handle_call({get_db_node}, _From, State) ->
	R = [NodeName || {?DB_NODE,NodeName} <- State#state.db_node],
	{reply, R, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_ping([Node|Tail], State) ->
	{NodeType, NodeName} = Node,
	NewState = case net_adm:ping(NodeName) of
					pong ->
						erlang:monitor_node(NodeName, true),
						case NodeType of
							?BACKEND_NODE 	-> State#state{cs_node = [Node|State#state.cs_node], disconnected = State#state.disconnected -- [Node]};
							?DB_NODE 		-> State#state{db_node = [Node|State#state.db_node], disconnected = State#state.disconnected -- [Node]}
						end;
					pang ->
						State
			   end,
	handle_ping(Tail, NewState);
handle_ping([],State) ->
	State.

findNode(_NodeName, []) -> undefined;
findNode(NodeName, [{_,NodeName} = Node | _]) -> Node;
findNode(NodeName,[_Node|Tail]) -> findNode(NodeName, Tail).


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
handle_cast(retry_connect, State) ->
	io:format("Retry with state ~p~n",[State]),
	NewState = handle_ping(State#state.disconnected, State),
	if 
		length(NewState#state.disconnected) == 0 ->
			timer:cancel(State#state.timer_ref),
			{noreply, NewState#state{timer_ref = undefined}};		
		true -> 
			{noreply, NewState}
	end;
handle_cast({handle_install_node,ListNode}, State) ->
	NewState = handle_ping(ListNode ++ State#state.disconnected, State#state{disconnected = ListNode ++ State#state.disconnected}),
	if 
		length(NewState#state.disconnected) == 0 ->
			{noreply, NewState#state{nodes = ListNode ++ NewState#state.nodes}};
		true -> 
			{ok,TimeRef} = timer:send_interval(1000, {'$gen_cast', retry_connect}),
			{noreply, NewState#state{timer_ref = TimeRef, nodes = State#state.nodes ++ ListNode}}
	end;
handle_cast(_Msg, State) ->
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
handle_info({nodedown,NodeName}, State) ->
	timer:cancel(State#state.timer_ref),
	{ok,TimeRef} = timer:send_interval(1000, {'$gen_cast', retry_connect}),
	Node = findNode(NodeName, State#state.nodes),
	{NodeType,NodeName} = Node,
	NewState = 	case NodeType of
					?DB_NODE -> State#state{disconnected = [Node|State#state.disconnected], db_node = State#state.db_node -- [Node]};
					?BACKEND_NODE -> State#state{disconnected = [Node|State#state.disconnected], cs_node = State#state.cs_node -- [Node]}
				end,
	{noreply, NewState#state{timer_ref = TimeRef}};
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


