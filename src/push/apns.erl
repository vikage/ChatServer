%% @author ThanhVu
%% @doc @todo Add description to apns.


-module(apns).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,send_device/2,quick_send/2]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {socket, address, port, option,timeout}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
	Address = "gateway.sandbox.push.apple.com",
  	Port = 2195,
  	Cert = "/etc/certs/cert.pem",% "apns-cert.pem",
  	Key  = "/etc/certs/key.pem", % "apns-key.pem",
  	Options = [{certfile, Cert}, {keyfile, Key}, {mode, binary}, {verify, verify_none}],
  	Timeout = 3000,
  	case ssl:connect(Address, Port, Options, Timeout) of
    	{ok, Socket} ->
      		{ok, #state{socket = Socket, address = Address, port = Port, option = Options, timeout = Timeout}};
    	{error, Reason} ->
      		{stop, Reason}
  	end.

quick_send(Msg,DeviceToken) ->
	NewMsg = if is_binary(Msg) -> binary_to_list(Msg); true -> Msg end,
	NewDevice = if(is_binary(DeviceToken)) -> binary_to_list(DeviceToken); true -> DeviceToken end,
	%lager:debug("sent: ~s to ~s~n", [NewMsg, NewDevice]),
	send_pn([{alert, NewMsg}, {badge, 1}, {sound, "chime"}], NewDevice).

send_pn(Msg, Device) ->
  Address = "gateway.sandbox.push.apple.com",
  Port = 2195,
  Cert = "/etc/certs/cert.pem",% "apns-cert.pem",
  Key  = "/etc/certs/key.pem", % "apns-key.pem",
  Options = [{certfile, Cert}, {keyfile, Key}, {mode, binary}, {verify, verify_none}],
  Timeout = 3000,
  Bin = hexstr_to_bin(Device),
  case ssl:connect(Address, Port, Options, Timeout) of
    {ok, Socket} ->
      PayloadString = create_json(Msg),
      Payload = list_to_binary(PayloadString),
      PayloadLength = size(Payload),
      Packet = <<0:8,
      32:16/big,
      Bin/binary,
      PayloadLength:16/big,
      Payload/binary>>,
      ssl:send(Socket, Packet),
      ssl:close(Socket),
      PayloadString;
    {error, Reason} ->
      Reason
  end.

send_device(Msg, DeviceToken) ->
	NewMsg = if is_binary(Msg) -> binary_to_list(Msg); true -> Msg end,
	NewDevice = if(is_binary(DeviceToken)) -> binary_to_list(DeviceToken); true -> DeviceToken end,
	%lager:debug("sent: ~s to ~s~n", [NewMsg, NewDevice]),
	Bin = hexstr_to_bin(NewDevice),
	PayloadString = create_json([{alert, NewMsg}, {badge, 1}, {sound, "mario.caf"}]),
    Payload = list_to_binary(PayloadString),
    PayloadLength = size(Payload),
    Packet = <<0:8,
    32:16/big,
    Bin/binary,
    PayloadLength:16/big,
    Payload/binary>>,
    gen_server:cast(?MODULE, {push,Packet}).



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
handle_call(_Request, _From, State) ->
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
handle_cast({push,Packet},State = #state{socket = Socket}) ->
	ssl:send(Socket, Packet),
	{noreply, State};
handle_cast(Msg, State) ->
	io:format("Unknow cast~p~n",[Msg]),
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
handle_info({ssl_closed, _Socket}, State = #state{address = Address, port = Port, option = Options, timeout = Timeout}) ->
	lager:info("APNS Socket disconnected"),
	case ssl:connect(Address, Port, Options, Timeout) of
    	{ok, NSocket} ->
			lager:info("APNS socket reconnected"),
      		{noreply, State#state{socket = NSocket}};
    	{error, Reason} ->
      		{stop, Reason}
  	end;
handle_info(Info, State) ->
	io:format("Unknow info~p~n",[Info]),
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

% helper for creating json
create_json(List) ->
  lists:append(["{\"aps\":{", create_keyvalue(List), "}}"]).

create_keyvalue([Head]) ->
  create_pair(Head);
create_keyvalue([Head|Tail]) ->
  lists:append([create_pair(Head), ",", create_keyvalue(Tail)]).

create_pair({Key, Value}) ->
	lists:append([add_quotes(atom_to_list(Key)), ":", add_quotes(Value)]).
add_quotes(String) when is_integer(String) ->
	integer_to_list(String);
add_quotes(String) when is_float(String) ->
	float_to_list(String);
add_quotes(String) ->
	lists:append(["\"", String, "\""]).
hexstr_to_bin(S) ->
   hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
   list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
   {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
   hexstr_to_bin(T, [V | Acc]).
