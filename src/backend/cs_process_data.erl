%% @author thanhvu
%% @doc @todo Add description to cs_process_data.


-module(cs_process_data).

%% ====================================================================
%% API functions
%% ====================================================================
-export([process_data/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

process_data(Bin,Socket) ->
	cs_decode:decode(Bin),
	Res = <<"xxx">>,
	Len = byte_size(Res),
	gen_tcp:send(Socket, <<Len:16,Res/binary>>),
ok.
