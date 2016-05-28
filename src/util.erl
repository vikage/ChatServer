%% @author thanhvu
%% @doc @todo Add description to util.


-module(util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([merge/2,get_date_time/1,get_time_stamp_integer/1,get_current_date_time/0]).


%% ====================================================================
%% Internal functions
%% ====================================================================

merge([],[]) ->
	[];
merge([H1|T1],[H2|T2]) ->
	if
		H2 == undefined ->
			[H1|merge(T1,T2)];
		true ->
			[H2|merge(T1,T2)]
	end.
get_date_time(TimeStamp) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_local_time(TimeStamp),
	list_to_binary(lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Day,Month,Year,Hour,Min,Sec]))).

get_current_date_time()->
	get_date_time(erlang:timestamp()).

get_time_stamp_integer(TimeStamp) ->
	{MegaSec, Sec,_} = TimeStamp,
	MegaSec*1000000 + Sec.