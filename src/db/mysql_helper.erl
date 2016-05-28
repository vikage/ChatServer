%% @author ThanhVu
%% @doc @todo Add description to mysql_helper.


-module(mysql_helper).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).


start_link() ->
	{ok, SqlPid} = mysql:start_link([{host, "localhost"}, {user, "root"},{password,"thanh"},{database,"cs"}]),
	register(mysql, SqlPid),
	{ok, SqlPid}.
%% ====================================================================
%% Internal functions
%% ====================================================================


