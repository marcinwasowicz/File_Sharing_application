%%%-------------------------------------------------------------------
%%% @author marci
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(file_app_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = #{id => file_app_server,
        start => {file_app_server, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [file_app_server]},

    {ok, {#{strategy => one_for_one,
        intensity => 5,
        period => 30},
        [AChild]}
    }.
