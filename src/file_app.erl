%%%-------------------------------------------------------------------
%%% @author marci
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(file_app).

-behaviour(application).

-export([start/0, start/2, stop/0, stop/1]).

start()->
    application:start(?MODULE).

start(_StartType, _StartArgs) ->
    mnesia:start(),
    file_app_sup:start_link().

stop()->
    application:stop(?MODULE).

stop(_State) ->
    ok.
