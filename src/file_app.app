%%%-------------------------------------------------------------------
%%% @author marci
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. lip 2020 18:31
%%%-------------------------------------------------------------------
{application, file_app, [
    {description, ""},
    {vsn, "1"},
    {registered, [file_app_server, file_app_sup]},
    {applications, [
        kernel,
        stdlib
    ]},
    {mod, {file_app, []}},
    {env, []}
]}.