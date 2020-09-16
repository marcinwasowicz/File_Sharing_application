%%%-------------------------------------------------------------------
%%% @author marci
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. lip 2020 10:06
%%%-------------------------------------------------------------------
-module(file_management).
-author("marci").

%% API
-export([download_file/3, overwrite_file/3, apply_fun_to_file/4]).

download_file(Host_Node, Host_File, Local_File)->
    Transfer = fun()->
        {_, File} = rpc:call(Host_Node, file, read_file, [Host_File]),
        file:write_file(Local_File, [File]),
        io:format("===== File Transfer Succesfull =====\n")
               end,
    spawn(Transfer).

overwrite_file(Host_Node, Host_File, Local_File)->
    Transfer = fun()->
        {_, File} = file:read_file(Local_File),
        rpc:call(Host_Node, file, write_file, [Host_File, [File]]),
        io:format("===== File Transfer Succesfull =====\n")
               end,
    spawn(Transfer).

apply_fun_to_file(Host_Node, Host_File, An_Fun, Args_List)->
    Apply = fun()-> An_Fun(Host_File, Args_List) end,
    spawn(Host_Node, Apply).
