%%%-------------------------------------------------------------------
%%% @author marci
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. lip 2020 15:08
%%%-------------------------------------------------------------------
-module(file_database).
-author("marci").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([create_file_database/1, init_file_database/1, add_file/2, delete_file/1]).
-export([find_file/1, get_file_node/1, change_file_owner/2, rename_file/2, list_node_files/1]).
-export([search_by_prefix/1, wake_up_schema/0, close_schema/0]).

-record(file_data, {file_name, node_name}).

wake_up_schema()->
    rpc:multicall(mnesia:system_info(db_nodes), file_app, start, []).

close_schema()->
    rpc:multicall(mnesia:system_info(db_nodes), file_app, stop, []).

init_file_database(Nodes)->
    try
        mnesia:table_info(type, file_data)
    catch
        exit: _ ->
            create_file_database(Nodes)
    end.

create_file_database(Nodes)->
    mnesia:create_table(file_data,
        [{attributes, record_info(fields, file_data)}, {type, set}, {disc_copies, Nodes}]).

add_file(File, Node)->
    Add = fun()->
        case mnesia:read({file_data, File}) of
            [#file_data{file_name = _Name, node_name = _Node}]
                -> {error, "file already exists"};
            _ -> mnesia:write(#file_data{file_name = File, node_name = Node})
        end
          end,
    mnesia:transaction(Add).

delete_file(File)->
    Delete = fun()->
        case mnesia:read({file_data, File}) of
            [#file_data{file_name = _Name, node_name = _Node}]
                -> mnesia:delete({file_data, File});
            _ -> {error, "file does not exists"}
        end
             end,
    mnesia:transaction(Delete).

find_file(File)->
    Get = fun()->
        mnesia:read({file_data, File})
          end,
    {_, List} = mnesia:transaction(Get),
    List.

get_file_node(File)->
    Data = find_file(File),
    case Data of
        [#file_data{file_name = _Name, node_name = Node}] -> [Node];
        _ -> []
    end.

change_file_owner(File, New_Host)->
    Data = find_file(File),
    case Data of
        [#file_data{file_name = File, node_name = _Old_Host}] ->
            mnesia:transaction(
                fun()->
                    mnesia:write(#file_data{file_name = File, node_name = New_Host})
                end);
        _ -> {error, "file does not exists"}
    end.

rename_file(Old_Name, New_Name)->
    Data = find_file(Old_Name),
    case Data of
        [#file_data{file_name = Old_Name, node_name = Node}] ->
            mnesia:transaction(
                fun()->
                    mnesia:delete({file_data, Old_Name}),
                    mnesia:write(#file_data{file_name = New_Name, node_name = Node})
                end);
        _ -> {error, "file does not exists"}
    end.

list_node_files(Node)->
    Run_Query = fun()->
        Query = qlc:q([File#file_data.file_name || File <- mnesia:table(file_data),
            File#file_data.node_name == Node]),
        qlc:e(Query)
                end,
    {_, File_List} = mnesia:transaction(Run_Query),
    File_List.

search_by_prefix(Prefix)->
    Run_Query = fun()->
        Query = qlc:q([File#file_data.file_name || File <- mnesia:table(file_data),
            lists:prefix(Prefix, File#file_data.file_name)]),
        qlc:e(Query)
                end,
    {_, File_List} = mnesia:transaction(Run_Query),
    File_List.
