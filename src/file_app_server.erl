%%%-------------------------------------------------------------------
%%% @author marci
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(file_app_server).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([add_file/1, delete_file/1, find_file/1, download_file/2, overwrite_file/2]).
-export([change_file_owner/1, change_file_owner/2, rename_file/2, list_node_files/1, stop/0]).
-export([apply_fun_to_file/3, search_by_prefix/1]).

-define(SERVER, ?MODULE).

-record(file_app_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    io:format("=============== File Server On This Node Correctly Initialized ===============\n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #file_app_server_state{}}.

handle_call({add_file, File}, _From, State = #file_app_server_state{}) ->
    {reply, file_database:add_file(File, node()), State};

handle_call({delete_file, File}, _From, State = #file_app_server_state{}) ->
    {reply, file_database:delete_file(File), State};

handle_call({find_file, File}, _From, State = #file_app_server_state{}) ->
    {reply, file_database:find_file(File), State};

handle_call({download_file, Host_File, Local_File}, _From, State = #file_app_server_state{}) ->
    case file_database:get_file_node(Host_File) of
        [Host_Node] ->
            {reply, file_management:download_file(Host_Node, Host_File, Local_File), State};
        _ ->
            {reply, {error, "file does not exists"}, State}
    end;

handle_call({overwrite_file, Host_File, Local_File}, _From, State = #file_app_server_state{}) ->
    case file_database:get_file_node(Host_File) of
        [Host_Node] ->
            {reply, file_management:overwrite_file(Host_Node, Host_File, Local_File), State};
        _ ->
            {reply, {error, "file does not exists"}, State}
    end;

handle_call({apply_fun_to_file, Host_File, An_Fun, Args_List}, _From, State = #file_app_server_state{}) ->
    case file_database:get_file_node(Host_File) of
        [Host_Node] ->
            {reply, file_management:apply_fun_to_file(Host_Node, Host_File, An_Fun, Args_List), State};
        _ ->
            {reply, {error, "file does not exists"}, State}
    end;

handle_call({change_file_owner, File, New_Host}, _From, State = #file_app_server_state{})->
    {reply, file_database:change_file_owner(File, New_Host), State};

handle_call({rename_file, Old_Name, New_Name}, _From, State = #file_app_server_state{})->
    {reply, file_database:rename_file(Old_Name, New_Name), State};

handle_call({list_node_files, Node}, _From, State = #file_app_server_state{})->
    {reply, file_database:list_node_files(Node), State};

handle_call({search_by_prefix, Prefix}, _From, State = #file_app_server_state{})->
    {reply, file_database:search_by_prefix(Prefix), State}.

handle_cast(stop, State = #file_app_server_state{}) ->
    {stop, normal, State}.

terminate(_Reason, _State = #file_app_server_state{})->
    io:format("=============== File Server Correctly Closed ===============\n").

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_file(File)->
    gen_server:call(?SERVER, {add_file, File}).

delete_file(File)->
    gen_server:call(?SERVER, {delete_file, File}).

find_file(File)->
    gen_server:call(?SERVER, {find_file, File}).

download_file(Host_File, Local_File)->
    gen_server:call(?SERVER, {download_file, Host_File, Local_File}).

overwrite_file(Host_File, Local_File)->
    gen_server:call(?SERVER, {overwrite_file, Host_File, Local_File}).

change_file_owner(File)->
    gen_server:call(?SERVER, {change_file_owner, File, node()}).

change_file_owner(File, New_Host)->
    gen_server:call(?SERVER, {change_file_owner, File, New_Host}).

apply_fun_to_file(Host_File, An_Fun, Args_List)->
    gen_server:call(?SERVER, {apply_fun_to_file, Host_File, An_Fun, Args_List}).

rename_file(Old_Name, New_Name)->
    gen_server:call(?SERVER, {rename_file, Old_Name, New_Name}).

list_node_files(Node)->
    gen_server:call(?SERVER, {list_node_files, Node}).

search_by_prefix(Prefix)->
    gen_server:call(?SERVER, {search_by_prefix, Prefix}).

stop()->
    gen_server:cast(?SERVER, stop).
