-module(mount3_server).
-include("mount3.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         mountproc_null_3/2, 
         mountproc_mnt_3/3,
         mountproc_dump_3/2,
         mountproc_umnt_3/3,
         mountproc_umntall_3/2,
         mountproc_export_3/2]).
 
init(_Args) ->
    {ok, void}.
 
handle_call(Req, _From, S) ->
    io:format(user, "[handle_call]req:~p from:~p~n",[Req, _From]),
    {reply, [], S}.
 
handle_cast(Req, S) ->
    io:format(user, "[handle_cast]req:~p~n",[Req]),
    {reply, [], S}.
 
handle_info(Req, S) ->
    io:format(user, "[handle_info]req:~p~n",[Req]),
    {noreply, S}.
 
terminate(_Reason, _S) ->
    ok.

%% private func
formalize_path(Path) ->
    case (binary:last(Path) == $/) of
        true ->
            binary:part(Path, {0, byte_size(Path) - 1});
        false ->
            Path
    end.

mount_add_entry(MountDir, Addr) ->
    MountPointDict = case application:get_env(simplenfs, mount_point) of
        undefined ->
            dict:new();
        {ok, Val} ->
            Val
    end,
    mount_add_entry(MountDir, Addr, MountPointDict).
mount_add_entry(MountDir, Addr, MountPointDict) ->
    case dict:find(MountDir, MountPointDict) of
        {ok, IPList}->
            case lists:member(Addr, IPList) of
                true -> void;
                false -> 
                    NewIPList = [Addr|IPList],
                    NewMountPointDict = dict:store(MountDir, NewIPList, MountPointDict),
                    application:set_env(simplenfs, mount_point, NewMountPointDict)
            end;
        error ->
            NewMountPointDict = dict:store(MountDir, [Addr], MountPointDict),
            application:set_env(simplenfs, mount_point, NewMountPointDict)
    end.

mount_del_entry(MountDir, Addr) ->
    case application:get_env(simplenfs, mount_point) of
        {ok, MountPointDict} ->
            mount_del_entry(MountDir, Addr, MountPointDict);
        undefined ->
            void
    end.
mount_del_entry(MountDir, Addr, MountPointDict) ->
    case dict:find(MountDir, MountPointDict) of
        {ok, IPList}->
            NewIPList = lists:delete(Addr, IPList),
            NewMountPointDict = dict:store(MountDir, NewIPList, MountPointDict),
            application:set_env(simplenfs, mount_point, NewMountPointDict);
        error ->
            void
    end.

%% callback impl
mountproc_null_3(_Clnt, State) ->
    {reply, [], State}.
 
mountproc_mnt_3(MountDir0, Clnt, State) ->
    io:format(user, "[mnt]args:~p client:~p~n",[MountDir0, Clnt]),
    % validate path
    MountDir = formalize_path(MountDir0),
    case filelib:is_dir(MountDir) of
        true ->
            {ok, {Addr, _Port}}= rpc_server:client_ip(Clnt),
            mount_add_entry(MountDir, Addr),
            %% @todo gen nfs3_fh
            {reply, {'MNT3_OK', {MountDir, []}}, State};
        false ->
            {reply, {'MNT3ERR_NOTDIR', []}, State}
    end.
 
mountproc_dump_3(Clnt, State) ->
    io:format(user, "[dump]client:~p~n",[Clnt]),
    {reply, void, State}.
 
mountproc_umnt_3(MountDir0, Clnt, State) ->
    io:format(user, "[umnt]args:~p client:~p~n",[MountDir0, Clnt]),
    MountDir = formalize_path(MountDir0),
    {ok, {Addr, _Port}}= rpc_server:client_ip(Clnt),
    catch mount_del_entry(MountDir, Addr),
    {reply, void, State}.
 
mountproc_umntall_3(Clnt, State) ->
    io:format(user, "[umntall]client:~p~n",[Clnt]),
    {reply, void, State}.
 
mountproc_export_3(Clnt, State) ->
    io:format(user, "[export]client:~p~n",[Clnt]),
    {reply, void, State}.
