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
 
mountproc_null_3(_Clnt, State) ->
    {reply, [], State}.
 
mountproc_mnt_3(_1, Clnt, State) ->
    io:format(user, "[mnt]args:~p client:~p~n",[_1, Clnt]),
    {reply, {'MNT3_OK', {<<"01234567">>, []}}, State}.
 
mountproc_dump_3(Clnt, State) ->
    io:format(user, "[dump]client:~p~n",[Clnt]),
    {reply, void, State}.
 
mountproc_umnt_3(_1, Clnt, State) ->
    io:format(user, "[umnt]args:~p client:~p~n",[_1, Clnt]),
    {reply, void, State}.
 
mountproc_umntall_3(Clnt, State) ->
    io:format(user, "[umntall]client:~p~n",[Clnt]),
    {reply, void, State}.
 
mountproc_export_3(Clnt, State) ->
    io:format(user, "[export]client:~p~n",[Clnt]),
    {reply, void, State}.
