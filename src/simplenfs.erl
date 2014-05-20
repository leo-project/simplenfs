-module(simplenfs).
-behaviour(application).

%% Include
-include("nfs_prot3.hrl").
-include("mount3.hrl").

% API
-export([start/0]).
% Callback for application
-export([start/2, stop/1]).

%% API
start() ->
    % argments for mountd
    MountdArgs = {rpc_app_arg,
                  mountd3,
                  128,
                  [{port, 22050}],
                  ?MOUNTPROG,
                  mountprog,
                  [],
                  ?MOUNTVERS3,
                  ?MOUNTVERS3,
                  true,
                  mount3_svc,
                  [],
                  []},
    % argments for nfsd
    NfsdArgs = {rpc_app_arg,
                  nfsd3,
                  128,
                  [{port, 2049}],
                  ?NFS3_PROGRAM,
                  nfs3_program,
                  [],
                  ?NFS_V3,
                  ?NFS_V3,
                  true,
                  nfs_prot3_svc,
                  [],
                  []},
    application:ensure_started(crypto),
    application:ensure_started(ranch),
    % below code is valid from R17
    % application:set_env(rpc_server, args, [MountdArgs, NfsdArgs], [{persistent, true}]),
    application:load(rpc_server),
    application:set_env(rpc_server, args, [MountdArgs, NfsdArgs]),
    application:ensure_started(rpc_server),
    application:start(?MODULE).

%% Callback  for application
start(_Type, _Args) ->
    simplenfs_sup:start_link().
 
stop(_State) ->
        ok.
