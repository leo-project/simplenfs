-module(simplenfs_sup).
-behaviour(supervisor).
%% Include
-include("nfs_prot3.hrl").
-include("mount3.hrl").

%% API.
-export([start_link/0]).
 
%% supervisor.
-export([init/1]).
 
%% API.
 
-spec start_link() -> {ok, pid()}.
start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).
 
%% supervisor.
 
init([]) ->
    MountdChildSpec = {mount3_svc,
                       {rpc_server, start_link, [
                           {local, nfs_mountd},
                            [{udp, any, 22050, true, []}],
                            ?MOUNTPROG, 
                            mountprog,
                            ?MOUNTVERS3,
                            ?MOUNTVERS3,
                            mount3_svc,
                            []
                       ]},
                       permanent,
                       5000,
                       worker,
                       [mount3_svc]},
    NfsdChildSpec = {nfs_prot3_svc,
                       {rpc_server, start_link, [
                           {local, nfs_rpc_nfsd},
                            [{tcp, any, 2049, true, []}],
                            ?NFS3_PROGRAM, 
                            nfs3_program,
                            ?NFS_V3,
                            ?NFS_V3,
                            nfs_prot3_svc,
                            []
                       ]},
                       permanent,
                       5000,
                       worker,
                       [nfs_prot3_svc]},
    {ok, {{one_for_one, 10, 10}, [MountdChildSpec, NfsdChildSpec]}}.

