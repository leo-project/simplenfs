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
    {ok, {{one_for_one, 10, 10}, []}}.

