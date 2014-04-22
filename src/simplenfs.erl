-module(simplenfs).
-behaviour(application).
%% API.
-export([start/2]).
-export([stop/1]).
 
start(_Type, _Args) ->
    simplenfs_sup:start_link().
 
stop(_State) ->
        ok.
