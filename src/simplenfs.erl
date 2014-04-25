-module(simplenfs).
-behaviour(application).

% API
-export([start/0]).
% Callback for application
-export([start/2, stop/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% API
start() ->
    ensure_started(crypto),
    application:start(?MODULE).

%% Callback  for application
start(_Type, _Args) ->
    simplenfs_sup:start_link().
 
stop(_State) ->
        ok.
