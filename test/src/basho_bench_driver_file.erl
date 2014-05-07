-module(basho_bench_driver_file).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-record(state, { target_dir_path    % target dir to issue read/write syscalls
               }).

%% ====================================================================
%% API
%% ====================================================================

new(_Id) ->
    %% The IPs, port and path we'll be testing
    TargetDir = basho_bench_config:get(target_dir_path, ["./"]),
    {ok, #state{ target_dir_path = TargetDir}}.

run(read, KeyGen, _ValueGen, State) ->
    NextFile = next_file(KeyGen, State),
    case file:read_file(NextFile) of
        {ok, _Binary} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(write, KeyGen, ValueGen, State) ->
    NextFile = next_file(KeyGen, State),
    case file:write_file(NextFile, ValueGen()) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

next_file(KeyGen, #state{target_dir_path = TargetDir}) ->
    filename:join(TargetDir, KeyGen()).

