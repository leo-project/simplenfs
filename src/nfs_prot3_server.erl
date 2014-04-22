-module(nfs_prot3_server).
-include("nfs_prot3.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         nfsproc3_null_3/2,
         nfsproc3_getattr_3/3,
         nfsproc3_setattr_3/3,
         nfsproc3_lookup_3/3,
         nfsproc3_access_3/3,
         nfsproc3_readlink_3/3,
         nfsproc3_read_3/3,
         nfsproc3_write_3/3,
         nfsproc3_create_3/3,
         nfsproc3_mkdir_3/3,
         nfsproc3_symlink_3/3,
         nfsproc3_mknod_3/3,
         nfsproc3_remove_3/3,
         nfsproc3_rmdir_3/3,
         nfsproc3_rename_3/3,
         nfsproc3_link_3/3,
         nfsproc3_readdir_3/3,
         nfsproc3_readdirplus_3/3,
         nfsproc3_fsstat_3/3,
         nfsproc3_pathconf_3/3,
         nfsproc3_commit_3/3,
         nfsproc3_fsinfo_3/3]).
 
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
 
nfsproc3_null_3(_Clnt, State) ->
    {reply, [], State}.
 
nfsproc3_getattr_3(_1, Clnt, State) ->
    io:format(user, "[getattr]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
           % fattr
           {'NF3DIR',
            0, % protection mode bits
            0, % # of hard links
            0, % uid
            0, % gid
            4096, % file size
            4096, % actual size used at disk(LeoFS should return `body + metadata + header/footer`)
            {0, 0}, % data used for special file(in Linux first is major, second is minor number)
            0, % fsid
            0, % fieldid 
            {0, 0}, % last access
            {0, 0}, % last modification
            {0, 0}} % last change
        }}, 
        State}.
 
nfsproc3_setattr_3(_1, Clnt, State) ->
    io:format(user, "[setattr]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_lookup_3(_1, Clnt, State) ->
    io:format(user, "[lookup]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {<<"pseudo nfs file handle">>}, %% pre_op_attr
            {false, void}, %% post_op_attr for obj
            {false, void}  %% post_op_attr for dir
        }}, 
        State}.
 
nfsproc3_access_3(_1, Clnt, State) ->
    io:format(user, "[access]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op_attr for obj
            63       %% access bits(up all)
        }}, 
        State}.
 
nfsproc3_readlink_3(_1, Clnt, State) ->
    io:format(user, "[readlink]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op_attr for obj
            <<"link path">>
        }}, 
        State}.
 
nfsproc3_read_3(_1, Clnt, State) ->
    io:format(user, "[read]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op_attr for obj
            11, %% count read bytes
            true, %% eof
            <<"hello world">>
        }}, 
        State}.
 
nfsproc3_write_3(_1, Clnt, State) ->
    io:format(user, "[write]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_NG',
        {
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_create_3(_1, Clnt, State) ->
    io:format(user, "[create]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op file handle
            {false, void}, %% post_op_attr
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_mkdir_3(_1, Clnt, State) ->
    io:format(user, "[mkdir]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op file handle
            {false, void}, %% post_op_attr
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_symlink_3(_1, Clnt, State) ->
    io:format(user, "[symlink]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op file handle
            {false, void}, %% post_op_attr
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_mknod_3(_1, Clnt, State) ->
    io:format(user, "[mknode]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op file handle
            {false, void}, %% post_op_attr
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_remove_3(_1, Clnt, State) ->
    io:format(user, "[remove]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_rmdir_3(_1, Clnt, State) ->
    io:format(user, "[rmdir]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_rename_3(_1, Clnt, State) ->
    io:format(user, "[rename]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {%% wcc_data(old)
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            },
            {%% wcc_data(new)
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_link_3(_1, Clnt, State) ->
    io:format(user, "[rmdir]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_NG',
        {
            {false, void}, %% post_op_attr
            {%% wcc_data(new)
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            }
        }}, 
        State}.
 
nfsproc3_readdir_3(_1, Clnt, State) ->
    io:format(user, "[readdir]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op_attr
            <<"12345678">>, %% cookie verfier
            {%% dir_list(empty)
                void, %% pre_op_attr
                true  %% eof
            }
        }}, 
        State}.
 
nfsproc3_readdirplus_3(_1, Clnt, State) ->
    io:format(user, "[readdirplus]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op_attr
            <<"12345678">>, %% cookie verfier
            {%% dir_list(empty)
                void, %% pre_op_attr
                true  %% eof
            }
        }}, 
        State}.
 
nfsproc3_fsstat_3(_1, Clnt, State) ->
    io:format(user, "[fsstat]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op_attr
            8192, %% total size
            1024, %% free size
            1024, %% free size(for auth user)
            16,   %% # of files
            8,    %% # of free file slots
            8,    %% # of free file slots(for auth user)
            10    %% invarsec
        }}, 
        State}.
 
nfsproc3_fsinfo_3(_1, Clnt, State) ->
    io:format(user, "[fsinfo]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op_attr
            4096, %% rtmax
            4096, %% rtperf
            1,    %% rtmult
            4096, %% wtmax
            4096, %% wtperf
            1,    %% wtmult
            4096, %% dperf
            1024 * 1024 * 1024 * 4, %% max size of a file
            {1, 0}, %% time_delta
            0     %% properties
        }}, 
        State}.
 
nfsproc3_pathconf_3(_1, Clnt, State) ->
    io:format(user, "[pathconf]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {false, void}, %% post_op_attr
            8,    %% linkmax
            1024, %% name_max
            true, %% no_trunc (mean make a reques error if filename's length was larger than max
            false,%% chown_restricted
            true, %% case_insensitive
            true  %% case_preserving
        }}, 
        State}.
 
nfsproc3_commit_3(_1, Clnt, State) ->
    io:format(user, "[commit]args:~p client:~p~n",[_1, Clnt]),
    {reply, 
        {'NFS3_OK',
        {
            {%% wcc_data
                {false, void}, %% pre_op_attr
                {false, void}  %% post_op_attr
            },
            <<"12345678">> %% write verfier
        }}, 
        State}.

