%%======================================================================
%%
%% Leo Cache
%%
%% Copyright (c) 2012-2014 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------
%% Leo Cache - [D]isc [C]ache [Erl]ng
%%
%% @doc The disc-cache server
%% @reference https://github.com/leo-project/leo_cache/blob/master/src/leo_cache_server_dcerl.erl
%% @end
%%======================================================================
-module(leo_cache_server_dcerl).
-author("Yosuke Hara").

-behaviour(leo_cache_behaviour).

-include("leo_cache.hrl").
-include_lib("leo_dcerl/include/leo_dcerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/2, stop/0,
         get_filepath/2, get_ref/2, get/2, get/3,
         put/3, put/4, put_begin_tran/2, put_end_tran/5,
         delete/2, stats/0]).

-define(ID_PREFIX, "leo_dcerl_").
-define(STR_SLASH, "/").


%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @doc Launch cache-server(s)
%%
-spec(start(Workers, Options) ->
             ok | {error, any()} when Workers::integer(),
                                      Options::[{atom(), any()}]).
start(Workers, Options) ->
    CacheCapacity = leo_misc:get_value(?PROP_DISC_CACHE_SIZE, Options),
    DataDir       = leo_misc:get_value(?PROP_DISC_CACHE_DATA_DIR, Options),
    JournalDir    = leo_misc:get_value(?PROP_DISC_CACHE_JOURNAL_DIR, Options),
    ThresholdLen  = leo_misc:get_value(?PROP_DISC_CACHE_THRESHOLD_LEN, Options),

    {DataDir2, Options2} =
        case (string:rstr(DataDir, ?STR_SLASH) == length(DataDir)) of
            true ->
                {DataDir, Options};
            false ->
                DataDir1 = lists:append([DataDir, ?STR_SLASH]),
                Options1 = [{?PROP_DISC_CACHE_DATA_DIR, DataDir1}
                            |lists:delete({?PROP_DISC_CACHE_DATA_DIR, DataDir}, Options)],
                {DataDir1, Options1}
        end,

    {JournalDir2, Options4} =
        case (string:rstr(JournalDir, ?STR_SLASH) == length(JournalDir)) of
            true ->
                {JournalDir, Options2};
            false ->
                JournalDir1 = lists:append([JournalDir, ?STR_SLASH]),
                Options3 = [{?PROP_DISC_CACHE_JOURNAL_DIR, JournalDir1}
                            |lists:delete({?PROP_DISC_CACHE_JOURNAL_DIR, JournalDir}, Options2)],
                {JournalDir1, Options3}
        end,

    ok = leo_misc:set_env(leo_cache, ?PROP_OPTIONS, Options4),
    Params = [DataDir2,
              JournalDir2,
              erlang:round(CacheCapacity/Workers),
              ThresholdLen],
    ok = start_1(Workers, Params),
    ok.


%% @doc Stop cache-server(s)
%%
-spec(stop() ->
             ok).
stop() ->
    stop_1(?get_workers()).


%% @doc Retrieve a meta data of cached object (for large-object)
%%
-spec(get_filepath(Id, Key) ->
             {ok, #cache_meta{}} |
             {error, undefined} when Id::integer(),
                                     Key::binary()|any()).
get_filepath(Id, Key) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "get_filepath/2", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {get_filepath, Key}) of
                {ok, Meta} ->
                    {ok, Meta};
                not_found ->
                    not_found;
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "get_filepath/2", Cause),
                    ok = restart(Id, Pid),
                    {error, Cause}
            end
    end.


%% @doc Retrieve a reference of cached object (for large-object)
%%
-spec(get_ref(Id, Key) ->
             {ok, reference()} |
             {error, undefined} when Id::integer(),
                                     Key::binary()|any()).
get_ref(Id, Key) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "get_ref/2", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {get_ref, Key}) of
                {ok, Ref} ->
                    {ok, Ref};
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "get_ref/2", Cause),
                    ok = restart(Id, Pid),
                    {error, Cause}
            end
    end.


%% @doc Retrieve an object from cache-server
-spec(get(Id, Key) ->
             not_found |
             {ok, binary()} |
             {error, any()} when Id::integer(),
                                 Key::binary()|any()).
get(Id, Key) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "get/2", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {get, Key}) of
                {ok, Value} ->
                    {ok, Value};
                not_found ->
                    not_found;
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "get/2", Cause),
                    ok = restart(Id, Pid),
                    {error, Cause}
            end
    end.


%% @doc Retrieve an object from cache-server (for large-object)
-spec(get(Id, Ref, Key) ->
             not_found |
             {ok, binary()} |
             {error, any()} when Id::integer(),
                                 Ref::reference(),
                                 Key::binary()|any()).

get(Id, Ref, Key) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "get/3", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {get, Ref, Key}) of
                {ok, {Value, false}} ->
                    {ok, Value};
                {ok, {<<>>, true}} ->
                    {ok, done};
                not_found ->
                    not_found;
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "get/3", Cause),
                    ok = restart(Id, Pid),
                    {error, Cause}
            end
    end.


%% @doc Insert an object into cache-serverx
-spec(put(Id, Key, Value) ->
             ok | {error, any()} when Id::integer(),
                                      Key::binary()|any(),
                                      Value::binary()|any()).
put(Id, Key, Value) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "put/3", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {put, Key, Value}) of
                ok ->
                    ok;
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "put/3", Cause),
                    ok = restart(Id, Pid),
                    {error, Cause}
            end
    end.


%% @doc Insert an object into the cache-server (for large-object)
-spec(put(Id, Ref, Key, Value) ->
             ok | {error, any()} when Id::integer(),
                                      Ref::reference(),
                                      Key::binary()|any(),
                                      Value::binary()|any()).
put(Id, Ref, Key, Value) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "put/4", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {put, Ref, Key, Value}) of
                ok ->
                    ok;
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "put/4", Cause),
                    ok = restart(Id, Pid),
                    {error, Cause}
            end
    end.


%% @doc Start put-transaction for large-object (for large-object)
-spec(put_begin_tran(Id, Key) ->
             ok | {error, any()} when Id::integer(),
                                      Key::binary()|any()).
put_begin_tran(Id, Key) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "put_begin_tran/2", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {put_begin_tran, Key}) of
                {ok, Ref} ->
                    {ok, Ref};
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "put_begin_tran/2", Cause),
                    ok = restart(Id, Pid),
                    {error, Cause}
            end
    end.


%% @doc End put-transaction for large-object (for large-object)
-spec(put_end_tran(Id, Ref, Key, Meta, IsCommit) ->
             ok | {error, any()} when Id::integer(),
                                      Ref::reference(),
                                      Key::binary()|any(),
                                      Meta::#cache_meta{},
                                      IsCommit::boolean()).
put_end_tran(Id, Ref, Key, Meta, IsCommit) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "put_end_tran/4", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {put_end_tran, Ref, Key, Meta, IsCommit}) of
                ok ->
                    ok;
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "put_end_tran/4", Cause),
                    {error, Cause}
            end
    end.


%% @doc Remove an object from cache-server
-spec(delete(Id, Key) ->
             ok | {error, any()} when Id::integer(),
                                      Key::binary()|any()).
delete(Id, Key) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "delete/2", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {delete, Key}) of
                ok ->
                    ok;
                {error, Cause} ->
                    ?warn(?MODULE_STRING, "delete/2", Cause),
                    ok = restart(Id, Pid),
                    {error, Cause}
            end
    end.


%% @doc Retrieve status of this application
%%
-spec(stats() ->
             {ok, any()} | {error, any()}).
stats() ->
    stats_1(?get_workers(), []).


%%====================================================================
%% INNER FUNCTIONS
%%====================================================================
%% @doc Start Proc(s)
%% @private
-spec(start_1(Id, Params) ->
             ok when Id::pos_integer(),
                     Params::[any()]).
start_1(0, _) ->
    ok;
start_1(Id, [DataDir, JournalDir, CacheCapacity, ThresholdLen] = Params) ->
    ProcId = ?gen_proc_id(Id, ?ID_PREFIX),
    DataDir1 = lists:append([DataDir, integer_to_list(Id), ?STR_SLASH]),
    JournalDir1 = lists:append([JournalDir, integer_to_list(Id), ?STR_SLASH]),
    %% {ok, Pid} = leo_dcerl_server:start_link(
    %%               ProcId, DataDir1, JournalDir1, CacheCapacity, ThresholdLen),
    {ok, Pid} = leo_dcerl_sup:start_child(
                  ProcId, DataDir1, JournalDir1, CacheCapacity, ThresholdLen),
    true = ets:insert(?ETS_DISC_CACHE_HANDLERS, {Id, Pid}),
    start_1(Id - 1, Params).


%% @doc Re-launch a process
%% @private
-spec(restart(Id) ->
             ok when Id::pos_integer()).
restart(Id) ->
    ?warn(?MODULE_STRING, "restart/1",
          lists:append(["leo_dcerl-id:", integer_to_list(Id),
                        " ", ?ERROR_PROC_IS_NOT_ALIVE])),

    Options = ?get_options(),
    CacheCapacity = leo_misc:get_value(?PROP_DISC_CACHE_SIZE, Options),
    Workers       = leo_misc:get_value(?PROP_DISC_CACHE_WORKERS, Options),
    DataDir       = leo_misc:get_value(?PROP_DISC_CACHE_DATA_DIR, Options),
    JournalDir    = leo_misc:get_value(?PROP_DISC_CACHE_JOURNAL_DIR, Options),
    ThresholdLen  = leo_misc:get_value(?PROP_DISC_CACHE_THRESHOLD_LEN, Options),

    ProcId = ?gen_proc_id(Id, ?ID_PREFIX),
    {ok, Pid} = leo_dcerl_server:start_link(ProcId, DataDir, JournalDir,
                                            erlang:round(CacheCapacity/Workers), ThresholdLen),
    true = ets:insert(?ETS_DISC_CACHE_HANDLERS, {ProcId, Pid}),
    ok.

-spec(restart(Id, Pid) ->
             ok when Id::pos_integer(),
                     Pid::pid()).
restart(Id, Pid) ->
    case erlang:is_process_alive(Pid) of
        true  -> ok;
        false ->
            restart(Id)
    end.


%% @doc Stop Proc(s)
%% @private
-spec(stop_1(Id) ->
             ok when Id::pos_integer()).
stop_1(0) ->
    ok;
stop_1(Id) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            void;
        Pid ->
            gen_server:cast(Pid, stop)
    end,
    stop_1(Id - 1).


%% @doc Retrieve and summarize stats
%% @private
-spec(stats_1(Id, Acc) ->
             {ok, [#stats{}]} | {error, any()} when Id::non_neg_integer(),
                                                    Acc::[#stats{}]).
stats_1(0, Acc) ->
    {ok, lists:foldl(fun([{'get',    G1},{'put', P1},
                          {'delete', D1},{'hits',H1},
                          {'files',  R1},{'size',S1}], #stats{get=G2, put=P2,
                                                              delete=D2, hits=H2,
                                                              records=R2, size=S2}) ->
                             #stats{get     = G1 + G2,
                                    put     = P1 + P2,
                                    delete  = D1 + D2,
                                    hits    = H1 + H2,
                                    records = R1 + R2,
                                    size    = S1 + S2}
                     end, #stats{}, Acc)};
stats_1(Id, Acc) ->
    case ?get_handler(?ETS_DISC_CACHE_HANDLERS, Id) of
        undefined ->
            {error, ?ERROR_COULD_NOT_GET_STATS};
        Pid ->
            case gen_server:call(Pid, {stats}) of
                {ok, #cache_stats{gets = Gets,
                                  puts = Puts,
                                  dels = Dels,
                                  hits = Hits,
                                  records = Recs,
                                  cached_size = Size}} ->
                    stats_1(Id - 1, [[{'get', Gets},{'put', Puts},
                                      {'delete', Dels},{'hits',Hits},
                                      {'files',Recs},{'size',Size}]|Acc]);
                _ ->
                    {error, ?ERROR_COULD_NOT_GET_STATS}
            end
    end.
