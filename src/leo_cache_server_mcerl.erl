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
%% Leo Cache - Cherly (RAM Cache)
%%
%% @doc The memory-cache server
%% @reference https://github.com/leo-project/leo_cache/blob/master/src/leo_cache_server_mcerl.erl
%% @end
%%======================================================================
-module(leo_cache_server_mcerl).
-author("Yosuke Hara").

-behaviour(leo_cache_behaviour).

-include("leo_cache.hrl").
-include_lib("leo_mcerl/include/leo_mcerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/2, stop/0,
         get_filepath/2, get_ref/2, get/2, get/3,
         put/3, put/4, put_begin_tran/2, put_end_tran/5,
         delete/2, stats/0]).

-define(ID_PREFIX, "leo_mcerl_").

-record(cache_meta, {
          size         = 0  :: non_neg_integer(),
          md5          = 0  :: integer(),
          mtime        = 0  :: non_neg_integer(),
          content_type = "NULL" :: string(),
          file_path    = "" :: file:name_all()
         }).


%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @doc Launch cache-server(s)
%%
-spec(start(Workers, Options) ->
             ok | {error, any()} when Workers::integer(),
                                      Options::[{atom(), any()}]).
start(Workers, Options) ->
    CacheCapacity = leo_misc:get_value(?PROP_RAM_CACHE_SIZE, Options),
    ok = start_1(Workers, erlang:round(CacheCapacity/Workers)),
    ok.


%% @doc Stop cache-server(s)
%%
-spec(stop() ->
             ok).
stop() ->
    stop_1(?get_workers()).


%% @doc Retrieve a reference of cached object (for large-object)
%%
-spec(get_ref(Id, Key) ->
             {ok, reference()} |
             {error, undefined} when Id::integer(),
                                     Key::binary()|any()).
get_ref(_Id, _Key) ->
    {error, undefined}.


%% @doc Retrieve a meta data of cached object (for large-object)
%%
-spec(get_filepath(Id, Key) ->
             {ok, #cache_meta{}} |
             {error, undefined} when Id::integer(),
                                     Key::binary()|any()).

get_filepath(_Id, _Key) ->
    {error, undefined}.


%% @doc Retrieve an object from cache-server
-spec(get(Id, Key) ->
             not_found |
             {ok, binary()} |
             {error, any()} when Id::integer(),
                                 Key::binary()|any()).
get(Id, Key) ->
    case ?get_handler(?ETS_RAM_CACHE_HANDLERS, Id) of
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
get(_Id,_Ref,_Key) ->
    not_found.


%% @doc Insert an object into the momory storage
-spec(put(Id, Key, Value) ->
             ok | {error, any()} when Id::integer(),
                                      Key::binary()|any(),
                                      Value::binary()|any()).
put(Id, Key, Value) ->
    case ?get_handler(?ETS_RAM_CACHE_HANDLERS, Id) of
        undefined ->
            ?warn(?MODULE_STRING, "put/3", ?ERROR_MAYBE_CRASH_SERVER),
            ok = restart(Id),
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case gen_server:call(Pid, {put, Key, Value}) of
                ok ->
                    ok;
                {error, 'out_of_memory' = Cause} ->
                    {error, Cause};
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
put(_Id,_Ref,_Key,_Value) ->
    {error, undefined}.


%% @doc Start put-transaction for large-object (for large-object)
-spec(put_begin_tran(Id, Key) ->
             ok | {error, any()} when Id::integer(),
                                      Key::binary()|any()).
put_begin_tran(_Id,_Key) ->
    {error, undefined}.


%% @doc End put-transaction for large-object (for large-object)
-spec(put_end_tran(Id, Ref, Key, Meta, IsCommit) ->
             ok | {error, any()} when Id::integer(),
                                      Ref::reference(),
                                      Key::binary()|any(),
                                      Meta::#cache_meta{},
                                      IsCommit::boolean()).
put_end_tran(_Id,_Ref,_Key,_Meta,_IdCommit) ->
    {error, undefined}.


%% @doc Remove an object from the momory storage
-spec(delete(Id, Key) ->
             ok | {error, any()} when Id::integer(),
                                      Key::binary()|any()).
delete(Id, Key) ->
    case ?get_handler(?ETS_RAM_CACHE_HANDLERS, Id) of
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
             {ok, any()}).
stats() ->
    stats_1(?get_workers(), []).


%%====================================================================
%% INNER FUNCTIONS
%%====================================================================
%% @doc Start Proc(s)
%% @private
-spec(start_1(Id, CacheCapacity) ->
             ok when Id::pos_integer(),
                     CacheCapacity::pos_integer()).
start_1(0, _) ->
    ok;
start_1(Id, CacheCapacity) ->
    ProcId = ?gen_proc_id(Id, ?ID_PREFIX),
    {ok, Pid} = leo_mcerl_sup:start_child(ProcId, CacheCapacity),
    true = ets:insert(?ETS_RAM_CACHE_HANDLERS, {Id, Pid}),
    start_1(Id - 1, CacheCapacity).


%% @doc Re-launch a process
%% @private
-spec(restart(Id) ->
             ok when Id::pos_integer()).
restart(Id) ->
    ?warn(?MODULE_STRING, "restart/1",
          lists:append(["leo_mcerl-id:", integer_to_list(Id),
                        " ", ?ERROR_PROC_IS_NOT_ALIVE])),

    Options = ?get_options(),
    CacheCapacity = leo_misc:get_value(?PROP_RAM_CACHE_SIZE, Options),
    Workers = leo_misc:get_value(?PROP_RAM_CACHE_WORKERS, Options),

    ProcId = ?gen_proc_id(Id, ?ID_PREFIX),
    {ok, Pid} = leo_mcerl_server:start_link(ProcId, erlang:round(CacheCapacity/Workers)),
    true = ets:insert(?ETS_RAM_CACHE_HANDLERS, {ProcId, Pid}),
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
    case ?get_handler(?ETS_RAM_CACHE_HANDLERS, Id) of
        undefined ->
            void;
        Pid ->
            gen_server:cast(Pid, stop)
    end,
    stop_1(Id - 1).


%% @doc Retrieve and summarize stats
%% @private
-spec(stats_1(Id, Acc) ->
             {ok, [#stats{}]} | {error, any()} when Id::pos_integer(),
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
    case ?get_handler(?ETS_RAM_CACHE_HANDLERS, Id) of
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
