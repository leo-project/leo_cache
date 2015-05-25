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
%% Leo Cache
%%
%% @doc The cache API
%% @reference https://github.com/leo-project/leo_cache/blob/master/src/leo_cache_server_dcerl.erl
%% @end
%%======================================================================
-module(leo_cache_api).
-author("Yosuke Hara").

-include("leo_cache.hrl").
-include_lib("leo_dcerl/include/leo_dcerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/0, start/1, stop/0, hold/1,
         get_filepath/1, get_ref/1, get/1, get/2,
         put/2, put/3, put_begin_tran/1, put_end_tran/4,
         delete/1, stats/0]).


%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @doc Launch cache-server(s)
%%
-spec(start() ->
             ok | {error, any()}).
start() ->
    start(?DEF_OPTIONS).

-spec(start(Options) ->
             ok | {error, any()} when Options::[{atom(), any()}]).
start(Options) ->
    ok = leo_misc:init_env(),
    RC = ?gen_mod_name(leo_misc:get_value(?PROP_RAM_CACHE_NAME,  Options)),
    DC = ?gen_mod_name(leo_misc:get_value(?PROP_DISC_CACHE_NAME, Options)),

    RAMCacheSize  = leo_misc:get_value(?PROP_RAM_CACHE_SIZE, Options),
    DiscCacheSize = leo_misc:get_value(?PROP_DISC_CACHE_SIZE, Options),

    IsActiveRAMCache  = (is_integer(RAMCacheSize)  andalso RAMCacheSize  > 0),
    IsActiveDiscCache = (is_integer(DiscCacheSize) andalso DiscCacheSize > 0
                         andalso IsActiveRAMCache == true),

    Options_1 = Options ++ [{?PROP_RAM_CACHE_MOD,  RC},
                            {?PROP_DISC_CACHE_MOD, DC},
                            {?PROP_RAM_CACHE_ACTIVE,  IsActiveRAMCache},
                            {?PROP_DISC_CACHE_ACTIVE, IsActiveDiscCache}
                           ],
    ok = leo_misc:set_env(leo_cache, ?PROP_OPTIONS, Options_1),

    {ok, Holder} = leo_cache_holder:start(),

    %% Create the cache-related tables to manage the cache servers
    case catch start_1() of
        {'EXIT', Cause} ->
            {error, Cause};
        ok ->
            %% Launch the memory-cache server
            CacheWorkers =
            case IsActiveRAMCache of
                true ->
                    Workers1 = leo_misc:get_value(?PROP_RAM_CACHE_WORKERS, Options_1),
                    ok = RC:start(Workers1, Options_1),
                    Workers1;
                false ->
                    0
            end,

            %% Launch the disk-cache server
            case IsActiveDiscCache of
                true ->
                    ok = DC:start(CacheWorkers, Options_1);
                false ->
                    void
            end,

            %% Set the each server's info
            ChunkThresholdLen = leo_misc:get_value(?PROP_DISC_CACHE_THRESHOLD_LEN, Options),
            true = ets:insert(?ETS_CACHE_SERVER_INFO,
                              {0, #cache_server{ram_cache_mod       = RC,
                                                ram_cache_active    = IsActiveRAMCache,
                                                disc_cache_mod      = DC,
                                                disc_cache_active   = IsActiveDiscCache,
                                                cache_workers       = CacheWorkers,
                                                chunk_threshold_len = ChunkThresholdLen,
                                                cache_holder        = Holder
                                               }}),
            ok
    end.

%% @private
start_1() ->
    case ets:info(?ETS_RAM_CACHE_HANDLERS) of
        undefined ->
            case ets:new(?ETS_RAM_CACHE_HANDLERS,
                         [named_table, set, public, {read_concurrency, true}]) of
                ?ETS_RAM_CACHE_HANDLERS ->
                    ok;
                _ ->
                    erlang:error({error, could_not_create_ets_table})
            end;
        _ ->
            ok
    end,
    case ets:info(?ETS_DISC_CACHE_HANDLERS) of
        undefined ->
            case ets:new(?ETS_DISC_CACHE_HANDLERS,
                         [named_table, set, public, {read_concurrency, true}]) of
                ?ETS_DISC_CACHE_HANDLERS ->
                    ok;
                _ ->
                    erlang:error({error, could_not_create_ets_table})
            end;
        _ ->
            ok
    end,
    case ets:info(?ETS_CACHE_SERVER_INFO) of
        undefined ->
            case ets:new(?ETS_CACHE_SERVER_INFO,
                         [named_table, set, public, {read_concurrency, true}])of
                ?ETS_CACHE_SERVER_INFO ->
                    ok;
                _ ->
                    erlang:error({error, could_not_create_ets_table})
            end;
        _ ->
            ok
    end.


%% @doc Stop cache-server(s)
%%
-spec(stop() ->
             ok).
stop() ->
    Options = ?get_options(),
    case leo_misc:get_value(?PROP_RAM_CACHE_MOD,  Options) of
        undefined -> void;
        RC -> RC:stop()
    end,
    case leo_misc:get_value(?PROP_DISC_CACHE_MOD,  Options) of
        undifined -> void;
        DC -> DC:stop()
    end,

    case ets:lookup(?ETS_CACHE_SERVER_INFO, 0) of
        [{_, Item}|_] ->
            Holder = Item#cache_server.cache_holder,
            leo_cache_holder:stop(Holder);
        _ ->
            void
    end,
    ok.


%% @doc Retrieve a reference of cached object (for large-object)
-spec(get_ref(Key) ->
             not_found |
             {ok, reference()} |
             {error, any()} when Key::binary()).
get_ref(Key) ->
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key),
    case Active of
        true ->
            DC:get_ref(Id, Key);
        false ->
            not_found
    end.


%% @doc Retrieve a meta data of cached object (for large-object)
-spec(get_filepath(Key) ->
             not_found |
             {ok, #cache_meta{}} |
             {error, any()} when Key::binary()).
get_filepath(Key) ->
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active,
                  cache_holder      = Holder} = ?cache_servers(Key),
    leo_cache_holder:wait(Holder, Key),
    case Active of
        true ->
            DC:get_filepath(Id, Key);
        false ->
            not_found
    end.

%% @doc Hold the cache to prevent duplicate retrival
-spec(hold(Key) -> 
    {ok, pid()} | {error, any()} when Key::binary()).
hold(Key)->
    case ets:lookup(?ETS_CACHE_SERVER_INFO, 0) of
        [{_, Item}|_] ->
            Holder = Item#cache_server.cache_holder,
            leo_cache_holder:hold(Holder, Key);
        _ ->
            void
    end.

%% @doc Retrieve an object from the momory storage
-spec(get(Key) ->
             not_found |
             {ok, binary()} |
             {error, any()} when Key::binary()).
get(Key) ->
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2, 
                  cache_holder      = Holder} = ?cache_servers(Key),

    leo_cache_holder:wait(Holder, Key),
    case Active1 of
        true ->
            case RC:get(Id1, Key) of
                {ok, Bin} ->
                    {ok, Bin};
                not_found when Active2 == true ->
                    DC:get(Id2, Key);
                not_found ->
                    not_found;
                {error, Cause} ->
                    {error, Cause}
            end;
        false ->
            not_found
    end.


%% @doc Retrieve a chunked-object from disc-cache (for large-object)
%%
-spec(get(Ref, Key) ->
             not_found |
             {ok, {binary(), boolean()}} |
             {error, any()} when Ref::reference(),
                                 Key::binary()).
get(Ref, Key) ->
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active,
                  cache_holder      = Holder} = ?cache_servers(Key),
    leo_cache_holder:wait(Holder, Key),
    case Active of
        true ->
            DC:get(Id, Ref, Key);
        false ->
            not_found
    end.


%% @doc Insert an object into the momory storage
-spec(put(Key, Value) ->
             ok | {error, any()} when Key::binary(),
                                      Value::binary()).
put(Key, Value) ->
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2,
                  chunk_threshold_len = ChunkThresholdLen,
                  cache_holder      = Holder} = ?cache_servers(Key),

    Ret = case (size(Value) < ChunkThresholdLen) of
              true when Active1 == true ->
                  RC:put(Id1, Key, Value);
              true ->
                  ok;
              false when Active2 == true ->
                  DC:put(Id2, Key, Value);
              false ->
                  ok
          end,

    leo_cache_holder:release(Holder, Key),
    Ret.

%% @doc Insert a chunked-object into the disc
-spec(put(Ref, Key, Value) ->
             ok | {error, any()} when Ref::reference(),
                                      Key::binary(),
                                      Value::binary()).
put(Ref, Key, Value) ->
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key),
    case Active of
        true ->
            DC:put(Id, Ref, Key, Value);
        false ->
            not_found
    end.


%% @doc Insert a chunked-object into the disc
-spec(put_begin_tran(Key) ->
             {ok, reference()} | {error, any()} when Key::binary()).
put_begin_tran(Key) ->
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key),
    case Active of
        true ->
            DC:put_begin_tran(Id, Key);
        false ->
            {error, ?ERROR_INVALID_OPERATION}
    end.


%% @doc Insert a chunked-object into the disc
-spec(put_end_tran(Ref, Key, Meta, IsCommit) ->
             {ok, reference()} | {error, any()} when Ref::reference(),
                                                     Key::binary(),
                                                     Meta::#cache_meta{},
                                                     IsCommit::boolean()).
put_end_tran(Ref, Key, Meta, IsCommit) ->
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active,
                  cache_holder = Holder} = ?cache_servers(Key),
    Ret = case Active of
              true ->
                  DC:put_end_tran(Id, Ref, Key, Meta, IsCommit);
              false ->
                  {error, ?ERROR_INVALID_OPERATION}
          end,

    leo_cache_holder:release(Holder, Key),
    Ret.


%% @doc Remove an object from the momory storage
-spec(delete(Key) ->
             ok | {error, any()} when Key::binary()).
delete(Key) ->
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2} = ?cache_servers(Key),

    case Active1 of
        true ->
            case RC:delete(Id1, Key) of
                ok when Active2 == true->
                    DC:delete(Id2, Key);
                ok ->
                    ok;
                {error, Cause} ->
                    {error, Cause}
            end;
        false ->
            ok
    end.


%% @doc Retrieve status of this application
%%
-spec(stats() ->
             {ok, any()}).
stats() ->
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_active = Active2} = ?cache_servers([]),
    case Active1 of
        true ->
            case RC:stats() of
                {ok, #stats{get     = G1,
                            put     = P1,
                            delete  = D1,
                            hits    = H1,
                            records = R1,
                            size    = S1} = Stats} when Active2 == true->
                    case DC:stats() of
                        {ok, #stats{get     = G2,
                                    put     = P2,
                                    delete  = D2,
                                    hits    = H2,
                                    records = R2,
                                    size    = S2}} ->
                            {ok, #stats{get     = G1+G2,
                                        put     = P1+P2,
                                        delete  = D1+D2,
                                        hits    = H1+H2,
                                        records = R1+R2,
                                        size    = S1+S2}};
                        _ ->
                            {ok, Stats}
                    end;
                {ok, Stats} ->
                    {ok, Stats};
                {error, Cause} ->
                    {error, Cause}
            end;
        false ->
            ok
    end.
