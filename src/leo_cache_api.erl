%%======================================================================
%%
%% Leo Cache
%%
%% Copyright (c) 2012-2013 Rakuten, Inc.
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
%% @doc
%% @end
%%======================================================================
-module(leo_cache_api).
-author("Yosuke Hara").

-include("leo_cache.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/0, start/1, stop/0,
         get_ref/1, get/1, get/2,
         put/2, put/3, put_begin_tran/1, put_end_tran/3,
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

-spec(start(list(tuple())) ->
             ok | {error, any()}).
start(Options) ->
    ok = leo_misc:init_env(),
    RC = ?gen_mod_name(leo_misc:get_value(?PROP_RAM_CACHE_NAME,  Options)),
    DC = ?gen_mod_name(leo_misc:get_value(?PROP_DISC_CACHE_NAME, Options)),

    RAMCacheSize  = leo_misc:get_value(?PROP_RAM_CACHE_SIZE, Options),
    DiscCacheSize = leo_misc:get_value(?PROP_DISC_CACHE_SIZE, Options),

    IsActiveRAMCache  = (is_integer(RAMCacheSize)  andalso RAMCacheSize  > 0),
    IsActiveDiscCache = (is_integer(DiscCacheSize) andalso DiscCacheSize > 0
                         andalso IsActiveRAMCache == true),

    Options1 = Options ++ [{?PROP_RAM_CACHE_MOD,  RC},
                           {?PROP_DISC_CACHE_MOD, DC},
                           {?PROP_RAM_CACHE_ACTIVE,  IsActiveRAMCache},
                           {?PROP_DISC_CACHE_ACTIVE, IsActiveDiscCache}
                          ],

    ok = leo_misc:set_env(leo_cache, ?PROP_OPTIONS, Options1),
    catch ets:new(?ETS_RAM_CACHE_HANDLERS,  [named_table, set, public, {read_concurrency, true}]),
    catch ets:new(?ETS_DISC_CACHE_HANDLERS, [named_table, set, public, {read_concurrency, true}]),
    catch ets:new(?ETS_CACHE_SERVER_INFO,   [named_table, set, public, {read_concurrency, true}]),

    CacheWorkers =
        case IsActiveRAMCache of
            true ->
                Workers1 = leo_misc:get_value(?PROP_RAM_CACHE_WORKERS, Options1),
                ok = RC:start(Workers1, Options1),
                Workers1;
            false ->
                0
        end,
    case IsActiveDiscCache of
        true ->
            ok = DC:start(CacheWorkers, Options1);
        false ->
            void
    end,
    ChunkThresholdLen = leo_misc:get_value(?PROP_DISC_CACHE_THRESHOLD_LEN, Options),

    true = ets:insert(?ETS_CACHE_SERVER_INFO,
                      {0, #cache_server{ram_cache_mod       = RC,
                                        ram_cache_active    = IsActiveRAMCache,
                                        disc_cache_mod      = DC,
                                        disc_cache_active   = IsActiveDiscCache,
                                        cache_workers       = CacheWorkers,
                                        chunk_threshold_len = ChunkThresholdLen
                                       }}),
    ok.


%% @doc Stop cache-server(s)
%%
-spec(stop() -> ok).
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
    ok.


%% @doc Retrieve a reference of cached object (for large-object)
-spec(get_ref(binary()) ->
             not_found | {ok, binary()} | {error, any()}).
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


%% @doc Retrieve an object from the momory storage
-spec(get(binary()) ->
             not_found | {ok, binary()} | {error, any()}).
get(Key) ->
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2} = ?cache_servers(Key),

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
-spec(get(reference(), binary()) ->
             not_found | {ok, {binary(), boolean()}} | {error, any()}).
get(Ref, Key) ->
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key),
    case Active of
        true ->
            DC:get(Id, Ref, Key);
        false ->
            not_found
    end.


%% @doc Insert an object into the momory storage
-spec(put(binary(), binary()) ->
             ok | {error, any()}).
put(Key, Value) ->
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2,
                  chunk_threshold_len = ChunkThresholdLen} = ?cache_servers(Key),

    case (size(Value) < ChunkThresholdLen) of
        true when Active1 == true ->
            RC:put(Id1, Key, Value);
        true ->
            ok;
        false when Active2 == true ->
            DC:put(Id2, Key, Value);
        false ->
            ok
    end.


%% @doc Insert a chunked-object into the disc
-spec(put(reference(), binary(), binary()) ->
             ok | {error, any()}).
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
-spec(put_begin_tran(binary()) ->
             {ok, reference()} | {error, any()}).
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
-spec(put_end_tran(reference(), binary(), boolean()) ->
             {ok, reference()} | {error, any()}).
put_end_tran(Ref, Key, IsCommit) ->
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key),
    case Active of
        true ->
            DC:put_end_tran(Id, Ref, Key, IsCommit);
        false ->
            {error, ?ERROR_INVALID_OPERATION}
    end.


%% @doc Remove an object from the momory storage
-spec(delete(binary()) ->
             ok | {error, any()}).
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


%%====================================================================
%% INNER FUNCTIONS
%%====================================================================
