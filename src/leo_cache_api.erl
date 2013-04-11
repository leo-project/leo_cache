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

    ok = leo_misc:set_env(leo_cache, ?PROP_OPTIONS,
                          Options ++ [{?PROP_RAM_CACHE_MOD,  RC},
                                      {?PROP_DISC_CACHE_MOD, DC},
                                      {?PROP_RAM_CACHE_ACTIVE,  IsActiveRAMCache},
                                      {?PROP_DISC_CACHE_ACTIVE, IsActiveDiscCache}
                                     ]),
    catch ets:new(?ETS_CACHE_HANDLERS, [named_table, set, public, {read_concurrency, true}]),

    case IsActiveRAMCache of
        true ->
            Workers1 = leo_misc:get_value(?PROP_RAM_CACHE_WORKERS, Options),
            ok = RC:start(Workers1, Options);
        false ->
            void
    end,

    case IsActiveDiscCache of
        true ->
            Workers2 = leo_misc:get_value(?PROP_DISC_CACHE_WORKERS, Options),
            ok = DC:start(Workers2, Options);
        false ->
            void
    end,
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
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key, Options),
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
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2} = ?cache_servers(Key, Options),

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
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key, Options),
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
    {ok, Options}  = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    ChunkThresholdLen = leo_misc:get_value(?PROP_DISC_CACHE_THRESHOLD_LEN, Options),
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2} = ?cache_servers(Key, Options),

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
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key, Options),
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
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key, Options),
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
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{disc_cache_mod    = DC,
                  disc_cache_index  = Id,
                  disc_cache_active = Active} = ?cache_servers(Key, Options),
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
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2} = ?cache_servers(Key, Options),

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
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_active = Active2} = ?cache_servers([], Options),
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
