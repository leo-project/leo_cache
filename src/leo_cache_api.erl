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
         put/2, put/3, get/1, get/2, delete/1, stats/0]).
-export([put_tran_begin/1, put_tran_end/2]).

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

    Workers = leo_misc:get_value(?PROP_RAM_CACHE_WORKERS, Options),
    ok = RC:start(Workers, Options),

    %% @Pending
    %% DC = leo_misc:get_value(?PROP_DISC_CACHE_MOD, Options),
    %% ok = DC:start(Workers, Options),
    ?debugVal(DC),
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
    case leo_misc:get_value(?PROP_RAM_CACHE_MOD,  Options) of
        undifined -> void;
        DC -> DC:stop()
    end,
    ok.


%% @doc Retrieve an object from the momory storage
-spec(get(binary()) ->
             not_found | {ok, binary()} | {error, any()}).
get(Key) ->
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = _DC,
                  disc_cache_index  = _Id2,
                  disc_cache_active = Active2} = ?cache_servers(Key, Options),

    case Active1 of
        true ->
            case RC:get(Id1, Key) of
                {ok, Bin} ->
                    {ok, Bin};
                not_found when Active2 == true ->
                    %% pending - DC:get(Id2, Key);
                    not_found;
                not_found ->
                    not_found;
                {error, Cause} ->
                    {error, Cause}
            end;
        false ->
            not_found
    end.


%% @doc Retrieve a chunked-object from the disc
%%
-spec(get(reference(), binary()) ->
             {ok, {binary(), boolean()}} | {error, any()}).
get(_Ref,_Key) ->
    %% TODO
    ok.


%% @doc Insert an object into the momory storage
-spec(put(binary(), binary()) ->
             ok | {error, any()}).
put(Key, Value) ->
    {ok, Options}  = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    MaxRamCacheLen = leo_misc:get_value(?PROP_RAM_CACHE_SIZE, Options),
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = DC,
                  disc_cache_index  = Id2,
                  disc_cache_active = Active2} = ?cache_servers(Key, Options),

    case (size(Value) < MaxRamCacheLen) of
        true when Active1 == true ->
            RC:put(Id1, Key, Value);
        true ->
            ok;
        false when Active2 == true ->
            DC:put(Id2, Key);
        false ->
            ok
    end.


%% @doc Insert a chunked-object into the disc
-spec(put(reference(), binary(), binary()) ->
             ok | {error, any()}).
put(_Ref,_Key,_Chunk) ->
    %% @TODO
    ok.


%% @doc Remove an object from the momory storage
-spec(delete(binary()) ->
             ok | {error, any()}).
delete(Key) ->
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_index   = Id1,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = _DC,
                  disc_cache_index  = _Id2,
                  disc_cache_active = Active2} = ?cache_servers(Key, Options),

    case Active1 of
        true ->
            case RC:delete(Id1, Key) of
                ok when Active2 == true->
                    %% pending - DC:delete(Id2, Key)
                    ok;
                ok ->
                    ok;
                {error, Cause} ->
                    {error, Cause}
            end;
        fasel ->
            ok
    end.


%% @doc Begin put-transaction
-spec(put_tran_begin(binary()) ->
             {ok, reference()} | {error, any()}).
put_tran_begin(_Key) ->
    %% @TODO
    ok.


%% @doc End put-transaction
-spec(put_tran_end(reference(), boolean()) ->
             ok | {error, any()}).
put_tran_end(_Ref,_Key) ->
    %% @TODO
    ok.


%% @doc Retrieve status of this application
%%
-spec(stats() ->
             {ok, any()}).
stats() ->
    {ok, Options} = leo_misc:get_env(leo_cache, ?PROP_OPTIONS),
    #cache_server{ram_cache_mod     = RC,
                  ram_cache_active  = Active1,
                  disc_cache_mod    = _DC,
                  disc_cache_active = Active2} = ?cache_servers([], Options),

    case Active1 of
        true ->
            case RC:stats() of
                {ok, Stats} when Active2 == true->
                    %% pending - DC:stats(Id2, Key)
                    {ok, Stats};
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
%% @doc Retrieve a srever id
%% @private

