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
%% Leo Cache - Cherly (RAM Cache)
%% @doc
%% @end
%%======================================================================
-module(leo_cache_server_cherly).
-author("Yosuke Hara").

-behaviour(leo_cache_behaviour).

-include("leo_cache.hrl").
-include_lib("cherly/include/cherly.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/2, stop/0,
         get_ref/2, get/2, get/3,
         put/3, put/4, put_begin_tran/2, put_end_tran/4,
         delete/2, stats/0]).

-define(ID_PREFIX, "cherly_").

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @doc Launch cache-server(s)
%%
-spec(start(integer(), list(tuple())) ->
             ok | {error, any()}).
start(Workers, Options) ->
    CacheCapacity = leo_misc:get_value(?PROP_RAM_CACHE_SIZE, Options),
    ok = start_1(Workers, erlang:round(CacheCapacity/Workers)),
    ok.


%% @doc Stop cache-server(s)
%%
-spec(stop() -> ok).
stop() ->
    stop_1(?get_workers()).


%% @doc Retrieve a reference of cached object (for large-object)
%%
-spec(get_ref(integer(), binary()) ->
             {ok, reference()} | {error, undefined}).
get_ref(_Id, _Key) ->
    {error, undefined}.


%% @doc Retrieve an object from cache-server
-spec(get(integer(), binary()) ->
             not_found | {ok, binary()} | {error, any()}).
get(Id, Key) ->
    case ?get_handler(Id, ?ID_PREFIX) of
        undefined ->
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case catch gen_server:call(Pid, {get, Key}) of
                {ok, Value} ->
                    {ok, Value};
                not_found ->
                    not_found;
                {_, Cause} ->
                    %% @TODO - process restart
                    {error, Cause}
            end
    end.


%% @doc Retrieve an object from cache-server (for large-object)
-spec(get(integer(), reference(), binary()) ->
             not_found | {ok, binary()} | {error, any()}).
get(_Id,_Ref,_Key) ->
    not_found.


%% @doc Insert an object into the momory storage
-spec(put(integer(), binary(), binary()) ->
             ok | {error, any()}).
put(Id, Key, Value) ->
    case ?get_handler(Id, ?ID_PREFIX) of
        undefined ->
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case catch gen_server:call(Pid, {put, Key, Value}) of
                ok ->
                    ok;
                {_, Cause} ->
                    %% @TODO - process restart
                    {error, Cause}
            end
    end.


%% @doc Insert an object into the cache-server (for large-object)
-spec(put(integer(), reference(), binary()|any(), binary()|any()) ->
             ok | {error, any()}).
put(_Id,_Ref,_Key,_Value) ->
    ok.


%% @doc Start put-transaction for large-object (for large-object)
-spec(put_begin_tran(integer(), binary()|any()) ->
             ok | {error, any()}).
put_begin_tran(_Id,_Key) ->
    {ok, undefine}.


%% @doc End put-transaction for large-object (for large-object)
-spec(put_end_tran(integer(), reference(), binary()|any(), boolean()) ->
             ok | {error, any()}).
put_end_tran(_Id,_Ref,_Key,_IdCommit) ->
    ok.


%% @doc Remove an object from the momory storage
-spec(delete(integer(), binary()) ->
             ok | {error, any()}).
delete( Id, Key) ->
    case ?get_handler(Id, ?ID_PREFIX) of
        undefined ->
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case catch gen_server:call(Pid, {delete, Key}) of
                ok ->
                    ok;
                {_, Cause} ->
                    %% @TODO - process restart
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
-spec(start_1(integer(), integer()) ->
             ok).
start_1(0, _) ->
    ok;
start_1(Id, CacheCapacity) ->
    ProcId = ?gen_proc_id(Id, ?ID_PREFIX),
    {ok, Pid} = cherly_server:start_link(ProcId, CacheCapacity),
    true = ets:insert(?ETS_CACHE_HANDLERS, {ProcId, Pid}),
    start_1(Id - 1, CacheCapacity).


%% @doc Stop Proc(s)
%% @private
stop_1(0) ->
    ok;
stop_1(Id) ->
    case ?get_handler(Id, ?ID_PREFIX) of
        undefined ->
            void;
        Pid ->
            gen_server:cast(Pid, stop)
    end,
    stop_1(Id - 1).


%% @doc Retrieve and summarize stats
%% @private
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
    case ?get_handler(Id, ?ID_PREFIX) of
        undefined ->
            {error, ?ERROR_COULD_NOT_GET_STATS};
        Pid ->
            case catch gen_server:call(Pid, {stats}) of
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

