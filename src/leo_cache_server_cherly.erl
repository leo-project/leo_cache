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
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/2, stop/0,
         get_ref/2, get/2, get/3,
         put/3, put/4, put_tran_begin/2, put_tran_end/3,
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
    RamCacheCapacity = leo_misc:get_value(?PROP_RAM_CACHE_SIZE, Options),
    ok = leo_misc:set_env(leo_cache, ?PROP_RAM_CACHE_ACTIVE, (RamCacheCapacity > 0)),
    ok = start_1(Workers, erlang:round(RamCacheCapacity/Workers)),
    ok.


%% @doc Stop cache-server(s)
%%
-spec(stop() -> ok).
stop() ->
    ok.


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
    case get_handler(Id) of
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
    case get_handler(Id) of
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
-spec(put_tran_begin(integer(), binary()|any()) ->
             ok | {error, any()}).
put_tran_begin(_Id,_Key) ->
    {ok, undefine}.


%% @doc End put-transaction for large-object (for large-object)
-spec(put_tran_end(integer(), reference(), binary()|any()) ->
             ok | {error, any()}).
put_tran_end(_Id,_Ref,_Key) ->
    ok.


%% @doc Remove an object from the momory storage
-spec(delete(integer(), binary()) ->
             ok | {error, any()}).
delete( Id, Key) ->
    case get_handler(Id) of
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
    %% @TODO - summarize counter
    ok.


%%====================================================================
%% INNER FUNCTIONS
%%====================================================================
%% @doc Generate Id
%%
-spec(gen_id(integer()) ->
             atom()).
gen_id(Id) ->
    ?gen_proc_id(Id, ?ID_PREFIX).


%% @doc Get a handler
%%
-spec(get_handler(integer()) ->
             reference() | undefine).
get_handler(Id) ->
    case ets:lookup(?ETS_CACHE_HANDLERS, gen_id(Id)) of
        [{_,Handler}|_] ->
            Handler;
        _ ->
            undefined
    end.

%% @doc Start Proc(s)
%%
-spec(start_1(integer(), integer()) ->
             ok).
start_1(0, _) ->
    ok;
start_1(Id, RamCacheCapacity) ->
    ProcId = gen_id(Id),
    {ok, Pid} = cherly_server:start_link(ProcId, RamCacheCapacity),
    true = ets:insert(?ETS_CACHE_HANDLERS, {ProcId, Pid}),
    start_1(Id - 1, RamCacheCapacity).

