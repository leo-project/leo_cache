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
-module(leo_cache_server_cherly).
-author("Yosuke Hara").

-include("leo_cache.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/2, stop/0,
         get/2, put/3, delete/2, stats/0, callback/1]).

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


%% @doc Retrieve an object from the momory storage
-spec(get(integer(), binary()) ->
             not_found | {ok, binary()} | {error, any()}).
get(Id, Key) ->
    case get_handler(Id) of
        undefined ->
            {error, ?ERROR_DISC_CACHE_INACTIVE};
        Pid ->
            case catch gen_server:call(Pid, {get, Key}) of
                {ok, Value} ->
                    %% @TODO - count-up
                    {ok, Value};
                not_found ->
                    not_found;
                {_, Cause} ->
                    %% @TODO - process restart
                    {error, Cause}
            end
    end.


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
                    %% @TODO - count-up
                    ok;
                {_, Cause} ->
                    %% @TODO - process restart
                    {error, Cause}
            end
    end.


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
                    %% @TODO - count-up
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
    %% @TODO - summarize count-values
    ok.


callback(_) ->
    ok.

%%====================================================================
%% INNER FUNCTIONS
%%====================================================================
%% @doc Generate Id
%%
-spec(gen_id(integer()) ->
             atom()).
gen_id(Id) ->
    list_to_atom(lists:append([?ID_PREFIX, integer_to_list(Id)])).


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

