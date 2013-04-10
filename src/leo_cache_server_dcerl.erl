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
-module(leo_cache_server_dcerl).
-author("Yosuke Hara").

-include("leo_cache.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/2, stop/0,
         get/2, put/3, delete/2, stats/0, callback/1]).

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @doc Launch cache-server(s)
%%
-spec(start(integer(), list(tuple())) ->
             ok | {error, any()}).
start(_Workers,_Options) ->
    ok.


%% @doc Stop cache-server(s)
%%
-spec(stop() -> ok).
stop() ->
    ok.


%% @doc Retrieve an object from the momory storage
-spec(get(integer(), binary()) ->
             not_found | {ok, binary()} | {error, any()}).
get(_Id,_Key) ->
    ok.


%% @doc Insert an object into the momory storage
-spec(put(integer(), binary(), binary()) ->
             ok | {error, any()}).
put(_Id,_Key,_Value) ->
    ok.


%% @doc Remove an object from the momory storage
-spec(delete(integer(), binary()) ->
             ok | {error, any()}).
delete(_Id,_Key) ->
    ok.


%% @doc Retrieve status of this application
%%
-spec(stats() ->
             {ok, any()}).
stats() ->
    ok.


callback(_) ->
    ok.

%%====================================================================
%% INNER FUNCTIONS
%%====================================================================
