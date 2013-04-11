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

-behaviour(leo_cache_behaviour).

-include("leo_cache.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start/2, stop/0,
         get_ref/2, get/2, get/3,
         put/3, put/4, put_tran_begin/2, put_tran_end/3,
         delete/2, stats/0]).

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


%% @doc Retrieve a reference of cached object (for large-object)
%%
-spec(get_ref(integer(), binary()) ->
             {ok, reference()} | {error, undefined}).
get_ref(_Id, _Key) ->
    {error, undefined}.


%% @doc Retrieve an object from cache-server
-spec(get(integer(), binary()) ->
             not_found | {ok, binary()} | {error, any()}).
get(_Id,_Key) ->
    ok.


%% @doc Retrieve an object from cache-server (for large-object)
-spec(get(integer(), reference(), binary()) ->
             not_found | {ok, binary()} | {error, any()}).
get(_Id,_Ref,_Key) ->
    not_found.


%% @doc Insert an object into cache-serverx
-spec(put(integer(), binary(), binary()) ->
             ok | {error, any()}).
put(_Id,_Key,_Value) ->
    ok.


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


%% @doc Remove an object from cache-server
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

%%====================================================================
%% INNER FUNCTIONS
%%====================================================================
