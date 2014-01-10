%% -------------------------------------------------------------------
%%
%% Leo Cache - Benchmarking Suite
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
%% -------------------------------------------------------------------
-module(basho_bench_driver_leo_cache).
-author("Yosuke Hara").

-include("leo_cache.hrl").

-export([new/1,
         run/4]).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    case Id of
        1 ->
            leo_cache_api:start(?DEF_OPTIONS);
        _ -> void
    end,
    {ok, null}.


run(get, KeyGen, _ValueGen, State) ->
    BinKey = list_to_binary(KeyGen()),
    case leo_cache_api:get(BinKey) of
        {ok, _Value} ->
            {ok, State};
        not_found ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(put, KeyGen, ValueGen, State) ->
    BinKey = list_to_binary(KeyGen()),
    case leo_cache_api:put(BinKey, ValueGen()) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

