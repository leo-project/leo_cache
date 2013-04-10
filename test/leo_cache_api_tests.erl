%%====================================================================
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
%% -------------------------------------------------------------------
%% Leo Cache - TEST
%% @doc
%% @end
%%====================================================================
-module(leo_cache_api_tests).
-author('Yosuke Hara').

-include("leo_cache.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

cache_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun suite_1_/1,
                           fun suite_2_/1
                          ]]}.

setup() ->
    ok.

teardown(_) ->
    ok.

%% for RAM Cache
suite_1_(_) ->
    leo_cache_api:start(),

    Key = <<"photo/image/hawaii-0.png">>,
    Value = crypto:rand_bytes(1024),

    ok = leo_cache_api:put(Key, Value),

    {ok, Value1} = leo_cache_api:get(Key),
    ?assertEqual(Value, Value1),

    ok = leo_cache_api:delete(Key),
    not_found = leo_cache_api:get(Key),
    ok.

%% for Disc Cache
suite_2_(_) ->
    ok.

-endif.
