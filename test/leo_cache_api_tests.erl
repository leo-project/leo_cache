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

    Key1 = <<"photo/image/hawaii-0.png">>,
    Key2 = <<"photo/image/hawaii-1.png">>,
    Value = crypto:rand_bytes(1024),

    ok = leo_cache_api:put(Key1, Value),
    ok = leo_cache_api:put(Key2, Value),

    {ok, Value1} = leo_cache_api:get(Key1),
    ?assertEqual(Value, Value1),

    ok = leo_cache_api:delete(Key1),
    not_found = leo_cache_api:get(Key1),

    {ok, Stats} = leo_cache_api:stats(),
    #stats{get     = G,
           put     = P,
           delete  = D,
           hits    = H,
           records = R,
           size    = S} = Stats,
    ?assertEqual(2, G),
    ?assertEqual(2, P),
    ?assertEqual(1, D),
    ?assertEqual(1, H),
    ?assertEqual(1, R),
    ?assertEqual(true, (S >= 1024)),
    leo_cache_api:stop(),
    ok.

%% for Disc Cache
suite_2_(_) ->
    ok.

-endif.
