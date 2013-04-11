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
    os:cmd("rm -rf ./cache"),
    ok.

teardown(_) ->
    ok.

%% for RAM Cache
suite_1_(_) ->
    leo_cache_api:start(),

    Key1 = <<"photo/image/hawaii-0.png">>,
    Key2 = <<"photo/image/hawaii-1.png">>,
    Value = crypto:rand_bytes(128),

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
    ?assertEqual(3, G),
    ?assertEqual(2, P),
    ?assertEqual(2, D),
    ?assertEqual(1, H),
    ?assertEqual(1, R),
    ?assertEqual(true, (S >= 128)),
    leo_cache_api:stop(),
    ok.

%% for Disc Cache
suite_2_(_) ->
    %% Launch Server
    leo_cache_api:start(),

    %% Test - Put#1
    Src = init_source(),
    BinBody = data_block(Src, 1024),
    BinKey  = <<"test.com/b/path_to_file.jpg">>,

    ok = leo_cache_api:put(BinKey, BinBody),

    {ok, CS} = leo_cache_api:stats(),
    ?assertEqual(1, CS#stats.put),
    ?assertEqual(1, CS#stats.records),

    %% Test - Get/Delete
    {ok, BinBody} = leo_cache_api:get(BinKey),
    ?assertEqual(1024, byte_size(BinBody)),

    ok = leo_cache_api:delete(BinKey),
    {ok, CS2} = leo_cache_api:stats(),
    ?assertEqual(2, CS2#stats.delete),
    ?assertEqual(0, CS2#stats.records),

    %% Test - PUT#2
    {ok, Ref} = leo_cache_api:put_begin_tran(BinKey),
    Chunk = data_block(Src, 1001),
    ok = leo_cache_api:put(Ref, BinKey, Chunk),
    ok = leo_cache_api:put(Ref, BinKey, Chunk),
    ok = leo_cache_api:put(Ref, BinKey, Chunk),
    ok = leo_cache_api:put_end_tran(Ref, BinKey, true),

    {ok, CS3} = leo_cache_api:stats(),
    ?assertEqual(2, CS3#stats.put),
    ?assertEqual(1, CS3#stats.records),

    %% Test - Get#2/Delete#2
    {ok, Bin2} = leo_cache_api:get(BinKey),
    ?assertEqual((1001*3), byte_size(Bin2)),

    {ok, Ref2} = leo_cache_api:get_ref(BinKey),
    ok = get_chunked(Ref2, BinKey, Chunk),

    {ok, CS4} = leo_cache_api:stats(),
    ?assertEqual(5, CS4#stats.get),
    ?assertEqual(3, CS4#stats.hits),
    ?assertEqual(1, CS4#stats.records),
    ok = leo_cache_api:delete(BinKey),
    ok.


%% gen test data
init_source() ->
    SourceSz = 1024 * 1024,
    {SourceSz, crypto:rand_bytes(SourceSz)}.

data_block({SourceSz, Source}, BlockSize) ->
    case SourceSz - BlockSize > 0 of
        true ->
            Offset = random:uniform(SourceSz - BlockSize),
            <<_:Offset/bytes, Slice:BlockSize/bytes, _Rest/binary>> = Source,
            Slice;
        false ->
            Source
    end.

get_chunked(Ref, Key, Chunk) ->
    case leo_cache_api:get(Ref, Key) of
        {ok, done} ->
            ok;
        {ok, Chunk} ->
            get_chunked(Ref, Key, Chunk)
    end.

-endif.
