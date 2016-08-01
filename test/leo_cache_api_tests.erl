%%====================================================================
%%
%% Leo Cache
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
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
-include_lib("leo_dcerl/include/leo_dcerl.hrl").
-include_lib("leo_tran/include/leo_tran.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).
-behaviour(leo_tran_behaviour).
%% Callbakcs for leo_tran_behaviour
-export([run/5, wait/5, resume/5, commit/5, rollback/6]).

cache_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun suite_1_/1,
                           fun suite_2_/1
                          ]]}.

setup() ->
    ok = application:start(leo_tran),
    ok = application:start(leo_cache),
    os:cmd("rm -rf ./cache"),
    ok.

teardown(_) ->
    ok = application:stop(leo_cache),
    ok = application:stop(leo_tran),
    ok.

%% for RAM Cache
suite_1_(_) ->
    Options =  [{?PROP_RAM_CACHE_NAME,     ?DEF_PROP_RAM_CACHE},
                {?PROP_RAM_CACHE_WORKERS,  2},
                {?PROP_RAM_CACHE_SIZE,     1024 * 1024 * 16},
                {?PROP_DISC_CACHE_NAME,    ?DEF_PROP_DISC_CACHE},
                {?PROP_DISC_CACHE_WORKERS, 0},
                {?PROP_DISC_CACHE_SIZE,    0},
                {?PROP_DISC_CACHE_THRESHOLD_LEN, 1001},
                {?PROP_DISC_CACHE_DATA_DIR,    ?DEF_PROP_DISC_CACHE_DATA_DIR},
                {?PROP_DISC_CACHE_JOURNAL_DIR, ?DEF_PROP_DISC_CACHE_JOURNAL_DIR}
               ],
    leo_cache_api:start(Options),

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
    ?assertEqual(2, G),
    ?assertEqual(2, P),
    ?assertEqual(1, D),
    ?assertEqual(1, H),
    ?assertEqual(1, R),
    ?assertEqual(true, (S >= 128)),

    %% leo_cache's ets has no records.
    ets:delete_all_objects(?ETS_CACHE_SERVER_INFO),
    _TestKey = <<"path/to/object">>,
    ok = leo_cache_api:put(_TestKey, <<>>),
    not_found = leo_cache_api:get(_TestKey),
    ok = leo_cache_api:delete(_TestKey),
    {ok,{stats,0,0,0,0,0,0}} = leo_cache_api:stats(),

    leo_cache_api:stop(),
    ok.

%% for Disc Cache
suite_2_(_) ->
    %% Launch Server
    Options =  [{?PROP_RAM_CACHE_NAME,     ?DEF_PROP_RAM_CACHE},
                {?PROP_RAM_CACHE_WORKERS,  2},
                {?PROP_RAM_CACHE_SIZE,     1024 * 1024 * 4},
                {?PROP_DISC_CACHE_NAME,    ?DEF_PROP_DISC_CACHE},
                {?PROP_DISC_CACHE_WORKERS, 2},
                {?PROP_DISC_CACHE_SIZE,    1024 * 1024 * 16},
                {?PROP_DISC_CACHE_THRESHOLD_LEN, 1001},
                {?PROP_DISC_CACHE_DATA_DIR,    ?DEF_PROP_DISC_CACHE_DATA_DIR},
                {?PROP_DISC_CACHE_JOURNAL_DIR, ?DEF_PROP_DISC_CACHE_JOURNAL_DIR}
               ],
    leo_cache_api:start(Options),

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
    Chunk = data_block(Src, 8192),
    PidList = [
    erlang:spawn(fun() ->
        case leo_tran:run(BinKey, null, null, ?MODULE, Chunk, [{?PROP_IS_WAIT_FOR_TRAN, false}]) of
            {error, ?ERROR_ALREADY_HAS_TRAN} ->
                io:format(user, "[reader] start bin:~p~n", [BinKey]),
                {ok, Ref} = leo_cache_api:put_begin_tran(read, BinKey),
                leo_tran:wait(BinKey, null, null),
                {ok, Chunk} = file:read(Ref, 8192),
                {ok, Chunk} = file:read(Ref, 8192),
                {ok, Chunk} = file:read(Ref, 8192),
                eof = file:read(Ref, 8192),
                ok = leo_cache_api:put_end_tran(Ref, read, BinKey, undef, true),
                %io:format(user, "[reader] end bin:~p ret:~p ~n", [BinKey, Ret]);
                io:format(user, "[reader] end bin:~p ~n", [BinKey]);
            {value, _} ->
                void
        end
    end) || _ <- lists:seq(1, 5)
    ],
    wait_processes(PidList),

    {ok, CS3} = leo_cache_api:stats(),
    ?assertEqual(2, CS3#stats.put),
    ?assertEqual(1, CS3#stats.records),

    %% Test - Get#2/Delete#2
    {ok, Bin2} = leo_cache_api:get(BinKey),
    ?assertEqual((8192*3), byte_size(Bin2)),

    {ok, Ref2} = leo_cache_api:get_ref(BinKey),
    ok = get_chunked(Ref2, BinKey),

    {ok, CM2} = leo_cache_api:get_filepath(BinKey),
    ?assertEqual(8192*3, CM2#cache_meta.size),
    ?assertEqual(1, CM2#cache_meta.md5),
    ?assertEqual(123, CM2#cache_meta.mtime),
    ?assertEqual("image/jpeg", CM2#cache_meta.content_type),

    {ok, CS4} = leo_cache_api:stats(),
    ?assertEqual(6, CS4#stats.get),
    ?assertEqual(4, CS4#stats.hits),
    ?assertEqual(1, CS4#stats.records),
    ok = leo_cache_api:delete(BinKey),
    ok.

wait_processes([]) ->
    ok;
wait_processes([H|Rest] = List) ->
    case is_process_alive(H) of
        true ->
            timer:sleep(100),
            wait_processes(List);
        false ->
            wait_processes(Rest)
    end.

%% Callbakcs for leo_tran_behaviour
run(BinKey, _, _, Chunk, State) ->
    io:format(user, "[writer] start bin:~p state~p~n", [BinKey, State]),
    {ok, Ref} = leo_cache_api:put_begin_tran(write, BinKey),
    timer:sleep(100),
    ok = leo_cache_api:put(Ref, BinKey, Chunk),
    ok = leo_cache_api:put(Ref, BinKey, Chunk),
    ok = leo_cache_api:put(Ref, BinKey, Chunk),
    CM = #cache_meta{
        md5 = 1,
        mtime = 123,
        content_type = "image/jpeg"},
    ok = leo_cache_api:put_end_tran(Ref, write, BinKey, CM, true),
    leo_tran:notify_all(BinKey, null, null),
    io:format(user, "[writer] finish bin:~p~n", [BinKey]),
    ok.

wait(_, _, _, _, _) ->
        ok.
resume(_, _, _, _, _) ->
        ok.
commit(_, _, _, _, _) ->
        ok.
rollback(_, _, _, _, _, _) ->
        ok.

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

get_chunked(Ref, Key) ->
    case leo_cache_api:get(Ref, Key) of
        {ok, done} ->
            ok;
        {ok, _Chunk} ->
            get_chunked(Ref, Key)
    end.

-endif.
