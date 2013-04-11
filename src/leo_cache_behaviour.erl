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
%% Leo Cache Behaviour
%% @doc
%% @end
%%======================================================================
-module(leo_cache_behaviour).
-author("Yosuke Hara").


%% @doc Launch target server
-callback(start(Workers::integer(), Options::list(tuple())) ->
                 ok | {error, any()}).

%% @doc Stop target server
-callback(stop() ->
                 ok | {error, any()}).

%% @doc Retrieve a cache reference
-callback(get_ref(Id::integer(), Key::binary()|any()) ->
                 {ok, reference()} | {error, undefined}).

%% @doc Retrieve an object from server
-callback(get(Id::integer(), Key::binary()|any()) ->
                 ok | {error, any()}).

-callback(get(Id::integer(), Ref::reference(), Key::binary()|any()) ->
                 ok | {error, any()}).

%% @doc Insert an object into server
-callback(put(Id::integer(), Key::binary()|any(), Value::binary()|any()) ->
                 ok | {error, any()}).

%% @doc Insert an object into server
-callback(put(Id::integer(), Ref::reference(), Key::binary()|any(), Value::binary()|any()) ->
                 ok | {error, any()}).

%% @doc Start put transaction for large-object
-callback(put_begin_tran(Id::integer(), Key::binary()|any()) ->
                 ok | {error, any()}).

%% @doc Start put transaction for large-object
-callback(put_end_tran(Id::integer(), Ref::reference(), Key::binary()|any(), IsCommit::boolean()) ->
                 ok | {error, any()}).

%% @doc Remove an object into server
-callback(delete(Id::integer(), Key::binary()|any()) ->
                 ok | {error, any()}).

%% @doc Retrieve status of server
-callback(stats() ->
                 ok | {error, any()}).

%% @TODO
%% -callback(callback() ->
%%                  ok | {error, any()}).

