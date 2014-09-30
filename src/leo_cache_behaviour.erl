%%======================================================================
%%
%% Leo Cache
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
%% ---------------------------------------------------------------------
%% Leo Cache Behaviour
%% @doc
%% @end
%%======================================================================
-module(leo_cache_behaviour).
-author("Yosuke Hara").

-include_lib("leo_dcerl/include/leo_dcerl.hrl").

-callback(start(Workers, Options) ->
                 ok | {error, any()} when Workers::integer(),
                                          Options::list(tuple())).

-callback(stop() ->
                 ok | {error, any()}).

-callback(get_ref(Id, Key) ->
                 {ok, reference()} | {error, undefined} when Id::integer(),
                                                             Key::binary()|any()).

-callback(get_filepath(Id, Key) ->
                 {ok, #cache_meta{}} | {error, undefined} when Id::integer(),
                                                               Key::binary()|any()).

-callback(get(Id, Key) ->
                 {ok, binary()} | not_found | {error, any()} when Id::integer(),
                                                                  Key::binary()|any()).

-callback(get(Id, Ref, Key) ->
                 {ok, binary()} | not_found | {error, any()} when Id::integer(),
                                                                  Ref::reference(),
                                                                  Key::binary()|any()).

-callback(put(Id, Key, Value) ->
                 ok | {error, any()} when Id::integer(),
                                          Key::binary()|any(),
                                          Value::binary()|any()).

-callback(put(Id, Ref, Key, Value) ->
                 ok | {error, any()} when Id::integer(),
                                          Ref::reference(),
                                          Key::binary()|any(),
                                          Value::binary()|any()).

-callback(put_begin_tran(Id, Key) ->
                 ok | {error, any()} when Id::integer(),
                                          Key::binary()|any()).

-callback(put_end_tran(Id, Ref, Key, Meta, IsCommit) ->
                 ok | {error, any()} when Id::integer(),
                                          Ref::reference(),
                                          Key::binary()|any(),
                                          Meta::#cache_meta{},
                                          IsCommit::boolean()).

-callback(delete(Id, Key) ->
                 ok | {error, any()} when Id::integer(),
                                          Key::binary()|any()).

-callback(stats() ->
                 {ok, any()} | {error, any()}).

%% @TODO
%% -callback(callback() ->
%%                  ok | {error, any()}).

