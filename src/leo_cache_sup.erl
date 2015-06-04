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
%% Leo Cache
%% @doc
%% @end
%%======================================================================
-module(leo_cache_sup).

-author('Yosuke Hara').

-behaviour(supervisor).

-include("leo_cache.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(MAX_RESTART,              5).
-define(MAX_TIME,                60).
-define(SHUTDOWN_WAITING_TIME, 2000).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    %% Create tables
    ok = leo_misc:init_env(),
    case ets:info(?ETS_RAM_CACHE_HANDLERS) of
        undefined ->
            case ets:new(?ETS_RAM_CACHE_HANDLERS,
                         [named_table, set, public, {read_concurrency, true}]) of
                ?ETS_RAM_CACHE_HANDLERS ->
                    ok;
                _ ->
                    erlang:error({error, could_not_create_ets_table})
            end;
        _ ->
            ok
    end,
    case ets:info(?ETS_DISC_CACHE_HANDLERS) of
        undefined ->
            case ets:new(?ETS_DISC_CACHE_HANDLERS,
                         [named_table, set, public, {read_concurrency, true}]) of
                ?ETS_DISC_CACHE_HANDLERS ->
                    ok;
                _ ->
                    erlang:error({error, could_not_create_ets_table})
            end;
        _ ->
            ok
    end,
    case ets:info(?ETS_CACHE_SERVER_INFO) of
        undefined ->
            case ets:new(?ETS_CACHE_SERVER_INFO,
                         [named_table, set, public, {read_concurrency, true}])of
                ?ETS_CACHE_SERVER_INFO ->
                    ok;
                _ ->
                    erlang:error({error, could_not_create_ets_table})
            end;
        _ ->
            ok
    end,

    %% Set supervised childrens
    Children = [
                {leo_mcerl_sup,
                 {leo_mcerl_sup, start_link, []},
                 permanent,
                 ?SHUTDOWN_WAITING_TIME,
                 supervisor,
                 [leo_mcerl_sup]},

                {leo_dcerl_sup,
                 {leo_dcerl_sup, start_link, []},
                 permanent,
                 ?SHUTDOWN_WAITING_TIME,
                 supervisor,
                 [leo_dcerl_sup]}
               ],
    {ok, {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME}, Children}}.

