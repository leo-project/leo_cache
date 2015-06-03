%%====================================================================
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
%% -------------------------------------------------------------------
%% Leo Cache - TEST
%% @doc
%% @end
%%====================================================================
-module(leo_cache_tran_tests).

-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).
lock_test() ->
    ?debugMsg(" ===== Basic Test ====="),
    leo_cache_tran:start_link(),

    ?debugMsg(" * Lock   {object, 1}"),
    leo_cache_tran:begin_tran(self(), object, 1),

    ?debugMsg(" * Check  {object, 1} (timeout)"),
    {error, timeout} = leo_cache_tran:wait_tran(object, 1),

    ?debugMsg(" * Check  {object, 2} (not found)"),
    {ok, not_found} = leo_cache_tran:wait_tran(object, 2),

    ?debugMsg(" * Unlock {object, 1} after short delay"),
    timer:apply_after(200, leo_cache_tran, end_tran, [object, 1]),

    ?debugMsg(" * Check  {object, 1} (done)"),
    {ok, done} = leo_cache_tran:wait_tran(object, 1),

    ?debugMsg(" * Check  {object, 1} (not found)"),
    {ok, not_found} = leo_cache_tran:wait_tran(object, 1),

    leo_cache_tran:stop(),
    ok.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

-endif.
