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
-author("Yosuke Hara").

-define(DEF_TOTAL_OF_PROCS, 32).
-define(ETS_CACHE_HANDLERS, 'leo_caceh_handlers').
-define(PROP_OPTIONS,           'options').
-define(PROP_WORKERS,           'num_of_workers').

-define(PROP_RAM_CACHE_MOD,     'ram_cache_mod').
-define(PROP_RAM_CACHE_SIZE,    'ram_cache_size').
-define(PROP_RAM_CACHE_ACTIVE,  'ram_cache_active').
-define(PROP_DISC_CACHE_MOD,    'disc_cache_mod').
-define(PROP_DISC_CACHE_SIZE,   'disc_cache_size').
-define(PROP_DISC_CACHE_ACTIVE, 'disc_cache_active').

-define(ERROR_RAM_CACHE_INACTIVE,  "RAM cache inactive").
-define(ERROR_DISC_CACHE_INACTIVE, "Disc cache inactive").

-define(cache_servers(_K,_O), {get_id(_K,_O),
                               leo_misc:get_value(?PROP_RAM_CACHE_MOD, _O),
                               leo_misc:get_value(?PROP_DISC_CACHE_MOD,_O)}).

