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

-define(ETS_CACHE_HANDLERS, 'leo_caceh_handlers').

-define(TYPE_RAM_CACHE,  'ram').
-define(TYPE_DISC_CACHE, 'disc').

-define(PROP_OPTIONS,            'options').
-define(PROP_RAM_CACHE_WORKERS,  'ram_cache_workers').
-define(PROP_RAM_CACHE_NAME,     'ram_cache_name').
-define(PROP_RAM_CACHE_MOD,      'ram_cache_mod').
-define(PROP_RAM_CACHE_SIZE,     'ram_cache_size').
-define(PROP_RAM_CACHE_ACTIVE,   'ram_cache_active').
-define(PROP_DISC_CACHE_WORKERS, 'disc_cache_workers').
-define(PROP_DISC_CACHE_NAME,    'disc_cache_name').
-define(PROP_DISC_CACHE_MOD,     'disc_cache_mod').
-define(PROP_DISC_CACHE_SIZE,    'disc_cache_size').
-define(PROP_DISC_CACHE_ACTIVE,  'disc_cache_active').
-define(PROP_DISC_CACHE_THRESHOLD_LEN, 'disc_cache_threshold_len').
-define(PROP_DISC_CACHE_DATA_DIR,    'disc_cache_data_dir').
-define(PROP_DISC_CACHE_JOURNAL_DIR, 'disc_cache_journal_dir').

-define(DEF_PROP_RAM_CACHE,  'cherly').
-define(DEF_PROP_DISC_CACHE, 'dcerl').
-define(DEF_PROP_RAM_CACHE_WORKERS,  8).
-define(DEF_PROP_DISC_CACHE_WORKERS, 4).
-define(DEF_PROP_RAM_CACHE_SIZE,  128000000). % about 128MB
-define(DEF_PROP_DISC_CACHE_SIZE, 128000000). % about 128MB
-define(DEF_PROP_DISC_CACHE_THRESHOLD_LEN, 1000000).
-define(DEF_PROP_DISC_CACHE_DATA_DIR,    "./cache/data/").
-define(DEF_PROP_DISC_CACHE_JOURNAL_DIR, "./cache/journal/").


-ifdef(TEST).
-define(DEF_OPTIONS, [
                      {?PROP_RAM_CACHE_NAME,     ?DEF_PROP_RAM_CACHE},
                      {?PROP_RAM_CACHE_WORKERS,  2},
                      {?PROP_RAM_CACHE_SIZE,     100000},

                      {?PROP_DISC_CACHE_NAME,    ?DEF_PROP_DISC_CACHE},
                      {?PROP_DISC_CACHE_WORKERS, 2},
                      {?PROP_DISC_CACHE_SIZE,    100000},
                      {?PROP_DISC_CACHE_THRESHOLD_LEN, 1001},
                      {?PROP_DISC_CACHE_DATA_DIR,      ?DEF_PROP_DISC_CACHE_DATA_DIR},
                      {?PROP_DISC_CACHE_JOURNAL_DIR,   ?DEF_PROP_DISC_CACHE_JOURNAL_DIR}
                     ]).
-else.
-define(DEF_OPTIONS, [
                      {?PROP_RAM_CACHE_NAME,     ?DEF_PROP_RAM_CACHE},
                      {?PROP_RAM_CACHE_WORKERS,  ?DEF_PROP_RAM_CACHE_WORKERS},
                      {?PROP_RAM_CACHE_SIZE,     ?DEF_PROP_RAM_CACHE_SIZE},

                      {?PROP_DISC_CACHE_NAME,    ?DEF_PROP_DISC_CACHE},
                      {?PROP_DISC_CACHE_WORKERS, ?DEF_PROP_DISC_CACHE_WORKERS},
                      {?PROP_DISC_CACHE_SIZE,    ?DEF_PROP_DISC_CACHE_SIZE},
                      {?PROP_DISC_CACHE_THRESHOLD_LEN, ?DEF_PROP_DISC_CACHE_THRESHOLD_LEN},
                      {?PROP_DISC_CACHE_DATA_DIR,      ?DEF_PROP_DISC_CACHE_DATA_DIR},
                      {?PROP_DISC_CACHE_JOURNAL_DIR,   ?DEF_PROP_DISC_CACHE_JOURNAL_DIR}
                     ]).
-endif.


-define(ERROR_RAM_CACHE_INACTIVE,  "RAM cache inactive").
-define(ERROR_DISC_CACHE_INACTIVE, "Disc cache inactive").
-define(ERROR_COULD_NOT_GET_STATS, "Could not get stats").
-define(ERROR_INVALID_OPERATION,   "Invalid operation").

-record(cache_server, {ram_cache_index   :: integer(),
                       ram_cache_mod     :: atom(),
                       ram_cache_active  :: boolean(),
                       disc_cache_index  :: integer(),
                       disc_cache_mod    :: atom(),
                       disc_cache_active :: boolean()
                      }).

-record(stats,  {get     = 0 :: integer(),
                 put     = 0 :: integer(),
                 delete  = 0 :: integer(),
                 hits    = 0 :: integer(),
                 records = 0 :: integer(),
                 size    = 0 :: integer()
                }).

%% Macros
%%
%% Retrieve options
-define(get_options(),
        case leo_misc:get_env(leo_cache, ?PROP_OPTIONS) of
            {ok, _V} -> _V;
            _ -> []
        end).

%% Retrieve process index
-define(get_proc_index(_W,_K1,_O1),
        case leo_misc:get_value(_W, _O1) of
            Val when is_integer(Val) andalso Val > 0 ->
                erlang:phash2(_K1, Val) +1;
            _ -> 0
        end).

%% Retrieve num of workers
-define(get_workers(),
        case leo_misc:get_env(leo_cache, ?PROP_OPTIONS) of
            {ok, Options} ->
                leo_misc:get_value(?PROP_RAM_CACHE_WORKERS, Options);
            _ ->
                0
        end).

%% Retrieve cache-server info
-define(cache_servers(_K2,_O2),
        #cache_server{ram_cache_index   = ?get_proc_index(?PROP_RAM_CACHE_WORKERS,   _K2,_O2),
                      ram_cache_mod     = leo_misc:get_value(?PROP_RAM_CACHE_MOD,    _O2),
                      ram_cache_active  = leo_misc:get_value(?PROP_RAM_CACHE_ACTIVE, _O2),
                      disc_cache_index  = ?get_proc_index(?PROP_DISC_CACHE_WORKERS,  _K2,_O2),
                      disc_cache_mod    = leo_misc:get_value(?PROP_DISC_CACHE_MOD,   _O2),
                      disc_cache_active = leo_misc:get_value(?PROP_DISC_CACHE_ACTIVE,_O2)
                     }).

%% Generate a process-id
-define(gen_proc_id(_I, _P),
        list_to_atom(lists:append([_P, integer_to_list(_I)]))).

%% Generate a module-name
-define(gen_mod_name(_M),
        list_to_atom(lists:append(["leo_cache_server_", atom_to_list(_M)]))).

%% Retrieve a handler
-define(get_handler(_Id, _Prefix),
        case ets:lookup(?ETS_CACHE_HANDLERS, ?gen_proc_id(_Id, _Prefix)) of
            [{_,Handler}|_] ->
                Handler;
            _ ->
                undefined
        end).
