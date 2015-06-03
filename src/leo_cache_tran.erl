%%======================================================================
%%
%% LeoProject
%%
%% Copyright (c) 2013-2014 Rakuten, Inc.
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
%%======================================================================
-module(leo_cache_tran).
-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_cache.hrl").
-include_lib("eunit/include/eunit.hrl").


%% API
-export([start_link/0,
         stop/0]).

-export([begin_tran/3,
         wait_tran/2, wait_tran/3,
         end_tran/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PROCESSDB, leo_cache_tran_proc_db).
-define(REPLYDB, leo_cache_tran_reply_db).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% @doc
-spec(begin_tran(pid(), atom(), binary()) ->
             ok | {error, in_process}).
begin_tran(Pid, Tbl, Key) ->
    gen_server:call(?MODULE, {begin_tran, Pid, Tbl, Key}, ?TRAN_TIMEOUT).


%% @doc
-spec(wait_tran(atom(), binary()) ->
             {ok, any()} | {error, any()}).
wait_tran(Tbl, Key) ->
    wait_tran(Tbl, Key, ?TRAN_WAITTIME).

%% @doc
-spec(wait_tran(atom(), binary(), integer()) ->
             {ok, any()} | {error, any()}).
wait_tran(Tbl, Key, Waittime) ->
    case catch gen_server:call(?MODULE, {wait_tran, Tbl, Key}, Waittime) of
        {'EXIT', {timeout, _}} ->
%%            gen_server:call(?MODULE, {unregister, Tbl, Key}, ?TRAN_TIMEOUT),
            {error, timeout};
        {'EXIT', Reason} ->
%%            gen_server:call(?MODULE, {unregister, Tbl, Key}, ?TRAN_TIMEOUT),
            {error, Reason};
        Ret ->
            Ret
    end.

%% @doc
-spec(end_tran(atom(), binary())->
            ok).
end_tran(Tbl, Key) ->
    gen_server:cast(?MODULE, {end_tran, Tbl, Key}).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([]) ->
    case ets:new(?PROCESSDB, [named_table, set, private]) of
        ?PROCESSDB ->
            ok;
        _ ->
            erlang:error({error, could_not_create_ets_table})
    end,
    case ets:new(?REPLYDB, [named_table, set, private]) of
        ?REPLYDB ->
            ok;
        _ ->
            erlang:error({error, could_not_create_ets_table})
    end,
    {ok, unused, ?TRAN_TIMEOUT}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

%%handle_call({unregister, Tbl, Key}, From, State) ->
%%    {Pid, _Ref} = From,
%%    case ets:lookup(?REPLYDB, {Tbl, Key}) of
%%        [{{Tbl, Key}, ReplyList}] ->
%%            ReplyList2 = lists:filter(fun({Pid2, _Ref2}) ->
%%                                              Pid2 =/= Pid
%%                                      end, ReplyList),
%%            true = ets:insert(?REPLYDB, {{Tbl, Key}, ReplyList2});
%%        _ ->
%%            void
%%    end,
%%    {reply, ok, State, ?TRAN_TIMEOUT};

handle_call({begin_tran, Pid, Tbl, Key}, _From, State) ->
    MonitorRef = erlang:monitor(process, Pid),
    true = ets:insert(?PROCESSDB, {MonitorRef, {Tbl, Key}}), 
    case ets:insert_new(?REPLYDB, {{Tbl, Key}, []}) of
        true ->
            {reply, ok, State, ?TRAN_TIMEOUT};
        false ->
            {reply, {error, in_process}, State, ?TRAN_TIMEOUT}
    end;

handle_call({wait_tran, Tbl, Key}, From, State) ->
    case ets:lookup(?REPLYDB, {Tbl, Key}) of
        [{{Tbl, Key}, ReplyList}] ->
            true = ets:insert(?REPLYDB, {{Tbl, Key}, [From | ReplyList]}),
            {noreply, State, ?TRAN_TIMEOUT};
        _ ->
            {reply, {ok, not_found}, State, ?TRAN_TIMEOUT}
    end;

handle_call({end_tran, Tbl, Key}, _From, State) ->
    reply_all(Tbl, Key),
    {reply, ok, State, ?TRAN_TIMEOUT}.

handle_cast({end_tran, Tbl, Key}, State) ->
    reply_all(Tbl, Key),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% handle_info({_Label, {_From, MRef}, get_modules}, State) ->
%%     {noreply, State};
handle_info({'DOWN', MonitorRef, _Type, _Pid,_Info}, State) ->
    case ets:lookup(?PROCESSDB, MonitorRef) of
        [{MonitorRef, {Tbl, Key}}] ->
            reply_all(Tbl, Key);
        _ ->
            void
    end,
    true = ets:delete(?PROCESSDB, MonitorRef),
    true = erlang:demonitor(MonitorRef),
    {noreply, State, ?TRAN_TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State, ?TRAN_TIMEOUT}.


%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason,_State) ->
    ets:foldl(fun({_Key, ReplyList}, Acc) ->
                      lists:foreach(fun(Target) ->
                                            gen_server:reply(Target, {error, terminated})
                                    end, ReplyList),
                      Acc
              end, [], ?REPLYDB),
    ok.


%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% INNER FUNCTIONS
%%--------------------------------------------------------------------
reply_all(Tbl, Key) ->
    case ets:lookup(?REPLYDB, {Tbl, Key}) of 
        [{{Tbl, Key}, ReplyList}] when is_list(ReplyList) ->
            lists:foreach(fun(Target) ->
                                  gen_server:reply(Target, {ok, done})
                          end, ReplyList);
        _ ->
            void
    end,
    true = ets:delete(?REPLYDB, {Tbl, Key}).
