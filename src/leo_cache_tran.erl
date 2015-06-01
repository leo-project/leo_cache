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

-export([tran/3,
         has_tran/2,
         done_tran/2]).

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
-spec(tran(pid(), atom(), binary()) ->
             ok).
tran(Pid, Tbl, Key) ->
    gen_server:call(?MODULE, {tran, Pid, Tbl, Key}, ?TRAN_TIMEOUT).


%% @doc
-spec(has_tran(atom(), binary()) ->
             {ok, any()} | {error, any()}).
has_tran(Tbl, Key) ->
    case catch gen_server:call(?MODULE, {has_tran, Tbl, Key}, ?TRAN_WAITTIME) of
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
-spec(done_tran(atom(), binary())->
            ok).
done_tran(Tbl, Key) ->
    gen_server:cast(?MODULE, {done_tran, Tbl, Key}).
%    gen_server:call(?MODULE, {done_tran, Tbl, Key}, ?TRAN_TIMEOUT).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([]) ->
    ets:new(?PROCESSDB, [named_table, set, private]),
    ets:new(?REPLYDB, [named_table, set, private]),
    {ok, unused, ?TRAN_TIMEOUT}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

%%handle_call({unregister, Tbl, Key}, From, State) ->
%%    {Pid, _Ref} = From,
%%    case ets:lookup(?REPLYDB, {Tbl, Key}) of
%%        [{{Tbl, Key}, ReplyList}] ->
%%            ?debugVal(ReplyList),
%%            ReplyList2 = lists:filter(fun({Pid2, _Ref2}) ->
%%                                              Pid2 =/= Pid
%%                                      end, ReplyList),
%%            ?debugVal(ReplyList2),
%%            ets:insert(?REPLYDB, {{Tbl, Key}, ReplyList2});
%%        _ ->
%%            void
%%    end,
%%    {reply, ok, State, ?TRAN_TIMEOUT};

handle_call({tran, Pid, Tbl, Key}, _From, State) ->
    MonitorRef = erlang:monitor(process, Pid),
    ets:insert(?PROCESSDB, {MonitorRef, {Tbl, Key}}), 
    ets:insert_new(?REPLYDB, {{Tbl, Key}, []}),
    {reply, ok, State, ?TRAN_TIMEOUT}; 

handle_call({has_tran, Tbl, Key}, From, State) ->
    case ets:lookup(?REPLYDB, {Tbl, Key}) of
        [{{Tbl, Key}, ReplyList}] ->
            ets:insert(?REPLYDB, {{Tbl, Key}, [From | ReplyList]}),
            {noreply, State, ?TRAN_TIMEOUT};
        _ ->
            {reply, {ok, not_found}, State, ?TRAN_TIMEOUT}
    end;

handle_call({done_tran, Tbl, Key}, _From, State) ->
    reply_all(Tbl, Key),
    {reply, ok, State, ?TRAN_TIMEOUT}.

handle_cast({done_tran, Tbl, Key}, State) ->
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
    ets:delete(?PROCESSDB, MonitorRef),
    erlang:demonitor(MonitorRef),
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
    ets:delete(?REPLYDB),
    ets:delete(?PROCESSDB),
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
    ets:delete(?REPLYDB, {Tbl, Key}).
