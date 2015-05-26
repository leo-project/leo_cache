-module(leo_cache_holder).

-include_lib("eunit/include/eunit.hrl").

-behavior(gen_server).

-export([start/0, stop/1, init/1]).
-export([hold/2, release/2, wait/2]).
-export([hold/3, wait/3]).
-export([handle_call/3]).
-export([terminate/2]).
-export([handle_cast/2, handle_info/2, code_change/3]).

-record(state, {db :: ets:tid()}).

-define(DEF_HOLD_TIME, 5000).
-define(DEF_WAIT_TIME, 1000).

start() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, stop),
    ok.

init(_Options) ->
    DB = ets:new(?MODULE, [named_table, set, public]),
    {ok, #state{db = DB}}.

hold(Pid, Key) ->
    hold(Pid, Key, ?DEF_HOLD_TIME).
hold(Pid, Key, HoldTime) ->
    gen_server:call(Pid, {hold, Key, HoldTime}).

release(Pid, Key) ->
    gen_server:call(Pid, {release, Key}).

wait(Pid, Key) ->
    wait(Pid, Key, ?DEF_WAIT_TIME).
wait(Pid, Key, WaitTime) ->
    case gen_server:call(Pid, {lookup, Key}) of
        {ok, Holder} when is_pid(Pid) ->
            monitor(process, Holder),
            receive
                _Any ->
                    ok
            after
                WaitTime -> ok
            end;
        {_, _} ->
            ok
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({hold, Key, HoldTime}, _From, State=#state{db = DB})->
    Pid = spawn(fun() ->
                        receive
                            _Any ->
                                ok
                        after
                            HoldTime -> 
                                ets:delete(DB, Key),
                                ok
                        end 
                end),
    case ets:insert_new(DB, {Key, Pid}) of
        true ->
            {reply, {ok, Pid}, State};
        false->
            {reply, {error, exist}, State}
    end;
handle_call({lookup, Key}, _From, State=#state{db = DB}) ->
    case ets:lookup(DB, Key) of
        [{Key, Pid}] ->
            case is_process_alive(Pid) of
                true ->
                    {reply, {ok, Pid}, State};
                false ->
                    ets:delete(DB, Key),
                    {reply, {error, ended}, State}
            end;
        _ ->
            {reply, {error, not_found}, State}
    end;
handle_call({release, Key}, _From, State=#state{db = DB}) ->
    case ets:lookup(DB, Key) of
        [{Key, Pid}] ->
            Pid ! {ok},
            ets:delete(DB, Key),
            {reply, {ok, Pid}, State};
        _ ->
            {reply, {error, not_found}, State}
    end.

terminate(normal, _State=#state{db = DB}) ->
    ets:foldl(fun({_Key, Pid}, Acc) ->
                      Pid ! {ok},
                      Acc
              end, [], DB),
    ets:match_delete(DB, "_"),
    ok.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
