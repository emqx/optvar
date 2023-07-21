%%--------------------------------------------------------------------
%% Copyright (c) 2021-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc This module implements a condition variable
%%
%% Warning: don't wait for condition variables that are never going to
%% be set with a timeout: it will leave a proxy process in the
%% system. Only use this module for a strictly known set of condition
%% variables that are expected to be set eventually.
-module(optvar).

%% API:
-export([init/0, stop/0,
         set/2, unset/1, is_set/1, read/1, read/2, peek/1, wait_vars/2,
         list/0, list_all/0]).

%% Internal exports:
-export([waker_entrypoint/2]).

%%================================================================================
%% Types
%%================================================================================

-type key() :: term().
-type value() :: term().

-define(status_tab, optvar_status_tab).

%%================================================================================
%% API funcions
%%================================================================================

-spec init() -> ok.
init() ->
    ets:new(?status_tab, [ set
                         , named_table
                         , public
                         , {read_concurrency, true}
                         , {write_concurrency, false}
                         ]),
    ok.

-spec stop() -> ok.
stop() ->
    Wakers = lists:flatten(ets:match(?status_tab, {'_', {unset, '$1'}})),
    [exit(I, kill) || I <- Wakers],
    ets:delete(?status_tab),
    ok.

%% @doc Set the value of a condition variable.
%%
%% Warning: only one process can set a condition variable at a
%% time. Race conditions between different processes setting the
%% condition variable are not handled.
-spec set(key(), value()) -> ok.
set(Key, Value) ->
    case ets:lookup(?status_tab, Key) of
        [{_, {set, _OldValue}}] ->
            ets:insert(?status_tab, {Key, {set, Value}});
        [{_, {unset, Pid}}] ->
            %% Notify waker and wait for it update the value:
            MRef = monitor(process, Pid),
            Pid ! {set, Value},
            receive
                {'DOWN', MRef, _, _, {optvar_set, _}} ->
                    ok;
                {'DOWN', MRef, _, _, noproc} ->
                    banish_zombie(Key, Pid),
                    set(Key, Value)
            end;
        [] ->
            %% The value is not set, and nobody waits for it:
            case ets:insert_new(?status_tab, {Key, {set, Value}}) of
                true  ->
                    ok;
                false ->
                    %% Race condition: someone just installed a waker
                    %% process. Retry:
                    set(Key, Value)
            end
    end,
    ok.

%% @doc Delete the value of the condition variable:
-spec unset(key()) -> ok.
unset(Key) ->
    case ets:lookup(?status_tab, Key) of
        [{_, {set, _OldValue}}] -> ets:delete(?status_tab, Key);
        %% If there is a waker process, we just leave it alone:
        _                       -> ok
    end,
    ok.

%% @doc Check if the variable is set
-spec is_set(key()) -> boolean().
is_set(Key) ->
    peek(Key) =/= undefined.

%% @doc Read the value of the variable if it's set, but don't wait for
%% it
-spec peek(key()) -> {ok, value()} | undefined.
peek(Key) ->
    case ets:lookup(?status_tab, Key) of
        [{_, {set, Val}}] -> {ok, Val};
        _                 -> undefined
    end.

%% @doc Wait for the variable to be set and return the value
-spec read(key()) -> value().
read(Key) ->
    {ok, Value} = read(Key, infinity),
    Value.

%% @doc Wait for the variable to be set and return the value if it was
%% set within the timeout
-spec read(key(), timeout()) -> {ok, value()} | timeout.
read(Key, Timeout) ->
    StartT = current_time(Timeout),
    case read_or_wait(Key) of
        {set, Value} ->
            {ok, Value};
        {wait, MRef} ->
            receive
                %% Rather unconventionally, the actual information is
                %% transmitted in a DOWN message from a temporary
                %% "waker" process. See `waker_loop':
                {'DOWN', MRef, _, _, Reason} ->
                    case Reason of % Assert
                        {optvar_set, Value} ->
                            {ok, Value};
                        noproc ->
                            %% race condition; retry
                            EndT = current_time(Timeout),
                            NewTimeout = new_timeout(Timeout, StartT, EndT),
                            read(Key, NewTimeout)
                    end
            after Timeout ->
                    demonitor(MRef, [flush]),
                    timeout
            end
    end.

%% @doc Wait for multiple variables
-spec wait_vars([key()], timeout()) -> ok | {timeout, [key()]}.
wait_vars(Keys, infinity) ->
    _ = [read(I) || I <- Keys],
    ok;
wait_vars(Keys, Timeout) ->
    L = [{I, MRef} || I <- Keys, {wait, MRef} <- [read_or_wait(I)]],
    {TimedOutKeys, MRefs} = lists:unzip(do_wait_vars(L, Timeout)),
    _ = [demonitor(I, [flush]) || I <- MRefs],
    case TimedOutKeys of
        [] -> ok;
        _  -> {timeout, TimedOutKeys}
    end.

%% @doc List keys that are set
-spec list() -> [key()].
list() ->
    Pattern = {'$1', {set, '_'}},
    ets:select(?status_tab, [{Pattern, [], ['$1']}]).

%% @doc List all keys
-spec list_all() -> [key()].
list_all() ->
    Pattern = {'$1', '_'},
    ets:select(?status_tab, [{Pattern, [], ['$1']}]).

-spec banish_zombie(key(), pid()) -> true.
banish_zombie(Key, Pid) ->
    ets:match_delete(?status_tab, {Key, {unset, Pid}}).

%%================================================================================
%% Internal functions
%%================================================================================

-spec read_or_wait(key()) -> {set, value()} | {wait, reference()}.
read_or_wait(Key) ->
    case ets:lookup(?status_tab, Key) of
        [] ->
            {Pid, MRef} = spawn_monitor(?MODULE, waker_entrypoint, [Key, self()]),
            %% Wait until the newly created process either establishes itself
            %% as a waker for the key, or exits:
            receive
                {Pid, proceed} ->
                    {wait, MRef};
                {'DOWN', MRef, _, _, Reason} ->
                    optvar_retry = Reason, %% assert
                    read_or_wait(Key)
            end;
        [{_, {set, Val}}] ->
            {set, Val};
        [{_, {unset, Pid}}] ->
            MRef = monitor(process, Pid),
            receive
                {'DOWN', MRef, _, _, noproc} ->
                    banish_zombie(Key, Pid),
                    read_or_wait(Key)
            after 0 ->
                    {wait, MRef}
            end
    end.

-spec do_wait_vars([{key(), reference()}], integer()) ->
          [key()].
do_wait_vars([], _) ->
    [];
do_wait_vars([{Key, MRef}|Rest], TimeLeft) ->
    T0 = erlang:monotonic_time(millisecond),
    receive
        {'DOWN', MRef, _, _, Reason} ->
            %% assert:
            case Reason of
                {optvar_set, _} -> ok;
                noproc        -> ok
            end,
            T1 = erlang:monotonic_time(millisecond),
            do_wait_vars(Rest, TimeLeft - (T1 - T0))
    after TimeLeft ->
            [{Key, MRef}|do_wait_vars(Rest, 0)]
    end.

%%================================================================================
%% Waker process implementation
%%================================================================================

-spec waker_entrypoint(key(), pid()) -> no_return().
waker_entrypoint(Key, Parent) ->
    %% The process must keep running, or Bad Things Will Happen
    process_flag(trap_exit, true),
    %% Set the group leader to avoid getting killed by the application
    %% controller when the calling application gets stopped:
    group_leader(whereis(init), self()),
    case ets_insert_new({Key, {unset, self()}}) of
        false ->
            %% Race condition: someone installed the waker before us,
            %% or the variable has been set, so exit and signal the
            %% parent to retry:
            exit(optvar_retry);
        true ->
            %% We are the official waker for the variable now. Wait
            %% for it to be set:
            Parent ! {self(), proceed},
            receive
                {set, Value} ->
                    ets_insert({Key, {set, Value}}),
                    %% This will broadcast the variable value in the
                    %% DOWN message to the processes that monitor us:
                    exit({optvar_set, Value})
            end
    end.

ets_insert(Value) ->
    try ets:insert(?status_tab, Value)
    catch
        error:badarg ->
            exit(optvar_stopped)
    end.

ets_insert_new(Value) ->
    try ets:insert_new(?status_tab, Value)
    catch
        error:badarg ->
            exit(optvar_stopped)
    end.

current_time(infinity = _Timeout) ->
    %% we'll wait indefinitely, no point in reading the time.
    undefined;
current_time(Timeout) when is_integer(Timeout) ->
    erlang:system_time(millisecond).

new_timeout(infinity = _Timeout, _StartT, _EndT) ->
    infinity;
new_timeout(Timeout, StartT, EndT) ->
    max(0, Timeout - (EndT - StartT)).
