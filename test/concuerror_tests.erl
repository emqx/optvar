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

%% NOTE: Concuerror doesn't pick up testcases automatically, add them
%% to the Makefile explicitly
-module(concuerror_tests).

-include_lib("eunit/include/eunit.hrl").

%% Note: the number of interleavings that Concuerror has to explore
%% grows _exponentially_ with the number of concurrent processes and
%% the number of I/O operations that they perform. So all tests in
%% this module should be kept as short and simple as possible and only
%% verify a single property.

optvar_read_test() ->
    init(),
    try
        Val = 42,
        spawn(fun() ->
                      optvar:set(foo, Val)
              end),
        case optvar:read(foo, 100) of
            {ok, Val} -> ok;
            timeout   -> ok
        end,
        ?assertEqual(Val, optvar:read(foo))
    after
        cleanup()
    end.

optvar_zombie_test() ->
    init(),
    Var1 = {foo, bar},
    Var2 = {bar, foo},
    Val1 = 1,
    Val2 = 1,
    try
        timeout = optvar:read(Var1, 0),
        timeout = optvar:read(Var2, 0),
        [{_, {unset, Pid}}] = ets:lookup(optvar_status_tab, Var1),
        %% Create a zombie entry in the status table:
        MRefWaker = monitor(process, Pid),
        exit(Pid, kill),
        receive
            {'DOWN', MRefWaker, _, _, _} -> ok
        end,
        %% Now spawn workers that will encounter the zombie:
        Setter1 = spawn(fun() ->
                                ?assertMatch(ok, optvar:set(Var1, Val1))
                        end),
        Setter2 = spawn(fun() ->
                                ?assertMatch(ok, optvar:set(Var2, Val2))
                        end),
        Reader = spawn(fun() ->
                               ?assertEqual(Val1, optvar:read(Var1)),
                               ?assertEqual(Val2, optvar:read(Var2))
                       end),
        %% Regardless, all the processes should avoid deadloop:
        wait_pids([Reader, Setter1, Setter2])
    after
        cleanup()
    end.

optvar_set_unset_race_test() ->
    init(),
    Var = {foo, bar},
    Val = 1,
    try
        Setter = spawn(fun() ->
                               ?assertMatch(ok, optvar:set(Var, Val))
                       end),
        Unsetter = spawn(fun() ->
                                 ?assertMatch(ok, optvar:unset(Var))
                         end),
        Reader1 = spawn(fun() ->
                                ?assertEqual(Val, optvar:read(Var))
                        end),
        Reader2 = spawn(fun() ->
                                ?assertEqual(Val, optvar:read(Var))
                        end),
        %% Wait until the variable is unset:
        wait_pids([Unsetter]),
        ?assertMatch(ok, optvar:set(Var, Val)),
        %% Now when the variable is set by either process, both reader
        %% processes should be able to terminate:
        wait_pids([Setter, Reader1, Reader2])
    after
        cleanup()
    end.

optvar_set_unset_test() ->
    init(),
    Var = {foo, bar},
    Val = 1,
    try
        Pid1 = spawn(fun() ->
                             ?assertEqual(Val, optvar:read(Var))
                     end),
        Pid2 = spawn(fun() ->
                             ?assertEqual(Val, optvar:read(Var))
                     end),
        spawn(fun() ->
                      exit(Pid1, intentional)
              end),
        optvar:unset(Var),
        optvar:set(Var, Val),
        optvar:unset(Var),
        optvar:set(Var, Val),
        wait_pids([Pid1, Pid2])
    after
        cleanup()
    end.

optvar_unset_test() ->
    init(),
    Var = {foo, bar},
    try
        Val = 42,
        optvar:set(Var, Val),
        ?assertEqual({ok, Val}, optvar:peek(Var)),
        spawn(fun() ->
                      catch optvar:unset(Var)
              end),
        case optvar:read(Var, 10) of
            {ok, Val} -> ok;
            timeout   -> ?assertMatch(undefined, optvar:peek(Var))
        end
    after
        %% Set the variable to avoid "deadlocked" error detected by
        %% concuerror for the waker process:
        optvar:set(Var, 1),
        cleanup()
    end.

%% Check multiple processes waiting for a condition var
optvar_double_wait_test() ->
    init(),
    try
        Val = 42,
        Parent = self(),
        [spawn(fun() ->
                       Parent ! optvar:read(foo)
               end) || _ <- [1, 2]],
        ?assertMatch(ok, optvar:set(foo, Val)),
        receive Val -> ok end,
        receive Val -> ok end,
        ?assertEqual({ok, Val}, optvar:peek(foo))
    after
        cleanup()
    end.

%% Check that killing a waiter process doesn't block other waiters
optvar_waiter_killed_test() ->
    init(),
    try
        Val = 42,
        Waiter = spawn(fun() ->
                               catch optvar:read(foo)
                       end),
        Killer = spawn(fun() ->
                               exit(Waiter, shutdown)
                       end),
        Setter = spawn(fun() ->
                               optvar:set(foo, Val)
                       end),
        ?assertEqual(Val, optvar:read(foo)),
        ?assertEqual({ok, Val}, optvar:peek(foo)),
        wait_pids([Waiter, Killer, Setter])
    after
        cleanup()
    end.

%% Check infinite waiting for multiple variables
optvar_wait_multiple_test() ->
    init(),
    try
        Val = 42,
        [spawn(fun() ->
                       optvar:set(Key, Val)
               end) || Key <- [foo, bar]],
        ?assertMatch(ok, optvar:wait_vars([foo, bar], infinity)),
        ?assertEqual({ok, Val}, optvar:peek(foo)),
        ?assertEqual({ok, Val}, optvar:peek(bar))
    after
        cleanup()
    end.

%% Check waiting for multiple variables
optvar_wait_multiple_timeout_test() ->
    init(),
    try
        [spawn(fun() ->
                       catch optvar:set(Key, Key)
               end) || Key <- [foo, bar]],
        Done = case optvar:wait_vars([foo, bar], 100) of
                   ok           -> [foo, bar];
                   {timeout, L} -> [foo, bar] -- L
               end,
        [?assertEqual({ok, I}, optvar:peek(I)) || I <- Done]
    after
        %% Set optvars to avoid "deadlocked" error detected by concuerror:
        optvar:set(foo, 1),
        optvar:set(bar, 2),
        cleanup()
    end.

%% Check waiting for multiple variables, one times out.
%%
%% Note: it doesn't run under concuerror, since we rely on the precise
%% timings here:
optvar_wait_multiple_timeout_one_test() ->
    init(),
    try
        [spawn(fun() ->
                       timer:sleep(100),
                       optvar:set(Key, Key)
               end) || Key <- [foo, baz]],
        ?assertMatch({timeout, [bar]}, optvar:wait_vars([foo, bar, baz], 200))
    after
        cleanup()
    end.

optvar_list_test() ->
  init(),
  Var = {foo, bar},
  Val = 1,
  try
    ?assertMatch(timeout, optvar:read(Var, 0)),
    ?assertMatch([], optvar:list()),
    ?assertMatch([Var], optvar:list_all()),
    optvar:set(Var, 1),
    ?assertMatch([Var], optvar:list()),
    ?assertMatch([Var], optvar:list_all())
  after
    cleanup()
  end.

wait_pids([]) ->
    ok;
wait_pids([Pid|Rest]) ->
    MRef = monitor(process, Pid),
    receive
        {'DOWN', MRef, _, _, _} -> wait_pids(Rest)
    end.

init() ->
    case is_concuerror() of
        true ->
            {ok, _} = optvar_sup:start_link();
        false ->
            {ok, _} = application:ensure_all_started(optvar)
    end.

cleanup() ->
    case is_concuerror() of
        true ->
            %% Cleanup causes more interleavings, skip it:
            ok;
        false ->
            catch application:stop(optvar)
    end.

%% Hack to detect if running under concuerror:
is_concuerror() ->
    code:is_loaded(concuerror) =/= false.
