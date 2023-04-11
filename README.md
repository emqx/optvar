# optvar


This library implements "optional variable": a synchronization primitive similar to condition variable.
Optional variables can be set, unset and read.

When a variable is unset, any process that tries to read it gets blocked until the variable is set.
Once another process sets the variable, all readers immediately get unblocked and get the value.
If the variable is already set, read function returns the value immediately.

This library has been optimized for very large numbers of readers and variables; it uses ets table for storing the variables, and handles different variables in parallel, so it shouldn't become a bottleneck.

## API

Assign value `Value` to a variable `Var`:

```erlang
set(Var, Value) -> ok.
```

Clear the value of variable `Var`:

```erlang
unset(Var) -> ok
```

Wait until the variable `Var` is set, and return the value:

```erlang
read(Var) -> Value
```

Read the value of a variable with timeout:

```erlang
read(Var, Timeout) -> {ok, Value} | timeout
```

Return the value of a variable immediately if it's set, or `undefined` if it's unset:

```erlang
peek(Var) -> {ok, Value} | undefined
```

Check if the variable is set:

```erlang
is_set(Var) -> boolean()
```

## Warning

optvar library has been designed under an assumption that any variable that is read eventually will be set.
When some process tries to read an *unset* variable, it spawns a special notifier process, which doesn't exit until the variable is set, or until `optvar` application is stopped.
Please keep this in mind when using `read/2` API with timeout: it leaves the notifier process running even after the timeout.
