# transaction_commit

Erlang implementation of TLA+ TCommit

## Current Status

Currently `rm_sup.erl` will initialize 3 rm's. Each rm follows the allowed state transitions per TCommit.

## Resource Manager(rm)

This is the client that can change it's state.

## Changelog

`a_rm.erl` is the first attempt of the rm fsm using case/switch

`rm.erl` is the second attempt of the rm fsm following the function names from [TCommit.tla](https://github.com/tlaplus/Examples/blob/master/specifications/transaction_commit/TCommit.tla)

## Testing

```shell script
$ rebar3 eunit
```

## Todo

The next step will be to implement the transaction manager then message passing.
