# simple

Simple code spikes

## imem

This module is a partial attempt at implementing [InternalMemory.tla](https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/CachingMemory/InternalMemory.tla) from the TLA+ Specifying Systems book.

First start the Erlang REPL and load the modules, then the API can be run as follows:

```erlang
% setup
dets:open_file(mem, [{file, "table.txt"}]).

% proc 1 writes to a table
{ok, Pid} = imem:start_link().
imem:req(Pid, {"adr1", "val1"}).
imem:do(Pid).
imem:rsp(Pid).

% proc 2 reads from a table
{ok, Pid2} = imem:start_link().
imem:req(Pid2, {"adr1"}).
imem:do(Pid2).
imem:rsp(Pid2).

% teardown
dets:close(mem).
```