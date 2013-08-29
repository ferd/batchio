Experimental project to batch calls to io:format/2 in order to save time over
communication and whatnot.

```
application:start(pobox).
application:start(batchio).
batchio:format("abc: ~p~n", [myreq]).
```

## Todo:

- Docs
- Benchmarks
- Introspection API

### Docs

More docs to come after some actual benchmarking.

### Benchmarks

Way to benchmark:

1. implement an Erlang IO server http://erlang.org/doc/apps/stdlib/io_protocol.html
2. have the io server behave like 'user' or whoever else outputs data to a tty to nowhere
3. measure how long it takes until the final message is output (can be determined by a
   special message call)
4. see what's the best!
5. tweak the batch size, parallelism.
6. report on memory usage or "messages in flight".

### Introspection

The introspection API should have a way to report dropped messages due to overflow
or for someone to poll that number in a sensible way.


