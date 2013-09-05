# batchio #

Experimental project to batch calls to io:format/2 in order to save time over
communication and whatnot whenever the central io server of an Erlang node
becomes a bottleneck

## building ##

`rebar get-deps compile`

## running tests ##

`rebar get-deps compile && rebar ct skip_deps=true`

## using ##

```
application:start(pobox).
application:start(batchio).
batchio:format("abc: ~p~n", [myreq]).
```

Note that if batchio isn't started, `batchio:format/1-2` will redirect calls
to `io:format/1-2`.

## configuration ##

All configurations can be done using Erlang's OTP applications' usual `env`
variables.

- `buffer`: Before starting batchio, you can define a maximal buffer size by
  setting the env variable `buffer` to an integer. As many slots will be
  allocated in batchio's queue buffer. Note that this size cannot be changed
  dynamically at this time, but is on the roadmap for whenever someone will
  need it.
- `page_size`: This variable allows to define how many bytes will be sent on
  each batch for the messages. This value can be modified dynamically, on a
  global level (which is fine because batchio exists globally only)
- `leader`: determines where to send the IO data. By default, batchio will use
  its original `group_leader()` value for that (its application controller,
  forwarding to either the local `user` process, or a remote one depending on
  how the node has been started (see
  http://ferd.ca/repl-a-bit-more-and-less-than-that.html for details).
  This value cannot be changed dynamically yet.

## Benchmarks

Benchmarks are run sequentially to provide an alright snapshot of performance,
while voiding complex issues of synchronizing concurrent pieces of code to know
when we're done. Benchmarks are run with:

```
batchio_bench:run(40000, 4096, [noop], 10000, 25).
```

Meaning batchio has 40k elements available in its buffer, will send data in
pages of 4096 bytes, will use a fake IO server that doesn't actually do output,
and will send 10000 random 25-bytes messages as fast as possible. The messages
are sent as lists (so we get a good picture of message passing overhead) called
with both `io:format/3` (directly to the fake IO Server) and
`benchio:format/1`.

The results are returned of the form:

```
{ok, [{regular,{MicroSecs,ok}},
      {batchio,{Microsecs,[{total,Handled}, {dropped, MessagesNotDelivered}]}}]}
```

All benchmarks here are run on a MBP running OSX 10.8.4, on 2 GHZ Intel Core
i7, with 8GB 1600Mhz DDR3 memory, running Erlang R16B.

### Multiple runs, noop ###

Raw results:

```
1> batchio_bench:run(40000, 4096, [noop], 10000, 25).
{ok,[{regular,{147307,ok}},
     {batchio,{82214,[{total,10000},{dropped,0}]}}]}
2> batchio_bench:run(40000, 4096, [noop], 100000, 25).
{ok,[{regular,{1556331,ok}},
     {batchio,{682564,[{total,100000},{dropped,0}]}}]}
3> batchio_bench:run(40000, 4096, [noop], 1000000, 25).
{ok,[{regular,{15140558,ok}},
     {batchio,{7104603,[{total,1000000},{dropped,0}]}}]}
4> batchio_bench:run(40000, 4096, [noop], 1000000, 100).
{ok,[{regular,{27556709,ok}},
     {batchio,{21181553,[{total,1000000},{dropped,0}]}}]}
5> batchio_bench:run(40000, 4096*2, [noop], 1000000, 100).
{ok,[{regular,{27357568,ok}},
     {batchio,{21349706,[{total,1000000},{dropped,0}]}}]}
```

Table:

- Buffer size: 40k entries
- Page size: 4096 bytes
- Io device: noop
- Message bytes: (N*Size)

| total bytes   | regular (µs)  | batchio (µs)  | speedup |
|---------------|---------------|---------------|---------|
| 250000        | 147307        | 82214         | 1.79    |
| 2500000       | 1556331       | 682564        | 2.28    |
| 25000000      | 15140558      | 7104603       | 2.13    |
| 100000000     | 27556709      | 21181553      | 1.30    |

The speedup remains somewhat good the entire time through, and appears to scale
somewhat linearly for both IO methods. The difference between the two could
just be overhead in message passing between the two, and a parallel/concurrent
test could reveal different results given all the sending of messages could be
done at once by many parties, rather than sequentially doing a request/response
pattern.


### Multiple runs, {passthrough, user} ###

Raw results (garbage output omitted):

```
6> batchio_bench:run(40000, 4096, [{passthrough, user}], 10000, 25).
{ok,[{regular,{663627,ok}},
     {batchio,{116301,[{total,10000},{dropped,0}]}}]}
7> batchio_bench:run(40000, 4096, [{passthrough, user}], 100000, 25).
{ok,[{regular,{7152307,ok}},
     {batchio,{1193573,[{total,100000},{dropped,0}]}}]}
8> batchio_bench:run(40000, 4096, [{passthrough, user}], 100000, 100).
{ok,[{regular,{6918967,ok}},
     {batchio,{3158728,[{total,100000},{dropped,0}]}}]}
9> batchio_bench:run(40000, 4096*2, [{passthrough, user}], 100000, 100).
{ok,[{regular,{7537927,ok}},
     {batchio,{3248407,[{total,100000},{dropped,0}]}}]}
10> batchio_bench:run(40000, 4096 div 2, [{passthrough, user}], 100000, 100).
{ok,[{regular,{6385057,ok}},
     {batchio,{3027430,[{total,100000},{dropped,0}]}}]}
```

Table:

- Buffer size: 40k entries
- Page size: 4096 bytes except where noted
- Io device: {passthrough, user}
- Message bytes: (N*Size)

| total bytes   | regular (µs)  | batchio (µs)  | speedup |
|---------------|---------------|---------------|---------|
| 250000        | 663627        | 116301        | 5.71    |
| 2500000       | 7152307       | 1193573       | 5.99    |
| 10000000      | 6918967       | 3158728       | 2.19    |
| 10000000*     | 7537927       | 3248407       | 2.32    |
| 10000000**    | 6385057       | 3027430       | 2.10    |

\* page size of 8192 bytes
\*\* page size of 2048 bytes

On smaller output sizes, batchio is much faster there, telling us there might
be a blocking component further away than the dummy IO server, past the real IO
system of the node (`user` outputs for real). At around 10000000 bytes, both
the regular and batchio times seem to go somewhat stable on the speedup despite
the page size, possibly pointing to the system's limit for IO, although more
results might be needed to confirm that

### Making the buffer size smaller to check for load-shedding ###

Raw results (garbage output omitted):

```
11>  batchio_bench:run(5000, 4096, [{passthrough, user}], 100000, 100).
{ok,[{regular,{7074854,ok}},
     {batchio,{3132005,[{total,100000},{dropped,0}]}}]}
12> batchio_bench:run(100, 4096, [{passthrough, user}], 100000, 100).
{ok,[{regular,{6269205,ok}},
     {batchio,{3057970,[{total,100000},{dropped,4834}]}}]}
```

Even if we're sending 100k messages rather fast, even on a 5k elements buffer,
no message gets dropped (and we have a 2.25 speedup, on par with the previous
results). However, turning down the buffer size to 100 entries at most ends up
shedding 4.8% of the messages sent, keeping a similar speedup.

The lossiness of batchio has to be tweaked to choose a lossiness vs. storage
space requirement adequate for the target node.


### Possible improvements ###

- allow for concurrent operations
- report on memory usage or "messages in flight".
- automate the runs of multiple batch sizes and reports, table building.
- simulate tests on a busy node

## Introspection

`batchio` doesn't have any official introspection API, but the `batchio_serv`
process will store incremental data in the process dictionary under the keys
`total`, `sent`, and `dropped`. Using `erlang:process_info/2` on that process
will allow someone to inspect the lossiness of items in flight, in absolute terms.

## Roadmap ##

- Allow dynamic configuration changes in a proper API
- Test configuration changes
- Figure out multiple-encoding support. Right now, batchio assumes that all
  output will use the same final encoding by just calling `io_lib:format/2`
  at the call site, rather than at the IO server (which is faster, but possibly
  wrong).

## Contributing ##

Send in a pull request including the changes, tests, and a description of what
the changes do (and why it is necessary).

The test suite as it is is a bit complex, as it sets up middlemen group leaders
to forward IO properly.

## Changelog ##

- 1.0.0: Proper `page_size` overflow handling. In 0.1.0, a message larger than
 the page size would have never made it through and would have needed to be
 dropped from the buffer (which would have needed to fill up) before message
 output could resume. This version makes it so that when a message too large is
 found, the current page is sent, and then the message larger than the
 `page_size` is sent alone.
- 0.1.0: Initial commit
