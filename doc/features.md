# Input/Output on Streams

## Opening Streams

Create a new stream with `stdin:fopen/2` function, it creates a new stream data-type and establishes a connection between stream and file. Semantic of open is compatible with Erlang `file:open/2`. In contrasts to traditional files, stream is either open for reading or writing. Use Erlang standard `file:mode()` to control how the file is opened.

```erlang
{ok, In} = stdio:fopen("/tmp/in.txt", [read]).
{ok, Eg} = stdio:fopen("/tmp/eg.txt", [write]).
```

## Closing Streams

Read-only streams are closed automatically when it reaches end of file. It is possible to explicitly close a stream when application is finished with it. When a stream is closed with `stdin:fclose/1`, the connection between the stream and the file is canceled. After you have closed a stream, you cannot perform any additional operations on it.

```erlang
stdio:fclose(In).
```

## Standard Streams

Standard streams are used by programs to pipe and redirect I/O. These streams open and available for use when main function of program is invoked. Shells provides these facilities using File System Interface. The library provides a stream data-type abstraction to these streams.

```erlang
%% 
%% the standard input stream, stream elements are delimiter by new-line `\n`
stdio:in().

%%
%% the standard input stream, stream elements has a fixed size
stdio:in( integer() ).

%%
%% the standard output stream
stdio:out()
``` 


## Streams and Erlang processes

A stream is immutable data structure, which can be shared across processes. It can be shared between multiple processes but developers should understand that connection between stream and file is implemented using side-effect. It is important to know that writes are not linearizable, therefore it is recommended to write using single process. It is possible to share a input stream between multiple processes.

The stream input blocks current process until data is received.


## Stream output

Streams supports a binary data output to stream:

```erlang
stdio:send(<<"test">>, Stream).
```

You can also sink entire content of input stream to output. 

```erlang
{ok, In} = stdio:fopen("/tmp/in.txt", [read]).
{ok, Eg} = stdio:fopen("/tmp/eg.txt", [write]).

stdio:sink(In, Eg).
```

## Stream input

This library implements an interface to establishes a connection between stream data structure and file. Use a [stream interface](https://github.com/fogfish/datum/blob/master/src/stream/stream.erl) to consumer data. 

```erlang
stream:head(In).
stream:tail(In).
```

## Formatted input

The library benefits from stream combinators, it implements variety of stream parsers using `stream:unfold` combinator.

```erlang
%%
%% splits stream to chunks separated with CRLF or LF
stdio:chunk(Stream).

%%
%% splits stream to blocks of size N
stdio:block(N, Stream).
```

