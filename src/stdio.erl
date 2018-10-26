%%
%%   Copyright 2012 - 2018 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%   Streams Input/Output interface
-module(stdio).

-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").
-include_lib("kernel/include/file.hrl").
-include("stdio.hrl").

-export([
   fopen/1,
   fopen/2,
   fstream/2,
   fclose/1,
   fsync/1,
   in/1,
   in/2,
   out/2,
   crlf/1,
   chunk/2
]).
-export_type([
   iostream/0,
   stream/0
]).

%%
%% data types
-type iostream()  :: #iostream{}.
-type filename()  :: string() | binary() | stdin | stdout.
-type stream()    :: datum:stream( binary() ).
-type fd()        ::
      file:fd()
   |  stdin
   |  stdout
   |  stderr
   .

%%
%% establishes a connection between file descriptor and stream data structure
-spec fopen(fd()) -> datum:either( iostream() ).

fopen(stdin) ->
   stdio_standard:new();
fopen(stdout) ->
   stdio_standard:new();
fopen(#file_descriptor{} = FD) ->
   stdio_file:new(FD).

%%
%% establishes a connection between file and stream data structure
-spec fopen(filename(), file:mode()) -> datum:either( iostream() ).

fopen(Filename, Opts) ->
   [either ||
      file:open(Filename, [raw, binary | Opts]),
      fopen(_)
   ].

%%
%% establishes a connection between open only file and stream data structure
-spec fstream(filename(), file:mode()) -> datum:either( iostream() ).

fstream(Filename, Opts) ->
   stdio_file_stream:new(Filename, Opts).


%%
%% closing file stream
-spec fclose( iostream() | stream() ) -> datum:either().

fclose(#stream{tail = {Module, _, FD}}) ->
   Module:free(FD);

fclose(#iostream{module = Module, fd = FD}) ->
   Module:free(FD);

fclose(#file_descriptor{} = FD) ->
   file:close(FD).

%%
%% sync file stream
-spec fsync( iostream() ) -> datum:either().

fsync(#iostream{module = Module, fd = FD}) ->
   Module:sync(FD).

%%
%% 
-spec in(iostream()) -> stream().
-spec in(integer(), iostream()) -> stream().

in(Stream) ->
   in(undefined, Stream).

in(Chunk, #iostream{module = Module, fd = FD}) ->
   Module:read(Chunk, FD);

in(Chunk, #stream{tail = {Module, _, FD}}) ->
   Module:read(Chunk, FD);

in(Chunk, #file_descriptor{} = FD) ->
   in(Chunk, fopen(FD)).

%%
%% sink input stream to output
-spec out(stream(), iostream()) -> datum:either( iostream() ).

out(#stream{} = Ingress, #iostream{module = Module, fd = FD} = Egress) ->
   [either ||
      Module:write(stream:head(Ingress), FD),
      out(stream:tail(Ingress), Egress#iostream{fd = _})
   ];

out(?stream(), Egress) ->
   {ok, Egress};

out(Data, #iostream{module = Module, fd = FD} = Egress) ->
   [either ||
      Module:write(Data, FD),
      cats:unit(Egress#iostream{fd = _})
   ].

%%
%% splits stream to chunks separated with CRLF or LF
-spec crlf(stream()) -> stream().

crlf(Stream) ->
   stdio_stream_crlf:unfold(Stream).

%%
%% split stream to chunk of size 
-spec chunk(integer(), stream()) -> stream().

chunk(N, Stream) ->
   stdio_stream_chunk:unfold(N, Stream).
