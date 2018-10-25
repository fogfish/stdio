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


   %% standard streams
   in/1,
   in/2,
   out/2,
   

   % %% stream i/o
   % send/2,
   % sink/2,
   % snapshot/1,

   %% stream utilities
   chunk/1,
   block/2
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

fopen(stdio) ->
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
   Module:free(FD).

%%
%% sync file stream
-spec fsync( iostream() ) -> datum:either().

fsync(#iostream{module = Module, fd = FD}) ->
   Module:sync(FD).

%%
%% 
-spec in(iostream()) -> stream().
-spec in(integer(), iostream()) -> stream().

in(#iostream{} = Stream) ->
   in(undefined, Stream);
in(#stream{} = Stream) ->
   in(undefined, Stream).

in(Chunk, #iostream{module = Module, fd = FD}) ->
   Module:read(Chunk, FD);
in(Chunk, #stream{tail = {Module, _, FD}}) ->
   Module:read(Chunk, FD).

%%
%% sink input stream to output
-spec out(stream(), iostream()) -> datum:either( iostream() ).

out(#stream{} = Ingress, #iostream{module = Module, fd = FD} = Egress) ->
   [either ||
      Module:write(stream:head(Ingress), FD),
      out(stream:tail(Ingress), Egress#iostream{fd = _})
   ];

out(?stream(), Egress) ->
   {ok, Egress}.


%%
%% standard output stream
% -spec out() -> datum:either( stream() ).

% out() ->
%    stdio_standard:writer().

%%
%% opening file stream
% -spec fopen( filename() ) -> datum:either( stream() ).
% -spec fopen( filename(), file:mode() ) -> datum:either( stream() ).

% fopen(File) ->
%    fopen(File, []).

% fopen(stdin, Opts) ->
%    stdio_standard:reader(Opts);
% fopen(stdout, Opts) ->
%    stdio_standard:writer(Opts);
% fopen(File, Opts) ->
%    fopen(Opts, File, Opts).

% fopen([], File, Opts) ->
%    stdio_file:reader(File, Opts);
% fopen([read | _], File, Opts) ->
%    stdio_file:reader(File, Opts);
% fopen([write | _], File, Opts) ->
%    stdio_file:writer(File, Opts);
% fopen([append | _], File, Opts) ->
%    stdio_file:writer(File, Opts);
% fopen([_ | T], File, Opts) ->
%    fopen(T, File, Opts).


%%
%% cast data into stream
% -spec send(binary(), stream()) -> stream().

% send(Data, #stream{tail = {Module, _, _}} = Stream) ->
%    Module:send(Data, Stream).

% %%
% %% sink input stream to output
% -spec sink(stream(), stream()) -> stream().

% sink(Ingress, Egress) -> 
%    stream:fold(fun stdio:send/2, Egress, Ingress).

% %%
% %% build a stream snapshot
% -spec snapshot(stream()) -> stream().

% snapshot(#stream{tail = {Module, _, _}} = Stream) ->
%    Module:snapshot(Stream).

%%
%% splits stream to chunks separated with CRLF or LF
-spec chunk(stream()) -> stream().

chunk(Stream) ->
   stdio_stream_chunk:unfold(Stream).

%%
%% split stream to block of size 
-spec block(integer(), stream()) -> stream().

block(N, Stream) ->
   stdio_stream_block:unfold(N, Stream).
