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
-include_lib("datum/include/datum.hrl").

-export([
   %% standard streams
   in/0,
   in/1,
   out/0,
   
   %% file stream
   fopen/1,
   fopen/2,

   %% append only file stream
   fstream/1,
   fstream/2,
   fclose/1,

   %% stream i/o
   send/2,
   sink/2,
   snapshot/1,

   %% stream utilities
   chunk/1,
   block/2
]).
-export_type([
   filename/0,
   stream/0
]).

%%
%% data types
-type filename()  :: string() | binary().
-type stream()    :: datum:stream( binary() ).


%%
%% standard input stream
-spec in() -> datum:either( stream() ).
-spec in(integer()) -> datum:either( stream() ).

in() ->
   stdio_standard:reader().

in(Chunk) ->
   stdio_standard:reader(Chunk).

%%
%% standard output stream
-spec out() -> datum:either( stream() ).

out() ->
   stdio_standard:writer().

%%
%% opening file stream
-spec fopen( filename() ) -> datum:either( stream() ).
-spec fopen( filename(), file:mode() ) -> datum:either( stream() ).

fopen(File) ->
   fopen(File, []).

fopen(File, Opts) ->
   fopen(Opts, File, Opts).

fopen([], File, Opts) ->
   stdio_file:reader(File, Opts);
fopen([read | _], File, Opts) ->
   stdio_file:reader(File, Opts);
fopen([write | _], File, Opts) ->
   stdio_file:writer(File, Opts);
fopen([append | _], File, Opts) ->
   stdio_file:writer(File, Opts);
fopen([_ | T], File, Opts) ->
   fopen(T, File, Opts).

%%
%% opening stream
-spec fstream( filename() ) -> datum:either( stream() ).
-spec fstream( filename(), file:mode() ) -> datum:either( stream() ).

fstream(File) ->
   fstream(File, []).

fstream(File, Opts) ->
   fstream(Opts, File, Opts).

fstream([], File, Opts) ->
   stdio_file_stream:forward(File, Opts);
fstream([forward | _], File, Opts) ->
   stdio_file_stream:forward(File, Opts);
fstream([reverse | _], File, Opts) ->
   stdio_file_stream:reverse(File, Opts);
fstream([_ | T], File, Opts) ->
   fstream(T, File, Opts).



%%
%% closing file stream
-spec fclose( stream() ) -> datum:either().

fclose(#stream{tail = {Module, _, _}} = Stream) ->
   Module:close(Stream).

%%
%% cast data into stream
-spec send(binary(), stream()) -> stream().

send(Data, #stream{tail = {Module, _, _}} = Stream) ->
   Module:send(Data, Stream).

%%
%% sink input stream to output
-spec sink(stream(), stream()) -> stream().

sink(Ingress, Egress) -> 
   stream:fold(fun stdio:send/2, Egress, Ingress).

%%
%% build a stream snapshot
-spec snapshot(stream()) -> stream().

snapshot(#stream{tail = {Module, _, _}} = Stream) ->
   Module:snapshot(Stream).

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
