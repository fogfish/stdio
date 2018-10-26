# Standard Input/Output on Streams

This library defines functions for creating streams and performing input and output operations on them. 
A stream is a fairly abstract data type defined by [datum](https://github.com/fogfish/datum/blob/master/src/stream/stream.erl) library - streams are a sequential data structures that contains on demand computed elements. Here, a fairly abstract stream definition is used to represent a communications channel to a file, device, or process. This library is inspired by [GNU Input/Output on Streams](https://www.gnu.org/software/libc/manual/html_node/I_002fO-on-Streams.html).

[![Build Status](https://secure.travis-ci.org/fogfish/stdio.svg?branch=master)](http://travis-ci.org/fogfish/stdio)
[![Coverage Status](https://coveralls.io/repos/github/fogfish/stdio/badge.svg?branch=master)](https://coveralls.io/github/fogfish/stdio?branch=master)

## Key features

The [feature overview](doc/features.md) provides an introduction to streams, use-cases and reasoning of they existence:

* Opening/Closing streams - creates a stream to talk to a file.
* Standard streams - streams to the standard input and output devices.
* Streams and Erlang processes - issue with stream in multi-process applications.
* Stream Output - writes data to stream
* Stream Input - read data from stream
* Formatted Input - reading line, record or other data format 

## Getting started

The latest version of the library is available at its `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.

The stable library release is available via github, add the library as dependency to `rebar.config`

```erlang
{deps, [
   {stdio, ".*",
      {git, "https://github.com/fogfish/stdio", {branch, master}}
   }   
]}.
``` 

Let's take a short tour to stdio interface, using development console `make run`

```erlang
%%
%% open a file
{ok, FD} = stdio:fopen("/tmp/example.txt", [read, write]).

%%
%% write to file
stdio:out(<<"0123456789">>, FD).

%%
%% establish a stream to file
Stream = stdio:in(1, FD). 

%%
%% read file content
%%   [<<"0">>,<<"1">>,<<"2">>,<<"3">>,<<"4">>,<<"5">>,<<"6">>,<<"7">>,<<"8">>,<<"9">>]
stream:list(Stream).

%%
%% close file
stdio:fclose(FD).
```

## How To Contribute

The library is Apache 2.0 licensed and accepts contributions via GitHub pull requests:

* Fork the repository on GitHub
* Read build instructions
* Make a pull request

The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 19.0 or later and essential build tools.

**Build** and **run** service in your development console. The following command boots Erlang virtual machine and opens Erlang shell.

```bash
git clone https://github.com/fogfish/stdio
cd stdio
make
make run
```


### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>
>

### bugs

If you experience any issues with the library, please let us know via [GitHub issues](https://github.com/fogfish/stdio/issue). We appreciate detailed and accurate reports that help us to identity and replicate the issue. 

* **Specify** the configuration of your environment. Include which operating system you use and the versions of runtime environments. 

* **Attach** logs, screenshots and exceptions, in possible.

* **Reveal** the steps you took to reproduce the problem, include code snippet or links to your project.



## License

Copyright 2012 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

