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


## License

Copyright 2012 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

