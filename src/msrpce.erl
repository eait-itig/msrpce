%% msrpce
%%
%% Copyright 2022 The University of Queensland
%% Author: Alex Wilson <alex@uq.edu.au>
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

-module(msrpce).

-export([

    ]).

-export_type([
    sid/0, filetime/0
    ]).
-export_type([
    uint8/0, uint16/0, uint32/0, uint64/0,
    int8/0, int16/0, int32/0, int64/0
    ]).
-export_type([
    fixed_array/2, array/1, pointer/1, str/0, unicode/0
    ]).

-include("include/records.hrl").

-type sid() :: #sid{}.
-type time_unit() :: microsecond.
-type filetime() :: null | never | {integer(), time_unit()}.

-type uint8() :: integer().
-type uint16() :: integer().
-type uint32() :: integer().
-type uint64() :: integer().

-type int8() :: integer().
-type int16() :: integer().
-type int32() :: integer().
-type int64() :: integer().

-type fixed_array(_N, T) :: [T].
-type array(T) :: [T].
-type pointer(T) :: null | T.

-type str() :: string().
-type unicode() :: string().
