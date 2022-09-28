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

% helpful type aliases

-type uint8() :: msrpce:uint8().
-type uint16() :: msrpce:uint16().
-type uint32() :: msrpce:uint32().
-type uint64() :: msrpce:uint64().

-type int8() :: msrpce:uint8().
-type int16() :: msrpce:uint16().
-type int32() :: msrpce:uint32().
-type int64() :: msrpce:uint64().

-type small() :: msrpce:int8().
-type usmall() :: msrpce:uint8().
-type short() :: msrpce:int16().
-type ushort() :: msrpce:uint16().
-type long() :: msrpce:int32().
-type ulong() :: msrpce:uint32().
-type hyper() :: msrpce:int64().
-type uhyper() :: msrpce:uint64().

-type chr() :: msrpce:int8().
-type byt() :: msrpce:uint8().

% These are actually aliased by the parse transform, since it doesn't really
% understand how to see through type arguments
-type fixed_array(N, T) :: msrpce:fixed_array(N, T).
-type conformant_array(T) :: msrpce:conformant_array(T).
-type varying_array(T) :: msrpce:varying_array(T).
-type array(T) :: msrpce:array(T).
-type pointer(T) :: msrpce:pointer(T).
-type fixed_bin(N) :: msrpce:fixed_bin(N).
-type aligned_bin(N, A) :: msrpce:aligned_bin(N, A).

-type bin() :: msrpce:bin().
-type varying_bin() :: msrpce:varying_bin().
-type str() :: msrpce:str().
-type varying_str() :: msrpce:varying_str().
-type unicode() :: msrpce:unicode().

-type uuid() :: msrpce:uuid().

-type sid() :: msrpce:builtin(
    #msrpce_sid{},
    msrpce:sid(),
    encode_sid, decode_sid).

-type rpc_unicode_str() :: msrpce:builtin(
    #msrpce_unicode_string{},
    string(),
    encode_rpc_unicode, decode_rpc_unicode).

-type filetime() :: msrpce:builtin(
    msrpce:aligned_bin(8, 4),
    msrpce:filetime(),
    encode_filetime, decode_filetime).

-type dword() :: msrpce:uint32().
-type qword() :: msrpce:uint64().
-type word() :: msrpce:uint16().
-type uchar() :: msrpce:uint8().

-type lpstr() :: pointer(string()).
-type lpwstr() :: pointer(unicode()).

-type ntstatus() :: msrpce:builtin(
    msrpce:uint32(),
    msrpce:ntstatus(),
    encode_ntstatus, decode_ntstatus).

-type rpc_multi_sz() :: msrpce:builtin(
    #msrpce_multi_sz{},
    [string()],
    encode_rpc_multi_sz, decode_rpc_multi_sz).

-type multi_string() :: msrpce:builtin(
    msrpce:pointer(msrpce:array(msrpce:uint8())),
    [string()],
    encode_multi_sz, decode_multi_sz).

-type multi_unicode() :: msrpce:builtin(
    msrpce:pointer(msrpce:array(msrpce:uint16())),
    [string()],
    encode_multi_sz, decode_multi_sz).

