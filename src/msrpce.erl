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
    encode_sid/1, decode_sid/1,
    encode_filetime/1, decode_filetime/1,
    encode_rpc_unicode/1, decode_rpc_unicode/1
    ]).

-export_type([
    sid/0, filetime/0, custom/4, builtin/4
    ]).
-export_type([
    uint8/0, uint16/0, uint32/0, uint64/0,
    int8/0, int16/0, int32/0, int64/0
    ]).
-export_type([
    fixed_array/2, conformant_array/1, varying_array/1, array/1,
    pointer/1, str/0, varying_str/0, unicode/0, bin/0, fixed_bin/1,
    varying_bin/0, aligned_bin/2
    ]).

-include("include/records.hrl").

-type sid() :: [integer()].
-type time_unit() :: microsecond | millisecond | second.
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
-type conformant_array(T) :: [T].
-type varying_array(T) :: [T].
-type array(T) :: [T].
-type pointer(T) :: undefined | T.

-type custom(_Base, RealType, _Encoder, _Decoder) :: RealType.
-type builtin(_Base, RealType, _Encoder, _Decoder) :: RealType.

-type bin() :: binary().
-type fixed_bin(_N) :: binary().
-type aligned_bin(_N, _Align) :: binary().
-type varying_bin() :: binary().
-type str() :: string().
-type varying_str() :: string().
-type unicode() :: string().

encode_sid([Rev, IdAuth | SubAuths]) ->
    #msrpce_sid{revision = Rev,
                sub_auth_count = length(SubAuths),
                identifier_auth = <<IdAuth:48/big>>,
                sub_auths = SubAuths}.

decode_sid(#msrpce_sid{revision = Rev,
                       sub_auth_count = SubAuthCount,
                       identifier_auth = <<IdAuth:48/big>>,
                       sub_auths = SubAuths}) when (length(SubAuths) == SubAuthCount) ->
    [Rev, IdAuth | SubAuths].

encode_filetime(null) ->
    <<0:64>>;
encode_filetime(never) ->
    <<16#7fffffffffffffff:64/little>>;
encode_filetime({N, microsecond}) ->
    V = (N * 10) + 116444736000000000,
    <<V:64/little>>;
encode_filetime({N, millisecond}) ->
    encode_filetime({N * 1000, microsecond});
encode_filetime({N, second}) ->
    encode_filetime({N * 1000, millisecond}).

decode_filetime(<<0:64>>) -> null;
decode_filetime(<<16#7fffffffffffffff:64/little>>) -> never;
decode_filetime(<<V:64/little>>) ->
    USec = (V - 116444736000000000) div 10,
    case (USec rem 1000) of
        0 ->
            MSec = USec div 1000,
            case (MSec rem 1000) of
                0 ->
                    Sec = MSec div 1000,
                    {Sec, second};
                _ ->
                    {MSec, millisecond}
            end;
        _ ->
            {USec, microsecond}
    end.

encode_rpc_unicode(String) ->
    Len = string:len(String),
    #msrpce_unicode_string{len = Len,
                           maxlen = Len + 1,
                           str = String}.

decode_rpc_unicode(R = #msrpce_unicode_string{len = L, maxlen = MaxL, str = S0}) ->
    case string:len(S0) of
        V when (V =< MaxL) and (V >= L) ->
            string:slice(S0, 0, L);
        _ ->
            error({bad_rpc_unicode, R})
    end.
