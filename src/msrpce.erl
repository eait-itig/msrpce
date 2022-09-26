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
    encode_rpc_unicode/1, decode_rpc_unicode/1,
    uuid_to_string/1, uuid_from_string/1
    ]).

-export_type([
    sid/0, filetime/0, custom/4, builtin/4
    ]).
-export_type([
    uint8/0, uint16/0, uint32/0, uint64/0,
    int8/0, int16/0, int32/0, int64/0,
    bitset/3, bitslice/3
    ]).
-export_type([
    fixed_array/2, conformant_array/1, varying_array/1, array/1,
    pointer/1, str/0, varying_str/0, unicode/0, bin/0, fixed_bin/1,
    varying_bin/0, aligned_bin/2, uuid/0
    ]).

-include("include/records.hrl").

-type sid() :: [integer()].
%% A Microsoft Security Identifier (SID) in numeric form (e.g. [1,5,1234,123])

-type time_unit() :: microsecond | millisecond | second.
-type filetime() :: null | never | {integer(), time_unit()}.
%% Common time specification format used in MSRPCE

-type uuid() :: aligned_bin(16, 4).
%% A UUID in binary form

-type uint8() :: integer().
-type uint16() :: integer().
-type uint32() :: integer().
-type uint64() :: integer().

-type int8() :: integer().
-type int16() :: integer().
-type int32() :: integer().
-type int64() :: integer().

-type fixed_array(_N, T) :: [T].
%% A fixed-size array with no length integer included.

-type conformant_array(T) :: [T].
%% A conformant array (has "maximum" length, offset and real length, then the
%% data). Maximum length may be hoisted.

-type varying_array(T) :: [T].
%% A varying array (has only a "maximum" length and then the data)

-type array(T) :: [T].
%% A conformant-varying array (the most commonly used kind)

-type pointer(T) :: undefined | T.
%% A pointer to another RPCE structure. A 32-bit pointer value is included in
%% the stream and then the actual content of it is serialised at the end.

-type custom(_Base, RealType, _Encoder, _Decoder) :: RealType.
%% Defines a custom extension to a base RPC type. The "Base" argument should be
%% the base RPCE type (e.g. <code>msrpce:uint16()</code>). Encoder and Decoder
%% specify the names of functions in the current module to use for encoding
%% and decoding. They should be of arity /1. Encoder takes the decoded base
%% type and outputs RealType. Decoder does the inverse.

-type builtin(_Base, RealType, _Encoder, _Decoder) :: RealType.
%% An extension type defined in the <code>msrpce</code> module.

-type bitset(_Base, BitName, _BitMap) :: #{BitName => boolean()}.
%% An integer which is made up of bits, each representing a boolean flag.
%% The Base type should be one of the unsigned integer types
%% (<code>msrpce:uint*</code>). BitName is a union of all possible bit
%% names. BitMap is a map of the form
%% <code>#{BitName => BitNumber :: integer()}</code>, where
%% <code>BitNumber</code> is 0 for LSB.

-type bitslice(_Base, PartName, _PartMap) :: #{PartName => integer()}.

-type bin() :: binary().
%% A conformant-varying binary string, with no terminator. Conformant string
%% maximum lengths are not hoisted.

-type fixed_bin(_N) :: binary().
%% A fixed-length binary inserted into the stream with alignment 1 and no
%% length integer attached.

-type aligned_bin(_N, _Align) :: binary().
%% Same as <code>fixed_bin(N)</code> but with the specified alignment. This is
%% a useful "escape hatch" for custom types.

-type varying_bin() :: binary().
%% A varying binary string, with no terminator.

-type str() :: string().
%% A conformant-varying UTF8 string with a NUL terminator.

-type varying_str() :: string().
%% A varying UTF8 string with a NUL terminator.

-type unicode() :: string().
%% A conformant-varying UTF16-LE string with a NUL terminator.

-spec encode_sid(sid()) -> #msrpce_sid{}.
encode_sid([Rev, IdAuth | SubAuths]) ->
    #msrpce_sid{revision = Rev,
                sub_auth_count = length(SubAuths),
                identifier_auth = <<IdAuth:48/big>>,
                sub_auths = SubAuths}.

-spec decode_sid(#msrpce_sid{}) -> sid().
decode_sid(#msrpce_sid{revision = Rev,
                       sub_auth_count = SubAuthCount,
                       identifier_auth = <<IdAuth:48/big>>,
                       sub_auths = SubAuths}) when (length(SubAuths) == SubAuthCount) ->
    [Rev, IdAuth | SubAuths].

-spec encode_filetime(filetime()) -> aligned_bin(16,4).
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

-spec decode_filetime(aligned_bin(16,4)) -> filetime().
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

-spec encode_rpc_unicode(string()) -> #msrpce_unicode_string{}.
encode_rpc_unicode(String) ->
    Len = string:len(String),
    #msrpce_unicode_string{len = Len * 2,
                           maxlen = Len * 2,
                           str = String}.

-spec decode_rpc_unicode(#msrpce_unicode_string{}) -> string().
decode_rpc_unicode(#msrpce_unicode_string{len = 0, maxlen = 0, str = _}) ->
    "";
decode_rpc_unicode(R = #msrpce_unicode_string{len = L, maxlen = MaxL, str = S0}) ->
    case string:len(S0) of
        V when (V * 2 =< MaxL) and (V * 2 >= L) ->
            string:slice(S0, 0, L div 2);
        _ ->
            error({bad_rpc_unicode, R})
    end.

-type string_uuid() :: string().
%% A UUID in string hex form (e.g. "5e8cb9bc-bbc2-38b2-afc9-a9218c3b1d9c")

-spec uuid_to_string(uuid()) -> string_uuid().
uuid_to_string(<<TimeLow:32/big, TimeMid:16/big,
                 TimeHiVer:16/big, ClockSeq:16/big,
                 Node:48/big>>) ->
    string:to_lower(io_lib:format(
        "~8.16.0B-~4.16.0B-~4.16.0B-~4.16.0B-~12.16.0B",
        [TimeLow, TimeMid, TimeHiVer, ClockSeq, Node])).

-spec uuid_from_string(string_uuid()) -> uuid().
uuid_from_string(Str0) ->
    Hex = lists:seq($0, $9) ++ lists:seq($a, $f),
    Str1 = string:to_lower(Str0),
    {TimeLowHex, [$- | Str2]} = string:take(Str1, Hex),
    {TimeMidHex, [$- | Str3]} = string:take(Str2, Hex),
    {TimeHighVerHex, [$- | Str4]} = string:take(Str3, Hex),
    {ClockSeqHex, [$- | Str5]} = string:take(Str4, Hex),
    {NodeHex, []} = string:take(Str5, Hex),
    <<(binary_to_integer(iolist_to_binary([TimeLowHex]), 16)):32/big,
      (binary_to_integer(iolist_to_binary([TimeMidHex]), 16)):16/big,
      (binary_to_integer(iolist_to_binary([TimeHighVerHex]), 16)):16/big,
      (binary_to_integer(iolist_to_binary([ClockSeqHex]), 16)):16/big,
      (binary_to_integer(iolist_to_binary([NodeHex]), 16)):48/big>>.
