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

%% @doc Contains type definitions for RPCE data types.
%%
%% Most of these data types have shorter aliases, which are available by
%% including <code>types.hrl</code> and <code>records.hrl</code>:
%%
%% <pre>
%% -include_lib("msrpce/include/records.hrl").
%% -include_lib("msrpce/include/types.hrl").
%% </pre>
%%
%% All types have at least their own name aliased by <code>types.hrl</code>,
%% so e.g. {@link msrpce:uint8()} is aliased as <code>uint8()</code> with
%% no module qualifier. These implied aliases are not shown in the tables
%% below.
%%
%% <h3>Base integer types (and their aliases)</h3>
%% <table>
%%  <tr style="background: #ddd;">
%%    <th>Type</th><th>Signed</th><th>Width (bits)</th><th>Aliases</th>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:uint8()}</td>
%%    <td>no</td>
%%    <td>8</td>
%%    <td><code>uchar()</code>, <code>byt()</code>, <code>usmall()</code></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:uint16()}</td>
%%    <td>no</td>
%%    <td>16</td>
%%    <td><code>word()</code>, <code>ushort()</code>, <code>wchar()</code></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:uint32()}</td>
%%    <td>no</td>
%%    <td>32</td>
%%    <td><code>ulong()</code>, <code>dword()</code></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:uint64()}</td>
%%    <td>no</td>
%%    <td>64</td>
%%    <td><code>uhyper()</code>, <code>qword()</code></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:int8()}</td>
%%    <td>yes</td>
%%    <td>8</td>
%%    <td><code>chr()</code>, <code>small()</code></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:int16()}</td>
%%    <td>yes</td>
%%    <td>16</td>
%%    <td><code>word()</code>, <code>short()</code></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:int32()}</td>
%%    <td>yes</td>
%%    <td>32</td>
%%    <td><code>long()</code></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:int64()}</td>
%%    <td>yes</td>
%%    <td>64</td>
%%    <td><code>hyper()</code></td>
%%  </tr>
%% </table>
%% <h3>String and binary types (and their aliases)</h3>
%% <table style="text-align: left; padding: 0.3em;">
%%  <tr style="background: #ddd;">
%%    <th>Type</th><th>Erlang type</th><th>Conformant</th><th>Varying</th><th>Encoding</th><th>Aliases</th>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:bin()}</td>
%%    <td>binary</td>
%%    <td>yes</td>
%%    <td>yes</td>
%%    <td>Raw bytes</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:varying_bin()}</td>
%%    <td>binary</td>
%%    <td>no</td>
%%    <td>yes</td>
%%    <td>Raw bytes</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:fixed_bin()}</td>
%%    <td>binary</td>
%%    <td>no</td>
%%    <td>no</td>
%%    <td>Raw bytes (fixed length)</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:aligned_bin()}</td>
%%    <td>binary</td>
%%    <td>no</td>
%%    <td>no</td>
%%    <td>Raw bytes (fixed length and alignment)</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:str()}</td>
%%    <td>string (list or binary)</td>
%%    <td>yes</td>
%%    <td>yes</td>
%%    <td>UTF-8</td>
%%    <td><code>lpstr()</code> (pointer)</td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:varying_str()}</td>
%%    <td>string (list or binary)</td>
%%    <td>no</td>
%%    <td>yes</td>
%%    <td>UTF-8</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:unicode()}</td>
%%    <td>string (list or binary)</td>
%%    <td>yes</td>
%%    <td>yes</td>
%%    <td>UTF16-LE</td>
%%    <td><code>lpwstr()</code> (pointer)</td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:varying_unicode()}</td>
%%    <td>string (list or binary)</td>
%%    <td>no</td>
%%    <td>yes</td>
%%    <td>UTF16-LE</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:uuid()}</td>
%%    <td>binary (128-bit)</td>
%%    <td>no</td>
%%    <td>no</td>
%%    <td>Raw bytes (4-byte aligned)</td>
%%    <td></td>
%%  </tr>
%% </table>
%% <h3>Array types</h3>
%%
%% Array types take another MSRPCE type as an argument, and represent an
%% array of that underlying type. In Erlang, they are used as a list.
%%
%% For example, <code>msrpce:array(msrpce:uint16())</code> is a
%% conformant-varying array of 16-bit unsigned integers. In Erlang it could
%% be set to e.g. <code>[1,2,3]</code>.
%%
%% <table style="text-align: left; padding: 0.3em;">
%%  <tr style="background: #ddd;">
%%    <th>Type</th><th>Conformant</th><th>Varying</th><th>Aliases</th>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:array()}</td>
%%    <td>yes</td>
%%    <td>yes</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:conformant_array()}</td>
%%    <td>yes</td>
%%    <td>no</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:varying_array()}</td>
%%    <td>no</td>
%%    <td>yes</td>
%%    <td></td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:fixed_array()}</td>
%%    <td>no</td>
%%    <td>no</td>
%%    <td></td>
%%  </tr>
%% </table>
%%
%% <h3>Pointers</h3>
%%
%% There is a single pointer type, {@link msrpce:pointer()}, which takes any
%% RPCE type as an argument. It represents just the reference pointer itself
%% (no length or size information). It is always nullable (NULL is represented
%% by the Erlang atom <code>undefined</code>).
%%
%% If you have a pointer with size information next to it, the annotation types
%% {@link msrpce:size_of()} and {@link msrpce:length_of()} can be used to
%% automatically set it. It will still require a separate field in your record.
%%
%% <h3>Annotation types</h3>
%%
%% These types wrap another RPCE type and change its behaviour.
%%
%% <table style="text-align: left; padding: 0.3em;">
%%  <tr style="background: #ddd;">
%%    <th>Type</th><th>Can wrap</th><th>Summary</th>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:be()}</td>
%%    <td>Any type</td>
%%    <td>Changes the wrapped type to big-endian representation</td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:le()}</td>
%%    <td>Any type</td>
%%    <td>Changes the wrapped type to little-endian representation</td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:size_of()}</td>
%%    <td>Integer type (other field can be any pointer, array or string)</td>
%%    <td>Replaces the value of the wrapped integer with the serialized size of another struct field</td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:length_of()}</td>
%%    <td>Integer type (other field can be any array or string)</td>
%%    <td>Replaces the value of the wrapped integer with the array length of another struct field</td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:bitset()}</td>
%%    <td>Integer type</td>
%%    <td>Represents an integer unpacked to a Map of bit fields</td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:bitset_mask()}</td>
%%    <td>Integer type</td>
%%    <td>Like <code>bitset()</code> but uses masks, not bit numbers</td>
%%  </tr>
%%  <tr>
%%    <td>{@link msrpce:custom()}</td>
%%    <td>Any type</td>
%%    <td>Passes the wrapped type through a custom decode/encode function</td>
%%  </tr>
%% </table>
%%
%% <h3>Other built-in compound types</h3>
%%
%% These types are defined in <code>types.hrl</code> and may only be used
%% by their bare names, after including that header.
%%
%% <table style="text-align: left; padding: 0.3em;">
%%  <tr style="background: #ddd;">
%%    <th>Type</th><th>Serialization</th><th>Erlang representation</th><th>Summary</th>
%%  </tr>
%%  <tr>
%%    <td><code>sid()</code></td>
%%    <td>SID</td>
%%    <td>{@link msrpce:sid()}</td>
%%    <td>A MS Security Identifier (SID), e.g. <code>[1,5,...]</code></td>
%%  </tr>
%%  <tr>
%%    <td><code>rpc_unicode_str()</code></td>
%%    <td>struct</td>
%%    <td><code>string()</code></td>
%%    <td>A <code>RPC_UNICODE_STRING</code> structure</td>
%%  </tr>
%%  <tr>
%%    <td><code>filetime()</code></td>
%%    <td><code>uint32</code></td>
%%    <td>{@link msrpce:filetime()}</td>
%%    <td>File modification timestamp (also used for other timestamps)</td>
%%  </tr>
%%  <tr>
%%    <td><code>ntstatus()</code></td>
%%    <td><code>uint32</code></td>
%%    <td>{@link msrpce:ntstatus()}</td>
%%    <td>Generic status code format used by many parts of Windows</td>
%%  </tr>
%%  <tr>
%%    <td><code>multi_string()</code></td>
%%    <td><code>pointer(array(uint8()))</code></td>
%%    <td><code>[string()]</code></td>
%%    <td>Multiple zero-terminated UTF8 strings, with a double-zero-terminator at the end (<code>msz</code>)</td>
%%  </tr>
%%  <tr>
%%    <td><code>multi_unicode()</code></td>
%%    <td><code>pointer(array(uint16()))</code></td>
%%    <td><code>[string()]</code></td>
%%    <td>Multiple zero-terminated UTF16 strings, with a double-zero-terminator (4 bytes) at the end</td>
%%  </tr>
%%  <tr>
%%    <td><code>rpc_multi_sz()</code></td>
%%    <td>struct</td>
%%    <td><code>[string()]</code></td>
%%    <td>A <code>RPC_MULTI_SZ</code> structure, containing a multi-string</td>
%%  </tr>
%% </table>
%%
-module(msrpce).

-export([
    encode_sid/1, decode_sid/1,
    encode_filetime/1, decode_filetime/1,
    encode_rpc_unicode/1, decode_rpc_unicode/1,
    uuid_to_string/1, uuid_from_string/1,
    encode_ntstatus/1, decode_ntstatus/1,
    encode_rpc_multi_sz/1, decode_rpc_multi_sz/1,
    encode_multi_sz/1, decode_multi_sz/1
    ]).

-export_type([
    sid/0, filetime/0, custom/4, builtin/4, ntstatus/0,
    le/1, be/1
    ]).
-export_type([
    uint8/0, uint16/0, uint32/0, uint64/0,
    int8/0, int16/0, int32/0, int64/0,
    bitset/3, bitset_mask/3,
    bitset_bitmap/0, bitset_maskmap/0,
    size_of/2, length_of/2
    ]).
-export_type([
    fixed_array/2, conformant_array/1, varying_array/1, array/1,
    pointer/1, str/0, varying_str/0, unicode/0, bin/0, fixed_bin/1,
    varying_bin/0, aligned_bin/2, uuid/0, varying_unicode/0
    ]).

-include("include/records.hrl").

-type sid() :: [integer()].
%% A Microsoft Security Identifier (SID) in numeric form (e.g. [1,5,1234,123])

-type time_unit() :: decimicrosecond | microsecond | millisecond | second.
-type filetime() :: null | never | {integer(), time_unit()}.
%% Common time specification format used in MSRPCE

-type uuid() :: aligned_bin(16, 4).
%% A UUID in binary form

-type uint8() :: integer().
%% An unsigned 8-bit integer
-type uint16() :: integer().
%% An unsigned 16-bit integer
-type uint32() :: integer().
%% An unsigned 32-bit integer (also known as a <code>ulong</code> or <code>dword</code>)
-type uint64() :: integer().
%% An unsigned 64-bit integer

-type int8() :: integer().
%% A signed 8-bit integer
-type int16() :: integer().
%% A signed 16-bit integer
-type int32() :: integer().
%% A signed 32-bit integer
-type int64() :: integer().
%% A signed 64-bit integer

-type fixed_array(_N, T) :: [T].
%% A fixed-size array with no length integer included. The first argument
%% (<code>N</code>) should be the length as an integer.

-type conformant_array(T) :: [T].
%% A conformant array (has "maximum" length, offset and real length, then the
%% data). Maximum length may be hoisted.

-type varying_array(T) :: [T].
%% A varying array (has only a "maximum" length and then the data)

-type array(T) :: [T].
%% A conformant-varying array (the most commonly used kind)

-type pointer(T) :: undefined | T.
%% A pointer to any RPCE type. A 32-bit pointer value is included in
%% the stream and then the actual content of it is serialised at the end.
%%
%% Pointer values are generated as either <code>16#00000000</code> (the
%% <code>NULL</code> value for <code>undefined</code>) or
%% <code>16#00020000 bor (Index bsl 2)</code>
%% to match the behaviour of the MS IDL compiler in most common cases.

-type custom(_Base, RealType, _Encoder, _Decoder) :: RealType.
%% Defines a custom extension to a base RPC type.
%%
%% <table>
%%   <tr>
%%     <td><code>Base</code></td>
%%     <td>The base RPCE type (e.g. {@link msrpce:uint16()})</td>
%%   </tr>
%%   <tr>
%%      <td><code>RealType</code></td>
%%      <td>Actual Erlang type of the final decoded value.</td>
%%   </tr>
%%   <tr>
%%      <td><code>Encoder</code></td>
%%      <td>Atom name of an arity-1 function which encodes the value (takes
%%          a value of type <code>RealType</code> and converts to type
%%          <code>Base</code>).</td>
%%   </tr>
%%   <tr>
%%      <td><code>Decoder</code></td>
%%      <td>Atom name of an arity-1 function which decodes the value (takes
%%          a value of type <code>Base</code> and converts to type
%%          <code>RealType</code>).</td>
%%   </tr>
%% </table>
%%
%% <h4>Example</h4>
%% <pre>
%% -type thing() :: msrpce:custom(uint8(), {integer(), integer()},
%%     encode_thing, decode_thing).
%%
%% encode_thing({A, B}) -> A + B * 10.
%% decode_thing(Sum) -> {Sum rem 10, Sum div 10}.
%%
%% -record(foobar, { field :: thing() }).
%% -msrpce_struct(foobar).
%%
%% #foobar{field = {1,3}}       % => encoded as a uint8 of value 13
%% </pre>

-type builtin(_Base, RealType, _Encoder, _Decoder) :: RealType.
%% An extension type defined in the <code>msrpce</code> module.

-type bitset(_Base, BitName, _BitMap) :: #{BitName => boolean()}.
%% An integer which is made up of bits, each representing a boolean flag.
%%
%% <table>
%%   <tr>
%%     <td><code>Base</code></td>
%%     <td>Any unsigned integer type (e.g. {@link msrpce:uint32()})</td>
%%     <td>The actual serialised format of this field</td>
%%   </tr>
%%   <tr>
%%     <td><code>BitName</code></td>
%%     <td>Union of atom types (e.g. <code>bit_a | bit_b</code>)</td>
%%     <td>All the possible bit names used in this set</td>
%%   </tr>
%%   <tr>
%%     <td><code>BitMap</code></td>
%%     <td>{@link msrpce:bitset_bitmap()} (e.g. <code>#{bit_a => 5, bit_b => 0}</code>)</td>
%%     <td>Map of bit name => bit number</td>
%%   </tr>
%% </table>
%%
%% <h4>Example</h4>
%% <pre>
%% -type field() :: msrpce:bitset(
%%     msrpce:uint32(),
%%     bit_a | bit_b,
%%     #{bit_a => 0, bit_b => 5}).
%%
%% -record(thing, {
%%     bar :: field()
%%     }).
%% -msrpce_struct(thing).
%%
%% #thing{bar = #{bit_a => true}}   % => encoded as 0x00000001
%% #thing{bar = #{bit_b => true}}   % => encoded as 0x00000020
%% </pre>

-type bitset_mask(_Base, BitName, _MaskMap) :: #{BitName => boolean()}.
%% Like a <code>bitset()</code> but the map takes masks rather than bit numbers.
%%
%% <h4>Example</h4>
%% <pre>
%% -type field() :: msrpce:bitset_mask(
%%     msrpce:uint32(),
%%     bit_a | bit_b,
%%     #{bit_a => 16#00100000, bit_b => 16#00000020}).
%%
%% -record(thing, {
%%     bar :: field()
%%     }).
%% -msrpce_struct(thing).
%%
%% #thing{bar = #{bit_a => true, bit_b => true}}   % => encoded as 0x00100020
%% </pre>

-type bitnum() :: integer().
%% Bit number, starting at 0 for LSB.

-type mask() :: integer().
%% Bit mask, represented as an integer that will be bitwise-OR'd into the
%% value if this bit is set.

-type bitset_bitmap() :: #{atom() => bitnum()}.
%% The type of the <code>BitMap</code> argument to {@link bitset()}.

-type bitset_maskmap() :: #{atom() => mask()}.
%% The type of the <code>MaskMap</code> argument to {@link bitset_mask()}.

-type le(T) :: T.
%% Forces the inner type to be little-endian always (ignores the stream
%% endian options).
-type be(T) :: T.
%% Forces the inner type to be big-endian always (ignores the stream
%% endian option).

-type size_of(_Field, IntType) :: IntType.
%% An integer type which represents the encoded byte size of a sibling field
%% in the same struct. It will be automatically calculated during encoding.

-type length_of(_Field, IntType) :: IntType.
%% An integer type which represents the array length of a sibling field in
%% the same struct. It will be automatically calculated during encoding.

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
%% A varying binary string.

-type str() :: string().
%% A conformant-varying UTF8 string.

-type varying_str() :: string().
%% A varying UTF8 string.

-type unicode() :: string().
%% A conformant-varying UTF16-LE string.

-type varying_unicode() :: string().
%% A varying UTF16-LE string.

%% @private
-spec encode_sid(sid()) -> #msrpce_sid{}.
encode_sid([Rev, IdAuth | SubAuths]) ->
    #msrpce_sid{revision = Rev,
                sub_auth_count = length(SubAuths),
                identifier_auth = <<IdAuth:48/big>>,
                sub_auths = SubAuths}.

%% @private
-spec decode_sid(#msrpce_sid{}) -> sid().
decode_sid(#msrpce_sid{revision = Rev,
                       sub_auth_count = SubAuthCount,
                       identifier_auth = <<IdAuth:48/big>>,
                       sub_auths = SubAuths}) when (length(SubAuths) == SubAuthCount) ->
    [Rev, IdAuth | SubAuths].

%% @private
-spec encode_filetime(filetime()) -> aligned_bin(16,4).
encode_filetime(null) ->
    <<0:64>>;
encode_filetime(never) ->
    <<16#7fffffffffffffff:64/little>>;
encode_filetime({N, decimicrosecond}) ->
    V = N + 116444736000000000,
    <<V:64/little>>;
encode_filetime({N, microsecond}) ->
    encode_filetime({N * 10, decimicrosecond});
encode_filetime({N, millisecond}) ->
    encode_filetime({N * 1000, microsecond});
encode_filetime({N, second}) ->
    encode_filetime({N * 1000, millisecond}).

%% @private
-spec decode_filetime(aligned_bin(16,4)) -> filetime().
decode_filetime(<<0:64>>) -> null;
decode_filetime(<<16#7fffffffffffffff:64/little>>) -> never;
decode_filetime(<<V:64/little>>) ->
    DUSec = V - 116444736000000000,
    case (DUSec rem 10) of
        0 ->
            USec = DUSec div 10,
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
            end;
        _ ->
            {DUSec, decimicrosecond}
    end.

%% @private
-spec encode_rpc_unicode(string()) -> #msrpce_unicode_string{}.
encode_rpc_unicode(String) ->
    Len = string:len(String),
    #msrpce_unicode_string{len = Len * 2,
                           maxlen = Len * 2,
                           str = String}.

%% @private
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

%% @doc Converts a UUID to the standard hex string format.
-spec uuid_to_string(uuid()) -> string_uuid().
uuid_to_string(<<TimeLow:32/big, TimeMid:16/big,
                 TimeHiVer:16/big, ClockSeq:16/big,
                 Node:48/big>>) ->
    string:to_lower(io_lib:format(
        "~8.16.0B-~4.16.0B-~4.16.0B-~4.16.0B-~12.16.0B",
        [TimeLow, TimeMid, TimeHiVer, ClockSeq, Node])).

%% @doc Parses a hex-string-format UUID and returns a binary.
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

-type ntstatus() :: {ntstatus:severity(), ntstatus:code() | integer()}.
%% windows NT status return value

%% @doc Converts an NTSTATUS value from integer to atom/tuple form.
-spec decode_ntstatus(uint32()) -> ntstatus().
decode_ntstatus(Int) ->
    Sev = case (Int bsr 30) band 3 of
        0 -> success;
        1 -> info;
        2 -> warn;
        3 -> error
    end,
    Code = ntstatus:int_to_code(Int),
    {Sev, Code}.

%% @doc Converts an NTSTATUS value from atom/tuple form to an integer.
-spec encode_ntstatus(ntstatus()) -> uint32().
encode_ntstatus({Sev, Code}) ->
    Int0 = if
        is_atom(Code) -> ntstatus:code_to_int(Code);
        is_integer(Code) -> Code
    end,
    SevMask = case Sev of
        success -> 0;
        info -> 1 bsl 30;
        warn -> 2 bsl 30;
        error -> 3 bsl 30
    end,
    Int0 bor SevMask.

%% @private
decode_rpc_multi_sz(#msrpce_multi_sz{value = Arr, nchar = N}) ->
    N = length(Arr),
    string:lexemes(Arr, [0]).

%% @private
encode_rpc_multi_sz(Strings) ->
    Arr = lists:flatten(lists:join(0, Strings) ++ [0,0]),
    #msrpce_multi_sz{value = Arr, nchar = length(Arr)}.

%% @private
decode_multi_sz(Arr) ->
    string:lexemes(Arr, [0]).

%% @private
encode_multi_sz(Strings) ->
    lists:flatten(lists:join(0, Strings) ++ [0,0]).
