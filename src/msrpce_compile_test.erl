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

-module(msrpce_compile_test).

-compile({parse_transform, msrpce_parse_transform}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("include/records.hrl").
-include("include/types.hrl").

-record(test1, {
    a :: thing(),
    b :: uint32(),
    c :: bar(),
    d :: uint64()
    }).

-type bar() :: thing().
-type thing() :: uint8().
-type what() :: #test1{}.

-record(test2, {
    a :: msrpce:uint32(),
    b :: pointer(what()),
    c :: msrpce:int32()
    }).

-record(test3, {
    a :: msrpce:uint8(),
    b :: str()
    }).

-record(test4, {
    a :: msrpce:uint16(),
    b :: bin()
    }).

-record(test5, {
    b :: uint16(),
    as :: array(uint16())
    }).

-record(test6, {
    c :: uint16(),
    as :: array(#test3{}),
    d :: uint32()
    }).

-record(test7, {
    c :: uint16(),
    as :: pointer(array(#test3{})),
    d :: uint32()
    }).

-record(test8, {
    arr :: varying_array(pointer(#test3{}))
    }).

-record(test9, {
    uuid :: uuid(),
    sid :: sid(),
    mtime :: filetime(),
    name :: rpc_unicode_str()
    }).

-rpce(#{endian => big}).

-rpce_struct(what).
-rpce_struct(test2).
-rpce_struct(test3).
-rpce_struct(test4).
-rpce_struct(test5).
-rpce_struct(test6).
-rpce_struct(test7).
-rpce_struct(test8).
-rpce_struct(test9).

-rpce_stream({strtest, [test1, test3]}).

test1_test() ->
    Struct = #test1{a = 1, b = 2, c = 3, d = 4},
    Data = encode_test1(Struct),
    ?assertMatch(<<1, 0:24, 2:32/big, 3, 0:24, 0:32, 4:64/big>>, Data),
    DeStruct = decode_test1(Data),
    ?assertMatch(#test1{a = 1, b = 2, c = 3, d = 4}, DeStruct).

test2_test() ->
    Struct = #test2{a = 61,
                    b = #test1{a = 1, b = 2, c = 3, d = 4},
                    c = -123},
    Data = encode_test2(Struct),
    ?assertMatch(<<61:32/big, 1:32/big, (-123):32/big-signed, 1, 0:24,
        2:32/big, 3, 0:24, 0:32, 4:64/big>>, Data),
    DeStruct = decode_test2(Data),
    ?assertMatch(Struct, DeStruct).

test3_test() ->
    Struct = #test3{a = 30, b = "hello world"},
    Data = encode_test3(Struct),
    ?assertMatch(<<30, 0:24, 12:32/big, 0:32, 12:32/big, "hello world", 0>>,
        Data),
    DeStruct = decode_test3(Data),
    ?assertMatch(Struct, DeStruct).

test3_offset_test() ->
    Data = <<30, 0:24, 16:32/big, 2:32/big, 12:32/big, 0, 0, "hello world", 0, 0, 0>>,
    DeStruct = decode_test3(Data),
    ?assertMatch(#test3{a = 30, b = "hello world"}, DeStruct).

test4_test() ->
    Struct = #test4{a = 30, b = <<1,2,3>>},
    Data = encode_test4(Struct),
    ?assertMatch(<<30:16/big, 0:16, 3:32/big, 0:32, 3:32/big, 1, 2, 3>>,
        Data),
    DeStruct = decode_test4(Data),
    ?assertMatch(Struct, DeStruct).

test5_test() ->
    Struct = #test5{as = [1,2,3], b = 1000},
    Data = encode_test5(Struct),
    ?assertMatch(<<3:32/big,1000:16/big,0:16,0:32,3:32/big,
        1:16/big,2:16/big,3:16/big>>, Data),
    DeStruct = decode_test5(Data),
    ?assertMatch(Struct, DeStruct).

test5_offset_test() ->
    Data = <<5:32/big,1000:16/big,0:16,1:32,3:32/big,
        10:16/big,1:16/big,2:16/big,3:16/big,50:16/big>>,
    DeStruct = decode_test5(Data),
    ?assertMatch(#test5{as = [1,2,3], b = 1000}, DeStruct).

test6_test() ->
    Struct = #test6{c = 31, d = 123145, as = [
        #test3{a=3,b="hello"}, #test3{a=4,b="world"}]},
    Data = encode_test6(Struct),
    ?assertMatch(<<2:32/big, 31:16/big, 0:16,
        0:32, 2:32/big,
        3, 0:24, 6:32/big, 0:32, 6:32/big, "hello", 0, 0:16,
        4, 0:24, 6:32/big, 0:32, 6:32/big, "world", 0, 0:16,
        123145:32/big>>, Data),
    DeStruct = decode_test6(Data),
    ?assertMatch(Struct, DeStruct).

test7_test() ->
    Struct = #test7{c = 31, d = 123145, as = [
        #test3{a=3,b="hello"}, #test3{a=4,b="world"}]},
    Data = encode_test7(Struct),
    ?assertMatch(<<31:16/big, 0:16, 1:32/big, 123145:32/big,
        2:32/big, 0:32, 2:32/big,
        3, 0:24, 6:32/big, 0:32, 6:32/big, "hello", 0, 0:16,
        4, 0:24, 6:32/big, 0:32, 6:32/big, "world", 0>>, Data),
    DeStruct = decode_test7(Data),
    ?assertMatch(Struct, DeStruct).

test8_test() ->
    Struct = #test8{arr = [#test3{a=3,b="hello"}, #test3{a=4,b="world"},
        #test3{a=3,b="hello"}, #test3{a=3,b="hello"}, #test3{a=4,b="hello"}]},
    Data = encode_test8(Struct),
    ?assertMatch(<<5:32/big,
        1:32/big, 2:32/big, 1:32/big, 1:32/big, 3:32/big,
        3, 0:24, 6:32/big, 0:32, 6:32/big, "hello", 0, 0:16,
        4, 0:24, 6:32/big, 0:32, 6:32/big, "world", 0, 0:16,
        4, 0:24, 6:32/big, 0:32, 6:32/big, "hello", 0>>, Data),
    DeStruct = decode_test8(Data),
    ?assertMatch(Struct, DeStruct).

test9_test() ->
    Struct = #test9{
        uuid = <<1:128/big>>,
        sid = [1,5,1,2,3],
        mtime = {5,second},
        name = "hello"
    },
    Data = encode_test9(Struct),
    ?assertMatch(<<3:32/big,
        1:128/big,
        1, 3, 5:48/big, 1:32/big, 2:32/big, 3:32/big,
        (5*1000*1000*10 + 116444736000000000):64/little,
        5:16/big, 6:16/big, 1:32/big,
        6:32/big, 0:32, 6:32/big, $h, 0, $e, 0, $l, 0, $l, 0, $o, 0, 0, 0>>,
        Data),
    DeStruct = decode_test9(Data),
    ?assertMatch(Struct, DeStruct).

stream_test() ->
    Struct1 = #test1{a = 1, b = 2, c = 3, d = 4},
    Struct3 = #test3{a = 30, b = "hello world"},
    Data = encode_strtest_v1([Struct1, Struct3]),
    <<1, 16#10, 8:16/little, _:32, Inner/binary>> = Data,
    <<I0Len:32/little, _:32, StructData0:I0Len/binary,
      I1Len:32/little, _:32, StructData1:I1Len/binary>> = Inner,
    ?assertMatch(<<1, 0:24, 2:32/big, 3, 0:24, 0:32, 4:64/big>>, StructData0),
    ?assertMatch(<<30, 0:24, 12:32/big, 0:32, 12:32/big, "hello world", 0,
                   _:32>>, StructData1),
    DeStream = decode_strtest(Data),
    ?assertMatch([Struct1, Struct3], DeStream).

-endif.
