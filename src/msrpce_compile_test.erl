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
    group :: sid(),
    mtime :: filetime(),
    name :: rpc_unicode_str(),
    value :: rpc_unicode_str()
    }).

-type user_session_key() :: msrpce:aligned_bin(16, 4).

-type sid_attrs() :: [mandatory | default | enabled | owner | resource].
-type rpce_sid_attrs() :: msrpce:custom(ulong(), sid_attrs(),
    encode_sid_attrs, decode_sid_attrs).
encode_sid_attrs(List) ->
    A = case lists:member(mandatory, List) of true -> 1; _ -> 0 end,
    B = case lists:member(default, List) of true -> 1; _ -> 0 end,
    C = case lists:member(enabled, List) of true -> 1; _ -> 0 end,
    D = case lists:member(owner, List) of true -> 1; _ -> 0 end,
    E = case lists:member(resource, List) of true -> 1; _ -> 0 end,
    <<N:32/big>> = <<0:2, E:1, 0:25, D:1, C:1, B:1, A:1>>,
    N.
decode_sid_attrs(N) ->
    <<_:2, E:1, _:25, D:1, C:1, B:1, A:1>> = <<N:32/big>>,
    case A of 1 -> [mandatory]; _ -> [] end ++
    case B of 1 -> [default]; _ -> [] end ++
    case C of 1 -> [enabled]; _ -> [] end ++
    case D of 1 -> [owner]; _ -> [] end ++
    case E of 1 -> [resource]; _ -> [] end.

-record(group_membership, {
    relative_id :: ulong(),
    attributes :: rpce_sid_attrs()
    }).

-record(sid_and_attrs, {
    sid :: pointer(sid()),
    attributes :: rpce_sid_attrs()
    }).

-record(kerb_validation_info, {
    logon_time :: filetime(),
    logoff_time :: filetime(),
    kickoff_time :: filetime(),
    password_last_set :: filetime(),
    password_can_change :: filetime(),
    password_must_change :: filetime(),
    effective_name :: rpc_unicode_str(),
    full_name :: rpc_unicode_str(),
    logon_script :: rpc_unicode_str(),
    profile_path :: rpc_unicode_str(),
    home_directory :: rpc_unicode_str(),
    home_directory_drive :: rpc_unicode_str(),
    logon_count :: ushort(),
    bad_password_count :: ushort(),
    user_id :: ulong(),
    primary_group_id :: ulong(),
    group_count :: ulong(),
    group_ids :: pointer(varying_array(#group_membership{})),
    user_flags :: ulong(),
    user_session_key :: user_session_key(),
    logon_server :: rpc_unicode_str(),
    logon_domain_name :: rpc_unicode_str(),
    logon_domain_id :: pointer(sid()),
    reserved1 :: fixed_array(2, ulong()),
    user_account_control :: ulong(),
    sub_auth_status :: ulong(),
    last_successful_ilogon :: filetime(),
    last_failed_ilogon :: filetime(),
    failed_ilogon_count :: ulong(),
    reserved3 :: ulong(),
    sid_count :: ulong(),
    extra_sids :: pointer(varying_array(#sid_and_attrs{})),
    resource_group_domain_sid :: pointer(sid()),
    resource_group_count :: ulong(),
    resource_groups :: pointer(array(#group_membership{}))
    }).

-record(pac_info_buffer, {
    info :: pointer(#kerb_validation_info{})
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

-rpce(#{endian => little}).
-rpce_stream({pac_logon_info, [pac_info_buffer]}).

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
    ?assertMatch(<<61:32/big, 1:32/big, (-123):32/big-signed,
        0:32, 1, 0:24, 2:32/big, 3, 0:24, 0:32, 4:64/big>>, Data),
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
        group = [1,5,1,2,3,56],
        mtime = {5,second},
        name = "hello",
        value = "world"
    },
    Data = encode_test9(Struct),
    ?assertMatch(<<3:32/big, 4:32/big,
        1:128/big,
        1, 3, 5:48/big, 1:32/big, 2:32/big, 3:32/big,
        1, 4, 5:48/big, 1:32/big, 2:32/big, 3:32/big, 56:32/big,
        (5*1000*1000*10 + 116444736000000000):64/little,
        10:16/big, 10:16/big, 1:32/big,
        10:16/big, 10:16/big, 2:32/big,
        6:32/big, 0:32, 5:32/big, $h, 0, $e, 0, $l, 0, $l, 0, $o, 0, 0, 0,
        6:32/big, 0:32, 5:32/big, $w, 0, $o, 0, $r, 0, $l, 0, $d, 0, 0, 0>>,
        Data),
    DeStruct = decode_test9(Data),
    ?assertMatch(Struct, DeStruct).

test9_empty_test() ->
    Struct = #test9{
        uuid = <<1:128/big>>,
        sid = [1,5,1,2,3],
        group = [1,5,1,2,3,56],
        mtime = never,
        name = "",
        value = ""
    },
    Data = encode_test9(Struct),
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

pac_test() ->
    WholePac = base64:decode(<<"
MIIFUjCCBU6gBAICAIChggVEBIIFQAQAAAAAAAAAAQAAALAEAABIAAAAAAAAAAoAAAASAAAA+AQA
AAAAAAAGAAAAFAAAABAFAAAAAAAABwAAABQAAAAoBQAAAAAAAAEQCADMzMzMoAQAAAAAAAAAAAIA
0YZmD2VqxgH/////////f/////////9/F9Q5/nhKxgEXlKMoQkvGARdUJJd6gcYBCAAIAAQAAgAk
ACQACAACABIAEgAMAAIAAAAAABAAAgAAAAAAFAACAAAAAAAYAAIAVBAAAJd5LAABAgAAGgAAABwA
AgAgAAAAAAAAAAAAAAAAAAAAAAAAABYAGAAgAAIACgAMACQAAgAoAAIAAAAAAAAAAAAQAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA0AAAAsAAIAAAAAAAAAAAAAAAAABAAAAAAAAAAEAAAA
bAB6AGgAdQASAAAAAAAAABIAAABMAGkAcQBpAGEAbgBnACgATABhAHIAcgB5ACkAIABaAGgAdQAJ
AAAAAAAAAAkAAABuAHQAZABzADIALgBiAGEAdAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAABoAAABhxDMABwAAAAnDLQAHAAAAXrQyAAcAAAABAgAABwAAAJe5LAAHAAAAK/Ey
AAcAAADOMDMABwAAAKcuLgAHAAAAKvEyAAcAAACYuSwABwAAAGLEMwAHAAAAlAEzAAcAAAB2xDMA
BwAAAK7+LQAHAAAAMtIsAAcAAAAWCDIABwAAAEJbLgAHAAAAX7QyAAcAAADKnDUABwAAAIVELQAH
AAAAwvAyAAcAAADp6jEABwAAAO2OLgAHAAAAtusxAAcAAACrLi4ABwAAAHIOLgAHAAAADAAAAAAA
AAALAAAATgBUAEQARQBWAC0ARABDAC0AMAA1AAAABgAAAAAAAAAFAAAATgBUAEQARQBWAAAABAAA
AAEEAAAAAAAFFQAAAFlRuBdmcl0lZGM7Cw0AAAAwAAIABwAAADQAAgAHAAAgOAACAAcAACA8AAIA
BwAAIEAAAgAHAAAgRAACAAcAACBIAAIABwAAIEwAAgAHAAAgUAACAAcAACBUAAIABwAAIFgAAgAH
AAAgXAACAAcAACBgAAIABwAAIAUAAAABBQAAAAAABRUAAAC5MBsut0FMbIw7NRUBAgAABQAAAAEF
AAAAAAAFFQAAAFlRuBdmcl0lZGM7C3RULwAFAAAAAQUAAAAAAAUVAAAAWVG4F2ZyXSVkYzsL6Dgy
AAUAAAABBQAAAAAABRUAAABZUbgXZnJdJWRjOwvNODIABQAAAAEFAAAAAAAFFQAAAFlRuBdmcl0l
ZGM7C120MgAFAAAAAQUAAAAAAAUVAAAAWVG4F2ZyXSVkYzsLQRY1AAUAAAABBQAAAAAABRUAAABZ
UbgXZnJdJWRjOwvo6jEABQAAAAEFAAAAAAAFFQAAAFlRuBdmcl0lZGM7C8EZMgAFAAAAAQUAAAAA
AAUVAAAAWVG4F2ZyXSVkYzsLKfEyAAUAAAABBQAAAAAABRUAAABZUbgXZnJdJWRjOwsPXy4ABQAA
AAEFAAAAAAAFFQAAAFlRuBdmcl0lZGM7Cy9bLgAFAAAAAQUAAAAAAAUVAAAAWVG4F2ZyXSVkYzsL
748xAAUAAAABBQAAAAAABRUAAABZUbgXZnJdJWRjOwsHXy4AAAAAAABJ2Q5lasYBCABsAHoAaAB1
AAAAAAAAAHb///9B7c6aNIFdOu97yYh0gF0lAAAAAHb////3pTTassAphu/g++URCk8yAAAAAA==">>),
    PacInfo = binary:part(WholePac, 16#5E, 16#4B0),
    [#pac_info_buffer{info = Info}] = decode_pac_logon_info(PacInfo),
    io:format("~p\n", [Info]),
    ?assertMatch(#kerb_validation_info{
        effective_name = "lzhu"
        }, Info).

-endif.
