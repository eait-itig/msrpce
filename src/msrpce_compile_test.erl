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

-record(dummy, {a, b = 5}).

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
    szb :: size_of(b, uint32()),
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
    as :: array(uint16()),
    acnt :: length_of(as, uint8())
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

-record(test10, {
    status :: ntstatus(),
    values :: multi_string(),
    uvalues :: multi_unicode()
    }).

-type user_session_key() :: msrpce:aligned_bin(16, 4).

% MS-PAC section 2.2.1
-type sid_attrs() :: msrpce:bitset(
    ulong(),
    mandatory | default | enabled | owner | resource,
    #{mandatory => 0,
      default   => 1,
      enabled   => 2,
      owner     => 3,
      resource  => 29}).

% MS-PAC section 2.5
-type user_flags() :: msrpce:bitset(
    ulong(),
    guest | no_encrypt | lanman_key | subauth_key | machine | ntlmv2_dc |
    profile | extra_sids | resource_groups,
    #{guest         => 0,
      no_encrypt    => 1,
      lanman_key    => 3,
      subauth_key   => 6,
      machine       => 7,
      ntlmv2_dc     => 8,
      profile       => 10,
      extra_sids    => 5,
      resource_groups => 9}).

% MS-SAMR section 2.2.1.12
-type samr_uac() :: msrpce:bitset_mask(
    ulong(),
    disabled | homedir_req | no_password | temp_dupe | normal | mns_logon |
    interdomain | workstation | server | no_expire_password | auto_locked |
    enc_text_pw_allowed | smartcard_only | delegation_trust | not_delegated |
    des_only | no_preauth | password_expired | delegation_auth_trust |
    no_auth_data | partial_secrets,
    #{disabled              => 16#00000001,
      homedir_req           => 16#00000002,
      no_password           => 16#00000004,
      temp_dupe             => 16#00000008,
      normal                => 16#00000010,
      mns_logon             => 16#00000020,
      interdomain           => 16#00000040,
      workstation           => 16#00000080,
      server                => 16#00000100,
      no_expire_password    => 16#00000200,
      auto_locked           => 16#00000400,
      enc_text_pw_allowed   => 16#00000800,
      smartcard_only        => 16#00001000,
      delegation_trust      => 16#00002000,
      not_delegated         => 16#00004000,
      des_only              => 16#00008000,
      no_preauth            => 16#00010000,
      password_expired      => 16#00020000,
      delegation_auth_trust => 16#00040000,
      no_auth_data          => 16#00080000,
      partial_secrets       => 16#00100000
      }).

-record(group_membership, {
    relative_id     :: ulong(),
    attributes      :: sid_attrs()
    }).

-record(sid_and_attrs, {
    sid             :: pointer(sid()),
    attributes      :: sid_attrs()
    }).

-record(kerb_validation_info, {
    logon_time              :: filetime(),
    logoff_time             :: filetime(),
    kickoff_time            :: filetime(),
    password_last_set       :: filetime(),
    password_can_change     :: filetime(),
    password_must_change    :: filetime(),
    effective_name          :: rpc_unicode_str(),
    full_name               :: rpc_unicode_str(),
    logon_script            :: rpc_unicode_str(),
    profile_path            :: rpc_unicode_str(),
    home_directory          :: rpc_unicode_str(),
    home_directory_drive    :: rpc_unicode_str(),
    logon_count             :: ushort(),
    bad_password_count      :: ushort(),
    user_id                 :: ulong(),
    primary_group_id        :: ulong(),
    group_count             :: ulong(),
    group_ids               :: pointer(varying_array(#group_membership{})),
    user_flags              :: user_flags(),
    user_session_key        :: user_session_key(),
    logon_server            :: rpc_unicode_str(),
    logon_domain_name       :: rpc_unicode_str(),
    logon_domain_id         :: pointer(sid()),
    reserved1               :: fixed_array(2, ulong()),
    user_account_control    :: samr_uac(),
    sub_auth_status         :: ulong(),

    last_successful_ilogon  :: filetime(),
    last_failed_ilogon      :: filetime(),
    failed_ilogon_count     :: ulong(),

    reserved3               :: ulong(),

    sid_count               :: ulong(),
    extra_sids              :: pointer(varying_array(#sid_and_attrs{})),

    resource_group_domain_sid   :: pointer(sid()),
    resource_group_count        :: ulong(),
    resource_groups             :: pointer(array(#group_membership{}))
    }).

-record(pac_info_buffer, {
    info            :: pointer(#kerb_validation_info{})
    }).

-rpce(#{endian => big, pointer_aliasing => true}).
-rpce_struct(what).
-rpce_struct(test2).
-rpce_struct(test3).
-rpce_struct(test4).
-rpce_struct(test5).
-rpce_struct(test6).
-rpce_struct(test7).
-rpce_struct(test8).
-rpce_struct(test9).
-rpce_struct(test10).

-rpce_stream({strtest, [test1, test3]}).

-rpce(#{endian => little, pointer_aliasing => false}).
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
    ?assertMatch(<<61:32/big, 24:32/big, 16#00020000:32/little,
        (-123):32/big-signed,
        1, 0:24, 2:32/big, 3, 0:24, 0:32, 4:64/big>>, Data),
    DeStruct = decode_test2(Data),
    ?assertMatch(#test2{a = 61,
                    b = #test1{a = 1, b = 2, c = 3, d = 4},
                    c = -123}, DeStruct).

test3_test() ->
    Struct = #test3{a = 30, b = "hello world"},
    Data = encode_test3(Struct),
    ?assertMatch(<<30, 0:24, 11:32/big, 0:32, 11:32/big, "hello world">>,
        Data),
    DeStruct = decode_test3(Data),
    ?assertMatch(Struct, DeStruct).

test3_offset_test() ->
    Data = <<30, 0:24, 16:32/big, 2:32/big, 11:32/big, 0, 0, "hello world", 0, 0, 0>>,
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
        1:16/big,2:16/big,3:16/big, 3>>, Data),
    DeStruct = decode_test5(Data),
    ?assertMatch(#test5{as = [1,2,3], b = 1000}, DeStruct).

test5_offset_test() ->
    Data = <<5:32/big,1000:16/big,0:16,1:32,3:32/big,
        10:16/big,1:16/big,2:16/big,3:16/big,50:16/big, 3>>,
    DeStruct = decode_test5(Data),
    ?assertMatch(#test5{as = [1,2,3], b = 1000}, DeStruct).

test6_test() ->
    Struct = #test6{c = 31, d = 123145, as = [
        #test3{a=3,b="hello"}, #test3{a=4,b="world"}]},
    Data = encode_test6(Struct),
    ?assertMatch(<<2:32/big, 31:16/big, 0:16,
        0:32, 2:32/big,
        3, 0:24, 5:32/big, 0:32, 5:32/big, "hello", 0, 0, 0,
        4, 0:24, 5:32/big, 0:32, 5:32/big, "world", 0, 0, 0,
        123145:32/big>>, Data),
    DeStruct = decode_test6(Data),
    ?assertMatch(Struct, DeStruct).

test7_test() ->
    Struct = #test7{c = 31, d = 123145, as = [
        #test3{a=3,b="hello"}, #test3{a=4,b="world"}]},
    Data = encode_test7(Struct),
    ?assertMatch(<<31:16/big, 0:16, 16#00020000:32/little, 123145:32/big,
        2:32/big, 0:32, 2:32/big,
        3, 0:24, 5:32/big, 0:32, 5:32/big, "hello", 0, 0, 0,
        4, 0:24, 5:32/big, 0:32, 5:32/big, "world">>, Data),
    DeStruct = decode_test7(Data),
    ?assertMatch(Struct, DeStruct).

test8_test() ->
    Struct = #test8{arr = [#test3{a=3,b="hello"}, #test3{a=4,b="world"},
        #test3{a=3,b="hello"}, #test3{a=3,b="hello"}, #test3{a=4,b="hello"}]},
    Data = encode_test8(Struct),
    ?assertMatch(<<5:32/big,
        16#00020000:32/little, 16#00020004:32/little, 16#00020000:32/little,
        16#00020000:32/little, 16#00020008:32/little,
        3, 0:24, 5:32/big, 0:32, 5:32/big, "hello", 0, 0, 0,
        4, 0:24, 5:32/big, 0:32, 5:32/big, "world", 0, 0, 0,
        4, 0:24, 5:32/big, 0:32, 5:32/big, "hello">>, Data),
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
        10:16/big, 10:16/big, 16#00020000:32/little,
        10:16/big, 10:16/big, 16#00020004:32/little,
        5:32/big, 0:32, 5:32/big, 0, $h, 0, $e, 0, $l, 0, $l, 0, $o, 0, 0,
        5:32/big, 0:32, 5:32/big, 0, $w, 0, $o, 0, $r, 0, $l, 0, $d>>,
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
    <<1, 16#00, 8:16/little, _:32, Inner/binary>> = Data,
    <<I0Len:32/little, _:32, StructData0:I0Len/binary,
      I1Len:32/little, _:32, StructData1:I1Len/binary>> = Inner,
    ?assertMatch(<<1, 0:24, 2:32/big, 3, 0:24, 0:32, 4:64/big>>, StructData0),
    ?assertMatch(<<30, 0:24, 11:32/big, 0:32, 11:32/big, "hello world", _,
                   _:32>>, StructData1),
    DeStream = decode_strtest(Data),
    ?assertMatch([Struct1, Struct3], DeStream).

test10_test() ->
    Struct = #test10{status = {success, 'STATUS_SUCCESS'},
                     values = ["foo", "bar"],
                     uvalues = ["test", "what"]},
    Data = encode_test10(Struct),
    ?assertMatch(<<0:32/big, 16#00020000:32/little, 16#00020004:32/little,
        9:32/big, 0:32, 9:32/big, "foo", 0, "bar", 0, 0, 0, 0, 0,
        11:32/big, 0:32, 11:32/big, 0, $t, 0, $e, 0, $s, 0, $t, 0, 0,
                                    0, $w, 0, $h, 0, $a, 0, $t, 0, 0,
                                    0, 0>>, Data).

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
    ?assertMatch(#kerb_validation_info{
        effective_name = "lzhu",
        user_flags = #{extra_sids := true},
        user_account_control = #{normal := true},
        user_id = 2914711
        }, Info),
    PacInfo2 = encode_pac_logon_info([#pac_info_buffer{info = Info}]),
    ?assertMatch([#pac_info_buffer{info = Info}],
        decode_pac_logon_info(PacInfo2)).

-endif.
