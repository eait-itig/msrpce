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

-record(msrpce_defer, {
    referent :: msrpce_runtime:referent(),
    typename :: msrpce_runtime:typename(),
    align = 1 :: integer(),
    func :: msrpce_runtime:decoder() | msrpce_runtime:encoder(),
    val :: term() | undefined
    }).

-record(msrpce_state, {
    mode :: encode | decode,
    aliasing :: boolean(),
    offset = 0 :: integer(),
    data :: binary(),
    referents = gb_sets:new() :: gb_sets:set(msrpce_runtime:referent()),
    defer_by_ref = #{} :: #{msrcpe_runtime:referent() => #msrpce_defer{}},
    defer_by_val = #{} :: #{term() => msrcpe_runtime:referent()}
    }).

-record(msrpce_ptr, {
    referent :: msrpce_runtime:referent()
    }).

-record(msrpce_sid, {
    revision :: msrpce:uint8(),
    sub_auth_count :: msrpce:uint8(),
    identifier_auth :: msrpce:fixed_bin(6),
    sub_auths :: msrpce:conformant_array(msrpce:uint32())
    }).

-record(msrpce_unicode_string, {
    len :: msrpce:uint16(),
    maxlen :: msrpce:uint16(),
    str :: msrpce:pointer(msrpce:array(msrpce:uint16()))
    }).

-record(msrpce_syntax_id, {
    uuid = msrpce:uuid_from_string("8a885d04-1ceb-11c9-9fe8-08002b104860") :: msrpce:uuid(),
    version = 2 :: msrpce:uint32()
    }).

-record(msrpce_interface_id, {
    uuid = <<0:16/unit:8>> :: msrpce:uuid(),
    ver_maj = 1 :: msrpce:uint16(),
    ver_min = 0 :: msrpce:uint16()
    }).

-record(msrpce_cte_v1, {
    version = 1 :: msrpce:uint8(),
    endian = 16#10 :: msrpce:uint8(),
    hdrlen = 16#0800 :: msrpce:uint16(),    % endian swapped!
    filler = <<16#CCCCCCCC:32/big>> :: msrpce:fixed_bin(4)
    }).

-record(msrpce_cte_v2, {
    version = 2 :: msrpce:uint8(),
    endian = 16#10 :: msrpce:uint8(),
    hdrlen = 16#4000 :: msrpce:uint16(),    % endian swapped!
    endinfo = 16#cccccccc :: msrpce:uint32(),
    reserved = <<16#cccccccc:32/big, 16#cccccccc:32/big, 16#cccccccc:32/big, 16#cccccccc:32/big>> :: msrpce:fixed_bin(16),
    xfersyntax = #msrpce_syntax_id{} :: #msrpce_syntax_id{},
    interfaceid = #msrpce_interface_id{} :: #msrpce_interface_id{}
    }).
