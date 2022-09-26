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

-module(msrpce_runtime).

-include("records.hrl").

-export([
    read_ptr/4,
    write_ptr/5,
    get_ptr_val/3,
    finish/1,
    align/2,
    array_decode/3,
    write_privhdr_v1/3,
    write_privhdr_v2/3,
    read_privhdr_v1/2,
    read_privhdr_v2/2
    ]).

-export_type([
    defer/0, state/0, ptrval/0, typename/0, referent/0, decoder/0,
    encoder/0, finalizer/0
    ]).

-type defer() :: #msrpce_defer{}.
-type state() :: #msrpce_state{}.
-type ptrval() :: #msrpce_ptr{}.
-type typename() :: atom() | {module(), atom()}.
-type referent() :: integer().
-type bytes() :: integer().
-type encoder() :: fun( (state(), term()) -> state() ).
-type decoder() :: fun( (state()) -> {term(), state()} ).
-type finalizer() :: fun( (term(), state()) -> term() ).

-spec pad_size(bytes(), bytes()) -> bytes().
pad_size(Size, Off) ->
    Rem = Off rem Size,
    case Rem of
        0 -> 0;
        _ -> Size - Rem
    end.

-spec array_decode(integer(), decoder(), state()) -> {[term()], state()}.
array_decode(0, _Dec, S0 = #msrpce_state{}) ->
    {[], S0};
array_decode(N, Dec, S0 = #msrpce_state{}) ->
    {Member, S1} = Dec(S0),
    {Rest, S2} = array_decode(N - 1, Dec, S1),
    {[Member | Rest], S2}.

-spec align(bytes(), state()) -> state().
align(Align, S0 = #msrpce_state{data = D0, offset = O0, mode = encode}) ->
    Pad = pad_size(Align, O0),
    D1 = <<D0/binary, 0:Pad/unit:8>>,
    S0#msrpce_state{data = D1, offset = O0 + Pad};
align(Align, S0 = #msrpce_state{data = D0, offset = O0, mode = decode}) ->
    Pad = pad_size(Align, O0),
    <<_:Pad/unit:8, D1/binary>> = D0,
    S0#msrpce_state{data = D1, offset = O0 + Pad}.

-spec read_ptr(typename(), bytes(), decoder(), state()) -> {ptrval(), state()}.
read_ptr(TypeName, Align, Func, S0 = #msrpce_state{data = D0,
                                                   offset = O0,
                                                   defer_by_ref = Refs0,
                                                   referents = RefSet0}) ->
    #msrpce_state{mode = decode} = S0,
    case D0 of
        <<Ref:32/big-unsigned, D1/binary>> ->
            case Ref of
                0 ->
                    {#msrpce_ptr{referent = 0},
                     S0#msrpce_state{data = D1, offset = O0 + 4}};
                _ ->
                    case Refs0 of
                        #{Ref := #msrpce_defer{typename = TypeName}} ->
                            {#msrpce_ptr{referent = Ref},
                             S0#msrpce_state{data = D1, offset = O0 + 4}};
                        _ ->
                            Defer = #msrpce_defer{referent = Ref,
                                                  typename = TypeName,
                                                  func = Func,
                                                  align = Align},
                            Refs1 = Refs0#{Ref => Defer},
                            RefSet1 = gb_sets:add_element(Ref, RefSet0),
                            {#msrpce_ptr{referent = Ref},
                             S0#msrpce_state{data = D1, defer_by_ref = Refs1,
                                             referents = RefSet1,
                                             offset = O0 + 4}}
                    end
            end;
        _ ->
            error({end_before_ptr, TypeName, O0})
    end.

-spec write_ptr(typename(), bytes(), encoder(), term(), state()) -> state().
write_ptr(_TypeName, _Align, _Func, undefined, S0 = #msrpce_state{data = D0,
                                                                  offset = O0}) ->
    D1 = <<D0/binary, 0:32>>,
    S0#msrpce_state{data = D1, offset = O0 + 4};
write_ptr(TypeName, Align, Func, V, S0 = #msrpce_state{data = D0,
                                                       offset = O0,
                                                       defer_by_ref = Refs0,
                                                       defer_by_val = Vals0,
                                                       referents = RefSet0}) ->
    #msrpce_state{mode = encode} = S0,
    case Vals0 of
        #{V := Ref} ->
            D1 = <<D0/binary, Ref:32/big-unsigned>>,
            S0#msrpce_state{data = D1, offset = O0 + 4};
        _ ->
            Ref = maps:size(Refs0) + 1,
            Defer = #msrpce_defer{referent = Ref,
                                  typename = TypeName,
                                  func = Func,
                                  val = V,
                                  align = Align},
            Refs1 = Refs0#{Ref => Defer},
            Vals1 = Vals0#{V => Ref},
            RefSet1 = gb_sets:add_element(Ref, RefSet0),
            D1 = <<D0/binary, Ref:32/big-unsigned>>,
            S0#msrpce_state{data = D1, defer_by_ref = Refs1, offset = O0 + 4,
                            defer_by_val = Vals1, referents = RefSet1}
    end.

-spec get_ptr_val(typename(), ptrval(), state()) -> term() | undefined.
get_ptr_val(_TypeName, #msrpce_ptr{referent = 0}, #msrpce_state{}) ->
    undefined;
get_ptr_val(TypeName, Ptr, S0 = #msrpce_state{defer_by_ref = Refs}) ->
    #msrpce_state{mode = decode} = S0,
    #msrpce_ptr{referent = Ref} = Ptr,
    #{Ref := Defer} = Refs,
    #msrpce_defer{typename = TypeName, val = V} = Defer,
    V.

-spec finish(state()) -> state().
finish(S0 = #msrpce_state{defer_by_ref = Refs0, referents = RefSet0}) ->
    case gb_sets:is_empty(RefSet0) of
        true ->
            S0;

        false ->
            {Ref, RefSet1} = gb_sets:take_smallest(RefSet0),
            #{Ref := Defer0} = Refs0,

            #msrpce_defer{func = Func, align = Align, val = V0} = Defer0,
            S1 = align(Align, S0#msrpce_state{referents = RefSet1}),
            {V1, S2} = case S0#msrpce_state.mode of
                encode ->
                    SS = Func(V0, S1),
                    {V0, SS};
                decode ->
                    Func(S1)
            end,

            Defer1 = Defer0#msrpce_defer{val = V1},
            #msrpce_state{defer_by_ref = Refs1} = S2,
            Refs2 = Refs1#{Ref => Defer1},
            S3 = S2#msrpce_state{defer_by_ref = Refs2},
            finish(S3)
    end.

write_privhdr_v1(Fun, Input, S0 = #msrpce_state{}) ->
    S1 = align(8, S0),
    #msrpce_state{data = D0, offset = O0} = S1,
    S2 = S1#msrpce_state{data = <<>>, offset = O0 + 8},
    S3 = finish(Fun(Input, S2)),
    S4 = align(8, S3),
    #msrpce_state{data = DD} = S4,
    D1 = iolist_to_binary([D0, <<(byte_size(DD)):32/little, 0:32>>, DD]),
    S4#msrpce_state{data = D1}.

write_privhdr_v2(Fun, Input, S0 = #msrpce_state{}) ->
    S1 = align(16, S0),
    #msrpce_state{data = D0, offset = O0} = S1,
    S2 = S1#msrpce_state{data = <<>>, offset = O0 + 16},
    S3 = finish(Fun(Input, S2)),
    S4 = align(16, S3),
    #msrpce_state{data = DD} = S4,
    D1 = iolist_to_binary([D0, <<(byte_size(DD)):32/little, 0:12/unit:8>>, DD]),
    S4#msrpce_state{data = D1}.

read_privhdr_v1(Fun, S0 = #msrpce_state{}) ->
    S1 = align(8, S0),
    #msrpce_state{data = D0, offset = O0} = S1,
    <<Len:32/little, _:32, DD:Len/binary, D1/binary>> = D0,
    S2 = S1#msrpce_state{data = DD, offset = O0 + 8},
    {Output, S3} = Fun(S2),
    S4 = finish(S3),
    {Output, S4#msrpce_state{data = D1, offset = O0 + 8 + Len}}.

read_privhdr_v2(Fun, S0 = #msrpce_state{}) ->
    S1 = align(16, S0),
    #msrpce_state{data = D0, offset = O0} = S1,
    <<Len:32/little, _:12/unit:8, DD:Len/binary, D1/binary>> = D0,
    S2 = S1#msrpce_state{data = DD, offset = O0 + 16},
    {Output, S3} = Fun(S2),
    S4 = finish(S3),
    {Output, S4#msrpce_state{data = D1, offset = O0 + 16 + Len}}.
