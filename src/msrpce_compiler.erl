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

-module(msrpce_compiler).

-export([encode_func_form/4, decode_func_form/4, compile_module/3]).

-type options() :: #{
    endian => big | little
    }.

-type rpce_type() :: basic_type() | {struct, atom(), [field_spec()]}.
-type basic_type() :: uint8 | uint16 | uint32 | uint64 | int8 | int16 | int32 |
    int64 | {fixed_array, integer(), rpce_type()} | {array, rpce_type()} |
    {pointer, rpce_type()} | str | unicode | sid | filetime.

-type field_spec() :: {Field :: atom(), rpce_type()}.

-type form() :: erl_syntax:syntaxTree().
-type expr() :: erl_syntax:syntaxTree().
-type var() :: erl_syntax:syntaxTree().
-type bytes() :: integer().
-type bits() :: integer().

-record(?MODULE, {
    opts = #{} :: options(),
    offset = 0 :: integer(),
    forms = [] :: [form()],
    packforms = [] :: [form()], % struct packing forms, used in decode
    defer = #{} :: #{atom() => {var(), rpce_type()}},
        % on encode, the defer var() is the original value
        % on decode, it's the variable we need to set before packforms
    inum = #{} :: #{integer() => integer()}, % intermediate binaries at each nesting level
    ilvl = 0 :: integer(), % intermediate nesting level
    tnum = 0 :: integer(), % temporary
    pnum = 0 :: integer(), % pointer map: value => ref
    rnum = 0 :: integer(), % pointer map: ref => defer key
    dnum = 0 :: integer(), % next defer id number
    ectx = [] :: [expr()]
    }).

-spec var(string(), integer()) -> var().
var(Prefix, N) ->
    erl_syntax:variable(list_to_atom(Prefix ++ integer_to_list(N))).

-spec ivar(integer(), integer()) -> var().
ivar(Lvl, N) ->
    erl_syntax:variable(list_to_atom("Bin" ++
        integer_to_list(Lvl) ++ "_" ++ integer_to_list(N))).
-spec tvar(integer()) -> var().
tvar(N) -> var("Temp", N).
-spec pvar(integer()) -> var().
pvar(N) -> var("PtrMap", N).
-spec rvar(integer()) -> var().
rvar(N) -> var("RefMap", N).

-spec inc_ivar(#?MODULE{}) -> {var(), var(), #?MODULE{}}.
inc_ivar(S0 = #?MODULE{ilvl = IL, inum = IN0}) ->
    I0 = maps:get(IL, IN0, 0),
    I1 = I0 + 1,
    IN1 = IN0#{IL => I1},
    S1 = S0#?MODULE{inum = IN1},
    {ivar(IL, I0), ivar(IL, I1), S1}.

-spec inc_pvar(#?MODULE{}) -> {var(), var(), #?MODULE{}}.
inc_pvar(S0 = #?MODULE{pnum = I0}) ->
    I1 = I0 + 1,
    S1 = S0#?MODULE{pnum = I1},
    {pvar(I0), pvar(I1), S1}.

-spec inc_rvar(#?MODULE{}) -> {var(), var(), #?MODULE{}}.
inc_rvar(S0 = #?MODULE{rnum = I0}) ->
    I1 = I0 + 1,
    S1 = S0#?MODULE{rnum = I1},
    {rvar(I0), rvar(I1), S1}.

-spec inc_tvar(#?MODULE{}) -> {var(), #?MODULE{}}.
inc_tvar(S0 = #?MODULE{tnum = T0}) ->
    S1 = S0#?MODULE{tnum = T0 + 1},
    {tvar(T0), S1}.

-spec ectx_push({atom(), atom()}, #?MODULE{}) -> #?MODULE{}.
ectx_push({TypAtom, NameAtom}, S0 = #?MODULE{ectx = ECtx0}) ->
    ECtx1 = [erl_syntax:tuple([
        erl_syntax:atom(TypAtom), erl_syntax:atom(NameAtom)]) | ECtx0],
    S0#?MODULE{ectx = ECtx1}.
-spec ectx_pop(#?MODULE{}) -> #?MODULE{}.
ectx_pop(S0 = #?MODULE{ectx = ECtx0}) ->
    [ _ | ECtx1 ] = ECtx0,
    S0#?MODULE{ectx = ECtx1}.

-spec pad_size(bytes(), bytes()) -> bytes().
pad_size(Size, Off) ->
    Rem = Off rem Size,
    PadSize = case Rem of
        0 -> 0;
        _ -> Size - Rem
    end.

-spec type_align(rpce_type()) -> bytes().
type_align(uint8) -> 1;
type_align(uint16) -> 2;
type_align(uint32) -> 4;
type_align(uint64) -> 8;
type_align(int8) -> 1;
type_align(int16) -> 2;
type_align(int32) -> 4;
type_align(int64) -> 8;
type_align({fixed_array, _, T}) -> type_align(T);
type_align({array, T}) -> lists:max([type_align(uint32), type_align(T)]);
type_align({pointer, _T}) -> type_align(uint32);
type_align(str) -> type_align(uint32);
type_align(unicode) -> type_align(uint32);
type_align(sid) -> type_align(uint32);
type_align(filetime) -> type_align(uint32);
type_align({struct, _Rec, Fields}) ->
    lists:max([ type_align(T) || {_Name, T} <- Fields ]).

-spec align_field_var(expr(), bytes()) -> expr().
align_field_var(V, PadSize) ->
    erl_syntax:binary_field(V, erl_syntax:integer(PadSize * 8), []).
-spec align_field_match(bytes()) -> expr().
align_field_match(Size) ->
    align_field_var(erl_syntax:underscore(), Size).
-spec align_field_zero(bytes()) -> expr().
align_field_zero(Size) ->
    align_field_var(erl_syntax:integer(0), Size).

-spec add_forms([form()], #?MODULE{}) -> #?MODULE{}.
add_forms(Fs, S0 = #?MODULE{forms = F0}) ->
    S1 = S0#?MODULE{forms = F0 ++ Fs},
    S1.
-spec add_pack_form(form(), #?MODULE{}) -> #?MODULE{}.
add_pack_form(F, S0 = #?MODULE{packforms = F0}) ->
    S1 = S0#?MODULE{packforms = [F | F0]},
    S1.

-spec int_encode(bits(), signed | unsigned, var(), #?MODULE{}) -> #?MODULE{}.
int_encode(Bits, Signed, SrcVar, S0 = #?MODULE{opts = Opts, offset = O0}) ->
    Endian = maps:get(endian, Opts, big),
    {IV0, IV1, S1} = inc_ivar(S0),
    Padding = pad_size(Bits div 8, O0),
    F = erl_syntax:match_expr(IV1,
        erl_syntax:binary([
            erl_syntax:binary_field(IV0, [erl_syntax:atom(binary)]),
            align_field_zero(Padding),
            erl_syntax:binary_field(SrcVar, erl_syntax:integer(Bits),
                [erl_syntax:atom(Endian), erl_syntax:atom(Signed)])
            ])),
    S2 = add_forms([F], S1),
    S2#?MODULE{offset = O0 + Padding + (Bits div 8)}.

-spec decode_case(var(), [var()], expr(), bytes(), [form()]) -> expr().
decode_case(InVar, OutVars, OkMatch, Off, ECtx) ->
    erl_syntax:case_expr(
        InVar, [
            erl_syntax:clause([OkMatch], [], [erl_syntax:atom(ok)]),
            erl_syntax:clause(
                [erl_syntax:underscore()], [],
                lists:map(fun (Var) ->
                    erl_syntax:match_expr(Var, erl_syntax:atom(error))
                end, OutVars) ++
                [
                    erl_syntax:application(
                        erl_syntax:atom('error'),
                        [erl_syntax:tuple([
                            erl_syntax:atom(invalid_data),
                            erl_syntax:integer(Off),
                            erl_syntax:list(ECtx)
                            ])])
                ])
        ]).

-spec int_decode(bits(), unsigned | signed, var(), #?MODULE{}) -> #?MODULE{}.
int_decode(Bits, Signed, OutVar, S0 = #?MODULE{opts = Opts, offset = O0}) ->
    Endian = maps:get(endian, Opts, big),
    {IV0, IV1, S1} = inc_ivar(S0),
    Padding = pad_size(Bits div 8, O0),
    F = decode_case(IV0, [IV1, OutVar],
        erl_syntax:binary([
            align_field_match(Padding),
            erl_syntax:binary_field(OutVar, erl_syntax:integer(Bits),
                [erl_syntax:atom(Endian), erl_syntax:atom(Signed)]),
            erl_syntax:binary_field(IV1, [erl_syntax:atom(binary)])
            ]),
        O0, S0#?MODULE.ectx),
    S2 = add_forms([F], S1),
    S2#?MODULE{offset = O0 + Padding + (Bits div 8)}.

-spec encode(rpce_type(), var(), #?MODULE{}) -> #?MODULE{}.
encode(uint8, SrcVar, S0 = #?MODULE{}) ->
    int_encode(8, unsigned, SrcVar, S0);
encode(uint16, SrcVar, S0 = #?MODULE{}) ->
    int_encode(16, unsigned, SrcVar, S0);
encode(uint32, SrcVar, S0 = #?MODULE{}) ->
    int_encode(32, unsigned, SrcVar, S0);
encode(uint64, SrcVar, S0 = #?MODULE{}) ->
    int_encode(64, unsigned, SrcVar, S0);
encode(int8, SrcVar, S0 = #?MODULE{}) ->
    int_encode(8, signed, SrcVar, S0);
encode(int16, SrcVar, S0 = #?MODULE{}) ->
    int_encode(16, signed, SrcVar, S0);
encode(int32, SrcVar, S0 = #?MODULE{}) ->
    int_encode(32, signed, SrcVar, S0);
encode(int64, SrcVar, S0 = #?MODULE{}) ->
    int_encode(64, signed, SrcVar, S0);

encode({struct, Record, Fields}, SrcVar, S0 = #?MODULE{offset = O0}) ->
    Alignment = type_align({struct, Record, Fields}),
    Padding = pad_size(Alignment, O0),
    S1 = S0#?MODULE{offset = O0 + Padding},
    {IV0, IV1, S2} = inc_ivar(S1),
    F0 = case Padding of
        0 ->
            erl_syntax:match_expr(IV1, IV0);
        _ ->
            erl_syntax:match_expr(IV1,
                erl_syntax:binary([
                    erl_syntax:binary_field(IV0, [erl_syntax:atom(binary)]),
                    align_field_zero(Padding)
                ]))
    end,
    {FieldMapRev, S3} = lists:foldl(fun ({FName, FType}, {M0, SS0}) ->
        {FVar, SS1} = inc_tvar(SS0),
        M1 = [{FName, FType, FVar} | M0],
        {M1, SS1}
    end, {[], S2}, Fields),
    FieldMap = lists:reverse(FieldMapRev),
    FieldExprs = lists:map(fun ({FName, _FType, FVar}) ->
        erl_syntax:record_field(
            erl_syntax:atom(FName),
            FVar)
    end, FieldMap),
    F1 = erl_syntax:match_expr(
        erl_syntax:record_expr(erl_syntax:atom(Record), FieldExprs),
        SrcVar
        ),
    S4 = add_forms([F0, F1], S3),
    lists:foldl(fun ({_FName, FType, FVar}, SS0) ->
        encode(FType, FVar, SS0)
    end, S4, FieldMap).

encode_defers(S0 = #?MODULE{defer = #{}}) ->
    S0;
encode_defers(S0 = #?MODULE{defer = Defers}) ->
    {DeferVar, S1} = inc_tvar(S0),
    {OuterInput, OuterOutput, S2} = inc_ivar(S1),
    #?MODULE{rnum = R0} = S2,
    RVar = rvar(R0),
    F0 = erl_syntax:match_expr(
        DeferVar,
        erl_syntax:application(
            erl_syntax:atom(lists), erl_syntax:atom(sort), [
                erl_syntax:application(
                    erl_syntax:atom(maps), erl_syntax:atom(keys),
                    [RVar0])])),
    F1 = erl_syntax:match_expr(
        OuterOutput,
        erl_syntax:application(
            erl_syntax:atom(lists), erl_syntax:atom(foldl), [
                erl_syntax:fun_expr([
                    ]),
                OuterInput, DeferVar])),
    S3 = add_forms([F0, F1], S2).

-spec encode_func_form(atom(), rpce_type(), undefined | integer(), options()) -> erl_parse:abstract_form().
encode_func_form(Name, Type, L0, Opts) ->
    L = case L0 of
        undefined -> 2;
        _ -> L0
    end,
    In0 = erl_syntax:match_expr(
        ivar(0, 0),
        erl_syntax:binary([])),
    In1 = erl_syntax:match_expr(
        pvar(0),
        erl_syntax:map_expr([])),
    In2 = erl_syntax:match_expr(
        rvar(0),
        erl_syntax:map_expr([])),
    S0 = #?MODULE{opts = Opts, forms = [In0, In1, In2]},
    InVar = erl_syntax:variable('Input'),
    S1 = encode(Type, InVar, S0),
    S2 = encode_defers(S1),
    #?MODULE{forms = InnerForms, inum = I} = S2,
    F0 = erl_syntax:function(
        erl_syntax:atom(Name),
        [erl_syntax:clause([InVar], [], InnerForms ++ [ivar(0, I)])]),
    F1 = erl_syntax:revert(F0),
    erl_parse:map_anno(fun (_) ->
        erl_anno:set_generated(true, L)
    end, F1).


-spec decode(rpce_type(), var(), #?MODULE{}) -> #?MODULE{}.
decode(uint8, OutVar, S0 = #?MODULE{}) ->
    int_decode(8, unsigned, OutVar, S0);
decode(uint16, OutVar, S0 = #?MODULE{}) ->
    int_decode(16, unsigned, OutVar, S0);
decode(uint32, OutVar, S0 = #?MODULE{}) ->
    int_decode(32, unsigned, OutVar, S0);
decode(uint64, OutVar, S0 = #?MODULE{}) ->
    int_decode(64, unsigned, OutVar, S0);
decode(int8, OutVar, S0 = #?MODULE{}) ->
    int_decode(8, signed, OutVar, S0);
decode(int16, OutVar, S0 = #?MODULE{}) ->
    int_decode(16, signed, OutVar, S0);
decode(int32, OutVar, S0 = #?MODULE{}) ->
    int_decode(32, signed, OutVar, S0);
decode(int64, OutVar, S0 = #?MODULE{}) ->
    int_decode(64, signed, OutVar, S0);

decode({struct, Record, Fields}, OutVar, S0 = #?MODULE{offset = O0}) ->
    Alignment = type_align({struct, Record, Fields}),
    Padding = pad_size(Alignment, O0),
    S1 = ectx_push({struct, Record}, S0#?MODULE{offset = O0 + Padding}),
    {IV0, IV1, S2} = inc_ivar(S1),
    F0 = case Padding of
        0 ->
            erl_syntax:match_expr(IV1, IV0);
        _ ->
            decode_case(IV0, [IV1],
                erl_syntax:binary([
                    align_field_match(Padding),
                    erl_syntax:binary_field(IV1, [erl_syntax:atom(binary)])
                    ]),
                O0, S0#?MODULE.ectx)
    end,
    {FieldMapRev, S3} = lists:foldl(fun ({FName, FType}, {M0, SS0}) ->
        {FVar, SS1} = inc_tvar(SS0),
        M1 = [{FName, FType, FVar} | M0],
        {M1, SS1}
    end, {[], S2}, Fields),
    FieldMap = lists:reverse(FieldMapRev),
    FieldExprs = lists:map(fun ({FName, _FType, FVar}) ->
        erl_syntax:record_field(
            erl_syntax:atom(FName),
            FVar)
    end, FieldMap),
    S4 = add_forms([F0], S3),
    S5 = lists:foldl(fun ({FName, FType, FVar}, SS0) ->
        SS1 = ectx_push({field, FName}, SS0),
        ectx_pop(decode(FType, FVar, SS1))
    end, S4, FieldMap),
    F1 = erl_syntax:match_expr(
        OutVar,
        erl_syntax:record_expr(erl_syntax:atom(Record), FieldExprs)
        ),
    ectx_pop(add_forms([F1], S5)).

-spec decode_func_form(atom(), rpce_type(), undefined | integer(), options()) -> erl_parse:abstract_form().
decode_func_form(Name, Type, L0, Opts) ->
    L = case L0 of
        undefined -> 2;
        _ -> L0
    end,
    S0 = #?MODULE{opts = Opts, forms = []},
    OutVar = erl_syntax:variable('Output'),
    S1 = decode(Type, OutVar, S0),
    #?MODULE{forms = InnerForms, inum = I} = S1,
    Out = erl_syntax:tuple([
        erl_syntax:atom(ok),
        OutVar,
        ivar(I)
        ]),
    F0 = erl_syntax:function(
        erl_syntax:atom(Name),
        [erl_syntax:clause([ivar(0)], [],
            InnerForms ++ [Out])]),
    F1 = erl_syntax:revert(F0),
    erl_parse:map_anno(fun (_) ->
        erl_anno:set_generated(true, L)
    end, F1).

make_forms([]) -> [];
make_forms(Tokens) ->
    {BeforeDot, AfterDot0} = lists:splitwith(fun
        ({dot, _}) -> false;
        (_) -> true
    end, Tokens),
    [Dot = {dot, _} | AfterDot1] = AfterDot0,
    {ok, Form} = erl_parse:parse_form(BeforeDot ++ [Dot]),
    [Form | make_forms(AfterDot1)].

compile_module(Name, Type, Opts) ->
    ModName = binary_to_atom(iolist_to_binary([
        "msrpce_dyn_", integer_to_binary(erlang:unique_integer([positive]))
        ])),
    RecordsPath = code:lib_dir(msrpce) ++ "/include/records.hrl",
    {ok, Data} = file:read_file(RecordsPath),
    {ok, RecTokens, _} = erl_scan:string(
        unicode:characters_to_list(Data, utf8)),
    RecForms = make_forms(RecTokens),
    NameStr = atom_to_list(Name),
    DecodeName = list_to_atom("decode_" ++ NameStr),
    EncodeName = list_to_atom("encode_" ++ NameStr),
    ModHeaderForms = [
        {attribute, 1, module, ModName},
        {attribute, 1, export, [{EncodeName, 1}, {DecodeName, 1}]}],
    EncodeForm = encode_func_form(EncodeName, Type, 2, Opts),
    DecodeForm = decode_func_form(DecodeName, Type, 2, Opts),
    Forms = ModHeaderForms ++ RecForms ++ [EncodeForm, DecodeForm],
    case compile:forms(Forms, [return_errors]) of
        {ok, Mod, Bin} ->
            {module, Mod} = code:load_binary(Mod, "msrpce_dynamic", Bin),
            {ok, Mod};
        {ok, Mod, Bin, _Warnings} ->
            {module, Mod} = code:load_binary(Mod, "msrpce_dynamic", Bin),
            {ok, Mod};
        {error, [{_, Errs}], _Warnings} ->
            FormatErrs = lists:map(fun ({_Loc, _Mod, Desc}) ->
                compile:format_error(Desc)
            end, Errs),
            {error, iolist_to_binary(FormatErrs)}
    end.
