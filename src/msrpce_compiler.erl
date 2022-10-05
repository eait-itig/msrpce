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

-export([
    new/0,
    new/1,
    set_options/2,
    define_type/3,
    compile_type/3,
    compile_stream/4,
    func_forms/1
    ]).
-export_type([
    state/0, options/0, type_options/0, rpce_type/0
    ]).

-type loc() :: erl_syntax:annotation_or_location().
-type options() :: #{
    endian => big | little,
    pointer_aliasing => boolean()
    }.
-type type_options() :: options() | #{
    location => loc()
    }.

-type type_name() :: atom().
-type record_name() :: atom().
-type record_field() :: atom().
-type rpce_type() :: basic_type() | struct_type() | bit_type() | 
    custom_type() | endian_type() | size_type() | type_name().
-type size_type() :: {size_of, record_field(), int_type()} |
    {length_of, record_field(), int_type()}.
-type endian_type() :: {le, rpce_type()} | {be, rpce_type()}.
-type custom_type() :: {custom, rpce_type(), expr(), expr()} |
    {builtin, rpce_type(), expr(), expr()}.
-type bitnum() :: integer().
-type bit_type() :: {bitset, int_type(), #{atom() => bitnum()}}.
-type struct_type() :: {struct, record_name(), [field_spec()]}.
-type array_type() :: {fixed_array, count(), rpce_type()} |
    {conformant_array, rpce_type()} |
    {varying_array, rpce_type()} |
    {array, rpce_type()}.
-type pointer_type() :: {pointer, rpce_type()}.
-type string_type() :: unicode |
    string | {fixed_string, bytes()} | varying_string |
    binary | {fixed_binary, bytes()} | varying_binary |
    {fixed_binary, Size :: bytes(), Alignment :: bytes()}.
-type basic_type() :: int_type() | string_type() | array_type() | pointer_type().
-type int_type() :: boolean | uint8 | uint16 | uint32 | uint64 | int8 | int16 |
    int32 | int64.

-type field_spec() :: {record_field(), rpce_type()}.

-type form() :: erl_syntax:syntaxTree().
-type expr() :: erl_syntax:syntaxTree().
-type var() :: erl_syntax:syntaxTree().
-type bytes() :: integer().
-type bits() :: integer().
-type count() :: integer().

-type type_index() :: #{type_name() => rpce_type()}.
-type type_path() :: [type_name()].

-record(?MODULE, {
    opts = #{} :: options(),
    types = #{} :: type_index(),
    sgen = #{} :: #{type_name() => boolean()},
    funcs = [] :: [form()]
    }).

-opaque state() :: #?MODULE{}.

%% @doc Creates a new blank compiler state.
-spec new() -> state().
new() -> new(#{}).

%% @doc Creates a new compiler state with the specified initial options.
-spec new(options()) -> state().
new(Opts) ->
    #?MODULE{opts = Opts}.

%% @doc Changes the options on a compiler state.
%%
%% These options will only affect subsequent calls to {@link compile_type/3},
%% not the result of any previous calls.
-spec set_options(options(), state()) -> state().
set_options(NewOpts, S0 = #?MODULE{opts = Opts0}) ->
    Opts1 = maps:merge(Opts0, NewOpts),
    S0#?MODULE{opts = Opts1}.

%% @doc Defines a type or struct.
-spec define_type(type_name(), rpce_type(), state()) -> {ok, state()} | {error, term()}.
define_type(Name, Type, S0 = #?MODULE{types = T0}) ->
    case T0 of
        #{Name := _} ->
            {error, {type_already_defined, Name}};
        _ ->
            T1 = T0#{Name => Type},
            S1 = S0#?MODULE{types = T1},
            {ok, S1}
    end.

%% @doc Compiles a type or struct.
-spec compile_type(type_name(), type_options(), state()) ->
    {ok, state()} | {error, term()}.
compile_type(Name, TOpts, S0 = #?MODULE{types = T0, funcs = F0, opts = Opts}) ->
    case T0 of
        #{Name := Type0} ->
            case resolve_typerefs(Type0, T0) of
                {ok, Type1 = {struct, RecName, _}} ->
                    #?MODULE{sgen = SG0} = S0,
                    SG1 = SG0#{RecName => true},
                    L = maps:get(location, TOpts, 2),
                    ToCompile = [{[], Type1} | get_deferred_types(Type1)],
                    F1 = lists:foldl(fun ({Path, FType}, Acc) ->
                        EForm = encode_struct_func_form(Path, FType, L, Opts),
                        DForm = decode_struct_func_form(Path, FType, L, Opts),
                        FForm = decode_finish_func_form(Path, FType, L, Opts),
                        [EForm, DForm, FForm | Acc]
                    end, F0, ToCompile),
                    EForm = encode_func_form(Type1, L, Opts),
                    DForm = decode_func_form(Type1, L, Opts),
                    F2 = [EForm, DForm | F1],
                    {ok, S0#?MODULE{funcs = F2, sgen = SG1}};
                {ok, Other} ->
                    {error, {not_struct_type, Other}};
                Err -> Err
            end;
        _ ->
            {error, {undefined_type, Name}}
    end.

%% @doc Compiles a stream.
-spec compile_stream(type_name(), [type_name()], type_options(), state()) ->
    {ok, state()} | {error, term()}.
compile_stream(Name, Structs, TOpts, S0 = #?MODULE{opts = Opts, sgen = SG0}) ->
    case SG0 of
        #{Name := _} ->
            {error, {duplicate_name, Name}};
        _ ->
            case ensure_structs(Structs, TOpts, S0) of
                {ok, S1 = #?MODULE{funcs = F0}} ->
                    L = maps:get(location, TOpts, 2),
                    EForm = encode_stream_func_form(Name, L, Opts),
                    E1Form = encode_stream_func_form(1, Name, Structs, L, Opts),
                    E2Form = encode_stream_func_form(2, Name, Structs, L, Opts),
                    DForm = decode_stream_func_form(Name, L, Opts),
                    D1Form = decode_stream_func_form(1, Name, Structs, L, Opts),
                    D2Form = decode_stream_func_form(2, Name, Structs, L, Opts),
                    F1 = [EForm, E1Form, E2Form, DForm, D1Form, D2Form | F0],
                    {ok, S1#?MODULE{funcs = F1}};
                Err ->
                    Err
            end
    end.

-spec ensure_structs([type_name()], type_options(), state()) ->
    {ok, state()} | {error, term()}.
ensure_structs([], _, S0 = #?MODULE{}) -> {ok, S0};
ensure_structs([Struct | Rest], TOpts, S0 = #?MODULE{sgen = SG0}) ->
    case SG0 of
        #{Struct := true} ->
            ensure_structs(Rest, TOpts, S0);
        _ ->
            case compile_type(Struct, TOpts, S0) of
                {ok, S1} ->
                    ensure_structs(Rest, TOpts, S1);
                Err ->
                    Err
            end
    end.

%% @doc Returns the compiled forms.
-spec func_forms(state()) -> [form()].
func_forms(#?MODULE{funcs = F0}) ->
    lists:flatten([unused_gag(F) || F <- F0]).

unused_gag(Form) ->
    case erl_syntax:type(Form) of
        function ->
            Name = erl_syntax:function_name(Form),
            Arity = erl_syntax:function_arity(Form),
            ArgsAbs = erl_syntax:abstract([{nowarn_unused_function,
                [{erl_syntax:atom_value(Name), Arity}]}]),
            AttrForm = erl_syntax:attribute(erl_syntax:atom(compile), [ArgsAbs]),
            [Form, erl_syntax:revert(AttrForm)];
        _ ->
            [Form]
    end.

tpdedupe(PathTypes) -> tpdedupe(PathTypes, #{}).

tpdedupe([], Map0) -> maps:to_list(Map0);
tpdedupe([{Path, Type} | Rest], Map0) ->
    Map1 = Map0#{Path => Type},
    tpdedupe(Rest, Map1).

-spec get_deferred_types(rpce_type()) -> [{type_path(), rpce_type()}].
get_deferred_types(Type) -> tpdedupe(get_deferred_types(Type, [], base)).

-spec get_deferred_types(rpce_type(), type_path(), atom()) -> [type_path()].
get_deferred_types({custom, Base, _E, _D}, Path0, Name) ->
    get_deferred_types(Base, Path0, Name);
get_deferred_types({le, Base}, Path0, Name) ->
    get_deferred_types(Base, Path0, Name);
get_deferred_types({be, Base}, Path0, Name) ->
    get_deferred_types(Base, Path0, Name);
get_deferred_types({size_of, _Field, Base}, Path0, Name) ->
    get_deferred_types(Base, Path0, Name);
get_deferred_types({length_of, _Field, Base}, Path0, Name) ->
    get_deferred_types(Base, Path0, Name);
get_deferred_types({bitset, Base, _Bits}, Path0, Name) ->
    get_deferred_types(Base, Path0, Name);
get_deferred_types({pointer, Type = {struct, RecName, Fields}}, Path0, _Name) ->
    Path1 = Path0 ++ [RecName],
    FieldTypes = lists:foldl(fun ({FName, FType}, Acc) ->
        Acc ++ get_deferred_types(FType, Path1, FName)
    end, [], Fields),
    [{Path0, Type} | FieldTypes];
get_deferred_types({pointer, Type}, Path0, Name) ->
    Path1 = Path0 ++ [{scalar_field, Name}],
    [{Path1, Type} | get_deferred_types(Type, Path0, Name)];
get_deferred_types({struct, RecName, Fields}, Path0, _Name) ->
    Path1 = Path0 ++ [RecName],
    lists:foldl(fun ({FName, FType}, Acc) ->
        Acc ++ get_deferred_types(FType, Path1, FName)
    end, [], Fields);
get_deferred_types({fixed_array, _N, SubType}, Path0, Name) ->
    Path1 = Path0 ++ [{array_field, Name}],
    [{Path1, SubType} | get_deferred_types(SubType, Path0, Name)];
get_deferred_types({conformant_array, SubType}, Path0, Name) ->
    Path1 = Path0 ++ [{array_field, Name}],
    [{Path1, SubType} | get_deferred_types(SubType, Path0, Name)];
get_deferred_types({varying_array, SubType}, Path0, Name) ->
    Path1 = Path0 ++ [{array_field, Name}],
    [{Path1, SubType} | get_deferred_types(SubType, Path0, Name)];
get_deferred_types({array, SubType}, Path0, Name) ->
    Path1 = Path0 ++ [{array_field, Name}],
    [{Path1, SubType} | get_deferred_types(SubType, Path0, Name)];
get_deferred_types(_, _, _) -> [].

-spec resolve_typerefs(rpce_type(), type_index()) ->
    {ok, rpce_type()} | {error, term()}.
resolve_typerefs(uint8, _) -> {ok, uint8};
resolve_typerefs(boolean, _) -> {ok, boolean};
resolve_typerefs(uint16, _) -> {ok, uint16};
resolve_typerefs(uint32, _) -> {ok, uint32};
resolve_typerefs(uint64, _) -> {ok, uint64};
resolve_typerefs(int8, _) -> {ok, int8};
resolve_typerefs(int16, _) -> {ok, int16};
resolve_typerefs(int32, _) -> {ok, int32};
resolve_typerefs(int64, _) -> {ok, int64};
resolve_typerefs(string, _) -> {ok, string};
resolve_typerefs(binary, _) -> {ok, binary};
resolve_typerefs(varying_string, _) -> {ok, varying_string};
resolve_typerefs(varying_binary, _) -> {ok, varying_binary};
resolve_typerefs(T = {fixed_string, _}, _) -> {ok, T};
resolve_typerefs(T = {fixed_binary, _}, _) -> {ok, T};
resolve_typerefs(T = {fixed_binary, _, _}, _) -> {ok, T};
resolve_typerefs(unicode, _) -> {ok, unicode};
resolve_typerefs({length_of, Field, SubType0}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {length_of, Field, SubType1}};
        Err -> Err
    end;
resolve_typerefs({size_of, Field, SubType0}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {size_of, Field, SubType1}};
        Err -> Err
    end;
resolve_typerefs({bitset, SubType0, Map}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {bitset, SubType1, Map}};
        Err -> Err
    end;
resolve_typerefs({custom, SubType0, Enc, Dec}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {custom, SubType1, Enc, Dec}};
        Err -> Err
    end;
resolve_typerefs({Endian, SubType0}, Idx) when (Endian =:= le) or
                                               (Endian =:= be) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {Endian, SubType1}};
        Err -> Err
    end;
resolve_typerefs({fixed_array, N, SubType0}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {fixed_array, N, SubType1}};
        Err -> Err
    end;
resolve_typerefs({conformant_array, SubType0}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {conformant_array, SubType1}};
        Err -> Err
    end;
resolve_typerefs({varying_array, SubType0}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {varying_array, SubType1}};
        Err -> Err
    end;
resolve_typerefs({array, SubType0}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {array, SubType1}};
        Err -> Err
    end;
resolve_typerefs({pointer, SubType0}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} -> {ok, {pointer, SubType1}};
        Err -> Err
    end;
resolve_typerefs({struct, RecName, []}, _Idx) ->
    {ok, {struct, RecName, []}};
resolve_typerefs({struct, RecName, [{Field, SubType0} | Rest]}, Idx) ->
    case resolve_typerefs(SubType0, Idx) of
        {ok, SubType1} ->
            case resolve_typerefs({struct, RecName, Rest}, Idx) of
                {ok, {struct, RecName, RestFields}} ->
                    {ok, {struct, RecName, [{Field, SubType1} | RestFields]}};
                Err -> Err
            end;
        Err -> Err
    end;
resolve_typerefs(TypeName, Idx) when is_atom(TypeName) ->
    case Idx of
        #{TypeName := SubType0} ->
            case resolve_typerefs(SubType0, Idx) of
                {ok, SubType1} -> {ok, SubType1};
                Err -> Err
            end;
        _ ->
            {error, {undefined_type, TypeName}}
    end.

-spec var(string(), integer()) -> var().
var(Prefix, N) ->
    erl_syntax:variable(list_to_atom(Prefix ++ integer_to_list(N))).

-spec concat_atoms([atom()]) -> expr().
concat_atoms(Parts) ->
    Name = lists:flatten(lists:join("_", [atom_to_list(X) || X <- Parts])),
    NameAtom = list_to_atom(Name),
    erl_syntax:atom(NameAtom).

-spec enc_fun_name(type_path()) -> expr().
enc_fun_name(Parts0) ->
    concat_atoms([rpce, encode | Parts0]).
-spec dec_fun_name(type_path()) -> expr().
dec_fun_name(Parts0) ->
    concat_atoms([rpce, decode | Parts0]).
-spec finish_fun_name(type_path()) -> expr().
finish_fun_name(Parts0) ->
    concat_atoms([rpce, decode | Parts0] ++ [finish]).

-spec enc_fun(type_path()) -> expr().
enc_fun(Parts) ->
    erl_syntax:implicit_fun(erl_syntax:arity_qualifier(enc_fun_name(Parts),
        erl_syntax:integer(2))).
-spec dec_fun(type_path()) -> expr().
dec_fun(Parts) ->
    erl_syntax:implicit_fun(erl_syntax:arity_qualifier(dec_fun_name(Parts),
        erl_syntax:integer(1))).

-type tvar() :: var().
-type dvar() :: var().
-type svar() :: var().
-type ovar() :: var().

-spec tvar(integer()) -> tvar().
tvar(N) -> var("Temp", N).
-spec dvar(integer()) -> dvar().
dvar(N) -> var("Data", N).
-spec svar(integer()) -> svar().
svar(N) -> var("State", N).
-spec ovar(integer()) -> ovar().
ovar(N) -> var("Offset", N).

-type fctx() :: [{struct, atom()} | {field, atom()}].

-record(fstate, {
    opts :: options(),
    forms = [] :: [form()],
    hoists = [] :: [{int_type(), expr()}],
    unpacks = #{} :: #{fctx() => #{atom() => var()}},
    maxlens = #{} :: #{fctx() => expr()},
    customs = #{} :: #{var() => var()},
    in_dblk = false :: boolean(),
    obase = unknown :: integer() | unknown,
    offset = 0 :: integer(),
    tn = 0 :: integer(),
    odn = 0 :: integer(),
    sn = 0 :: integer(),
    ctx = [] :: fctx()
    }).
-type fstate() :: #fstate{}.

-spec inc_odvar(fstate()) -> {dvar(), dvar(), ovar(), ovar(), fstate()}.
inc_odvar(S0 = #fstate{in_dblk = false}) ->
    S1 = open_dblk(S0),
    inc_odvar(S1);
inc_odvar(S0 = #fstate{odn = OD0, in_dblk = true}) ->
    OD1 = OD0 + 1,
    S1 = S0#fstate{odn = OD1},
    {dvar(OD0), dvar(OD1), ovar(OD0), ovar(OD1), S1}.

open_dblk(S0 = #fstate{in_dblk = true}) ->
    S0;
open_dblk(S0 = #fstate{in_dblk = false, sn = SN0, odn = ODN0, forms = F0}) ->
    SV0 = svar(SN0),
    ODN1 = ODN0 + 1,
    DV0 = dvar(ODN1),
    OV0 = ovar(ODN1),
    F = erl_syntax:match_expr(
        erl_syntax:record_expr(erl_syntax:atom(msrpce_state),
            [erl_syntax:record_field(erl_syntax:atom(data), DV0),
             erl_syntax:record_field(erl_syntax:atom(offset), OV0)]),
        SV0),
    S0#fstate{in_dblk = true, odn = ODN1, forms = F0 ++ [F]}.

close_dblk(S0 = #fstate{in_dblk = false}) ->
    S0;
close_dblk(S0 = #fstate{in_dblk = true, sn = SN0, odn = ODN0, forms = F0}) ->
    SN1 = SN0 + 1,
    SV0 = svar(SN0),
    SV1 = svar(SN1),
    DV0 = dvar(ODN0),
    OV0 = ovar(ODN0),
    F = erl_syntax:match_expr(
        SV1,
        erl_syntax:record_expr(SV0, erl_syntax:atom(msrpce_state),
            [erl_syntax:record_field(erl_syntax:atom(data), DV0),
             erl_syntax:record_field(erl_syntax:atom(offset), OV0)])),
    S0#fstate{in_dblk = false, sn = SN1, forms = F0 ++ [F]}.

-spec inc_tvar(fstate()) -> {tvar(), fstate()}.
inc_tvar(S0 = #fstate{tn = T0}) ->
    S1 = S0#fstate{tn = T0 + 1},
    {tvar(T0), S1}.

-spec inc_svar(fstate()) -> {svar(), svar(), fstate()}.
inc_svar(S0 = #fstate{in_dblk = true}) ->
    S1 = close_dblk(S0),
    inc_svar(S1);
inc_svar(S0 = #fstate{sn = I0, in_dblk = false}) ->
    I1 = I0 + 1,
    S1 = S0#fstate{sn = I1},
    {svar(I0), svar(I1), S1}.

-spec cur_svar(fstate()) -> {svar(), fstate()}.
cur_svar(S0 = #fstate{in_dblk = true}) -> cur_svar(close_dblk(S0));
cur_svar(S0 = #fstate{sn = I, in_dblk = false}) -> {svar(I), S0}.

-spec push_struct(atom(), fstate()) -> fstate().
push_struct(Struct, S0 = #fstate{ctx = Ctx0}) ->
    Ctx1 = [{struct, Struct} | Ctx0],
    S0#fstate{ctx = Ctx1}.
-spec push_field(atom(), fstate()) -> fstate().
push_field(Field, S0 = #fstate{ctx = Ctx0}) ->
    Ctx1 = [{field, Field} | Ctx0],
    S0#fstate{ctx = Ctx1}.
-spec pop_struct(fstate()) -> fstate().
pop_struct(S0 = #fstate{ctx = Ctx0}) ->
    [{struct, _} | Ctx1] = Ctx0,
    S0#fstate{ctx = Ctx1}.
-spec pop_field(fstate()) -> fstate().
pop_field(S0 = #fstate{ctx = Ctx0}) ->
    [{field, _} | Ctx1] = Ctx0,
    S0#fstate{ctx = Ctx1}.
-spec build_struct_path(fstate()) -> type_path().
build_struct_path(#fstate{ctx = Ctx}) ->
    lists:foldl(fun
        ({struct, Name}, Acc) -> [Name | Acc];
        ({field, _}, Acc) -> Acc
    end, [], Ctx).
-spec build_enc_ectx(fstate()) -> expr().
build_enc_ectx(#fstate{ctx = Ctx}) ->
    StepsRev = lists:map(fun ({Type, Name}) ->
        erl_syntax:tuple([erl_syntax:atom(Type), erl_syntax:atom(Name)])
    end, Ctx),
    erl_syntax:tuple(lists:reverse(StepsRev)).
-spec build_ectx(fstate()) -> expr().
build_ectx(#fstate{ctx = Ctx, odn = ODN0}) ->
    StepsRev = lists:map(fun ({Type, Name}) ->
        erl_syntax:tuple([erl_syntax:atom(Type), erl_syntax:atom(Name)])
    end, Ctx),
    PathExpr = erl_syntax:tuple(lists:reverse(StepsRev)),
    erl_syntax:tuple([
        erl_syntax:atom(invalid_data),
        ovar(ODN0), PathExpr]).

-spec pad_size(bytes(), bytes()) -> bytes().
pad_size(Size, Off) ->
    Rem = Off rem Size,
    case Rem of
        0 -> 0;
        _ -> Size - Rem
    end.

-spec type_align(rpce_type()) -> bytes().
type_align(uint8) -> 1;
type_align(boolean) -> 1;
type_align(uint16) -> 2;
type_align(uint32) -> 4;
type_align(uint64) -> 8;
type_align(int8) -> 1;
type_align(int16) -> 2;
type_align(int32) -> 4;
type_align(int64) -> 8;
type_align({bitset, Base, _Map}) -> type_align(Base);
type_align({custom, Base, _E, _D}) -> type_align(Base);
type_align({fixed_array, _, T}) -> type_align(T);
type_align({array, T}) -> lists:max([ type_align(uint32), type_align(T) ]);
type_align({conformant_array, T}) -> lists:max([ type_align(uint32), type_align(T) ]);
type_align({varying_array, T}) -> lists:max([ type_align(uint32), type_align(T) ]);
type_align({pointer, _T}) -> type_align(uint32);
type_align(string) -> type_align(uint32);
type_align(binary) -> type_align(uint32);
type_align(varying_binary) -> type_align(uint32);
type_align(varying_string) -> type_align(uint32);
type_align({fixed_string, _}) -> 1;
type_align({fixed_binary, _}) -> 1;
type_align({fixed_binary, _, A}) -> A;
type_align({length_of, _, T}) -> type_align(T);
type_align({size_of, _, T}) -> type_align(T);
type_align({le, T}) -> type_align(T);
type_align({be, T}) -> type_align(T);
type_align(unicode) -> type_align(uint32);
type_align({struct, _Rec, Fields}) ->
    lists:max([ type_align(T) || {_Name, T} <- Fields ]).

-spec add_encode_step(bytes(), [expr()], bytes() | expr(), fstate()) -> fstate().
add_encode_step(Align, Fields, OffInc, S0 = #fstate{obase = N}) when Align > N ->
    add_encode_step(Align, Fields, OffInc, S0#fstate{obase = unknown});
add_encode_step(Align, Fields, OffInc, S0 = #fstate{obase = unknown}) ->
    {SV0, SV1, S1} = inc_svar(S0),
    Form = erl_syntax:match_expr(SV1,
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(align),
            [erl_syntax:integer(Align), SV0])),
    S2 = add_forms([Form], S1),
    S3 = S2#fstate{obase = Align, offset = 0},
    add_encode_step(Align, Fields, OffInc, S3);
add_encode_step(Align, Fields, OffsetInc0, S0 = #fstate{offset = Offset0})
                                                when is_integer(OffsetInc0) ->
    {AlignFields, OffsetInc1} = case pad_size(Align, Offset0) of
        0 -> {[], OffsetInc0};
        Pad -> {[erl_syntax:binary_field(erl_syntax:integer(0),
            erl_syntax:integer(Pad * 8), [])], OffsetInc0 + Pad}
    end,
    case {Fields, AlignFields} of
        {[], []} ->
            S0;
        _ ->
            {DV0, DV1, OV0, OV1, S1} = inc_odvar(S0),
            F0 = erl_syntax:match_expr(DV1, erl_syntax:binary(
                [erl_syntax:binary_field(DV0, [erl_syntax:atom(binary)])] ++
                AlignFields ++
                Fields)),
            OffsetInc2 = erl_syntax:integer(OffsetInc1),
            F1 = erl_syntax:match_expr(OV1, erl_syntax:infix_expr(
                OV0, erl_syntax:operator('+'), OffsetInc2)),
            S2 = S1#fstate{offset = Offset0 + OffsetInc1},
            add_dblk_forms([F0, F1], S2)
    end;
add_encode_step(Align, Fields, OffsetInc0, S0 = #fstate{offset = Offset0}) ->
    {AlignFields, OffsetInc1} = case pad_size(Align, Offset0) of
        0 -> {[], OffsetInc0};
        Pad -> {[erl_syntax:binary_field(erl_syntax:integer(0),
            erl_syntax:integer(Pad * 8), [])],
            erl_syntax:infix_expr(OffsetInc0, erl_syntax:operator('+'),
                erl_syntax:integer(Pad))}
    end,
    case {Fields, AlignFields} of
        {[], []} ->
            S0;
        _ ->
            {DV0, DV1, OV0, OV1, S1} = inc_odvar(S0),
            F0 = erl_syntax:match_expr(DV1, erl_syntax:binary(
                [erl_syntax:binary_field(DV0, [erl_syntax:atom(binary)])] ++
                AlignFields ++
                Fields)),
            F1 = erl_syntax:match_expr(OV1, erl_syntax:infix_expr(
                OV0, erl_syntax:operator('+'), OffsetInc1)),
            S2 = S1#fstate{obase = unknown, offset = 0},
            add_dblk_forms([F0, F1], S2)
    end.

-spec add_decode_step(bytes(), [expr()], bytes(), fstate()) -> fstate().
add_decode_step(Align, Fields, OffInc, S0 = #fstate{obase = N}) when Align > N ->
    add_decode_step(Align, Fields, OffInc, S0#fstate{obase = unknown});
add_decode_step(Align, Fields, OffInc, S0 = #fstate{obase = unknown}) ->
    {SV0, SV1, S1} = inc_svar(S0),
    Form = erl_syntax:match_expr(SV1,
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(align),
            [erl_syntax:integer(Align), SV0])),
    S2 = add_forms([Form], S1),
    S3 = S2#fstate{obase = Align, offset = 0},
    add_decode_step(Align, Fields, OffInc, S3);
add_decode_step(Align, Fields, OffsetInc0, S0 = #fstate{offset = Offset0}) ->
    {AlignFields, OffsetInc1} = case pad_size(Align, Offset0) of
        0 -> {[], OffsetInc0};
        Pad ->
            OO = if
                is_integer(OffsetInc0) -> OffsetInc0 + Pad;
                true -> erl_syntax:infix_expr(OffsetInc0,
                    erl_syntax:operator('+'), erl_syntax:integer(Pad))
            end,
            {[erl_syntax:binary_field(erl_syntax:underscore(),
                erl_syntax:integer(Pad * 8), [])], OO}
    end,
    case {Fields, AlignFields} of
        {[], []} ->
            S0;
        _ ->
            S1 = open_dblk(S0),
            ECtx = build_ectx(S1),
            {DV0, DV1, OV0, OV1, S2} = inc_odvar(S1),
            OutVars0 = lists:foldl(fun (Field, Acc) ->
                Body = erl_syntax:binary_field_body(Field),
                case erl_syntax:type(Body) of
                    size_qualifier ->
                        Var = erl_syntax:size_qualifier_body(Body),
                        [Var | Acc];
                    variable ->
                        [Body | Acc]
                end
            end, [], Fields),
            OutVars1 = [DV1 | OutVars0],
            Clause0 = erl_syntax:clause([erl_syntax:binary(
                    AlignFields ++
                    Fields ++
                    [erl_syntax:binary_field(DV1, [erl_syntax:atom(binary)])]
                )], [], [erl_syntax:atom(ok)]),
            Clause1 = erl_syntax:clause([erl_syntax:underscore()], [],
                lists:map(fun (Var) ->
                    erl_syntax:match_expr(Var, erl_syntax:atom(error))
                end, OutVars1) ++ [
                    erl_syntax:application(
                        erl_syntax:atom('error'),
                        [ECtx])
                ]),
            F0 = erl_syntax:case_expr(DV0, [Clause0, Clause1]),
            OffsetIncExpr = if
                is_integer(OffsetInc1) -> erl_syntax:integer(OffsetInc1);
                true -> OffsetInc1
            end,
            F1 = erl_syntax:match_expr(OV1, erl_syntax:infix_expr(
                OV0, erl_syntax:operator('+'), OffsetIncExpr)),
            S3 = if
                is_integer(OffsetInc1) ->
                    S2#fstate{offset = Offset0 + OffsetInc1};
                true ->
                    S2#fstate{offset = 0, obase = unknown}
            end,
            add_dblk_forms([F0, F1], S3)
    end.

-spec add_forms([form()], fstate()) -> fstate().
add_forms(Fs, S0 = #fstate{forms = F0}) ->
    S0#fstate{forms = F0 ++ Fs}.

-spec add_forms([form()], integer(), fstate()) -> fstate().
add_forms(Fs, OffsetIncr, S0 = #fstate{forms = F0, offset = O0}) ->
    S0#fstate{forms = F0 ++ Fs, offset = O0 + OffsetIncr}.

-spec add_dblk_forms([form()], fstate()) -> fstate().
add_dblk_forms(Fs, S0 = #fstate{in_dblk = true}) ->
    add_forms(Fs, S0).

-spec add_hoist(int_type(), expr(), fstate()) -> fstate().
add_hoist(Type, Expr, S0 = #fstate{hoists = H0}) ->
    S0#fstate{hoists = H0 ++ [{Type, Expr}]}.

-spec int_encode(bits(), signed | unsigned, var(), fstate()) -> fstate().
int_encode(Bits, Signed, SrcVar, S0 = #fstate{opts = Opts}) ->
    Endian = maps:get(endian, Opts, big),
    Field = erl_syntax:binary_field(SrcVar, erl_syntax:integer(Bits),
        [erl_syntax:atom(Endian), erl_syntax:atom(Signed)]),
    add_encode_step(Bits div 8, [Field], Bits div 8, S0).

-spec int_decode(bits(), unsigned | signed, var(), fstate()) -> fstate().
int_decode(Bits, Signed, OutVar, S0 = #fstate{opts = Opts}) ->
    Endian = maps:get(endian, Opts, big),
    Field = erl_syntax:binary_field(OutVar, erl_syntax:integer(Bits),
        [erl_syntax:atom(Endian), erl_syntax:atom(Signed)]),
    add_decode_step(Bits div 8, [Field], Bits div 8, S0).

str_len_expr(Var) ->
    erl_syntax:if_expr([
        erl_syntax:clause([],
            [erl_syntax:application(erl_syntax:atom(is_binary), [Var])],
            [erl_syntax:application(erl_syntax:atom(byte_size), [Var])]),
        erl_syntax:clause([],
            [erl_syntax:application(erl_syntax:atom(is_list), [Var])],
            [erl_syntax:application(erl_syntax:atom(length), [Var])])
        ]).

-spec encode_data(rpce_type(), var(), fstate()) -> fstate().
encode_data({custom, Base, _Enc, _Dec}, SrcVar, S0 = #fstate{customs = C}) ->
    #{SrcVar := TVar} = C,
    encode_data(Base, TVar, S0);

encode_data({le, Base}, SrcVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => little},
    S1 = encode_data(Base, SrcVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};
encode_data({be, Base}, SrcVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => big},
    S1 = encode_data(Base, SrcVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};

encode_data({length_of, Field, Base}, SrcVar, S0 = #fstate{customs = C}) ->
    #fstate{ctx = Ctx0, unpacks = U} = S0,
    [{field, _ThisField} | Ctx1] = Ctx0,
    #{Ctx1 := FieldMap} = U,
    #{Field := {_Type, RealVar0}} = FieldMap,
    RealVar1 = case C of
        #{RealVar0 := Alias} -> Alias;
        _ -> RealVar0
    end,
    {TVar, S1} = inc_tvar(S0),
    F0 = erl_syntax:match_expr(erl_syntax:underscore(), SrcVar),
    F1 = erl_syntax:match_expr(TVar,
        erl_syntax:case_expr(RealVar1,
            [erl_syntax:clause([erl_syntax:atom(undefined)], [],
                [erl_syntax:integer(0)]),
             erl_syntax:clause([erl_syntax:underscore()], [],
                [erl_syntax:application(erl_syntax:atom(length), [RealVar1])])])),
    S2 = add_forms([F0, F1], S1),
    encode_data(Base, TVar, S2);
encode_data({size_of, Field, Base}, SrcVar, S0 = #fstate{customs = C}) ->
    #fstate{ctx = Ctx0, unpacks = U} = S0,
    [{field, _ThisField} | Ctx1] = Ctx0,
    #{Ctx1 := FieldMap} = U,
    #{Field := {RealType, RealVar0}} = FieldMap,
    RealVar1 = case C of
        #{RealVar0 := Alias} -> Alias;
        _ -> RealVar0
    end,
    RefType0 = case RealType of
        {custom, {pointer, T}, _E, _D} -> T;
        {pointer, T} -> T;
        _ -> error({not_implemented, {size_of, RealType}})
    end,
    RefType1 = case RefType0 of
        {custom, Base2, _Enc, _Dec} -> Base2;
        {le, Base2} -> Base2;
        {be, Base2} -> Base2;
        Other -> Other
    end,
    {Fun, Subtract} = case RefType1 of
        {struct, RefStruct, _} ->
            {enc_fun(build_struct_path(push_struct(RefStruct, S0))), 0};
        {array, _T} ->
            {enc_fun(build_struct_path(S0) ++ ['_F', Field]), 8};
        {varying_array, _T} ->
            {enc_fun(build_struct_path(S0) ++ ['_F', Field]), 4};
        {conformant_array, _T} ->
            {enc_fun(build_struct_path(S0) ++ ['_F', Field]), 4};
        T2 when (T2 =:= binary) or (T2 =:= string) or (T2 =:= unicode) ->
            {enc_fun(build_struct_path(S0) ++ ['_F', Field]), 12};
        T2 when (T2 =:= varying_binary) or (T2 =:= varying_string) ->
            {enc_fun(build_struct_path(S0) ++ ['_F', Field]), 4};
        _ ->
            {enc_fun(build_struct_path(S0) ++ ['_F', Field]), 0}
    end,
    {TVar, S1} = inc_tvar(S0),
    Expr0 = erl_syntax:application(
        erl_syntax:atom(msrpce_runtime), erl_syntax:atom(size_of),
        [Fun, RealVar1]),
    Expr1 = case Subtract of
        0 -> Expr0;
        _ ->
            erl_syntax:application(erl_syntax:atom(lists),
                erl_syntax:atom(max), [erl_syntax:list([
                    erl_syntax:integer(0),
                    erl_syntax:infix_expr(Expr0, erl_syntax:operator('-'),
                        erl_syntax:integer(Subtract))])])
    end,
    F0 = erl_syntax:match_expr(erl_syntax:underscore(), SrcVar),
    F1 = erl_syntax:match_expr(TVar, Expr1),
    S2 = add_forms([F0, F1], S1),
    encode_data(Base, TVar, S2);

encode_data(T, SrcVar, S0 = #fstate{opts = Opts}) when (T =:= string) or
                                                       (T =:= binary) ->
    Endian = maps:get(endian, Opts, big),
    {TVar, S1} = inc_tvar(S0),
    F0 = erl_syntax:match_expr(TVar, str_len_expr(SrcVar)),
    S2 = add_forms([F0], S1),
    % max count = length(Input)
    F1 = erl_syntax:binary_field(TVar,
        erl_syntax:integer(32), [erl_syntax:atom(Endian)]),
    % offset = 0
    F2 = erl_syntax:binary_field(erl_syntax:integer(0),
        erl_syntax:integer(32), []),
    % actual count = length(Input)
    F3 = erl_syntax:binary_field(TVar,
        erl_syntax:integer(32), [erl_syntax:atom(Endian)]),
    % string data
    DataExpr = erl_syntax:application(erl_syntax:atom(iolist_to_binary),
        [erl_syntax:list([SrcVar])]),
    F4 = erl_syntax:binary_field(DataExpr, [erl_syntax:atom(binary)]),
    OffsetInc = erl_syntax:infix_expr(erl_syntax:integer(4 + 4 + 4),
        erl_syntax:operator('+'), TVar),
    add_encode_step(4, [F1, F2, F3, F4], OffsetInc, S2);

encode_data(T, SrcVar, S0 = #fstate{opts = Opts}) when (T =:= varying_string) or
                                                       (T =:= varying_binary) ->
    Endian = maps:get(endian, Opts, big),
    {TVar, S1} = inc_tvar(S0),
    F0 = erl_syntax:match_expr(TVar, str_len_expr(SrcVar)),
    S2 = add_forms([F0], S1),
    % actual count = length(Input)
    F1 = erl_syntax:binary_field(TVar,
        erl_syntax:integer(32), [erl_syntax:atom(Endian)]),
    % string data
    DataExpr = erl_syntax:application(erl_syntax:atom(iolist_to_binary),
        [erl_syntax:list([SrcVar])]),
    F2 = erl_syntax:binary_field(DataExpr, [erl_syntax:atom(binary)]),
    OffsetInc = erl_syntax:infix_expr(erl_syntax:integer(4),
        erl_syntax:operator('+'), TVar),
    add_encode_step(4, [F1, F2], OffsetInc, S2);

encode_data(unicode, SrcVar, S0 = #fstate{opts = Opts}) ->
    Endian = maps:get(endian, Opts, big),
    {TVar, S1} = inc_tvar(S0),
    F0 = erl_syntax:match_expr(TVar,
        erl_syntax:application(
            erl_syntax:atom(unicode), erl_syntax:atom(characters_to_binary),
            [SrcVar, erl_syntax:atom(utf8),
             erl_syntax:tuple(
                [erl_syntax:atom(utf16), erl_syntax:atom(little)])
            ])),
    % max count = string:length(Input)
    LenExpr = erl_syntax:application(
            erl_syntax:atom(string), erl_syntax:atom(length), [SrcVar]),
    F1 = erl_syntax:binary_field(LenExpr,
        erl_syntax:integer(32), [erl_syntax:atom(Endian)]),
    % offset = 0
    F2 = erl_syntax:binary_field(erl_syntax:integer(0),
        erl_syntax:integer(32), []),
    % actual count = string:length(Input)
    F3 = erl_syntax:binary_field(erl_syntax:application(
            erl_syntax:atom(string), erl_syntax:atom(length),
            [SrcVar]),
        erl_syntax:integer(32), [erl_syntax:atom(Endian)]),
    % string data
    F4 = erl_syntax:binary_field(TVar, [erl_syntax:atom(binary)]),
    % terminator
    F5 = erl_syntax:binary_field(erl_syntax:integer(0),
        erl_syntax:integer(16), []),
    OffsetInc = erl_syntax:infix_expr(erl_syntax:integer(4 + 4 + 4),
        erl_syntax:operator('+'),
        erl_syntax:infix_expr(LenExpr, erl_syntax:operator('*'),
            erl_syntax:integer(2))),
    S2 = add_forms([F0], S1),
    add_encode_step(4, [F1, F2, F3, F4, F5], OffsetInc, S2);

encode_data({fixed_binary, N, A}, SrcVar, S0 = #fstate{}) ->
    LenExpr = erl_syntax:application(erl_syntax:atom(byte_size), [SrcVar]),
    AssertForm = erl_syntax:case_expr(LenExpr, [
        erl_syntax:clause([erl_syntax:integer(N)], [],
            [erl_syntax:atom(ok)]),
        erl_syntax:clause([erl_syntax:underscore()], [],
            [erl_syntax:application(
                erl_syntax:atom(error),
                [erl_syntax:tuple([
                    erl_syntax:atom(bad_fixed_binary_size),
                    erl_syntax:integer(N),
                    LenExpr,
                    build_enc_ectx(S0)])])])
        ]),
    S1 = add_forms([AssertForm], S0),
    Field = erl_syntax:binary_field(SrcVar, [erl_syntax:atom(binary)]),
    add_encode_step(A, [Field], N, S1);

encode_data({fixed_binary, N}, SrcVar, S0 = #fstate{}) ->
    encode_data({fixed_binary, N, 1}, SrcVar, S0);

encode_data({fixed_array, Count, Type}, SrcVar, S0 = #fstate{}) ->
    LenExpr = erl_syntax:application(erl_syntax:atom(length), [SrcVar]),
    AssertForm = erl_syntax:case_expr(LenExpr, [
        erl_syntax:clause([erl_syntax:integer(Count)], [],
            [erl_syntax:atom(ok)]),
        erl_syntax:clause([erl_syntax:underscore()], [],
            [erl_syntax:application(
                erl_syntax:atom(error),
                [erl_syntax:tuple([
                    erl_syntax:atom(bad_fixed_array_length),
                    erl_syntax:integer(Count),
                    LenExpr,
                    build_enc_ectx(S0)])])])
        ]),
    S1 = add_forms([AssertForm], S0),
    encode_data({conformant_array, Type}, SrcVar, S1);

encode_data({conformant_array, _Type}, SrcVar, S0 = #fstate{}) ->
    % max has already been hoisted out
    #fstate{ctx = [{field, FName} | _]} = S0,
    EncodeFun = enc_fun(build_struct_path(S0) ++ ['_A', FName]),
    {SV0, SV1, S1} = inc_svar(S0),
    Form = erl_syntax:match_expr(SV1,
        erl_syntax:application(
            erl_syntax:atom(lists), erl_syntax:atom(foldl),
            [EncodeFun, SV0, SrcVar])),
    S2 = add_forms([Form], S1),
    S2#fstate{obase = unknown};

encode_data({varying_array, Type}, SrcVar, S0 = #fstate{opts = Opts}) ->
    % max has already been hoisted out
    Endian = maps:get(endian, Opts, big),
    LenExpr = erl_syntax:application(erl_syntax:atom(length), [SrcVar]),
    % actual count
    F0 = erl_syntax:binary_field(LenExpr, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    S1 = add_encode_step(4, [F0], 4, S0),
    encode_data({conformant_array, Type}, SrcVar, S1);

encode_data({array, Type}, SrcVar, S0 = #fstate{opts = Opts}) ->
    % max has already been hoisted out
    Endian = maps:get(endian, Opts, big),
    LenExpr = erl_syntax:application(erl_syntax:atom(length), [SrcVar]),
    % offset = 0
    F0 = erl_syntax:binary_field(erl_syntax:integer(0),
        erl_syntax:integer(32), []),
    % actual count
    F1 = erl_syntax:binary_field(LenExpr, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    S1 = add_encode_step(4, [F0, F1], 8, S0),
    encode_data({conformant_array, Type}, SrcVar, S1);

encode_data(boolean, SrcVar, S0 = #fstate{}) ->
    IntValExpr = erl_syntax:case_expr(SrcVar,
        [erl_syntax:clause([erl_syntax:atom(true)], [], [erl_syntax:integer(1)]),
         erl_syntax:clause([erl_syntax:atom(false)], [], [erl_syntax:integer(0)])]),
    int_encode(8, unsigned, IntValExpr, S0);

encode_data({bitset, Base, BitMap}, SrcVar, S0 = #fstate{}) when is_atom(Base) ->
    {TVar, S1} = inc_tvar(S0),
    ValExpr = maps:fold(fun (Name, BitNum, Acc) ->
        Mask = case BitNum of
            {mask, M} -> M;
            _ -> 1 bsl BitNum
        end,
        Expr = erl_syntax:case_expr(SrcVar,
            [erl_syntax:clause(
                [erl_syntax:map_expr([
                    erl_syntax:map_field_exact(
                        erl_syntax:atom(Name),
                        erl_syntax:atom(true))])],
                [],
                [erl_syntax:integer(Mask)]),
             erl_syntax:clause(
                [erl_syntax:underscore()], [],
                [erl_syntax:integer(0)])]),
        erl_syntax:infix_expr(Expr, erl_syntax:operator('bor'), Acc)
    end, erl_syntax:integer(0), BitMap),
    Form = erl_syntax:match_expr(TVar, ValExpr),
    S2 = add_forms([Form], S1),
    encode_data(Base, TVar, S2);

encode_data(PrimType, SrcVar, S0 = #fstate{}) when is_atom(PrimType) ->
    case PrimType of
        uint8   -> int_encode(8 , unsigned, SrcVar, S0);
        uint16  -> int_encode(16, unsigned, SrcVar, S0);
        uint32  -> int_encode(32, unsigned, SrcVar, S0);
        uint64  -> int_encode(64, unsigned, SrcVar, S0);
        int8    -> int_encode(8 , signed,   SrcVar, S0);
        int16   -> int_encode(16, signed,   SrcVar, S0);
        int32   -> int_encode(32, signed,   SrcVar, S0);
        int64   -> int_encode(64, signed,   SrcVar, S0)
    end;

encode_data(Type = {pointer, RefType}, SrcVar, S0 = #fstate{}) ->
    S1 = add_encode_step(type_align(Type), [], 0, S0),
    {SV0, SV1, S2} = inc_svar(S1),
    {Fun, ErrName} = case RefType of
        {struct, RefStruct, _} ->
            {enc_fun(build_struct_path(push_struct(RefStruct, S2))),
             RefStruct};
        _Other ->
            #fstate{ctx = [{field, FName} | _]} = S0,
            {enc_fun(build_struct_path(S2) ++ ['_F', FName]),
             FName}
    end,
    Form = erl_syntax:match_expr(
        SV1,
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(write_ptr),
            [erl_syntax:atom(ErrName),
             erl_syntax:integer(type_align(RefType)),
             Fun, SrcVar, SV0])),
    add_forms([Form], 4, S2);

encode_data(Type = {struct, Record, Fields}, _SrcVar, S0 = #fstate{}) ->
    Align = type_align(Type),
    S1 = push_struct(Record, S0),
    #fstate{ctx = Ctx, unpacks = U} = S1,
    #{Ctx := FieldMap} = U,
    S2 = add_encode_step(Align, [], 0, S1),
    S3 = lists:foldl(fun ({FName, FType}, SS0) ->
        #{FName := {_, FVar}} = FieldMap,
        SS1 = push_field(FName, SS0),
        SS2 = encode_data(FType, FVar, SS1),
        pop_field(SS2)
    end, S2, Fields),
    pop_struct(S3).

-spec encode_unpacks(rpce_type(), var(), fstate()) -> fstate().
encode_unpacks(PrimType, _SrcVar, S0 = #fstate{}) when is_atom(PrimType) ->
    S0;
encode_unpacks({custom, Base, Enc, _Dec}, SrcVar, S0 = #fstate{customs = C0}) ->
    {TVar, S1} = inc_tvar(S0),
    C1 = C0#{SrcVar => TVar},
    S2 = S1#fstate{customs = C1},
    Form = erl_syntax:match_expr(TVar,
        erl_syntax:application(Enc, [SrcVar])),
    S3 = add_forms([Form], S2),
    encode_unpacks(Base, TVar, S3);
encode_unpacks({size_of, _Field, _Base}, _SrcVar, S0 = #fstate{}) ->
    S0;
encode_unpacks({length_of, _Field, _Base}, _SrcVar, S0 = #fstate{}) ->
    S0;
encode_unpacks({le, Base}, SrcVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => little},
    S1 = encode_unpacks(Base, SrcVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};
encode_unpacks({be, Base}, SrcVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => big},
    S1 = encode_unpacks(Base, SrcVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};
encode_unpacks({bitset, Base, _Map}, _SrcVar, S0 = #fstate{}) when is_atom(Base) ->
    S0;
encode_unpacks({conformant_array, _Type}, SrcVar, S0 = #fstate{}) ->
    MaxLenExpr = erl_syntax:application(erl_syntax:atom(length),
        [SrcVar]),
    add_hoist(uint32, MaxLenExpr, S0);
encode_unpacks({array, _Type}, SrcVar, S0 = #fstate{}) ->
    MaxLenExpr = erl_syntax:application(erl_syntax:atom(length),
        [SrcVar]),
    add_hoist(uint32, MaxLenExpr, S0);
encode_unpacks({fixed_array, _N, _Type}, _SrcVar, S0 = #fstate{}) ->
    S0;
encode_unpacks({varying_array, _Type}, _SrcVar, S0 = #fstate{}) ->
    S0;
encode_unpacks({pointer, _RefType}, _SrcVar, S0 = #fstate{}) ->
    S0;
encode_unpacks({fixed_binary, _N}, _SrcVar, S0 = #fstate{}) ->
    S0;
encode_unpacks({fixed_binary, _N, _A}, _SrcVar, S0 = #fstate{}) ->
    S0;
encode_unpacks({fixed_string, _N}, _SrcVar, S0 = #fstate{}) ->
    S0;
encode_unpacks({struct, Record, Fields}, SrcVar, S0 = #fstate{}) ->
    S1 = push_struct(Record, S0),
    {FieldMap, S2} = lists:foldl(fun ({FName, FType}, {M0, SS0}) ->
        {FVar, SS1} = inc_tvar(SS0),
        M1 = M0#{FName => {FType, FVar}},
        {M1, SS1}
    end, {#{}, S1}, Fields),
    FieldExprs = maps:fold(fun (FName, {_, FVar}, Acc) ->
        [erl_syntax:record_field(
            erl_syntax:atom(FName),
            FVar) | Acc]
    end, [], FieldMap),
    UnpackForm = erl_syntax:match_expr(
        erl_syntax:record_expr(erl_syntax:atom(Record), FieldExprs),
        SrcVar),
    S3 = add_forms([UnpackForm], S2),
    #fstate{ctx = Ctx, unpacks = U0} = S3,
    U1 = U0#{Ctx => FieldMap},
    S4 = S3#fstate{unpacks = U1},
    S5 = lists:foldl(fun ({FName, FType}, SS0) ->
        #{FName := {_, FVar}} = FieldMap,
        SS1 = push_field(FName, SS0),
        SS2 = encode_unpacks(FType, FVar, SS1),
        pop_field(SS2)
    end, S4, Fields),
    pop_struct(S5).

-spec encode_hoists(rpce_type(), var(), fstate()) -> fstate().
encode_hoists(_TopType, _SrcVar, S0 = #fstate{hoists = []}) ->
    S0;
encode_hoists(TopType, SrcVar, S0 = #fstate{hoists = [{Type, Expr} | Rest]}) ->
    S1 = encode_data(Type, Expr, S0),
    encode_hoists(TopType, SrcVar, S1#fstate{hoists = Rest}).

-spec set_anno(loc(), atom(), form()) -> form().
set_anno(Loc0, Func, Forms) ->
    Loc1 = erl_anno:set_generated(true, Loc0),
    File0 = erl_anno:file(Loc1),
    File1 = File0 ++ "(" ++ atom_to_list(Func) ++ ")",
    Loc2 = erl_anno:set_file(File1, Loc1),
    erl_parse:map_anno(fun (_) -> Loc2 end, Forms).

-spec encode_struct_func_form(type_path(), rpce_type(), loc(), options()) -> [form()].
encode_struct_func_form(Path0, Type, Loc, Opts) ->
    Ctx0 = lists:foldl(fun (Struct, Acc) ->
        [{struct, Struct} | Acc]
    end, [], Path0),
    {FunName, Ctx1} = case (catch lists:last(Path0)) of
        {'EXIT', _} ->
            {struct, RecName, _} = Type,
            {enc_fun_name(Path0 ++ [RecName]), Ctx0};
        {scalar_field, FieldName} ->
            [_ | C] = Ctx0,
            P = lists:droplast(Path0),
            {enc_fun_name(P ++ ['_F', FieldName]), [{field, FieldName} | C]};
        {array_field, FieldName} ->
            [_ | C] = Ctx0,
            P = lists:droplast(Path0),
            {enc_fun_name(P ++ ['_A', FieldName]), [{field, FieldName} | C]};
        _ ->
            {struct, RecName, _} = Type,
            {enc_fun_name(Path0 ++ [RecName]), Ctx0}
    end,
    S0 = #fstate{ctx = Ctx1, opts = Opts},
    InVar = erl_syntax:variable('Input'),
    SV0 = svar(0),
    S1 = encode_unpacks(Type, InVar, S0),
    S2 = encode_hoists(Type, InVar, S1),
    S3 = encode_data(Type, InVar, S2),
    S4 = close_dblk(S3),
    #fstate{forms = Forms, sn = SN1} = S4,
    SV1 = svar(SN1),
    F0 = erl_syntax:function(
        FunName,
        [erl_syntax:clause([InVar, SV0], [], Forms ++ [SV1])]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(FunName), F1).

encode_func_form({struct, RecName, _}, Loc, Opts) ->
    InVar = erl_syntax:variable('Input'),
    Aliasing = maps:get(pointer_aliasing, Opts, false),
    FF0 = erl_syntax:match_expr(svar(0),
        erl_syntax:record_expr(erl_syntax:atom(msrpce_state),
            [erl_syntax:record_field(erl_syntax:atom(mode),
                erl_syntax:atom(encode)),
             erl_syntax:record_field(erl_syntax:atom(aliasing),
                erl_syntax:atom(Aliasing)),
             erl_syntax:record_field(erl_syntax:atom(data),
                erl_syntax:binary([]))])),
    FF1 = erl_syntax:match_expr(svar(1),
        erl_syntax:application(enc_fun_name([RecName]),
            [InVar, svar(0)])),
    FF2 = erl_syntax:match_expr(svar(2), erl_syntax:application(
        erl_syntax:atom(msrpce_runtime), erl_syntax:atom(finish),
        [svar(1)])),
    FF3 = erl_syntax:match_expr(
        erl_syntax:record_expr(erl_syntax:atom(msrpce_state),
            [erl_syntax:record_field(erl_syntax:atom(data), dvar(0))]),
        svar(2)),
    FF4 = dvar(0),
    F0 = erl_syntax:function(
        concat_atoms([encode, RecName]),
        [erl_syntax:clause([InVar], [], [FF0, FF1, FF2, FF3, FF4])]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(concat_atoms([encode, RecName])), F1).

-spec decode_data(rpce_type(), var(), fstate()) -> fstate().
decode_data({custom, Base, _Enc, _Dec}, DstVar, S0 = #fstate{}) ->
    decode_data(Base, DstVar, S0);

decode_data({le, Base}, DstVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => little},
    S1 = decode_data(Base, DstVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};
decode_data({be, Base}, DstVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => big},
    S1 = decode_data(Base, DstVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};

decode_data({length_of, _Field, Base}, DstVar, S0 = #fstate{}) ->
    decode_data(Base, DstVar, S0);
decode_data({size_of, _Field, Base}, DstVar, S0 = #fstate{}) ->
    decode_data(Base, DstVar, S0);

decode_data(T, DstVar, S0 = #fstate{opts = Opts}) when (T =:= string) or
                                                       (T =:= binary) ->
    Endian = maps:get(endian, Opts, big),
    {MaxLenVar, S1} = inc_tvar(S0),
    {RestVar, S2} = inc_tvar(S1),
    {OffsetVar, S3} = inc_tvar(S2),
    {LenVar, S4} = inc_tvar(S3),
    {UnsplitVar, S5} = inc_tvar(S4),
    {SplitVar, S6} = inc_tvar(S5),
    Field0 = erl_syntax:binary_field(MaxLenVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    Field1 = erl_syntax:binary_field(OffsetVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    Field2 = erl_syntax:binary_field(LenVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    Field3 = erl_syntax:binary_field(RestVar, MaxLenVar,
        [erl_syntax:atom(binary)]),
    OffsetInc = erl_syntax:infix_expr(erl_syntax:integer(4 + 4 + 4),
        erl_syntax:operator('+'), MaxLenVar),
    S7 = add_decode_step(4, [Field0, Field1, Field2, Field3], OffsetInc, S6),
    Form0 = erl_syntax:match_expr(
        erl_syntax:binary([
            erl_syntax:binary_field(erl_syntax:underscore(),
                erl_syntax:infix_expr(OffsetVar, erl_syntax:operator('*'),
                    erl_syntax:integer(8)), []),
            erl_syntax:binary_field(UnsplitVar, [erl_syntax:atom(binary)])
            ]),
        RestVar),
    InnerField0 = erl_syntax:binary_field(SplitVar, LenVar,
        [erl_syntax:atom(binary)]),
    InnerField1 = erl_syntax:binary_field(erl_syntax:underscore(),
        [erl_syntax:atom(binary)]),
    Form1 = erl_syntax:match_expr(
        erl_syntax:binary([InnerField0, InnerField1]),
        UnsplitVar),
    Form2 = case T of
        string ->
            erl_syntax:match_expr(DstVar,
                erl_syntax:application(erl_syntax:atom(unicode),
                    erl_syntax:atom(characters_to_list),
                    [SplitVar, erl_syntax:atom(utf8)]));
        binary ->
            erl_syntax:match_expr(DstVar, SplitVar)
    end,
    add_forms([Form0, Form1, Form2], S7);

decode_data(unicode, DstVar, S0 = #fstate{opts = Opts}) ->
    Endian = maps:get(endian, Opts, big),
    {MaxLenVar, S1} = inc_tvar(S0),
    {RestVar, S2} = inc_tvar(S1),
    {OffsetVar, S3} = inc_tvar(S2),
    {LenVar, S4} = inc_tvar(S3),
    {UnsplitVar, S5} = inc_tvar(S4),
    {SplitVar, S6} = inc_tvar(S5),
    Field0 = erl_syntax:binary_field(MaxLenVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    Field1 = erl_syntax:binary_field(OffsetVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    Field2 = erl_syntax:binary_field(LenVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    Field3 = erl_syntax:binary_field(RestVar, erl_syntax:infix_expr(
        MaxLenVar, erl_syntax:operator('*'), erl_syntax:integer(2)),
        [erl_syntax:atom(binary)]),
    OffsetInc = erl_syntax:infix_expr(erl_syntax:integer(4 + 4 + 4),
        erl_syntax:operator('+'), erl_syntax:infix_expr(
            MaxLenVar, erl_syntax:operator('*'), erl_syntax:integer(2))),
    S7 = add_decode_step(4, [Field0, Field1, Field2, Field3], OffsetInc, S6),
    Form0 = erl_syntax:match_expr(
        erl_syntax:binary([
            erl_syntax:binary_field(erl_syntax:underscore(),
                erl_syntax:infix_expr(OffsetVar, erl_syntax:operator('*'),
                    erl_syntax:integer(16)), []),
            erl_syntax:binary_field(UnsplitVar, [erl_syntax:atom(binary)])
            ]),
        RestVar),
    Form1 = erl_syntax:match_expr(
        erl_syntax:binary([
            erl_syntax:binary_field(SplitVar, erl_syntax:infix_expr(
                LenVar, erl_syntax:operator('*'), erl_syntax:integer(2)),
                [erl_syntax:atom(binary)]),
            erl_syntax:binary_field(erl_syntax:underscore(),
                [erl_syntax:atom(binary)])
            ]),
        UnsplitVar),
    Form2 = erl_syntax:match_expr(
        DstVar,
        erl_syntax:application(
            erl_syntax:atom(unicode), erl_syntax:atom(characters_to_list),
            [SplitVar, erl_syntax:tuple([
                erl_syntax:atom(utf16), erl_syntax:atom(little)])])),
    add_forms([Form0, Form1, Form2], S7);

decode_data(T, DstVar, S0 = #fstate{opts = Opts}) when (T =:= varying_string) or
                                                       (T =:= varying_binary) ->
    Endian = maps:get(endian, Opts, big),
    {LenVar, S1} = inc_tvar(S0),
    {SplitVar, S2} = inc_tvar(S1),
    F0 = erl_syntax:binary_field(LenVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    F1 = erl_syntax:binary_field(SplitVar, LenVar, [erl_syntax:atom(binary)]),
    OffsetInc = erl_syntax:infix_expr(erl_syntax:integer(4),
        erl_syntax:operator('+'), LenVar),
    S3 = add_decode_step(4, [F0, F1], OffsetInc, S2),
    Form = case T of
        varying_string ->
            erl_syntax:match_expr(DstVar,
                erl_syntax:application(erl_syntax:atom(unicode),
                    erl_syntax:atom(characters_to_list),
                    [SplitVar, erl_syntax:atom(utf8)]));
        varying_binary ->
            erl_syntax:match_expr(DstVar, SplitVar)
    end,
    add_forms([Form], S3);

decode_data(boolean, DstVar, S0 = #fstate{}) ->
    {TVar, S1} = inc_tvar(S0),
    S2 = int_decode(8, unsigned, TVar, S1),
    Form = erl_syntax:match_expr(DstVar,
        erl_syntax:case_expr(TVar, [
            erl_syntax:clause([erl_syntax:integer(0)], [],
                [erl_syntax:atom(false)]),
            erl_syntax:clause([erl_syntax:underscore()], [],
                [erl_syntax:atom(true)])])),
    add_forms([Form], S2);

decode_data({bitset, Base, BitMap}, DstVar, S0 = #fstate{}) when is_atom(Base) ->
    {TVar, S1} = inc_tvar(S0),
    S2 = decode_data(Base, TVar, S1),
    Fields = maps:fold(fun (Name, BitNum, Acc) ->
        Mask = case BitNum of
            {mask, M} -> M;
            _ -> 1 bsl BitNum
        end,
        Expr = erl_syntax:case_expr(
            erl_syntax:infix_expr(TVar, erl_syntax:operator('band'),
                erl_syntax:integer(Mask)),
            [erl_syntax:clause([erl_syntax:integer(0)], [],
                [erl_syntax:atom(false)]),
             erl_syntax:clause([erl_syntax:underscore()], [],
                [erl_syntax:atom(true)])]),
        [erl_syntax:map_field_assoc(erl_syntax:atom(Name), Expr) | Acc]
    end, [], BitMap),
    Form = erl_syntax:match_expr(DstVar, erl_syntax:map_expr(Fields)),
    add_forms([Form], S2);

decode_data(PrimType, DstVar, S0 = #fstate{}) when is_atom(PrimType) ->
    case PrimType of
        uint8   -> int_decode(8 , unsigned, DstVar, S0);
        uint16  -> int_decode(16, unsigned, DstVar, S0);
        uint32  -> int_decode(32, unsigned, DstVar, S0);
        uint64  -> int_decode(64, unsigned, DstVar, S0);
        int8    -> int_decode(8 , signed,   DstVar, S0);
        int16   -> int_decode(16, signed,   DstVar, S0);
        int32   -> int_decode(32, signed,   DstVar, S0);
        int64   -> int_decode(64, signed,   DstVar, S0)
    end;

decode_data({fixed_binary, Len}, DstVar, S0 = #fstate{}) ->
    Field = erl_syntax:binary_field(DstVar, erl_syntax:integer(Len),
        [erl_syntax:atom(binary)]),
    add_decode_step(1, [Field], Len, S0);

decode_data({fixed_binary, Len, Align}, DstVar, S0 = #fstate{}) ->
    Field = erl_syntax:binary_field(DstVar, erl_syntax:integer(Len),
        [erl_syntax:atom(binary)]),
    add_decode_step(Align, [Field], Len, S0);

decode_data({fixed_array, Count, _Type}, DstVar, S0 = #fstate{}) ->
    #fstate{ctx = Ctx} = S0,
    [{field, FName} | _] = Ctx,
    DecodeFun = dec_fun(build_struct_path(S0) ++ ['_A', FName]),
    {SV0, SV1, S1} = inc_svar(S0),
    Form0 = erl_syntax:match_expr(
        erl_syntax:tuple([DstVar, SV1]),
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(array_decode),
            [erl_syntax:integer(Count), DecodeFun, SV0])),
    S2 = add_forms([Form0], S1),
    S2#fstate{obase = unknown};

decode_data({conformant_array, _Type}, DstVar, S0 = #fstate{}) ->
    % max has already been hoisted out
    #fstate{ctx = Ctx, maxlens = ML} = S0,
    #{Ctx := MaxLenVar} = ML,
    [{field, FName} | _] = Ctx,
    DecodeFun = dec_fun(build_struct_path(S0) ++ ['_A', FName]),
    {SV0, SV1, S1} = inc_svar(S0),
    Form0 = erl_syntax:match_expr(
        erl_syntax:tuple([DstVar, SV1]),
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(array_decode),
            [MaxLenVar, DecodeFun, SV0])),
    S2 = add_forms([Form0], S1),
    S2#fstate{obase = unknown};

decode_data({varying_array, _Type}, DstVar, S0 = #fstate{opts = Opts}) ->
    Endian = maps:get(endian, Opts, big),
    #fstate{ctx = Ctx} = S0,
    [{field, FName} | _] = Ctx,
    DecodeFun = dec_fun(build_struct_path(S0) ++ ['_A', FName]),
    {LenVar, S1} = inc_tvar(S0),
    Field = erl_syntax:binary_field(LenVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    S2 = add_decode_step(4, [Field], 4, S1),
    {SV0, SV1, S3} = inc_svar(S2),
    Form0 = erl_syntax:match_expr(
        erl_syntax:tuple([DstVar, SV1]),
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(array_decode),
            [LenVar, DecodeFun, SV0])),
    S4 = add_forms([Form0], S3),
    S4#fstate{obase = unknown};

decode_data({array, _Type}, DstVar, S0 = #fstate{opts = Opts}) ->
    % max has already been hoisted out
    Endian = maps:get(endian, Opts, big),
    #fstate{ctx = Ctx, maxlens = ML} = S0,
    #{Ctx := MaxLenVar} = ML,
    [{field, FName} | _] = Ctx,
    DecodeFun = dec_fun(build_struct_path(S0) ++ ['_A', FName]),
    {OffsetVar, S1} = inc_tvar(S0),
    {LenVar, S2} = inc_tvar(S1),
    {UnsplitVar, S3} = inc_tvar(S2),

    % offset
    Field0 = erl_syntax:binary_field(OffsetVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    % actual count
    Field1 = erl_syntax:binary_field(LenVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    S4 = add_decode_step(4, [Field0, Field1], 8, S3),

    {SV0, SV1, S5} = inc_svar(S4),
    Form0 = erl_syntax:match_expr(
        erl_syntax:tuple([UnsplitVar, SV1]),
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(array_decode),
            [MaxLenVar, DecodeFun, SV0])),
    StartExpr = erl_syntax:infix_expr(OffsetVar, erl_syntax:operator('+'),
        erl_syntax:integer(1)),
    Form1 = erl_syntax:match_expr(DstVar,
        erl_syntax:application(
            erl_syntax:atom(lists), erl_syntax:atom(sublist),
            [UnsplitVar, StartExpr, LenVar])),
    S6 = add_forms([Form0, Form1], S5),
    S6#fstate{obase = unknown};

decode_data(Type = {pointer, RefType}, DstVar, S0 = #fstate{}) ->
    S1 = add_decode_step(type_align(Type), [], 0, S0),
    {SV0, SV1, S2} = inc_svar(S1),
    {Fun, ErrName} = case RefType of
        {struct, RefStruct, _} ->
            {dec_fun(build_struct_path(push_struct(RefStruct, S2))),
             RefStruct};
        _Other ->
            #fstate{ctx = [{field, FName} | _]} = S0,
            {dec_fun(build_struct_path(S2) ++ ['_F', FName]),
             FName}
    end,
    Form = erl_syntax:match_expr(
        erl_syntax:tuple([DstVar, SV1]),
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(read_ptr),
            [erl_syntax:atom(ErrName),
             erl_syntax:integer(type_align(RefType)),
             Fun, SV0])),
    add_forms([Form], 4, S2);

decode_data(Type = {struct, Record, Fields}, _DstVar, S0 = #fstate{}) ->
    Align = type_align(Type),
    S1 = push_struct(Record, S0),
    #fstate{ctx = Ctx, unpacks = U} = S1,
    #{Ctx := FieldMap} = U,
    S2 = add_decode_step(Align, [], 0, S1),
    S3 = lists:foldl(fun ({FName, FType}, SS0) ->
        #{FName := FVar} = FieldMap,
        SS1 = push_field(FName, SS0),
        SS2 = decode_data(FType, FVar, SS1),
        pop_field(SS2)
    end, S2, Fields),
    pop_struct(S3).

decode_packs({custom, Base, _E, _D}, DstVar, S0 = #fstate{}) ->
    decode_packs(Base, DstVar, S0);
decode_packs({le, Base}, DstVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => little},
    S1 = decode_packs(Base, DstVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};
decode_packs({be, Base}, DstVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => big},
    S1 = decode_packs(Base, DstVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};
decode_packs({struct, Record, Fields}, DstVar, S0 = #fstate{}) ->
    S1 = push_struct(Record, S0),
    #fstate{ctx = Ctx, unpacks = U} = S1,
    #{Ctx := FieldMap} = U,
    FieldExprs = maps:fold(fun (FName, FVar, Acc) ->
        [erl_syntax:record_field(
            erl_syntax:atom(FName),
            FVar) | Acc]
    end, [], FieldMap),
    PackForm = erl_syntax:match_expr(
        DstVar,
        erl_syntax:record_expr(erl_syntax:atom(Record), FieldExprs)),
    S2 = lists:foldl(fun ({FName, FType}, SS0) ->
        #{FName := FVar} = FieldMap,
        SS1 = push_field(FName, SS0),
        SS2 = decode_packs(FType, FVar, SS1),
        pop_field(SS2)
    end, S1, Fields),
    S3 = add_forms([PackForm], S2),
    pop_struct(S3);
decode_packs(_Type, _DstVar, S0 = #fstate{}) ->
    S0.

decode_hoists({custom, Base, _E, _D}, DstVar, S0 = #fstate{}) ->
    decode_hoists(Base, DstVar, S0);
decode_hoists({le, Base}, DstVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => little},
    S1 = decode_hoists(Base, DstVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};
decode_hoists({be, Base}, DstVar, S0 = #fstate{opts = Opts0}) ->
    Opts1 = Opts0#{endian => big},
    S1 = decode_hoists(Base, DstVar, S0#fstate{opts = Opts1}),
    S1#fstate{opts = Opts0};
decode_hoists({size_of, _Field, Base}, DstVar, S0 = #fstate{}) ->
    decode_hoists(Base, DstVar, S0);
decode_hoists({length_of, _Field, Base}, DstVar, S0 = #fstate{}) ->
    decode_hoists(Base, DstVar, S0);
decode_hoists({bitset, _Base, _Map}, _DstVar, S0 = #fstate{}) ->
    S0;
decode_hoists(PrimType, _DstVar, S0 = #fstate{}) when is_atom(PrimType) ->
    S0;
decode_hoists({fixed_string, _N}, _DstVar, S0 = #fstate{}) ->
    S0;
decode_hoists({fixed_binary, _N}, _DstVar, S0 = #fstate{}) ->
    S0;
decode_hoists({fixed_binary, _N, _A}, _DstVar, S0 = #fstate{}) ->
    S0;
decode_hoists({conformant_array, _Type}, _DstVar, S0 = #fstate{opts = Opts}) ->
    Endian = maps:get(endian, Opts, big),
    {TVar, S1} = inc_tvar(S0),
    Field = erl_syntax:binary_field(TVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    S2 = add_decode_step(4, [Field], 4, S1),
    #fstate{ctx = Ctx, maxlens = ML0} = S2,
    ML1 = ML0#{Ctx => TVar},
    S2#fstate{maxlens = ML1};
decode_hoists({array, _Type}, _DstVar, S0 = #fstate{opts = Opts}) ->
    Endian = maps:get(endian, Opts, big),
    {TVar, S1} = inc_tvar(S0),
    Field = erl_syntax:binary_field(TVar, erl_syntax:integer(32),
        [erl_syntax:atom(Endian)]),
    S2 = add_decode_step(4, [Field], 4, S1),
    #fstate{ctx = Ctx, maxlens = ML0} = S2,
    ML1 = ML0#{Ctx => TVar},
    S2#fstate{maxlens = ML1};
decode_hoists({fixed_array, _N, _Type}, _SrcVar, S0 = #fstate{}) ->
    S0;
decode_hoists({varying_array, _Type}, _SrcVar, S0 = #fstate{}) ->
    S0;
decode_hoists({pointer, _RefType}, _SrcVar, S0 = #fstate{}) ->
    S0;
decode_hoists({struct, Record, Fields}, _DstVar, S0 = #fstate{}) ->
    S1 = push_struct(Record, S0),
    {FieldMap, S2} = lists:foldl(fun ({FName, _FType}, {M0, SS0}) ->
        {FVar, SS1} = inc_tvar(SS0),
        M1 = M0#{FName => FVar},
        {M1, SS1}
    end, {#{}, S1}, Fields),
    #fstate{ctx = Ctx, unpacks = U0} = S1,
    U1 = U0#{Ctx => FieldMap},
    S3 = S2#fstate{unpacks = U1},
    S4 = lists:foldl(fun ({FName, FType}, SS0) ->
        #{FName := FVar} = FieldMap,
        SS1 = push_field(FName, SS0),
        SS2 = decode_hoists(FType, FVar, SS1),
        pop_field(SS2)
    end, S3, Fields),
    pop_struct(S4).

decode_finish({custom, Base, _E, Dec}, SrcVar, DstVar, S0 = #fstate{}) ->
    {TVar, S1} = inc_tvar(S0),
    S2 = decode_finish(Base, SrcVar, TVar, S1),
    Form = erl_syntax:match_expr(DstVar,
        erl_syntax:application(Dec, [TVar])),
    add_forms([Form], S2);

decode_finish({le, Base}, SrcVar, DstVar, S0 = #fstate{}) ->
    decode_finish(Base, SrcVar, DstVar, S0);
decode_finish({be, Base}, SrcVar, DstVar, S0 = #fstate{}) ->
    decode_finish(Base, SrcVar, DstVar, S0);

decode_finish({size_of, _Field, Base}, SrcVar, DstVar, S0 = #fstate{}) ->
    decode_finish(Base, SrcVar, DstVar, S0);
decode_finish({length_of, _Field, Base}, SrcVar, DstVar, S0 = #fstate{}) ->
    decode_finish(Base, SrcVar, DstVar, S0);

decode_finish({ArrType, _MembType}, SrcVar, DstVar, S0 = #fstate{})
        when (ArrType =:= array) or (ArrType =:= fixed_array) or
             (ArrType =:= conformant_array) or (ArrType =:= varying_array) ->
    #fstate{ctx = [{field, FName} | _]} = S0,
    Fun = finish_fun_name(build_struct_path(S0) ++ ['_A', FName]),
    {SV, _} = cur_svar(S0),
    {TVar, S1} = inc_tvar(S0),
    Form = erl_syntax:match_expr(DstVar,
        erl_syntax:list_comp(
            erl_syntax:application(Fun, [TVar, SV]),
            [erl_syntax:generator(TVar, SrcVar)])),
    add_forms([Form], S1);

decode_finish({pointer, RefType}, SrcVar, DstVar, S0 = #fstate{}) ->
    {TmpVar, S1} = inc_tvar(S0),
    {SV, _} = cur_svar(S1),
    {Fun, ErrName} = case RefType of
        {struct, RefStruct, _} ->
            {finish_fun_name(build_struct_path(push_struct(RefStruct, S1))),
             RefStruct};
        _Other ->
            #fstate{ctx = [{field, FName} | _]} = S0,
            {finish_fun_name(build_struct_path(S1) ++ ['_F', FName]),
             FName}
    end,
    GetValForm = erl_syntax:match_expr(TmpVar,
        erl_syntax:application(
            erl_syntax:atom(msrpce_runtime), erl_syntax:atom(get_ptr_val),
            [erl_syntax:atom(ErrName), SrcVar, SV])),
    ProcessForm = erl_syntax:match_expr(DstVar,
        erl_syntax:application(Fun,
            [TmpVar, SV])),
    add_forms([GetValForm, ProcessForm], S1);

decode_finish({struct, Record, Fields}, SrcVar, DstVar, S0 = #fstate{}) ->
    S1 = push_struct(Record, S0),
    {FieldMapRev, S2} = lists:foldl(fun ({FName, FType}, {M0, SS0}) ->
        {FSrcVar, SS1} = inc_tvar(SS0),
        {FDstVar, SS2} = inc_tvar(SS1),
        M1 = [{FName, FType, FSrcVar, FDstVar} | M0],
        {M1, SS2}
    end, {[], S1}, Fields),
    FieldMap = lists:reverse(FieldMapRev),
    FieldUnpackExprs = lists:map(fun ({FName, _FType, FSrcVar, _FDstVar}) ->
        erl_syntax:record_field(
            erl_syntax:atom(FName),
            FSrcVar)
    end, FieldMap),
    FieldPackExprs = lists:map(fun ({FName, _FType, _FSrcVar, FDstVar}) ->
        erl_syntax:record_field(
            erl_syntax:atom(FName),
            FDstVar)
    end, FieldMap),
    UnpackForm = erl_syntax:match_expr(
        erl_syntax:record_expr(erl_syntax:atom(Record), FieldUnpackExprs),
        SrcVar),
    PackForm = erl_syntax:match_expr(
        DstVar,
        erl_syntax:record_expr(erl_syntax:atom(Record), FieldPackExprs)),
    S3 = add_forms([UnpackForm], S2),
    S4 = lists:foldl(fun ({FName, FType, FSrcVar, FDstVar}, SS0) ->
        SS1 = push_field(FName, SS0),
        SS2 = decode_finish(FType, FSrcVar, FDstVar, SS1),
        pop_field(SS2)
    end, S3, FieldMap),
    S5 = add_forms([PackForm], S4),
    pop_struct(S5);

decode_finish(_Type, SrcVar, DstVar, S0 = #fstate{}) ->
    Form = erl_syntax:match_expr(DstVar, SrcVar),
    add_forms([Form], S0).

-spec decode_struct_func_form(type_path(), rpce_type(), loc(), options()) -> [form()].
decode_struct_func_form(Path0, Type, Loc, Opts) ->
    Ctx0 = lists:foldl(fun (Struct, Acc) ->
        [{struct, Struct} | Acc]
    end, [], Path0),
    {FunName, Ctx1} = case (catch lists:last(Path0)) of
        {'EXIT', _} ->
            {struct, RecName, _} = Type,
            {dec_fun_name(Path0 ++ [RecName]), Ctx0};
        {scalar_field, FieldName} ->
            [_ | C] = Ctx0,
            P = lists:droplast(Path0),
            {dec_fun_name(P ++ ['_F', FieldName]), [{field, FieldName} | C]};
        {array_field, FieldName} ->
            [_ | C] = Ctx0,
            P = lists:droplast(Path0),
            {dec_fun_name(P ++ ['_A', FieldName]), [{field, FieldName} | C]};
        _ ->
            {struct, RecName, _} = Type,
            {dec_fun_name(Path0 ++ [RecName]), Ctx0}
    end,
    S0 = #fstate{ctx = Ctx1, opts = Opts},
    OutVar = erl_syntax:variable('Output'),
    SV0 = svar(0),
    S1 = decode_hoists(Type, OutVar, S0),
    S2 = decode_data(Type, OutVar, S1),
    S3 = decode_packs(Type, OutVar, S2),
    S4 = close_dblk(S3),
    #fstate{forms = Forms, sn = SN1} = S4,
    SV1 = svar(SN1),
    F0 = erl_syntax:function(
        FunName,
        [erl_syntax:clause([SV0], [],
            Forms ++ [erl_syntax:tuple([OutVar, SV1])])]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(FunName), F1).

-spec decode_finish_func_form(type_path(), rpce_type(), loc(), options()) -> [form()].
decode_finish_func_form(Path0, Type, Loc, Opts) ->
    Ctx0 = lists:foldl(fun (Struct, Acc) ->
        [{struct, Struct} | Acc]
    end, [], Path0),
    {FunName, Ctx1} = case (catch lists:last(Path0)) of
        {'EXIT', _} ->
            {struct, RecName, _} = Type,
            {finish_fun_name(Path0 ++ [RecName]), Ctx0};
        {scalar_field, FieldName} ->
            [_ | C] = Ctx0,
            P = lists:droplast(Path0),
            {finish_fun_name(P ++ ['_F', FieldName]), [{field, FieldName} | C]};
        {array_field, FieldName} ->
            [_ | C] = Ctx0,
            P = lists:droplast(Path0),
            {finish_fun_name(P ++ ['_A', FieldName]), [{field, FieldName} | C]};
        _ ->
            {struct, RecName, _} = Type,
            {finish_fun_name(Path0 ++ [RecName]), Ctx0}
    end,
    S0 = #fstate{ctx = Ctx1, opts = Opts},
    InVar = erl_syntax:variable('Input'),
    OutVar = erl_syntax:variable('Output'),
    SV0 = svar(0),
    SillyForm = erl_syntax:match_expr(
        erl_syntax:record_expr(erl_syntax:atom(msrpce_state), []),
        SV0),
    S1 = decode_finish(Type, InVar, OutVar, S0),
    #fstate{forms = Forms} = S1,
    F0 = erl_syntax:function(
        FunName,
        [
            erl_syntax:clause([erl_syntax:atom(undefined),
                erl_syntax:underscore()], [],
                [erl_syntax:atom(undefined)]),
            erl_syntax:clause([InVar, SV0], [],
                [SillyForm] ++ Forms ++ [OutVar])
        ]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(FunName), F1).

decode_func_form({struct, RecName, _}, Loc, Opts) ->
    InVar = erl_syntax:variable('Input'),
    Aliasing = maps:get(pointer_aliasing, Opts, false),
    FF0 = erl_syntax:match_expr(svar(0),
        erl_syntax:record_expr(erl_syntax:atom(msrpce_state),
            [erl_syntax:record_field(erl_syntax:atom(mode),
                erl_syntax:atom(decode)),
             erl_syntax:record_field(erl_syntax:atom(aliasing),
                erl_syntax:atom(Aliasing)),
             erl_syntax:record_field(erl_syntax:atom(data),
                InVar)])),
    FF1 = erl_syntax:match_expr(
        erl_syntax:tuple([tvar(0), svar(1)]),
        erl_syntax:application(dec_fun_name([RecName]),
            [svar(0)])),
    FF2 = erl_syntax:match_expr(svar(2), erl_syntax:application(
        erl_syntax:atom(msrpce_runtime), erl_syntax:atom(finish),
        [svar(1)])),
    FF3 = erl_syntax:application(
        finish_fun_name([RecName]),
        [tvar(0), svar(2)]),
    F0 = erl_syntax:function(
        concat_atoms([decode, RecName]),
        [erl_syntax:clause([InVar], [], [FF0, FF1, FF2, FF3])]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(finish_fun_name([RecName])), F1).

decode_stream_func_form(Name, Loc, _Opts) ->
    InVar = erl_syntax:variable('Input'),
    FuncName = concat_atoms([decode, Name]),
    V1Fun = concat_atoms([decode, Name, v1]),
    V2Fun = concat_atoms([decode, Name, v2]),
    FF0 = erl_syntax:case_expr(InVar,
        [erl_syntax:clause([
            erl_syntax:binary([
                erl_syntax:binary_field(erl_syntax:integer(1)),
                erl_syntax:binary_field(erl_syntax:underscore(),
                    [erl_syntax:atom(binary)])])],
            [], [erl_syntax:application(V1Fun, [InVar])]),
         erl_syntax:clause([
            erl_syntax:binary([
                erl_syntax:binary_field(erl_syntax:integer(2)),
                erl_syntax:binary_field(erl_syntax:underscore(),
                    [erl_syntax:atom(binary)])])],
            [], [erl_syntax:application(V2Fun, [InVar])]),
         erl_syntax:clause(
            [erl_syntax:binary([
                erl_syntax:binary_field(tvar(0)),
                erl_syntax:binary_field(erl_syntax:underscore(),
                    [erl_syntax:atom(binary)])])],
            [],
            [erl_syntax:application(erl_syntax:atom(error),
                [erl_syntax:tuple([
                    erl_syntax:atom(bad_rpc_ver), tvar(0)])])])]),
    F0 = erl_syntax:function(
        FuncName,
        [erl_syntax:clause([InVar], [], [FF0])]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(FuncName), F1).

decode_stream_func_form(Ver, Name, Structs, Loc, Opts) ->
    {CteRec, PrivHdrFun, FuncName} = case Ver of
        1 -> {msrpce_cte_v1, read_privhdr_v1, concat_atoms([decode, Name, v1])};
        2 -> {msrpce_cte_v2, read_privhdr_v2, concat_atoms([decode, Name, v2])}
    end,
    Aliasing = maps:get(pointer_aliasing, Opts, false),
    InVar = erl_syntax:variable('Input'),
    S0 = #fstate{opts = Opts},
    {StructMap, S1} = lists:foldl(fun (Struct, {Acc, SS0}) ->
        {TVar, SS1} = inc_tvar(SS0),
        {Acc ++ [{TVar, dec_fun([Struct]), finish_fun_name([Struct])}], SS1}
    end, {[], S0}, Structs),
    {SV0, SV1, S2} = inc_svar(S1),
    FF0 = erl_syntax:match_expr(SV0,
        erl_syntax:record_expr(erl_syntax:atom(msrpce_state),
            [erl_syntax:record_field(erl_syntax:atom(mode),
                erl_syntax:atom(decode)),
             erl_syntax:record_field(erl_syntax:atom(aliasing),
                erl_syntax:atom(Aliasing)),
             erl_syntax:record_field(erl_syntax:atom(data),
                InVar)])),
    CteVar = erl_syntax:variable('CTE'),
    CteFun = dec_fun_name([CteRec]),
    FF1 = erl_syntax:match_expr(
        erl_syntax:tuple([CteVar, SV1]),
        erl_syntax:application(CteFun, [SV0])),
    %% TODO: assert about contents of cte rec
    CTEFields = case Ver of
        1 -> [
            erl_syntax:record_field(erl_syntax:atom(hdrlen),
                erl_syntax:integer(16#08))
            ];
        2 -> [
            erl_syntax:record_field(erl_syntax:atom(hdrlen),
                erl_syntax:integer(16#40)),
            erl_syntax:record_field(erl_syntax:atom(xfersyntax),
                erl_syntax:record_expr(erl_syntax:atom(msrpce_syntax_id), [
                    erl_syntax:record_field(erl_syntax:atom(version),
                        erl_syntax:integer(2)),
                    erl_syntax:record_field(erl_syntax:atom(uuid),
                        erl_syntax:abstract(
                            msrpce:uuid_from_string("8a885d04-1ceb-11c9-9fe8-08002b104860")))
                    ]))
            ]
    end,
    Endian = maps:get(endian, Opts, big),
    EndianNum = case Endian of
        little -> 16#10;
        big    -> 16#00
    end,
    FF2 = erl_syntax:match_expr(
        erl_syntax:record_expr(erl_syntax:atom(CteRec), [
            erl_syntax:record_field(erl_syntax:atom(version),
                erl_syntax:integer(Ver)),
            erl_syntax:record_field(erl_syntax:atom(endian),
                erl_syntax:integer(EndianNum))
            ] ++ CTEFields),
        CteVar),
    S3 = add_forms([FF0, FF1, FF2], S2),
    S4 = lists:foldl(fun ({TVar, DecFun, FinishFunName}, SS0) ->
        {SSV0, SSV1, SS1} = inc_svar(SS0),
        {UnfinishVar, SS2} = inc_tvar(SS1),
        Form0 = erl_syntax:match_expr(
            erl_syntax:tuple([UnfinishVar, SSV1]),
            erl_syntax:application(
                erl_syntax:atom(msrpce_runtime),
                erl_syntax:atom(PrivHdrFun),
                [DecFun, SSV0])),
        Form1 = erl_syntax:match_expr(TVar,
            erl_syntax:application(
                FinishFunName, [UnfinishVar, SSV1])),
        add_forms([Form0, Form1], SS2)
    end, S3, StructMap),
    OutVars = [TVar || {TVar,_DecFun,_FinFun} <- StructMap],
    FF3 = erl_syntax:list(OutVars),
    S5 = add_forms([FF3], S4),
    #fstate{forms = Forms} = S5,
    F0 = erl_syntax:function(
        FuncName,
        [erl_syntax:clause([InVar], [], Forms)]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(FuncName), F1).

encode_stream_func_form(Name, Loc, _Opts) ->
    ArgsVar = erl_syntax:variable('Args'),
    FF0 = erl_syntax:application(
        concat_atoms([encode, Name, v1]),
        [ArgsVar]),
    F0 = erl_syntax:function(
        concat_atoms([encode, Name]),
        [erl_syntax:clause([ArgsVar], [], [FF0])]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(concat_atoms([encode, Name])), F1).

encode_stream_func_form(Ver, Name, Structs, Loc, Opts) ->
    {CteRec, PrivHdrFun, FuncName} = case Ver of
        1 -> {msrpce_cte_v1, write_privhdr_v1, concat_atoms([encode, Name, v1])};
        2 -> {msrpce_cte_v2, write_privhdr_v2, concat_atoms([encode, Name, v2])}
    end,
    Aliasing = maps:get(pointer_aliasing, Opts, false),
    Endian = maps:get(endian, Opts, big),
    EndianNum = case Endian of
        little -> 16#10;
        big    -> 16#00
    end,
    S0 = #fstate{opts = Opts},
    {StructMap, S1} = lists:foldl(fun (Struct, {Acc, SS0}) ->
        {TVar, SS1} = inc_tvar(SS0),
        {Acc ++ [{TVar, enc_fun([Struct])}], SS1}
    end, {[], S0}, Structs),
    {SV0, SV1, S2} = inc_svar(S1),
    FF0 = erl_syntax:match_expr(SV0,
        erl_syntax:record_expr(erl_syntax:atom(msrpce_state),
            [erl_syntax:record_field(erl_syntax:atom(mode),
                erl_syntax:atom(encode)),
             erl_syntax:record_field(erl_syntax:atom(aliasing),
                erl_syntax:atom(Aliasing)),
             erl_syntax:record_field(erl_syntax:atom(data),
                erl_syntax:binary([]))])),
    CteFun = enc_fun_name([CteRec]),
    FF1 = erl_syntax:match_expr(SV1,
        erl_syntax:application(CteFun,
            [erl_syntax:record_expr(
                erl_syntax:atom(CteRec), [
                    erl_syntax:record_field(erl_syntax:atom(endian),
                        erl_syntax:integer(EndianNum))
                ]),
             SV0])),
    S3 = add_forms([FF0, FF1], S2),
    S4 = lists:foldl(fun ({TVar, Fun}, SS0) ->
        {SSV0, SSV1, SS1} = inc_svar(SS0),
        Form = erl_syntax:match_expr(SSV1,
            erl_syntax:application(
                erl_syntax:atom(msrpce_runtime),
                erl_syntax:atom(PrivHdrFun),
                [Fun, TVar, SSV0])),
        add_forms([Form], SS1)
    end, S3, StructMap),
    {SV2, S5} = cur_svar(S4),
    FF2 = erl_syntax:match_expr(
        erl_syntax:record_expr(erl_syntax:atom(msrpce_state),
            [erl_syntax:record_field(erl_syntax:atom(data), dvar(0))]),
        SV2),
    S6 = add_forms([FF2, dvar(0)], S5),
    #fstate{forms = Forms} = S6,
    InVars = [TVar || {TVar,_Fun} <- StructMap],
    F0 = erl_syntax:function(
        FuncName,
        [erl_syntax:clause([erl_syntax:list(InVars)], [], Forms)]),
    F1 = erl_syntax:revert(F0),
    set_anno(Loc, erl_syntax:atom_value(FuncName), F1).

% make_forms([]) -> [];
% make_forms(Tokens) ->
%     {BeforeDot, AfterDot0} = lists:splitwith(fun
%         ({dot, _}) -> false;
%         (_) -> true
%     end, Tokens),
%     [Dot = {dot, _} | AfterDot1] = AfterDot0,
%     {ok, Form} = erl_parse:parse_form(BeforeDot ++ [Dot]),
%     [Form | make_forms(AfterDot1)].

% compile_module(Name, Type, Opts) ->
%     ModName = binary_to_atom(iolist_to_binary([
%         "msrpce_dyn_", integer_to_binary(erlang:unique_integer([positive]))
%         ])),
%     RecordsPath = code:lib_dir(msrpce) ++ "/include/records.hrl",
%     {ok, Data} = file:read_file(RecordsPath),
%     {ok, RecTokens, _} = erl_scan:string(
%         unicode:characters_to_list(Data, utf8)),
%     RecForms = make_forms(RecTokens),
%     NameStr = atom_to_list(Name),
%     DecodeName = list_to_atom("decode_" ++ NameStr),
%     EncodeName = list_to_atom("encode_" ++ NameStr),
%     ModHeaderForms = [
%         {attribute, 1, module, ModName},
%         {attribute, 1, export, [{EncodeName, 1}, {DecodeName, 1}]}],
%     EncodeForm = encode_func_form(EncodeName, Type, 2, Opts),
%     DecodeForm = decode_func_form(DecodeName, Type, 2, Opts),
%     Forms = ModHeaderForms ++ RecForms ++ [EncodeForm, DecodeForm],
%     case compile:forms(Forms, [return_errors]) of
%         {ok, Mod, Bin} ->
%             {module, Mod} = code:load_binary(Mod, "msrpce_dynamic", Bin),
%             {ok, Mod};
%         {ok, Mod, Bin, _Warnings} ->
%             {module, Mod} = code:load_binary(Mod, "msrpce_dynamic", Bin),
%             {ok, Mod};
%         {error, [{_, Errs}], _Warnings} ->
%             FormatErrs = lists:map(fun ({_Loc, _Mod, Desc}) ->
%                 compile:format_error(Desc)
%             end, Errs),
%             {error, iolist_to_binary(FormatErrs)}
%     end.
