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

%% @doc A parse transform which provides attributes for compiling record
%%      and type definitions into encode/decode functions for the MS RPC
%%      DCE format.
%%
%% This parse transform defines serveral new attributes:
%%
%% <table>
%%   <tr>
%%     <td><code>-rpce(msrpce_compiler:options())</code></td>
%%     <td>Sets compiler options. See {@link msrpce_compiler:options()}. These
%%         affect all other attributes following
%%         it until the next <code>-rpce()</code> attribute.</td>
%%   </tr>
%%   <tr>
%%     <td><code>-rpce_struct(Name :: record_name()).</code></td>
%%     <td>Generates:
%%        <ul><li><code>encode_Name/1</code></li><li><code>decode_Name/1</code></li></ul>
%%        These encode the given record and all deferred pointer
%%        values, with no stream headers attached.</td>
%%   </tr>
%%   <tr>
%%     <td><code>-rpce_stream(Name :: atom(), [record_name()]).</code></td>
%%     <td>Generates:
%%        <ul><li><code>encode_Name_v1/1</code></li><li><code>encode_Name_v2/1</code></li>
%%        <li><code>decode_Name/1</code></li></ul>
%%        These encode the given list
%%        of records as a MS-RPCE Type Serialization stream, with one Common
%%        Type Header, and one Private Header per argument.</td>
%%   </tr>
%% </table>
%%
%% The records named in these attributes must be fully type-annotated, with
%% types taken from the {@link msrpce} module (e.g.
%% {@link msrpce:uint32()} or {@type msrpce:pointer(msrpce:string())}).
%%
%% The header file <code>include/types.hrl</code> also adds local type
%% definitions and shortcuts which make this more readable.
%%
%% <h3>Example</h3>
%%
%% <pre>
%% -include_lib("msrpce/include/types.hrl").
%%
%% -record(foobar, {
%%     field_a :: ulong(),
%%     field_b :: pointer(str())
%%     }).
%%
%% -rpce_struct(foobar).
%%
%% do_something() ->
%%     Bin = encode_foobar(#foobar{field_a = 123, field_b = "hello"}),
%%     ...
%% </pre>
-module(msrpce_parse_transform).

-export([parse_transform/2, parse_transform_info/0]).

-record(?MODULE, {
    opts :: [term()],
    file :: undefined | string(),
    cte = false :: boolean(),
    compiler :: msrpce_compiler:state()
    }).

-spec parse_transform_info() -> #{'error_location' => 'column' | 'line'}.
parse_transform_info() ->
    #{error_location => column}.

-spec parse_transform([erl_parse:abstract_form()], [compile:option()]) ->
    [erl_parse:abstract_form()].
parse_transform(Forms, Options) ->
    S0 = #?MODULE{opts = Options, compiler = msrpce_compiler:new()},
    transform_all(Forms, S0).

transform_all([], _) -> [];
transform_all([Form0 | Rest], S0 = #?MODULE{}) ->
    {Forms1, S1} = transform(erl_syntax:type(Form0), Form0, S0),
    Forms1 ++ transform_all(Rest, S1).

transform(attribute, Form, S0 = #?MODULE{}) ->
    case erl_syntax:atom_value(erl_syntax:attribute_name(Form)) of
        record ->
            S1 = add_record(Form, S0),
            {[Form], S1};
        type ->
            S1 = add_type(Form, S0),
            {[Form], S1};
        rpce ->
            transform_opts(Form, S0);
        rpce_struct ->
            transform_struct(Form, S0);
        rpce_stream ->
            transform_stream(Form, S0);
        file ->
            [FileNameTree, _] = erl_syntax:attribute_arguments(Form),
            S1 = S0#?MODULE{file = erl_syntax:string_value(FileNameTree)},
            {[Form], S1};
        _Other ->
            {[Form], S0}
    end;
transform(eof_marker, Form, S0 = #?MODULE{compiler = C0}) ->
    FuncForms = msrpce_compiler:func_forms(C0),
    %lists:foreach(fun (FForm) ->
    %  io:format("~s\n", [erl_pp:form(FForm)])
    %end, lists:reverse(FuncForms)),
    {FuncForms ++ [Form], S0};
transform(_Type, Form, S0) ->
    {[Form], S0}.

add_record(Form, S0 = #?MODULE{compiler = C0}) ->
    [RecNameTree, RecFieldsTup] = erl_syntax:attribute_arguments(Form),
    RecFields = erl_syntax:tuple_elements(RecFieldsTup),
    RecName = erl_syntax:atom_value(RecNameTree),
    FieldTypes = lists:map(fun
        % erl_syntax seems to die on its arse if you feed it untyped
        % record fields?
        ({tree,record_field,_,{record_field, FieldNameTree, _}}) ->
            FieldName = erl_syntax:atom_value(FieldNameTree),
            {FieldName, untranslatable_type};
        (FieldType) ->
            FieldNameTree = erl_syntax:typed_record_field_body(FieldType),
            FieldName = erl_syntax:atom_value(
                erl_syntax:record_field_name(FieldNameTree)),
            T = erl_syntax:typed_record_field_type(FieldType),
            case (catch type_to_msrpce_type(T)) of
                {'EXIT', _Why} ->
                    {FieldName, {untranslatable_type, T}};
                RpceType ->
                    {FieldName, RpceType}
            end
    end, RecFields),
    Type = {struct, RecName, FieldTypes},
    {ok, C1} = msrpce_compiler:define_type(RecName, Type, C0),
    S0#?MODULE{compiler = C1}.

type_to_msrpce_type(Form) ->
    case erl_syntax:type(Form) of
        record_type ->
            erl_syntax:atom_value(
                erl_syntax:record_type_name(Form));
        user_type_application ->
            Name = erl_syntax:atom_value(
                erl_syntax:user_type_application_name(Form)),
            case Name of
                fixed_array ->
                    [N, RefType] =
                        erl_syntax:user_type_application_arguments(Form),
                    {fixed_array, erl_syntax:integer_value(N),
                        type_to_msrpce_type(RefType)};
                ReferenceType when (ReferenceType =:= size_of) or
                                   (ReferenceType =:= length_of) ->
                    [Field, RefType] =
                        erl_syntax:user_type_application_arguments(Form),
                    {ReferenceType, erl_syntax:atom_value(Field),
                        type_to_msrpce_type(RefType)};
                ReferenceType when (ReferenceType =:= conformant_array) or
                                   (ReferenceType =:= varying_array) or
                                   (ReferenceType =:= array) or
                                   (ReferenceType =:= pointer) ->
                    [RefType] =
                        erl_syntax:user_type_application_arguments(Form),
                    {ReferenceType, type_to_msrpce_type(RefType)};
                fixed_str ->
                    [N] = erl_syntax:user_type_application_arguments(Form),
                    {fixed_string, erl_syntax:integer_value(N)};
                fixed_bin ->
                    [N] = erl_syntax:user_type_application_arguments(Form),
                    {fixed_binary, erl_syntax:integer_value(N)};
                aligned_bin ->
                    [N, A] = erl_syntax:user_type_application_arguments(Form),
                    {fixed_binary, erl_syntax:integer_value(N),
                        erl_syntax:integer_value(A)};
                _ ->
                    Name
            end;
        type_application ->
            Qualified = erl_syntax:type_application_name(Form),
            case erl_syntax:type(Qualified) of
                module_qualifier ->
                    Module = erl_syntax:atom_value(
                        erl_syntax:module_qualifier_argument(Qualified)),
                    Type = erl_syntax:atom_value(
                        erl_syntax:module_qualifier_body(Qualified)),
                    case {Module, Type} of
                        {msrpce, fixed_array} ->
                            [N, RefType] =
                                erl_syntax:type_application_arguments(Form),
                            {fixed_array, erl_syntax:integer_value(N),
                                type_to_msrpce_type(RefType)};

                        {msrpce, RType} when (RType =:= length_of) or
                                             (RType =:= size_of) ->
                            [Field, RefType] =
                                erl_syntax:type_application_arguments(Form),
                            {RType, erl_syntax:atom_value(Field),
                                type_to_msrpce_type(RefType)};

                        {msrpce, RType} when (RType =:= conformant_array) or
                                             (RType =:= varying_array) or
                                             (RType =:= array) or
                                             (RType =:= pointer) or
                                             (RType =:= le) or
                                             (RType =:= be) ->
                            [RefType] =
                                erl_syntax:type_application_arguments(Form),
                            {RType, type_to_msrpce_type(RefType)};

                        {msrpce, uuid} -> {fixed_binary, 16, 4};

                        {msrpce, str} -> string;
                        {msrpce, bin} -> binary;
                        {msrpce, varying_str} -> varying_string;
                        {msrpce, varying_bin} -> varying_binary;
                        {msrpce, fixed_str} ->
                            [N] = erl_syntax:type_application_arguments(Form),
                            {fixed_string, erl_syntax:integer_value(N)};
                        {msrpce, fixed_bin} ->
                            [N] = erl_syntax:type_application_arguments(Form),
                            {fixed_binary, erl_syntax:integer_value(N)};
                        {msrpce, aligned_bin} ->
                            [N, A] = erl_syntax:type_application_arguments(Form),
                            {fixed_binary, erl_syntax:integer_value(N),
                                erl_syntax:integer_value(A)};

                        {msrpce, bitset} ->
                            [BaseType, _BitNameUnion, BitMapType] =
                                erl_syntax:type_application_arguments(Form),
                            Fields = erl_syntax:map_type_fields(BitMapType),
                            BitMap = lists:foldl(fun (X, Acc) ->
                                Key = erl_syntax:atom_value(
                                    erl_syntax:map_type_assoc_name(X)),
                                Value = erl_syntax:integer_value(
                                    erl_syntax:map_type_assoc_value(X)),
                                Acc#{Key => Value}
                            end, #{}, Fields),
                            {bitset, type_to_msrpce_type(BaseType), BitMap};

                        {msrpce, bitset_mask} ->
                            [BaseType, _BitNameUnion, BitMapType] =
                                erl_syntax:type_application_arguments(Form),
                            Fields = erl_syntax:map_type_fields(BitMapType),
                            BitMap = lists:foldl(fun (X, Acc) ->
                                Key = erl_syntax:atom_value(
                                    erl_syntax:map_type_assoc_name(X)),
                                Value = erl_syntax:integer_value(
                                    erl_syntax:map_type_assoc_value(X)),
                                Acc#{Key => {mask, Value}}
                            end, #{}, Fields),
                            {bitset, type_to_msrpce_type(BaseType), BitMap};

                        {msrpce, custom} ->
                            [BaseType, _RealType, Enc, Dec] =
                                erl_syntax:type_application_arguments(Form),
                            {custom, type_to_msrpce_type(BaseType), Enc, Dec};
                        {msrpce, builtin} ->
                            [BaseType, _RealType, Enc, Dec] =
                                erl_syntax:type_application_arguments(Form),
                            EncQ = erl_syntax:module_qualifier(
                                erl_syntax:atom(msrpce), Enc),
                            DecQ = erl_syntax:module_qualifier(
                                erl_syntax:atom(msrpce), Dec),
                            {custom, type_to_msrpce_type(BaseType), EncQ, DecQ};

                        {msrpce, Primitive} when (Primitive =:= uint8) or
                                                 (Primitive =:= boolean) or
                                                 (Primitive =:= uint16) or
                                                 (Primitive =:= uint32) or
                                                 (Primitive =:= uint64) or
                                                 (Primitive =:= int8) or
                                                 (Primitive =:= int16) or
                                                 (Primitive =:= int32) or
                                                 (Primitive =:= int64) or
                                                 (Primitive =:= unicode) or
                                                 (Primitive =:= varying_unicode) ->
                            Primitive;
                        _ ->
                            error(unknown_type)
                    end;
                atom ->
                    erl_syntax:atom_value(Qualified)
            end;
        _ ->
            error(unknown_type)
    end.

add_type(Form, S0 = #?MODULE{compiler = C0}) ->
    [TypeDefAbs] = erl_syntax:attribute_arguments(Form),
    {Name, TypeDef, _Args} = erl_syntax:concrete(TypeDefAbs),
    case (catch type_to_msrpce_type(TypeDef)) of
        {'EXIT', _Why} ->
            %io:format("unsupported typedef for ~s\n", [Name]),
            S0;
        Type ->
            {ok, C1} = msrpce_compiler:define_type(Name, Type, C0),
            S0#?MODULE{compiler = C1}
    end.

transform_struct(Form, S0 = #?MODULE{compiler = C0, file = FN}) ->
    [ArgsAbs] = erl_syntax:attribute_arguments(Form),
    case erl_syntax:concrete(ArgsAbs) of
        {TypeName, Opts0} when is_atom(TypeName) and is_map(Opts0) ->
            Loc0 = erl_syntax:get_pos(Form),
            Loc1 = erl_anno:set_file(FN, Loc0),
            Opts1 = Opts0#{location => Loc1},
            {ok, C1} = msrpce_compiler:compile_type(TypeName, Opts1, C0),
            {[], S0#?MODULE{compiler = C1}};

        TypeName when is_atom(TypeName) ->
            Loc0 = erl_syntax:get_pos(Form),
            Loc1 = erl_anno:set_file(FN, Loc0),
            Opts = #{location => Loc1},
            {ok, C1} = msrpce_compiler:compile_type(TypeName, Opts, C0),
            {[], S0#?MODULE{compiler = C1}};

        Args ->
            error({badarg, rpce_struct, Args})
    end.

transform_stream(Form, S0 = #?MODULE{compiler = C0, cte = false, file = FN}) ->
    Loc0 = erl_syntax:get_pos(Form),
    Loc1 = erl_anno:set_file(FN, Loc0),
    Opts = #{location => Loc1},
    {ok, C1} = msrpce_compiler:compile_type(msrpce_cte_v1, Opts, C0),
    {ok, C2} = msrpce_compiler:compile_type(msrpce_cte_v2, Opts, C1),
    transform_stream(Form, S0#?MODULE{cte = true, compiler = C2});

transform_stream(Form, S0 = #?MODULE{compiler = C0, file = FN}) ->
    Loc0 = erl_syntax:get_pos(Form),
    Loc1 = erl_anno:set_file(FN, Loc0),
    Opts0 = #{location => Loc1},
    [ArgsAbs] = erl_syntax:attribute_arguments(Form),
    Opts2 = case erl_syntax:concrete(ArgsAbs) of
        {StreamName, Structs} when is_atom(StreamName) and is_list(Structs) ->
            Opts0;
        {StreamName, Structs, Opts1} when is_atom(StreamName) and
                                          is_list(Structs) and
                                          is_map(Opts1) ->
            maps:merge(Opts0, Opts1)
    end,
    {ok, C1} = msrpce_compiler:compile_stream(StreamName, Structs, Opts2, C0),
    {[], S0#?MODULE{compiler = C1}}.

transform_opts(Form, S0 = #?MODULE{compiler = C0}) ->
    [ArgsAbs] = erl_syntax:attribute_arguments(Form),
    Opts = erl_syntax:concrete(ArgsAbs),
    C1 = msrpce_compiler:set_options(Opts, C0),
    {[], S0#?MODULE{compiler = C1}}.
