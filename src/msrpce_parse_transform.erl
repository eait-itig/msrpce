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

-module(msrpce_parse_transform).

-export([parse_transform/2, parse_transform_info/0]).

-record(?MODULE, {
    opts :: [term()],
    utypes = #{} :: #{atom() => term()},
    records = #{} :: #{atom() => term()}
    }).

-spec parse_transform_info() -> #{'error_location' => 'column' | 'line'}.
parse_transform_info() ->
    #{error_location => column}.

-spec parse_transform([erl_parse:abstract_form()], [compile:option()]) ->
    [erl_parse:abstract_form()].
parse_transform(Forms, Options) ->
    S0 = #?MODULE{opts = Options},
    transform_all(Forms, S0).

transform_all([], _) -> [];
transform_all([Form0 | Rest], S0 = #?MODULE{}) ->
    {Forms1, S1} = transform(Form0, S0),
    Forms1 ++ transform_all(Rest, S1).

transform(Other, S0) ->
    {[Other], S0}.
