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

-include("include/records.hrl").

-record(test1, {
	a :: msrpce:uint8(),
	b :: msrpce:uint32(),
	c :: msrpce:uint8(),
	d :: msrpce:uint64()
	}).

%-rpce_struct(test1, [big_endian]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test1_test() ->
	Struct = #test1{a = 1, b = 2, c = 3, d = 4},
	Data = encode_test1(Struct),
	?assertMatch(<<1, 0:24, 2:32/big, 3, 0:24, 4:64/big>>, Data),
	{ok, DeStruct} = decode_test1(Data),
	?assertMatch(#test1{a = 1, b = 2, c = 3, d = 4}, DeStruct).

-endif.
