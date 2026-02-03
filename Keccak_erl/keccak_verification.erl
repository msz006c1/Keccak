-module(keccak_verification).
-export([test_basic/0]).

test_basic() ->
    % Test with a simple input and compare to known SHA3-256 result
    % For input "", SHA3-256 should be: a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a
    
    EmptyInput = <<>>,
    EmptyHash = keccak_real_fixed:sha3_256(EmptyInput),
    EmptyExpected = <<"a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a">>,
    HexEmptyHash = hex_encode(EmptyHash),
    io:format("SHA3-256(\"\") = ~s~n", [HexEmptyHash]),
    io:format("Expected: ~s~n", [binary_to_list(EmptyExpected)]),
    io:format("Match: ~p~n~n", [HexEmptyHash =:= binary_to_list(EmptyExpected)]),

    % Test with "abc"
    ABCInput = <<"abc">>,
    ABCHash = keccak_real_fixed:sha3_256(ABCInput),
    ABCExpected = <<"3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532">>,
    HexABCHash = hex_encode(ABCHash),
    io:format("SHA3-256(\"abc\") = ~s~n", [HexABCHash]),
    io:format("Expected: ~s~n", [binary_to_list(ABCExpected)]),
    io:format("Match: ~p~n~n", [HexABCHash =:= binary_to_list(ABCExpected)]),

    % Test streaming with "abc"
    Ctx = keccak_real_fixed:init(32),
    Ctx1 = keccak_real_fixed:update(Ctx, <<"a">>),
    Ctx2 = keccak_real_fixed:update(Ctx1, <<"bc">>),
    StreamHash = keccak_real_fixed:final(Ctx2),
    HexStreamHash = hex_encode(StreamHash),
    io:format("Stream SHA3-256(\"abc\") = ~s~n", [HexStreamHash]),
    io:format("Expected: ~s~n", [binary_to_list(ABCExpected)]),
    io:format("Stream Match: ~p~n", [HexStreamHash =:= binary_to_list(ABCExpected)]),

    % Compare direct vs streaming
    io:format("Direct vs Streaming match: ~p~n", [ABCHash =:= StreamHash]).

hex_encode(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X:8>> <= Bin]).