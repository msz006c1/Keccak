-module(keccak_real_test).
-export([run_tests/0]).

run_tests() ->
    io:format("Running Keccak Real Implementation Tests...~n"),
    
    % Test basic functionality
    Data = <<"Hello, World!">>,
    
    % Test SHA3-256
    Hash256 = keccak_real_fixed:sha3_256(Data),
    io:format("SHA3-256(~s) = ~s~n", [Data, binary_to_hex(Hash256)]),
    
    % Test SHA3-512
    Hash512 = keccak_real_fixed:sha3_512(Data),
    io:format("SHA3-512(~s) = ~s~n", [Data, binary_to_hex(Hash512)]),
    
    % Test SHA3-1024
    Hash1024 = keccak_real_fixed:sha3_1024(Data),
    io:format("SHA3-1024(~s) = ~s~n", [Data, binary_to_hex(Hash1024)]),
    
    % Test generic Keccak
    HashCustom = keccak_real_fixed:keccak(Data, 32),
    io:format("Keccak-256(~s) = ~s~n", [Data, binary_to_hex(HashCustom)]),
    
    % Test different inputs
    Data2 = <<"Test">>,
    Hash256_2 = keccak_real_fixed:sha3_256(Data2),
    io:format("SHA3-256(~s) = ~s~n", [Data2, binary_to_hex(Hash256_2)]),
    
    % Test different output lengths
    Hash16 = keccak_real_fixed:keccak(Data, 16),
    io:format("Keccak-128(~s) = ~s~n", [Data, binary_to_hex(Hash16)]),
    
    % Test streaming API
    Ctx = keccak_real_fixed:init(32),
    Ctx1 = keccak_real_fixed:update(Ctx, <<"Hello, ">>),
    Ctx2 = keccak_real_fixed:update(Ctx1, <<"World!">>),
    StreamHash = keccak_real_fixed:final(Ctx2),
    io:format("Streaming SHA3-256(~s) = ~s~n", [<<"Hello, World!">>, binary_to_hex(StreamHash)]),
    
    % Verify that direct and streaming produce same result
    case Hash256 =:= StreamHash of
        true ->
            io:format("✓ Direct and streaming APIs produce same result~n");
        false ->
            io:format("✗ Direct and streaming APIs produce different results~n")
    end,
    
    io:format("Tests completed.~n").

binary_to_hex(<<>>) ->
    [];
binary_to_hex(Bin) ->
    <<H:4, T:4, Rest/binary>> = Bin,
    [hex_char(H), hex_char(T) | binary_to_hex(Rest)].

hex_char(N) when N < 10 -> $0 + N;
hex_char(N) -> $a + N - 10.