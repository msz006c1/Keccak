-module(test_keccak).
-export([run_tests/0]).

run_tests() ->
    io:format("Running Keccak/SHA3 tests...~n"),
    
    % Test SHA3-256
    Data1 = <<"Hello, World!">>,
    Hash1 = keccak:sha3_256(Data1),
    io:format("SHA3-256(~s) = ~p~n", [Data1, Hash1]),
    
    % Test SHA3-512
    Hash2 = keccak:sha3_512(Data1),
    io:format("SHA3-512(~s) = ~p~n", [Data1, Hash2]),
    
    % Test SHA3-1024
    Hash3 = keccak:sha3_1024(Data1),
    io:format("SHA3-1024(~s) = ~p~n", [Data1, Hash3]),
    
    % Test generic Keccak
    Hash4 = keccak:keccak(Data1, 32),  % Same as SHA3-256
    io:format("Keccak-256(~s) = ~p~n", [Data1, Hash4]),
    
    % Test streaming API
    Ctx = keccak:init(sha3_256),
    Ctx1 = keccak:update(Ctx, <<"Hello, ">>),
    Ctx2 = keccak:update(Ctx1, <<"World!">>),
    StreamHash = keccak:final(Ctx2),
    io:format("Streaming SHA3-256(~s) = ~p~n", [Data1, StreamHash]),
    
    % Verify that direct and streaming produce same result
    case Hash1 =:= StreamHash of
        true -> 
            io:format("✓ Direct and streaming APIs produce same result~n");
        false -> 
            io:format("✗ Direct and streaming APIs produce different results~n")
    end,
    
    io:format("Tests completed.~n").