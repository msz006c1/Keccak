-module(stream_test).
-export([test/0]).

test() ->
    % Test streaming function
    Data = <<"Hello, World!">>,
    
    % Direct approach
    DirectHash = keccak_real_fixed:sha3_256(Data),
    io:format("Direct SHA3-256: ~s~n", [binary_to_list(DirectHash)]),
    
    % Streaming approach
    Ctx = keccak_real_fixed:init(32),
    Ctx1 = keccak_real_fixed:update(Ctx, <<"Hello, ">>),
    Ctx2 = keccak_real_fixed:update(Ctx1, <<"World!">>),
    StreamHash = keccak_real_fixed:final(Ctx2),
    io:format("Stream SHA3-256: ~s~n", [binary_to_list(StreamHash)]),
    
    % Check if they match
    case DirectHash =:= StreamHash of
        true -> 
            io:format("SUCCESS: Direct and streaming produce same result~n");
        false -> 
            io:format("FAILURE: Direct and streaming produce different results~n")
    end.