%% @doc
%% Keccak/SHA3 hash functions for Erlang
%%
%% This module provides implementations of the Keccak hash function family,
%% including SHA3-256, SHA3-512, SHA3-1024, and generic Keccak hashing.
%% It uses NIFs to call the optimized C implementation for performance.
%%
%% ## Features
%% - NIF-based implementation for performance
%% - Support for single-call and streaming APIs
%% - FIPS 202 compliant
%%
%% ## Quick Start
%% ```
%% %% Single-call API
%% Hash = keccak:sha3_256(<<"Hello, World!">>).
%%
%% %% Streaming API
%% Ctx = keccak:init(sha3_256),
%% Ctx1 = keccak:update(Ctx, <<"Hello, ">>),
%% Ctx2 = keccak:update(Ctx1, <<"World!">>),
%% Hash = keccak:final(Ctx2).
%% '''
-module(keccak).

-export([sha3_256/1, sha3_512/1, sha3_1024/1, keccak/2]).
-export([init/1, update/2, final/1]).

%% NIF functions - will be implemented in C
-on_load(load_nif/0).
load_nif() ->
    % Try multiple possible locations for the NIF
    PossiblePaths = [
        filename:join("./priv", "keccak"),
        filename:join("../priv", "keccak"),
        filename:join(code:lib_dir(), "priv/keccak"),
        "priv/keccak",
        "./keccak"
    ],
    SoName = find_existing_nif(PossiblePaths),
    erlang:load_nif(SoName, 0).

find_existing_nif([Path|Rest]) ->
    case filelib:is_regular(Path ++ ".so") orelse
         filelib:is_regular(Path ++ ".dylib") orelse
         filelib:is_regular(Path ++ ".dll") of
        true -> Path;
        false -> find_existing_nif(Rest)
    end;
find_existing_nif([]) ->
    % If none found, try default path - this will cause failure but with clearer error
    filename:join("./priv", "keccak").

%% @doc Compute SHA3-256 hash of binary data
-spec sha3_256(binary()) -> binary().
sha3_256(Data) ->
    sha3_256_nif(Data).

%% @doc Compute SHA3-512 hash of binary data
-spec sha3_512(binary()) -> binary().
sha3_512(Data) ->
    sha3_512_nif(Data).

%% @doc Compute SHA3-1024 hash of binary data
-spec sha3_1024(binary()) -> binary().
sha3_1024(Data) ->
    sha3_1024_nif(Data).

%% @doc Generic Keccak hash function
-spec keccak(binary(), non_neg_integer()) -> binary().
keccak(Input, OutputLen) ->
    keccak_nif(Input, OutputLen).

%% @doc Initialize streaming hash context
-spec init(atom() | integer()) -> term().
init(sha3_256) -> init(32);
init(sha3_512) -> init(64);
init(sha3_1024) -> init(128);
init(OutputLen) when is_integer(OutputLen), OutputLen > 0 ->
    init_stream_nif(OutputLen).

%% @doc Update streaming hash with new data
-spec update(term(), binary()) -> term().
update(Ctx, Data) ->
    update_stream_nif(Ctx, Data).

%% @doc Finalize streaming hash and return result
-spec final(term()) -> binary().
final(Ctx) ->
    final_stream_nif(Ctx).

%% Internal NIF functions - these will be replaced by NIF implementations
sha3_256_nif(_Data) ->
    erlang:nif_error({not_loaded, ?MODULE, ?LINE}).

sha3_512_nif(_Data) ->
    erlang:nif_error({not_loaded, ?MODULE, ?LINE}).

sha3_1024_nif(_Data) ->
    erlang:nif_error({not_loaded, ?MODULE, ?LINE}).

keccak_nif(_Input, _OutputLen) ->
    erlang:nif_error({not_loaded, ?MODULE, ?LINE}).

init_stream_nif(_OutputLen) ->
    erlang:nif_error({not_loaded, ?MODULE, ?LINE}).

update_stream_nif(_Ctx, _Data) ->
    erlang:nif_error({not_loaded, ?MODULE, ?LINE}).

final_stream_nif(_Ctx) ->
    erlang:nif_error({not_loaded, ?MODULE, ?LINE}).