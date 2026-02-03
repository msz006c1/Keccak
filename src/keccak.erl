%% @doc Erlang NIF interface for Keccak/SHA3.
%% Supports SHA3-256/512/1024, generic Keccak, and streaming API.
%% Requires the NIF shared library in priv/keccak.so.
%% @end
-module(keccak).

-export([sha3_256/1, sha3_512/1, sha3_1024/1, keccak/2]).
-export([init/1, update/2, final/1]).

-on_load(load_nif/0).

%% @doc Loads the NIF library.
%% Tries multiple possible paths to locate the shared library.
%% @end
load_nif() ->
    PossiblePaths = [
        filename:join("./priv", "keccak"),
        filename:join("../priv", "keccak"),
        filename:join(code:lib_dir(), "priv/keccak"),
        "priv/keccak",
        "./keccak"
    ],
    SoName = find_existing_nif(PossiblePaths),
    erlang:load_nif(SoName, 0).

%% @private
find_existing_nif([Path|Rest]) ->
    case filelib:is_regular(Path ++ ".so") orelse
         filelib:is_regular(Path ++ ".dylib") orelse
         filelib:is_regular(Path ++ ".dll") of
        true -> Path;
        false -> find_existing_nif(Rest)
    end;
find_existing_nif([]) ->
    "./priv/keccak".

%% @doc Computes SHA3-256 hash of the input binary.
%% @param Data Input binary data to hash
%% @return Binary containing the 32-byte hash result
%% @end
-spec sha3_256(binary()) -> binary().
sha3_256(Data) -> sha3_256_nif(Data).

%% @doc Computes SHA3-512 hash of the input binary.
%% @param Data Input binary data to hash
%% @return Binary containing the 64-byte hash result
%% @end
-spec sha3_512(binary()) -> binary().
sha3_512(Data) -> sha3_512_nif(Data).

%% @doc Computes SHA3-1024 hash of the input binary.
%% @param Data Input binary data to hash
%% @return Binary containing the 128-byte hash result
%% @end
-spec sha3_1024(binary()) -> binary().
sha3_1024(Data) -> sha3_1024_nif(Data).

%% @doc Computes a generic Keccak hash with specified output length.
%% @param Input Input binary data to hash
%% @param OutputLen Desired output length in bytes (1-1024)
%% @return Binary containing the hash result
%% @end
-spec keccak(binary(), non_neg_integer()) -> binary().
keccak(Input, OutputLen) -> keccak_nif(Input, OutputLen).

%% @doc Initializes a streaming hash context.
%% Accepts either predefined algorithm atoms or an integer output length.
%% @param sha3_256 | sha3_512 | sha3_1024 | OutputLen Output length in bytes
%% @return Reference to the initialized streaming context
%% @end
-spec init(atom() | integer()) -> term().
init(sha3_256) -> init(32);
init(sha3_512) -> init(64);
init(sha3_1024) -> init(128);
init(OutputLen) when is_integer(OutputLen), OutputLen > 0 ->
    init_stream_nif(OutputLen).

%% @doc Updates a streaming hash context with new data.
%% @param Ctx Reference to the streaming context
%% @param Data Binary data to add to the hash
%% @return Updated context reference
%% @end
-spec update(term(), binary()) -> term().
update(Ctx, Data) -> update_stream_nif(Ctx, Data).

%% @doc Finalizes a streaming hash context and returns the result.
%% @param Ctx Reference to the streaming context
%% @return Binary containing the final hash result
%% @end
-spec final(term()) -> binary().
final(Ctx) -> final_stream_nif(Ctx).

%% @private
%% NIF stubs
sha3_256_nif(_Data) -> erlang:nif_error({not_loaded, ?MODULE, ?LINE}).
sha3_512_nif(_Data) -> erlang:nif_error({not_loaded, ?MODULE, ?LINE}).
sha3_1024_nif(_Data) -> erlang:nif_error({not_loaded, ?MODULE, ?LINE}).
keccak_nif(_Input, _OutputLen) -> erlang:nif_error({not_loaded, ?MODULE, ?LINE}).
init_stream_nif(_OutputLen) -> erlang:nif_error({not_loaded, ?MODULE, ?LINE}).
update_stream_nif(_Ctx, _Data) -> erlang:nif_error({not_loaded, ?MODULE, ?LINE}).
final_stream_nif(_Ctx) -> erlang:nif_error({not_loaded, ?MODULE, ?LINE}).
