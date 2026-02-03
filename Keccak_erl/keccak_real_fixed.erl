-module(keccak_real_fixed).
-compile({no_auto_import, [element/2]}).
-export([sha3_256/1, sha3_512/1, sha3_1024/1, keccak/2]).
-export([init/1, update/2, final/1]).

-spec sha3_256(binary()) -> binary().
sha3_256(Data) -> keccak_hash(Data, 32).

-spec sha3_512(binary()) -> binary().
sha3_512(Data) -> keccak_hash(Data, 64).

-spec sha3_1024(binary()) -> binary().
sha3_1024(Data) -> keccak_hash(Data, 128).

-spec keccak(binary(), non_neg_integer()) -> binary().
keccak(Input, OutputLen) when OutputLen > 0 -> keccak_hash(Input, OutputLen).

-spec init(pos_integer()) -> map().
init(OutputLen) when OutputLen > 0 ->
    Rate = calculate_rate(OutputLen),
    #{state => init_state(),
      rate => Rate,
      output_len => OutputLen,
      pos => 0,
      finalized => false
     }.

-spec update(map(), binary()) -> map().
update(#{finalized := true} = Ctx, _Data) -> Ctx;
update(Ctx, Data) -> update_internal(Ctx, Data).

-spec final(map()) -> binary().
final(#{finalized := true, result := Result}) -> Result;
final(Ctx) -> finalize_context(Ctx).

update_internal(Ctx, <<>>) -> Ctx;
update_internal(Ctx, Data) ->
    State = maps:get(state, Ctx),
    Rate = maps:get(rate, Ctx),
    Pos = maps:get(pos, Ctx),
    DataSize = byte_size(Data),
    RemainingSpace = Rate - Pos,
    case DataSize =< RemainingSpace of
        true ->
            NewState = absorb_bytes_at_position(State, Data, Pos),
            Ctx#{state := NewState, pos := Pos + DataSize};
        false ->
            <<FirstPart:RemainingSpace/binary, Rest/binary>> = Data,
            UpdatedState = absorb_bytes_at_position(State, FirstPart, Pos),
            PermutedState = keccak_f1600(UpdatedState),
            update_internal(Ctx#{state := PermutedState, pos := 0}, Rest)
    end.

absorb_bytes_at_position(State, <<>>, _Pos) -> State;
absorb_bytes_at_position(State, Data, Pos) ->
    <<Byte:8, Rest/binary>> = Data,
    LaneIndex = Pos div 8,
    ByteOffset = Pos rem 8,
    NewState = xor_byte_into_state(State, LaneIndex + 1, Byte, ByteOffset),
    absorb_bytes_at_position(NewState, Rest, Pos + 1).

xor_byte_into_state(State, LaneIdx, Byte, ByteOffset) ->
    Lane = get_element(LaneIdx, State),
    NewLane = Lane bxor (Byte bsl (ByteOffset * 8)),
    set_element(State, LaneIdx, NewLane).

keccak_hash(Input, OutputLen) when is_binary(Input), OutputLen > 0 ->
    Rate = calculate_rate(OutputLen),
    State = init_state(),
    {AbsorbedState, Remaining} = absorb_blocks(State, Input, Rate),

    %% SHA3 padding: XOR 0x06 at message end, 0x80 at block end.
    %% When output_len >= 100 (e.g. SHA3-1024), rate exceeds the state size,
    %% so the 0x80 end-of-block marker falls outside the state and is skipped.
    Pos = byte_size(Remaining),
    State1 = xor_block_into_state(AbsorbedState, Remaining, 0),
    State2 = xor_byte_into_state(State1, (Pos div 8) + 1, 16#06, Pos rem 8),
    State3 = case valid_rate(OutputLen) of
        true  -> xor_byte_into_state(State2, ((Rate - 1) div 8) + 1, 16#80, (Rate - 1) rem 8);
        false -> State2
    end,
    PaddedState = keccak_f1600(State3),
    squeeze_output(PaddedState, OutputLen, Rate).

init_state() ->
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}.

absorb_blocks(State, Data, Rate) when byte_size(Data) >= Rate ->
    <<Block:Rate/binary, Rest/binary>> = Data,
    NewState = xor_block_into_state(State, Block, 0),
    PermutedState = keccak_f1600(NewState),
    absorb_blocks(PermutedState, Rest, Rate);
absorb_blocks(State, Data, _Rate) ->
    {State, Data}.

squeeze_output(State, OutputLen, Rate) ->
    squeeze_output_internal(State, OutputLen, Rate, <<>>).

squeeze_output_internal(_State, 0, _Rate, Acc) -> Acc;
squeeze_output_internal(State, OutputLen, Rate, Acc) when OutputLen >= Rate ->
    OutputBlock = state_to_binary(State),
    <<Block:Rate/binary, _/binary>> = OutputBlock,
    NextState = keccak_f1600(State),
    squeeze_output_internal(NextState, OutputLen - Rate, Rate, <<Acc/binary, Block/binary>>);
squeeze_output_internal(State, OutputLen, _Rate, Acc) ->
    OutputBlock = state_to_binary(State),
    <<Block:OutputLen/binary, _/binary>> = OutputBlock,
    <<Acc/binary, Block/binary>>.

keccak_f1600(State) ->
    lists:foldl(fun(Round, AccState) ->
        keccak_round(AccState, Round)
    end, State, lists:seq(0, 23)).

keccak_round(State, Round) ->
    %% Theta
    C = [get_element(X+1, State) bxor get_element(X+6, State) bxor
         get_element(X+11, State) bxor get_element(X+16, State) bxor
         get_element(X+21, State) || X <- lists:seq(0, 4)],
    D = [lists:nth(((X + 4) rem 5) + 1, C) bxor rotl64(lists:nth(((X + 1) rem 5) + 1, C), 1) ||
         X <- lists:seq(0, 4)],
    State1 = apply_theta(State, D),

    %% Rho and Pi
    State2 = apply_rho_pi(State1),

    %% Chi
    State3 = apply_chi(State2),

    %% Iota
    apply_iota(State3, Round).

apply_theta(State, D) ->
    lists:foldl(fun(Y, AccState) ->
        lists:foldl(fun(X, InnerAccState) ->
            Index = X + 5 * Y + 1,
            NewValue = get_element(Index, InnerAccState) bxor lists:nth(X + 1, D),
            set_element(InnerAccState, Index, NewValue)
        end, AccState, lists:seq(0, 4))
    end, State, lists:seq(0, 4)).

apply_rho_pi(State) ->
    B = erlang:make_tuple(25, 0),
    B0 = set_element(B, 1, get_element(1, State)),
    rho_pi_chain(State, B0, 1, 0, get_element(1 + 5*0 + 1, State), 0).

rho_pi_chain(_State, B, _X, _Y, _Current, 24) -> B;
rho_pi_chain(State, B, X, Y, Current, T) ->
    NewX = Y,
    NewY = (2 * X + 3 * Y) rem 5,
    RhoOffset = ((T + 1) * (T + 2)) div 2,
    DestIndex = NewX + 5 * NewY + 1,
    Temp = get_element(NewX + 5 * NewY + 1, State),
    RotatedValue = rotl64(Current, RhoOffset rem 64),
    NewB = set_element(B, DestIndex, RotatedValue),
    rho_pi_chain(State, NewB, NewX, NewY, Temp, T + 1).

apply_chi(State) ->
    Mask = 16#FFFFFFFFFFFFFFFF,
    NewState = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    lists:foldl(fun(Y, AccState) ->
        lists:foldl(fun(X, InnerAccState) ->
            Index = X + 5 * Y + 1,
            A = get_element(Index, State),
            B = get_element(((X + 1) rem 5) + 5 * Y + 1, State),
            C = get_element(((X + 2) rem 5) + 5 * Y + 1, State),
            NewValue = A bxor ((B bxor Mask) band C),
            set_element(InnerAccState, Index, NewValue)
        end, AccState, lists:seq(0, 4))
    end, NewState, lists:seq(0, 4)).

apply_iota(State, Round) ->
    RC = get_round_constant(Round),
    set_element(State, 1, get_element(1, State) bxor RC).

get_element(Index, Tuple) when Index >= 1, Index =< tuple_size(Tuple) ->
    erlang:element(Index, Tuple);
get_element(_, _) -> 0.

set_element(Tuple, Index, Value) when Index >= 1, Index =< tuple_size(Tuple) ->
    erlang:setelement(Index, Tuple, Value).

rotl64(Value, 0) -> Value;
rotl64(Value, N) ->
    Mask = 16#FFFFFFFFFFFFFFFF,
    ((Value bsl N) band Mask) bor ((Value bsr (64 - N)) band Mask).

get_round_constant(0)  -> 16#0000000000000001;
get_round_constant(1)  -> 16#0000000000008082;
get_round_constant(2)  -> 16#800000000000808A;
get_round_constant(3)  -> 16#8000000080008000;
get_round_constant(4)  -> 16#000000000000808B;
get_round_constant(5)  -> 16#0000000080000001;
get_round_constant(6)  -> 16#8000000080008081;
get_round_constant(7)  -> 16#8000000000008009;
get_round_constant(8)  -> 16#000000000000008A;
get_round_constant(9)  -> 16#0000000000000088;
get_round_constant(10) -> 16#0000000080008009;
get_round_constant(11) -> 16#000000008000000A;
get_round_constant(12) -> 16#000000008000808B;
get_round_constant(13) -> 16#800000000000008B;
get_round_constant(14) -> 16#8000000000008089;
get_round_constant(15) -> 16#8000000000008003;
get_round_constant(16) -> 16#8000000000008002;
get_round_constant(17) -> 16#8000000000000080;
get_round_constant(18) -> 16#000000000000800A;
get_round_constant(19) -> 16#800000008000000A;
get_round_constant(20) -> 16#8000000080008081;
get_round_constant(21) -> 16#8000000000008080;
get_round_constant(22) -> 16#0000000080000001;
get_round_constant(23) -> 16#8000000080008008;
get_round_constant(_) -> 0.

state_to_binary(State) ->
    <<<<(get_element(I, State)):64/little-unit:1>> || I <- lists:seq(1, 25)>>.

calculate_rate(OutputLen) ->
    case 200 - 2 * OutputLen of
        Val when Val > 0 -> Val;
        _ -> 200
    end.

%% Returns true when the standard rate formula (200 - 2*OutputLen) is valid,
%% false when it overflows (output_len >= 100).
valid_rate(OutputLen) ->
    200 - 2 * OutputLen > 0.

xor_block_into_state(State, <<>>, _Offset) -> State;
xor_block_into_state(State, Block, Offset) when byte_size(Block) >= 8 ->
    <<Lane:64/little, Rest/binary>> = Block,
    NewState = set_element(State, (Offset rem 25) + 1, get_element((Offset rem 25) + 1, State) bxor Lane),
    xor_block_into_state(NewState, Rest, Offset + 1);
xor_block_into_state(State, Block, Offset) ->
    BlockSize = byte_size(Block),
    PaddingSize = 8 - BlockSize,
    PaddedBlock = <<Block/binary, 0:(PaddingSize*8)>>,
    <<Lane:64/little>> = PaddedBlock,
    AbsorbPos = (Offset rem 25) + 1,
    set_element(State, AbsorbPos, get_element(AbsorbPos, State) bxor Lane).

finalize_context(#{state := State, rate := Rate, output_len := OutputLen, pos := Pos}) ->
    State1 = xor_byte_into_state(State, (Pos div 8) + 1, 16#06, Pos rem 8),
    State2 = case valid_rate(OutputLen) of
        true  -> xor_byte_into_state(State1, ((Rate - 1) div 8) + 1, 16#80, (Rate - 1) rem 8);
        false -> State1
    end,
    PaddedState = keccak_f1600(State2),
    squeeze_output(PaddedState, OutputLen, Rate).
