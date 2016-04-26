-module(snffr_utils).

-export([print_hex/1, print_eth/1, print_ipv4/1]).

%% Helper functions

hex(C) when C < 10 -> $0 + C;
hex(C) -> $A + C - 10.

binary_to_hexstring(Binary) when is_binary(Binary) ->
  lists:flatten([ [hex(H), hex(L), " "] || <<H:4, L:4>> <= Binary]).

l3(16#0806) -> 'ARP';
l3(16#0800) -> 'IPV4';
l3(16#86dd) -> 'IPV6';
l3(_V) -> unknown.

l4(16#01) -> 'ICMP';
l4(16#02) -> 'IGMP';
l4(16#3A) -> 'MLD';
l4(16#06) -> 'TCP';
l4(16#11) -> 'UDP';
l4(_V) -> unknown.

print_line_number(Line) ->
  S = integer_to_list(Line, 16),
  S1 = case length(S) of
    1 -> "000" ++ S;
    2 -> "00" ++ S;
    3 -> "0" ++ S;
    _ -> S
  end,
  io:format("~s  ", [S1]).

%% Printers
-define(BYTES_PER_LINE, 16).

print_hex(Packet) when is_binary(Packet) -> io:format("HEX dump:~n"), print_hex(Packet, 0).

print_hex(Packet, Line) when is_binary(Packet) andalso size(Packet) =< ?BYTES_PER_LINE->
  print_line_number(Line), 
  case Packet of 
    <<L:(?BYTES_PER_LINE div 2)/binary, R/binary>> ->
      io:format("~s    ", [binary_to_hexstring(L)]),
      io:format("~s~n", [binary_to_hexstring(R)]);
    Packet -> io:format("~s~n", [binary_to_hexstring(Packet)])
  end;

print_hex(<<Packet:?BYTES_PER_LINE/binary, Rest/binary>>, Line) ->
  print_hex(Packet, Line),
  print_hex(Rest, Line + ?BYTES_PER_LINE).

print_eth(<<Dst:6/bytes, Src:6/bytes, Type:16/integer, _Packet/binary>>) ->
  io:format("ETH: ===> DST: ~s | SRC: ~s| Type: ~p ~n", [binary_to_hexstring(Dst), binary_to_hexstring(Src), l3(Type)]);
print_eth(_P) -> ok.

print_ipv4(<<_MAC:12/bytes, 16#0800:16, Packet/binary>>) ->
 <<_V:4, _HLen:4, _SrvcType:8, _TotLen:16, _ID:16, _Flgs:3, _FragOff:13, _TTL:8, Proto:8, _HdrChkSum:16, 
    SrcIP_1:8, SrcIP_2:8, SrcIP_3:8, SrcIP_4:8,
    DestIP_1:8, DestIP_2:8, DestIP3:8, DestIP4:8, _Rest/binary>> = Packet,
  io:format("IPv4: ===> SRC: ~p.~p.~p.~p | DST: ~p.~p.~p.~p | Type: ~p ~n", 
    [SrcIP_1, SrcIP_2, SrcIP_3, SrcIP_4, DestIP_1, DestIP_2, DestIP3, DestIP4, l4(Proto)]);
print_ipv4(_P) -> ok.


