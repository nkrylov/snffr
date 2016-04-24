%% API wrapper for snffr app

-module(snffr).
-compile(export_all).


attach(Iface) ->
  snffr_port:attach(Iface).

packet() ->
  case snffr_port:get_packet() of
    {ok, [Packet]} -> Packet;
    {error, Reason} -> io:format("Coudn't capture the packet: ~p~n", [Reason])
  end.

add_printer(Fun) ->
  snffr_packet_mgr:add_printer(Fun).

print(Packet) when is_binary(Packet) ->
  snffr_packet_mgr:print(Packet).
