%% API wrapper for snffr app

-module(snffr).
-compile(export_all).


print_hex() ->
  case snffr_port:get_packet() of
    {ok, Packet} -> snffr_packet_mgr:print_hex(Packet);
    {error, Reason} -> io:format("Coudn't capture the packet: ~p~n", [Reason])
  end.
