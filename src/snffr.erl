%% API wrapper for snffr app

-module(snffr).
-compile(export_all).


attach(Iface) ->
  snffr_port:attach(Iface).

packet() ->
  case snffr_port:get_packet() of
    {ok, [Packet]} -> Packet;
    {error, Reason} = RC -> io:format("Coudn't capture the packet: ~p~n", [Reason]), RC
  end.

add_printer(Fun) when is_atom(Fun) ->
  AllPrinters = [ Name || {Name, _Arity} <- snffr_utils:module_info(exports), Name /= module_info ],
  case lists:member(Fun, AllPrinters) of 
    true ->
      snffr_packet_mgr:add_printer(fun snffr_utils:Fun/1), ok;
    _ -> {error, invalid}
  end;

add_printer(Funs) when is_list(Funs) ->
  [add_printer(Fun) || Fun <- Funs].

print(Packet) when is_binary(Packet) ->
  snffr_packet_mgr:print(Packet);

print(Other) -> io:format("~p doesnt appear to be a binary", [Other]).
