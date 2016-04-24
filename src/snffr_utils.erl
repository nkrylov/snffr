-module(snffr_utils).

-compile(export_all).

hex(C) when C < 10 -> $0 + C;
hex(C) -> $A + C - 10.

%% Helper functions
binary_to_hexstring(Binary) when is_binary(Binary) ->
  lists:flatten([ [hex(H), hex(L), " "] || <<H:4, L:4>> <= Binary]).


%% Printers
print_hex(Packet) when is_binary(Packet) ->
  Str = binary_to_hexstring(Packet),
  io:format("~s~n", [Str]).
