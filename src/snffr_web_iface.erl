-module(snffr_web_iface).
-export([start_link/0,service/3, stop/0]).

start_link() ->
 inets:start(httpd, [
  {modules, [
   mod_alias, 
   mod_auth, 
   mod_esi, 
   mod_actions, 
   mod_cgi, 
   mod_dir, 
   mod_get, 
   mod_head, 
   mod_log, 
   mod_disk_log
  ]},
  {port,8081},
  {server_name,"snffr_web_iface"},
  {server_root,"log"},
  {document_root,"www"},
  {erl_script_alias, {"/erl", [snffr_web_iface]}},
  {error_log, "error.log"},
  {security_log, "security.log"},
  {transfer_log, "transfer.log"},
  {mime_types,[
   {"html","text/html"},
   {"css","text/css"},
   {"js","application/x-javascript"}
  ]}
 ]).
 

service(SessionID, _Env, _Input) ->
  PacketStr = case snffr_packet_mgr:get_last_packet() of
    {error, no_packets} -> "No packets to display";
    {ok, Packet} -> snffr_utils:print_hex(Packet)
  end,
  Body = [case El of "\n" -> "<br/>"; _ -> El end || El <- PacketStr],
  mod_esi:deliver(SessionID, 
    [
      "Content-Type: text/html\r\n\r\n", 
      "<html><body>", Body, "</body></html>"
    ]).

stop() ->
  io:format("stopping the web iface~n"),
  inets:stop(httpd),
  inets:stop(). 
