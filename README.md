# Snffr 

Snffr is a simple Erlang application with a C-port interface to [libpcap]. It was designed to demonstrate one of Erlang's most powerful features - binary pattern matching. The idea was to create an environment where a packet can be parsed and printed to the console using just a few lines of code. 

For example, this function prints some fields from an IP header and produces no output if the binary doesn't match:

```erlang
print_ipv4(<<_MAC:12/bytes, 16#0800:16, Packet/binary>>) ->
 <<_V:4, _HLen:4, _SrvcType:8, _TotLen:16, _ID:16, _Flgs:3, _FragOff:13, _TTL:8, Proto:8, _HdrChkSum:16,
    SrcIP_1:8, SrcIP_2:8, SrcIP_3:8, SrcIP_4:8,
    DestIP_1:8, DestIP_2:8, DestIP3:8, DestIP4:8, _Rest/binary>> = Packet,
  io:format("IPv4: ===> SRC: ~p.~p.~p.~p | DST: ~p.~p.~p.~p | Type: ~p ~n",
    [SrcIP_1, SrcIP_2, SrcIP_3, SrcIP_4, DestIP_1, DestIP_2, DestIP3, DestIP4, l4(Proto)]);
print_ipv4(_P) -> ok.
```

Module [snffr_utils.erl] contains more examples of such printer funtions. More functions like this can be added to the module as long as they take a binary as a parametter and produce some output depending on the binary's contents.

### Dependencies

Snffr was built using:
* [Erlang/OTP] - capture post-processing  
* [libpcap] - capture IP packets
* [rebar3] - build system
* [ostinato] - traffic generator for testing

### Build

```sh
$ git clone ttps://github.com/nkrylov/snffr.git 
$ cd snffr
$ make
```

### Use in Elang console
```sh
$ make console
```

### Usage example

```erlang
snffr:attach(enp0s3). %% start listening on enp0s3
snffr:add_printer([print_eth, print_hex]). %% make snffr:print() print Ethernet header and HEX dump
snffr:print(snffr:packet()). %% grab a packet and print it
```

### Console output example

```code
1> snffr:attach(enp0s3).
attching to enp0s3
                  {ok,[<<"enp0s3">>]}
2> snffr:add_printer([print_hex,print_eth,print_ipv4]).
[ok,ok,ok]
3> snffr:print(snffr:packet()).
IPv4: ===> SRC: 192.168.0.1 | DST: 192.169.0.2 | Type: 'UDP' 
ETH: ===> DST: 01 02 03 04 05 06  | SRC: 0A 0B 0C 0D 0E 0F | Type: 'IPV4' 
HEX dump:
0000  01 02 03 04 05 06 0A 0B     0C 0D 0E 0F 08 00 45 00 
0010  00 1C 04 D2 00 00 7F 11     B5 AA C0 A8 00 01 C0 A9 
0020  00 02 FF FF FF FF 00 08     7E 89 00 00 00 00 00 00 
0030  00 00 00 00 00 00 00 00     00 00 00 00 00 00 00 00 
0040  00 00 00 00 00 00 00 00     00 00 00 00 00 00 00 00 
0050  00 00 00 00 00 00 00 00     00 00 00 00 00 00 00 00 
0060  00 00 00 00 00 00 00 00     00 00 00 00 00 00 00 00 
0070  00 00 00 00 00 00 00 00     00 00 00 00 
ok
4>
```
### TODOs:

 - More examples
 - Tests
 - Use YAWS to display captures on a web page


   [Erlang/OTP]: <http://http://www.erlang.org/>
   [libpcap]: <http://www.tcpdump.org/release/libpcap-1.7.4.tar.gz>
   [rebar3]: <https://www.rebar3.org/>
   [git-repo-url]: <https://github.com/nkrylov/snffr.git>
   [ostinato]: <http://ostinato.org/>
   [snffr_utils.erl]: <https://github.com/nkrylov/snffr/blob/master/src/snffr_utils.erl> 
