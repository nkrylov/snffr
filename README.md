# Snffr 

Snffr is a simple Erlang application with a C-port interface to libpcap. It was design to demonstrate one of Erlang's most powerful features - binary pattern matching. For example, after receiving a packet in binary form from libpcap, we can convert it to a HEX string using just a few lines of code:

```erlang
hex(C) when C < 10 -> $0 + C;
hex(C) -> $A + C - 10.
binary_to_hexstring(Binary) when is_binary(Binary) -> 
 lists:flatten([ [hex(H), hex(L)] || <<H:4, L:4>> <= Binary]).
```


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

### Todos

 - More examples
 - Tests
 - Use YAWS to display captures on a web page


   [Erlang/OTP]: <http://http://www.erlang.org/>
   [libpcap]: <http://www.tcpdump.org/release/libpcap-1.7.4.tar.gz>
   [rebar3]: <https://www.rebar3.org/>
   [git-repo-url]: <https://github.com/nkrylov/snffr.git>
   [ostinato]: <http://ostinato.org/>
