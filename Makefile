
all:
	rebar3 compile 

clean:
	rebar3 clean

console:
	PWD=`pwd`; cd _build/default/lib/snffr; sudo erl -pa ../snffr/ebin -eval "application:start(snffr)"; cd $(PWD)
