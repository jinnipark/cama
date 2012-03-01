REBAR=./rebar

all:
	$(REBAR) compile

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean

test: all
	$(REBAR) eunit

