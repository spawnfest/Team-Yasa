all: app

app: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

test: 
	@./rebar eunit skip_deps=true
dist-clean: clean