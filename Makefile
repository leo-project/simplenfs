REBAR=$(shell which rebar 2>/dev/null || echo ./rebar)

all:
	@$(REBAR) update-deps
	@$(REBAR) get-deps
	@$(REBAR) compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://github.com/downloads/basho/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean
