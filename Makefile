REBAR=$(shell which rebar 2>/dev/null || echo ./rebar)

.PHONY: deps compile

all: gen_nfs
	@$(REBAR) compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://github.com/downloads/basho/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

deps: $(REBAR)
	@$(REBAR) get-deps

compile: $(REBAR)
	@$(REBAR) compile

gen_rpc: deps
	(cd deps/erpcgen/;make rpc)

gen_nfs: gen_rpc
	./deps/erpcgen/priv/erpcgen -a [svc_callback,xdr,hrl] src/nfs_prot3.x
	./deps/erpcgen/priv/erpcgen -a [svc_callback,xdr,hrl] src/mount3.x

clean: $(REBAR)
	@$(REBAR) clean
