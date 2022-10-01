.PHONY: all deps beam game

game: beam

all: deps beam

beam:
	@erl -make

deps:
	cd deps && escript rebar compile

clean:
	@rm ebin/*.beam

tar:
	@tar zcvf ebin.tar.gz ebin -C deps/Emysql ebin \
		-C ../goldrush/ ebin \
		-C ../lager/ ebin \
		-C ../luerl/ ebin \
		-C ../mochiweb/ ebin \
		-C ../protobuffs/ ebin \
		-C ../recon/ ebin \
		-C ../percent_encoding/ ebin \
		-C ../rfc4627_jsonrpc/ ebin \
		-C ../rsrc_pool/ ebin \
		-C ../sgte/ ebin

run:
	@start werl -pa deps/*/ebin -pa ./ebin -name dev@192.168.1.5 -setcookie tianshu \
		-s lager_test test -boot start_sasl
