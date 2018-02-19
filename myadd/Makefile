## set the paths for a default setup

all:
	rm -rf ebin/* */*~ src/*.beam *~ *.dump
	erlc -o ebin src/*.erl;
	cp src/*.app ebin
#	cp ebin/* ../ebin;
#	erl -pa ebin -pa /home/joq/erlang/lib/ebin -s dbase start

create:
	erlc -o ../../ebin_working_nodes src/*.erl;
	cp src/*.app ../../ebin_working_nodes

clean:
	rm -rf ebin/*.beam */*~ src/*.beam *~ *.dump
tar:
	tar -cvf myadd.tar *

test:
	rm -rf ebin/* */*~ src/*.beam *~ *.dump
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s test_myadd test -sname test_myadd
