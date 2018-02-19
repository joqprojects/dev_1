## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	erl -pa ebin -s dbase start
#	cp ebin/* ../ebin;
#	erl -pa ebin -pa /home/joq/erlang/lib/ebin -s dbase start

create:
	erlc -o ../../ebin_working_nodes src/*.erl;
	cp src/*.app ../../ebin_working_nodes

clean:
	rm -rf ebin/*.beam */*~ src/*.beam *~ *.dump
tar:
	tar -cvf mem_dbase.tar *

test:
	rm -rf ebin/* */*~ src/*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s test_mem_dbase test -sname test_mem_dbase
