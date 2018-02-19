## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	erl -pa ebin -s dbase start
#	cp ebin/* ../ebin;
#	erl -pa ebin -pa /home/joq/erlang/lib/ebin -s dbase start
#start_mgr:
	erl -pa ebin -run brd_mgr boot_brd_mgr -sname start_brd_mgr

start_mgr:
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa ../sd/ebin -run brd_mgr start_brd_mgr -sname start_brd_mgr
create:
	erlc -o ../../ebin_working_nodes src/*.erl;
	cp src/*.app ../../ebin_working_nodes

clean:
	rm -rf ebin/*.beam */*~ src/*.beam *~ *.dump
tar:
	tar -cvf brd_mgr.tar *

test:
	rm -rf ebin/* */*~ src/*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa ../sd/ebin -s test_brd_mgr test -sname test_brd_mgr
