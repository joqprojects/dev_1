## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s infra_master_lib start -sname master_node 

build:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin
create:
	erlc -o ../ebin_working_nodes src/*.erl;
	cp src/*.app ../ebin_working_nodes
clean:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump
start:
	erl -pa ebin -pa ../sd/ebin -run infra_master start_infra -sname infra_master
tar:
	tar -cvf infra_master.tar *

test:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa ../sd/ebin ../infra_dbase/ebin -s test_infra_master test -sname infra_master
