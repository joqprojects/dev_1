## set the paths for a default setup

all:
	erl -pa ebin -pa ../dev_area/sd/ebin -run repo_mgr start_repo -sname repo_mgr 

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
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin *.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa -s infra_master_test start -name infra_master@home.joqhome.eu
tar:
	tar -cvf infra_master.tar *

test:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa ../dev_area/sd/ebin  -s test_repo_mgr test -sname repo_mgr
