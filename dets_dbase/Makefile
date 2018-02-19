## set the paths for a default setup

all:
	erl -pa ebin -s infra_dbase start_dbase -sname infra_dbase
#	cp ebin/* ../ebin;
#	erl -pa ebin -pa /home/joq/erlang/lib/ebin -s dbase start

clean:
	rm -rf ebin/*.beam */*~ src/*.beam *~ *.dump
tar:
	tar -cvf infra_dbase.tar *

test:
	rm -rf ebin/* */*~ src/*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa ../sd/ebin -s test_infra_dbase test -sname test_infra_dbase
