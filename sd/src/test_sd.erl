%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% 
%%% -------------------------------------------------------------------
-module(test_sd).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-record(mytest,{
	  id,
	  rev,
	  desc,
	  geo}).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).
%%
%% API Functions
%%


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
start_test()->
    application:load(sd),
    application:start(sd),
    ok.

insert_remove_test()->
    true=sd: add_resource("sim_service","localhost",20001),
    [{"sim_service","localhost",20001}]=sd:resources("sim_service"),
    []=sd:resources("glurk"),
    true=sd:add_resource("remove_test","localhost",20011),
    [{"remove_test","localhost",20011}]=sd:resources("remove_test"),
    true=sd:delete_resource("glurk","localhost",20011),
    true=sd:delete_resource("remove_test","localhost",glurk),
    true=sd:delete_resource("remove_test","glurk",20011),
    
    [{"remove_test","localhost",20011}]=sd:resources("remove_test"),
    true=sd:delete_resource("remove_test","localhost",20011),
    []=sd:resources("remove_test"),
    ok.

start_listner_test()->
    [{{tcp_setup,server},ServerSetUp}]=sd:resources({tcp_setup,server}),
    [{"sim_service",IpAddr,Port}]=sd:resources("sim_service"),
    _Pid=spawn(fun()->sim_infra_dbase(Port,ServerSetUp) end),
    ok.

send_msg_2_test()->
    TimeOut=1000,
    Date=date(),
    Date=sd:call("sim_service",erlang,date,[],TimeOut),
    ok.

    
stop_test()->    
    application:stop(sd),
    application:unload(sd),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.

%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
sim_infra_dbase(Port,ServerSetUp)->
    {ok,LSock}=gen_tcp:listen(Port,ServerSetUp),  
    {ok,Socket}=gen_tcp:accept(LSock),  
    loop(Socket).

loop(Socket)->
    receive
	{tcp,Socket,Bin}->
	    {MsgRef,Service,Request,Args}=binary_to_term(Bin),
	    Result=rpc:call(node(),Service,Request,Args),
	    ReplyBin=term_to_binary({MsgRef,Result}),
	    gen_tcp:send(Socket,ReplyBin),
	    loop(Socket);
	{tcp_closed,Socket} ->
	   tcp_closed;
	{Pid,check}->
	    Pid!{self(),ok},
	    loop(Socket)
    end.



send_receive(Socket,Msg)->
    gen_tcp:send(Socket,term_to_binary(Msg)),
    receive
	{tcp,Socket,Bin}->
	    Result=binary_to_term(Bin);
	{tcp_closed,Socket} ->
	    Result={error,tcp_closed,Socket}
    after 500 ->
	    Result={error,timeout}
    end,
    Result.
