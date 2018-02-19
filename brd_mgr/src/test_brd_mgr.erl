%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description : Test brd_mgr 
%%% Connect to infra_master and starts q_mgr
%%% 
%%% -------------------------------------------------------------------
-module(test_brd_mgr).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-define (BRDCONFIG,"include/brd.config").

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
boot_test()->
    {ok,ok}=brd_mgr:start_brd_mgr(),
    ok.
start_sd_test()->
    ok=application:load(sd),
    ok=application:start(sd),
    case file:consult("service.config") of
	{ok,ConfigInfo}->
	    {{brd_mgr,port},PortBrdMgr}=lists:keyfind({brd_mgr,port},1,ConfigInfo),
	    true=sd:add_resource("test_brd_mgr","localhost",PortBrdMgr)
    end,
    ok.

initial_connect_test()->
    standby=sd:call("test_brd_mgr",brd_mgr,connect,[],1000),
    ok.

activated_test()->
    active=sd:call("test_brd_mgr",brd_mgr,activate,[],1000),
    ok.

activated_connect_test()->
    active=sd:call("test_brd_mgr",brd_mgr,connect,[],1000),
    ok.

activate_when_activate_test()->
    active=sd:call("test_brd_mgr",brd_mgr,connect,[],1000),
    ok.

worker_test()->
    Date=date(),
    Date=sd:call("test_brd_mgr",erlang,date,[],1000),
    ok.


stop_test()->
    spawn(fun()->kill_session() end),
    ok.
		  
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.

%%-----------------------------------------------------------------------
%% Local Functions
%%----------------------------------------------------------------------
