%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_repo_mgr).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").


%% --------------------------------------------------------------------
-export([]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------
start_test()->
    {ok,ok}=repo_mgr:start_repo(),
       %check if dbase is alive and kickin
    'infra_dbase@joq-desktop'=sd:call(dbase,erlang,node,[],1000),
    ok.

build_release_ok_100_test()->
    DevAreaDir="/home/joq/erlang/dist_erlang_4-0-0/dev/dev_area",
    Release="1.0.0",
    ok=repo_mgr:build_release(Release,DevAreaDir),
    ok.
stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
