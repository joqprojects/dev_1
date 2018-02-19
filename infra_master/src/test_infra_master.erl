%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_infra_master).

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

start_dbase_sd_dbase_test()->
%    file:delete("infra_dbase.file"),
 %   c:cd("../infra_dbase"),
  %  {ok,ok}=infra_dbase:start_dbase(),
   % c:cd("../infra_master"),
    ok.

start_test()->
    {ok,ok}=infra_master:start_infra(),
    timer:sleep(300),

%   ok=application:load(sd),
  %  ok=application:start(sd),

    true=sd:add_resource(master,"80.216.90.159",10010),
   [{master,"80.216.90.159",10010}]=sd:resources(master),
   [{dbase,"80.216.90.159",10000}]=sd:resources(dbase),
    ok.

id_all_workers_test()->
    {ok,["w2","w0","w1"]}=sd:call(dbase,infra_dbase,id_all_workers,[],1000),
    [{brd_ipaddr,"80.216.90.159"},
     {brd_port,10521},
     {worker_port,10522}]=sd:call(dbase,infra_dbase,read_object,[object,{worker,"w2"}],5000),
    [{brd_ipaddr,"80.216.90.159"},
     {brd_port,10511},
     {worker_port,10512}]=sd:call(dbase,infra_dbase,read_object,[object,{worker,"w1"}],5000),
    [{brd_ipaddr,"80.216.90.159"},
     {brd_port,10501},
     {worker_port,10502}]=sd:call(dbase,infra_dbase,read_object,[object,{worker,"w0"}],5000),
    ok.


stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.

%%-----------------------------------------------------
