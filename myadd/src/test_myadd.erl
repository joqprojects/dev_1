%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% 
%%% -------------------------------------------------------------------
-module(test_myadd).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

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
%    application:load(myadd),
 %   application:start(myadd),
    ok.

add_test()->
    3=rpc:call(node(),myadd,add,[1,2]),
    ok.

add_crash_test()->
    {badrpc,
     {'EXIT',
      {badarith,_}}}=rpc:call(node(),myadd,add,[glurk,2]),
    {badrpc,
     {'EXIT',
      {undef,_}}}=rpc:call(node(),myadd,add,[]),
    {badrpc,
     {'EXIT',
      {undef,_}}}=rpc:call(node(),myadd,add,[1,2,3]),
    ok.

divi_test()->
    5.0=rpc:call(node(),myadd,divi,[10,2]),
    ok.

divi_crash_test()->
    {badrpc,
     {'EXIT',
      {badarith,_}}}=rpc:call(node(),myadd,divi,[10,0]),
    {badrpc,
     {'EXIT',
      {undef,_}}}=rpc:call(node(),myadd,divi,[]),
    {badrpc,
     {'EXIT',
      {undef,_}}}=rpc:call(node(),myadd,divi,[1,2,3]),
    ok.

added_7_test()->
    ok.

added_8_test()->
    ok.

stop_test()->    
  %  application:stop(myadd),
  %  application:unload(myadd),
  %  spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(100),
    erlang:halt().

%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
