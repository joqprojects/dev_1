%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% 
%%% -------------------------------------------------------------------
-module(test_mem_dbase).

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
  %  application:load(mem_dbase),
   % application:start(mem_dbase),
    ok.

create_test()->
    {ok,TableId}=mem_dbase:create([bag],?MODULE),
    true=mem_dbase:exists(TableId),
    {ok,T2}=mem_dbase:create([bag],?MODULE),
    true=mem_dbase:exists(T2),
    ok =mem_dbase:delete(TableId),
    ok =mem_dbase:delete(T2),
    {error,{undefined,_,Module,Line}}=mem_dbase:exists(TableId),
    {error,{undefined,_,Module,Line}}=mem_dbase:exists(T2),
    ok.

create_negative_test()->
    {error,
     {{'EXIT',{badarg,_info}},
      _TableId,_Module,_Line}}
	=mem_dbase:create([glurk],?MODULE),
    {error,{{'EXIT',{badarg,_}},_,_,_}}	=mem_dbase:create(glurk,?MODULE),
    {error,{{'EXIT',{badarg,_}},_,_,_}}	=mem_dbase:create([set],"glurk"),
    ok.

store__get_all_test()->
    {ok,TableId}=mem_dbase:create([set],?MODULE),
    R=#mytest{id=stugan_1,rev="1-2-1",desc=rpi3_b,geo=living_room},
    true=mem_dbase:store({mytest,R},TableId),	
    {ok,[{mytest,Value}]}=mem_dbase:get(mytest,TableId),
    #mytest{id=stugan_1,rev="1-2-1",desc=rpi3_b,geo=living_room}=Value,
    true=mem_dbase:store([],TableId),
    {ok,[{mytest,Value}]}=mem_dbase:get(mytest,TableId),
    {ok,[]}=mem_dbase:get(noentry,TableId),
    true=mem_dbase:store({test2,ok},TableId),

    {ok,
     [{test2,ok},
      {mytest,{mytest,stugan_1,"1-2-1",rpi3_b,living_room}}]}=mem_dbase:all(TableId),
    ok =mem_dbase:delete(TableId),
    ok.

store__get_all_negative_test()->
    {ok,TableId}=mem_dbase:create([set],?MODULE),
    R=#mytest{id=stugan_1,rev="1-2-1",desc=rpi3_b,geo=living_room},
    true=mem_dbase:store({mytest,R},TableId),	
    {ok,[{mytest,Value}]}=mem_dbase:get(mytest,TableId),
    #mytest{id=stugan_1,rev="1-2-1",desc=rpi3_b,geo=living_room}=Value,

    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:get(mytest,glurk),	
    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:get(glurk,glurk),

    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:all(glurk),	
    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:all("glurk"),

    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:store({mytest,ok},glurk),	
    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:store(glurk,TableId),
    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:store([glurk],TableId),
    ok =mem_dbase:delete(TableId),
    ok.

exists_test()->
    {error,{undefined,glurk,_Mem_dbase_lib,_LINE_58}}=mem_dbase:exists(glurk),
    {ok,TableId}=mem_dbase:create([set],?MODULE),
    true=mem_dbase:exists(TableId),
    {error,{undefined,?MODULE,_Mem_dbase_lib,_LINE_58}}=mem_dbase:exists(?MODULE),
    ok =mem_dbase:delete(TableId),
    ok.

remove_test()->    
    {ok,TableId}=mem_dbase:create([set],?MODULE),
    true=mem_dbase:store([{a,1},{b,2}],TableId),
    {ok,[{b,2},{a,1}]}=mem_dbase:all(TableId),
    true=mem_dbase:remove(a,TableId),
    true=mem_dbase:remove(z,TableId),
    true=mem_dbase:remove([],TableId),
    {ok,[{b,2}]}=mem_dbase:all(TableId),
    ok =mem_dbase:delete(TableId),
    ok.

remove_negative_test()->    
    {ok,TableId}=mem_dbase:create([set],?MODULE),
    true=mem_dbase:store([{a,1},{b,2}],TableId),
    {ok,[{b,2},{a,1}]}=mem_dbase:all(TableId),
    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:remove(a,?MODULE),
    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:remove(a,glurk),
    {error,{{'EXIT',{badarg,_}},_,_,_}}=mem_dbase:remove([a],glurk),
    {ok,[{b,2},{a,1}]}=mem_dbase:all(TableId),
    ok =mem_dbase:delete(TableId),
    ok.

member_test()->    
    {ok,TableId}=mem_dbase:create([set],?MODULE),
    true=mem_dbase:store([{a,1},{b,2}],TableId),
    {ok,[{b,2},{a,1}]}=mem_dbase:all(TableId),
    true=mem_dbase:member(a,TableId),
    false=mem_dbase:member(z,TableId),
    false=mem_dbase:member([],TableId),
    ok =mem_dbase:delete(TableId),
    ok.

added_11_test()->
    ok.

stop_test()->    
   % application:stop(mem_dbase),
  %  application:unload(mem_dbase),
    %spawn(fun()->kill_session() end),
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
