%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(mem_dbase_lib).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([member/2,delete/1,exists/1,create/2,get/2,store/2,all/1,remove/2]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
member(Key,TableId) ->
    Reply=case rpc:call(node(),ets,member,[TableId,Key]) of
	      undefined->
		  {error,{undefined,TableId,?MODULE,?LINE}};
	      {badrpc,Reason}->
		  {error,{Reason,TableId,?MODULE,?LINE}};
	      Result->
		  Result
    end,
    Reply.



delete(TableId) ->
    Reply=case rpc:call(node(),ets,delete,[TableId]) of
	      undefined->
		  {error,{undefined,TableId,?MODULE,?LINE}};
	      {badrpc,Reason}->
		  {error,{Reason,TableId,?MODULE,?LINE}};
	      _->
		  ok
    end,
    Reply.

exists(TableId) ->
    Reply= case rpc:call(node(),ets,info,[TableId]) of
	       undefined->
		   {error,{undefined,TableId,?MODULE,?LINE}};
	       {badrpc,Reason}->
		   {error,{Reason,TableId,?MODULE,?LINE}};
	       _->
		   true
	   end,
    Reply.

create(Type,TableName) ->
    Reply=case rpc:call(node(),ets,new,[TableName,Type]) of
	      undefined->
		  {error,{undefined,TableName,?MODULE,?LINE}};
	      {badrpc,Reason}->
		  {error,{Reason,TableName,?MODULE,?LINE}};
	      TableId->
		  {ok,TableId}
	  end,
    Reply.


store(ObjectOrObjects,TableId) ->
    Reply=case rpc:call(node(),ets,insert,[TableId,ObjectOrObjects]) of
	      undefined->
		  {error,{undefined,TableId,?MODULE,?LINE}};
	      {badrpc,Reason}->
		  {error,{Reason,TableId,?MODULE,?LINE}};
	      Result->
		  Result
    end,
    Reply.

get(Key,TableId) ->
    Reply=case rpc:call(node(),ets,lookup,[TableId,Key]) of
	      undefined->
		  {error,{undefined,TableId,?MODULE,?LINE}};
	      {badrpc,Reason}->
		  {error,{Reason,TableId,?MODULE,?LINE}};
	      Result->
		  {ok,Result}
    end,
    Reply.

all(TableId) ->
    Reply=case rpc:call(node(),ets,tab2list,[TableId]) of
	      undefined->
		  {error,{undefined,TableId,?MODULE,?LINE}};
	      {badrpc,Reason}->
		  {error,{Reason,TableId,?MODULE,?LINE}};
	      Result->
		  {ok,Result}
    end,
    Reply.

remove(Key,TableId) ->
    Reply=case rpc:call(node(),ets,delete,[TableId,Key]) of
	      undefined->
		  {error,{undefined,TableId,?MODULE,?LINE}};
	      {badrpc,Reason}->
		  {error,{Reason,TableId,?MODULE,?LINE}};
	      Result->
		  Result
    end,
    Reply.   


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
