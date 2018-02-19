%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(mem_dbase).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([member/2,delete/1,exists/1,create/2,get/2,store/2,all/1,remove/2,start/0,stop/0]).

-export([test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External support functions
%% ====================================================================

test()->
    Reply=test_mem_dbase:test(),
    Reply.

%% ====================================================================
%% External Server functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
 
member(Key,TableId)->
    gen_server:call(?MODULE, {member,Key,TableId},infinity).

delete(TableId)-> 
    gen_server:call(?MODULE, {delete,TableId},infinity).
exists(TableId)-> 
    gen_server:call(?MODULE, {exists,TableId},infinity).
create(Type,TableName)-> 
    gen_server:call(?MODULE, {create,Type,TableName},infinity).
store(ObjectOrObjects,TableId)->
    gen_server:call(?MODULE, {store,ObjectOrObjects,TableId},infinity).     
get(Key,TableId)->
    gen_server:call(?MODULE, {get,Key,TableId},infinity).
all(TableId)->
    gen_server:call(?MODULE, {all,TableId},infinity).
remove(Key,TableId)->
    gen_server:call(?MODULE, {remove,Key,TableId},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    io:format("Application Starting ~p~n",[?MODULE]),
    {ok, #state{}}.

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
handle_call({member,Key,TableId},_From,State) ->
    Reply=rpc:call(node(),mem_dbase_lib,member,[Key,TableId]),
    {reply, Reply, State}; 

handle_call({delete,TableId},_From,State) ->
    Reply=rpc:call(node(),mem_dbase_lib,delete,[TableId]),
    {reply, Reply, State};

handle_call({exists,TableId},_From,State) ->
    Reply=rpc:call(node(),mem_dbase_lib,exists,[TableId]),
    {reply, Reply, State};

handle_call({create,Type,TableName},_From,State) ->
    Reply=rpc:call(node(),mem_dbase_lib,create,[Type,TableName]),
    {reply, Reply, State};

handle_call({store,ObjectOrObjects,TableId},_From,State) ->
    Reply=rpc:call(node(),mem_dbase_lib,store,[ObjectOrObjects,TableId]),
    {reply, Reply, State};

handle_call({get,Key,TableId},_From,State) ->
    Reply=rpc:call(node(),mem_dbase_lib,get,[Key,TableId]),
    {reply, Reply, State};    

handle_call({all,TableId},_From,State) ->
    Reply=rpc:call(node(),mem_dbase_lib,all,[TableId]),
    {reply, Reply, State};

handle_call({remove,Key,TableId},_From,State) ->
     Reply=rpc:call(node(),mem_dbase_lib,remove,[Key,TableId]),
    {reply, Reply, State};

% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: get_all/0
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
