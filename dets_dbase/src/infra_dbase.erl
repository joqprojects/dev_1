%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%%
%%%
%%% -------------------------------------------------------------------
-module(infra_dbase).

-behaviour(gen_server).


%% --------------------------------------------------------------------
%% Data structure
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% -include("../infra_master/include/infra.hrl").
%% --------------------------------------------------------------------
%% External exports
-define(CONFIGFILE,"service.config").


-export([latest_release/1,initiated/0,
	 init_dbase/1,
	 start_dbase/0,stop_dbase/0,
	 id_all_workers/0]).

-export([member_object/1,
	 create_object/2,delete_object/1,
	 read_object/2,update_object/2]).

-export([delete/0,exists/0,create/1,get/1,store/2,all/0,remove/1,
	 start/0,stop/0]).

-export([test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listner_port,server_setup,dbase_file,dbase_config,initiated,lsock}).

%% ====================================================================
%% External support functions
%% ====================================================================

start_dbase()->
    Load=application:load(?MODULE),
    Start=application:start(?MODULE),
    {Load,Start}.

stop_dbase()->
    Stop=application:stop(infra_dbase),
    UnLoad=application:unload(infra_dbase),
    {Stop,UnLoad}.




test()->
    Reply=test_infra_dbase:test(),
    Reply.
	
							       
%% ====================================================================
%% External Server functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
 
%%%
id_all_workers()->
    gen_server:call(?MODULE, {id_all_workers},infinity).
initiated()->
    gen_server:call(?MODULE, {initiated},infinity).
init_dbase(InitialValues)->
    gen_server:call(?MODULE, {init_dbase,InitialValues},infinity).
latest_release(ServiceStr)->
    gen_server:call(?MODULE, {latest_release,ServiceStr},infinity).  


%%%------------- CRUD ---------------------------------
member_object(Key)-> 
    gen_server:call(?MODULE, {member_object,Key},infinity).

create_object(Key,Object)-> 
    gen_server:call(?MODULE, {create_object,Key,Object},infinity).
delete_object(Key)-> 
    gen_server:call(?MODULE, {delete_object,Key},infinity).
read_object(Item,Key)-> 
    gen_server:call(?MODULE, {read_object,Item,Key},infinity).
update_object({Item,Value},Key)-> 
    gen_server:call(?MODULE, {update_object,{Item,Value},Key},infinity).


%%%
delete()-> 
    gen_server:call(?MODULE, {delete},infinity).
exists()-> 
    gen_server:call(?MODULE, {exists},infinity).
create(Type)-> 
    gen_server:call(?MODULE, {create,Type},infinity).
store(Key,Value)->
    gen_server:call(?MODULE, {store,Key,Value},infinity).     
get(Key)->
    gen_server:call(?MODULE, {get,Key},infinity).
all()->
    gen_server:call(?MODULE, {all},infinity).
remove(Key)->
    gen_server:call(?MODULE, {remove,Key},infinity).

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
    case file:consult(?CONFIGFILE) of
	{ok,ConfigInfo}->
	    {{infra_dbase,port},PortDbase}=lists:keyfind({infra_dbase,port},1,ConfigInfo),
	    {{infra_dbase,config},ConfigDbase}=lists:keyfind({infra_dbase,config},1,ConfigInfo),
	    {{infra_dbase,file_dbase},FileInfraDbase}=lists:keyfind({infra_dbase,file_dbase},1,ConfigInfo),
	    {{tcp_setup,server},ServerSetup}=lists:keyfind({tcp_setup,server},1,ConfigInfo),
	    case filelib:is_file(FileInfraDbase) of
		false->
		    Initiated=false;
		true ->
		    Initiated=true
	    end,
	    {ok,LSock}=gen_tcp:listen(PortDbase,ServerSetup),
	    spawn(fun()->par_connect(LSock) end),
	    io:format("Application Starting ~p~n",[{?MODULE}]),
	    InitResult={ok, #state{listner_port=PortDbase,
				   server_setup=ServerSetup,
				   dbase_file=FileInfraDbase,
				   dbase_config=ConfigDbase,
				   initiated=Initiated,
				   lsock=LSock}};   
	 _Err ->
	    InitResult={error, #state{}}
    end,
    InitResult.
	


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

%%------------------ CRUD -------------------------------------------
handle_call({member_object,Key},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,member_object,[Key,File]),
    {reply, Reply, State};


handle_call({create_object,Key,Object},_From,State) ->
    File=State#state.dbase_file,
    case rpc:call(node(),infra_dbase_lib,create_object,[Key,Object,File]) of
	{ok,store}->
	    Reply=ok;
	Err->
	    Reply=Err
    end,
    {reply, Reply, State};


handle_call({delete_object,Key},_From,State) ->
    File=State#state.dbase_file,
    case rpc:call(node(),infra_dbase_lib,delete_object,[Key,File]) of
	{ok,{_ObjectType,_Id}}->
	    Reply=ok;
	Err->
	    Reply=Err
    end,
    {reply, Reply, State};


handle_call({read_object,Item,Key},_From,State) ->
    Value=Item,
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,read_object,[Value,Key,File]),
    {reply, Reply, State};


handle_call({update_object,{Item,Value},Key},_From,State) ->
    NewValue={Item,Value},
    File=State#state.dbase_file,
    case rpc:call(node(),infra_dbase_lib,update_object,[NewValue,Key,File]) of
	{ok,store}->
	    Reply=ok;
	Err->
	    Reply=Err
    end,

    {reply, Reply, State};


%%---------- End CRUD ----------------------------



handle_call({latest_release,ServiceStr},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,latest_release,[ServiceStr,File]),
    {reply, Reply, State};


handle_call({id_all_workers},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,id_all_workers,[File]),
    {reply, Reply, State};

handle_call({initiated},_From,State) ->
    Reply=State#state.initiated,
    {reply, Reply, State};


handle_call({init_dbase,InitialValues},_From,State) ->
    case State#state.initiated of
	false->
	    File=State#state.dbase_file,
	    Config=State#state.dbase_config,
	    case rpc:call(node(),infra_dbase_lib,create,[Config,File]) of
		{badrpc,Err}->
		    NewState=State,
		    Reply={error,{?MODULE,?LINE,Err}};
		{ok,file_created}->
		    case dets:open_file(File) of
			{error,Err}->
			    NewState=State,
			    Reply={error,{?MODULE,?LINE,Err}};
			{ok,Ref}->	    
			    true=dets:insert_new(Ref,InitialValues),
			    ok=dets:close(Ref),
			    NewState=State#state{initiated=true},
			    Reply=ok		    
		    end
	    end;
	true->
	    Reply={error,{?MODULE,?LINE,already_initiated}},
	    NewState=State
    end,
		   
    {reply, Reply, NewState};


handle_call({delete},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,delete,[File]),
    {reply, Reply, State};

handle_call({exists},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,exists,[File]),
    {reply, Reply, State};

handle_call({create,Type},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,create,[Type,File]),
    {reply, Reply, State};

handle_call({store,Key,Value},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,store,[Key,Value,File]),
    {reply, Reply, State};

handle_call({get,Key},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,get,[Key,File]),
    {reply, Reply, State};    

handle_call({all},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,all,[File]),
    {reply, Reply, State};

handle_call({remove,Key},_From,State) ->
    File=State#state.dbase_file,
    Reply=rpc:call(node(),infra_dbase_lib,remove,[Key,File]),
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
par_connect(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),  
    spawn(fun()->par_connect(LSock) end),
    loop(Socket).

loop(Socket)->
    receive
	{tcp,Socket,Bin}->
	    case binary_to_term(Bin) of
		{MsgRef,dets,F,A}->
		    Result=rpc:call(node(),?MODULE,dets,[F,A]),
		    ReplyBin=term_to_binary({MsgRef,Result}),
		    gen_tcp:send(Socket,ReplyBin),
		    loop(Socket);
		{MsgRef,Service,Request,Args}->
		    io:format("~p~n",[{?MODULE,?LINE,Service,Request,Args}]),
		    Result=rpc:call(node(),Service,Request,Args),
		    ReplyBin=term_to_binary({MsgRef,Result}),
		    gen_tcp:send(Socket,ReplyBin),
		    loop(Socket);
		Err->
		  %  ReplyBin=Err,
		    ReplyBin=term_to_binary({error,{?MODULE,?LINE,Err}}),
		    gen_tcp:send(Socket,ReplyBin),
		    loop(Socket)
	    end;
	{tcp_closed,Socket} ->
	    tcp_closed;
	   % io:format("Socket closed ~p~n",[{?MODULE,?LINE,Socket}]);
	_NotImplemented->
	    loop(Socket)
    after 1000*60*10 ->       % Kill if inactive in 10 mins
	    inactive_too_long
    end.
