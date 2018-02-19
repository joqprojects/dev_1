%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(infra_master).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("/home/joq/erlang/dist_erlang_pc1/infra/include/infra.hlr").
-include_lib("kernel/include/file.hrl").
-include("../include/infra.hrl").
%% --------------------------------------------------------------------
-define(POLL_TIME,60*1000).
-define(CONFIGFILE,"service.config").

%% External exports Support functions


%-export([have_capability/2,have_capabilities/2]).
-export([start_infra/0,stop_infra/0]).

%% External exports Gen server

% Administrate workers

%-export([add_worker/2,delete_worker/1,update_worker/2,read_info/1]).

-export([connect_workers/1,connect/4,get_active/0,get_allworkers/0,

	 worker_info/2,
	 get_candidate_workers/1,
	 release_worker/1,
	 active_workers/0,passive_workers/0,
	 find_worker/1]).

-export([start/0,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {active,disconnected,allWorkers,listner_port,port_dbase,ipaddr_dbase,server_setup,lsock}).
-record(worker,{id,addr,room,cpu,ram,disc,num_allocated,capabilities}).
%% ====================================================================
%% External functions
%% ====================================================================

start_infra()->
    Load=application:load(?MODULE),
    Start=application:start(?MODULE),
    {Load,Start}.

stop_infra()->
    ok=application:stop(infra_master),
    ok=application:unload(infra_master),
    ok.


%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

%% Final :-)

connect_workers(Interval)->
        gen_server:cast(?MODULE, {connect_workers,Interval}).
% 
connect(IdBrd,IpAddrWorker,PortBrd,PortWorker)->
    gen_server:call(?MODULE, {connect,IdBrd,IpAddrWorker,PortBrd,PortWorker},infinity).

get_active()->
        gen_server:call(?MODULE, {get_active},infinity).
get_allworkers()->
        gen_server:call(?MODULE, {get_allworkers},infinity).

%%%%%%%%%%%

worker_info(WorkerId,Info)->
     gen_server:call(?MODULE,{worker_info,WorkerId,Info},infinity).

get_candidate_workers(NeededCapabilities)->
    gen_server:call(?MODULE, {get_candidate_workers,NeededCapabilities},infinity).
    
find_worker(WorkerId)->
    gen_server:call(?MODULE, {find_worker,WorkerId},infinity).

active_workers()->
    gen_server:call(?MODULE, {active_workers},infinity).

passive_workers()->
    gen_server:call(?MODULE, {passive_workers},infinity).

%connected_workers()->
 %   gen_server:call(?MODULE, {connected_workers},infinity).

release_worker(WorkerId)->
    gen_server:cast(?MODULE, {release_worker,WorkerId}).


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
% dict:fetch(oam_rpi3,D1).
% [{brd_ip_port,"80.216.90.159"},
% {port,6001},
% {worker_ip_port,"80.216.90.159"},
%  {port,6002}]
%
%% --------------------------------------------------------------------
init([]) ->
    case file:consult(?CONFIGFILE) of
	{ok,ConfigInfo}->
	    {{infra_master,port},PortMaster}=lists:keyfind({infra_master,port},1,ConfigInfo),
	    {{infra_master,initial_file},InitialFile}=lists:keyfind({infra_master,initial_file},1,ConfigInfo),
	    {{infra_master,hearbeat_interval},HeartBeatInterval}=lists:keyfind({infra_master,hearbeat_interval},1,ConfigInfo),
	    {{infra_dbase,port},PortDbase}=lists:keyfind({infra_dbase,port},1,ConfigInfo),
	    {{infra_dbase,ip_addr},IpAddrDbase}=lists:keyfind({infra_dbase,ip_addr},1,ConfigInfo),
	    {{tcp_setup,server},ServerSetup}=lists:keyfind({tcp_setup,server},1,ConfigInfo),
	    %% 
	    ok=application:load(sd),
	    ok=application:start(sd),
	    true=sd:add_resource(dbase,IpAddrDbase,PortDbase),
	    %% dbase init
	    case sd:call(dbase,infra_dbase,initiated,[],5000) of
		true->
		    no_action;
		false->
		    {ok,InitialValues}=file:consult(InitialFile),
		    ok =sd:call(dbase,infra_dbase,init_dbase,[InitialValues],5000)   
	    end,
	    {ok,LSock}=gen_tcp:listen(PortMaster,ServerSetup),
	    spawn(fun()->par_connect(LSock) end),
	    %% SD to all workers ! 
	    ok=infra_master_lib:add_all_workers(),
            %%% Connect + heartbeat workers   

	    spawn(infra_master_lib,poll_workers,[start,10000]),    
%	    spawn(infra_master_lib,connect_workers,[HeartBeatInterval]),
	 %   glurk=infra_master_lib:connect_workers(HeartBeatInterval),
	    %% Ready to go
	    io:format("Application started ~p~n",[{?MODULE}]),
	    InitResult={ok, #state{listner_port=PortMaster,
				   server_setup=ServerSetup,
				   port_dbase=PortDbase,
				   ipaddr_dbase=IpAddrDbase,
				   lsock=LSock}};   
	Err ->
	    io:format("Application crashed  ~p~n",[{?MODULE,?LINE,Err}]),
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

% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
% _IpAddrWorker not used already stored in config file - later release can use it



handle_call({connect,IdBrd,_IpAddrWorker,_PortBrd,_PortWorker},_From, State) -> 
    case dict:is_key(IdBrd,State#state.allWorkers) of
	true->
	    WorkerInfo=dict:fetch(IdBrd,State#state.allWorkers),
	    NewDictActive=dict:store(IdBrd,WorkerInfo,State#state.active),
	    Reply=ok,
	    NewState=State#state{active=NewDictActive};
	false->
	    Reply={error,{?MODULE,?LINE,unknown_brdid,IdBrd}},
	    NewState=State
    end,
    {reply, Reply, NewState};


handle_call({get_active},_From, State) ->
    Reply=sd:call(infra_dbase,infra_dbase,initiated,[],1000),
    {reply, Reply, State};

handle_call({get_allworkers},_From, State) ->
    Reply=sd:call(infra_dbase,infra_dbase,id_all_workers,[],1000),
    %Reply=sd:call(infra_dbase,infra_dbase,id_all_workers,[],1000),
    {reply, Reply, State};

handle_call({get_candidate_workers,NeededCapabilities},_From, State) ->
    case check_capabilties(NeededCapabilities,State#state.allWorkers) of
	{error,no_workers_available}->
	    Reply={error,no_workers_available};
	Workers->
	    Reply=Workers
    end,
    {reply, Reply, State};


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
handle_cast({connect_workers,Interval}, State) ->
%    io:format("CONNECT WORKERS = ~p~n",[{?MODULE,?LINE}]),
    Result=infra_master_lib:connect_workers(),
    NewState=State#state{active=lists:keyfind(active,1,Result),
			 disconnected=lists:keyfind(disconnected,1,Result)},


   % infra_master_lib:add_resorces_sd(ActiveWorkers),
   % infra_master_lib:delete_resources_sd(DisConnectedWorkers),
    
    io:format("time = = ~p~n",[time()]),
    io:format("active = ~p~n",[lists:keyfind(active,1,Result)]),
    io:format("standby = ~p~n",[lists:keyfind(active,1,Result)]),
    io:format("disconnected = ~p~n",[lists:keyfind(disconnected,1,Result)]),
    io:format("loaded_services = ~p~n",[lists:keyfind(loaded_services,1,Result)]),
    io:format("ok_services = ~p~n",[lists:keyfind(ok_services,1,Result)]),
    io:format("failed_Services = ~p~n",[lists:keyfind(failed_services,1,Result)]),
    io:format("updated_services, = ~p~n",[lists:keyfind(updated_services,1,Result)]),
    io:format("tested_services, = ~p~n",[lists:keyfind(tested_services,1,Result)]),
    spawn(infra_master_lib,poll_workers,[Interval]),
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_info(Info, State) ->
    io:format("unmatched match cast ~p~n",[{time(),?MODULE,?LINE,Info}]),
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
%% Function: 
%% Description:
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
		{MsgRef,Service,Request,Args}->
		   % io:format("~p~n",[{?MODULE,?LINE,Service,Request,Args}]),
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
    
check_capabilties(Capabilities,AllWorkers)->
    case check_capabilties(Capabilities,AllWorkers,[]) of
        []->
	    Reply={error,no_workers_available};
	Workers->
	    Reply=Workers
    end,
    Reply.
    

check_capabilties(_Capabilities,[],Workers)->  
    Workers;
check_capabilties(Capabilities,[Worker|T],Workers)->
    case have_capability(Capabilities,Worker,Workers) of
	true->
	    NewWorkers=[Worker|Workers];
	false ->
	    NewWorkers=Workers
    end,
    check_capabilties(Capabilities,T,NewWorkers).

have_capability([],_Worker,Have)->
    Have;
have_capability([Capability|T],Worker,_Have)->		 
    case Capability of
	{addr,Addr}->
	    WAddr=Worker#worker.addr,
	    if 
		Addr==WAddr->
		    NewHave=true,
		    NewT=T;
		true ->
		    NewHave=false,
		    NewT=[]
	    end;
	{room,Room} ->
	    WRoom=Worker#worker.room,
	    if 
		Room==WRoom->
		    NewHave=true,
		    NewT=T;
		true ->
		    NewHave=false,
		    NewT=[]
	    end;
	{cpu,CPU}->
	    WCPU=Worker#worker.cpu,
	    if 
		CPU==WCPU->
		    NewHave=true,
		    NewT=T;
		true ->
		    NewHave=false,
		    NewT=[]
	    end;
	{ram,RAM}->
	    WRAM=Worker#worker.ram,
	    if 
		RAM==WRAM->
		    NewHave=true,
		    NewT=T;
		true ->
		    NewHave=false,
		    NewT=[]
	    end;
	{disc,Disc}->
	    WDisc= Worker#worker.disc,
	    if 
		Disc==WDisc->
		    NewHave=true,
		    NewT=T;
		true ->
		    NewHave=false,
		    NewT=[]
	    end;
	{AnyCapability,Info}->
	    case lists:keyfind(AnyCapability,1,Worker#worker.capabilities) of
		{AnyCapability,Info}->
		    NewHave=true,
		    NewT=T;
		false->
		    NewHave=false,
		    NewT=[]
	    end
    end,
    have_capability(NewT,Worker,NewHave).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
init_all_workers(Config)->
    AllWorkers= init_all_workers(Config,[]), 
    AllWorkers.
init_all_workers([],AllWorkers)-> 
    AllWorkers;
init_all_workers([WorkerConfig|T],AllWorkers) ->
    {{id,Id},
     {addr,Addr},
     {room,Room},
     {cpu,Cpu},
     {ram,Ram},
     {disc,Disc},
     {num_allocated,NumAllocated},
     {capabilities,Capabilities}}=WorkerConfig,
    Worker=#worker{id=Id,addr=Addr,room=Room,cpu=Cpu,ram=Ram,disc=Disc,
		   num_allocated=NumAllocated,capabilities=Capabilities},
    NewAllWorkers=[Worker|AllWorkers],
    init_all_workers(T,NewAllWorkers). 


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
