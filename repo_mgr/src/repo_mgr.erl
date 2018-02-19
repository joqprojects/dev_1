%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(repo_mgr).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(POLL_TIME,2000).
-define(CONFIGFILE,"service.config").

%-export([have_capability/2,have_capabilities/2]).
-export([start_repo/0,stop_repo/0]).

%% External exports Gen server
-export([build_release/2,
	 create_update_service/2,update_service/2,read_service/2,delete_service/1,
	 latest_release/1,store_service/3,remove_service/2,get_service/2,get_service_releases/1]).

-export([start/0,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start_repo()->
    Load=application:load(?MODULE),
    Start=application:start(?MODULE),
    {Load,Start}.

stop_repo()->
    Stop=application:stop(?MODULE),
    Unload=application:unload(?MODULE),
    {Stop,Unload}.


%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

% 

build_release(Release,DevAreaDir)->
    gen_server:call(?MODULE, {build_release,Release,DevAreaDir},infinity).

create_update_service(ServiceDir,DevAreaDir)->
    gen_server:call(?MODULE, {create_update_service,ServiceDir,DevAreaDir},infinity).
update_service(ServiceDir,DevAreaDir)->
    gen_server:call(?MODULE, {update_service,ServiceDir,DevAreaDir},infinity).
read_service(Item,Service)->
    gen_server:call(?MODULE, {read_service,Item,Service},infinity).
delete_service(Service)->
    gen_server:call(?MODULE, {delete_service,Service},infinity).

latest_release(ServiceStr)->
    gen_server:call(?MODULE, {latest_release,ServiceStr},infinity).
store_service(ServiceStr,ReleaseStr,RepoDir)->
    gen_server:call(?MODULE, {store_service,ServiceStr,ReleaseStr,RepoDir},infinity).
remove_service(ServiceStr,ReleaseStr)->
    gen_server:call(?MODULE, {remove_service,ServiceStr,ReleaseStr},infinity).

get_service(ServiceStr,ReleaseStr)->
     gen_server:call(?MODULE, {get_service,ServiceStr,ReleaseStr},infinity).
get_service_releases(ServiceStr)->
     gen_server:call(?MODULE, {get_service_releases,ServiceStr},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}     
%  Dat Structure
%% --------------------------------------------------------------------
init([]) ->
    case file:consult(?CONFIGFILE) of
	{ok,ConfigInfo}->
	    {{infra_master,port},PortMaster}=lists:keyfind({infra_master,port},1,ConfigInfo),
	    {{infra_master,ip_addr},IpAddrMaster}=lists:keyfind({infra_master,ip_addr},1,ConfigInfo),
	    {{infra_dbase,port},PortDbase}=lists:keyfind({infra_dbase,port},1,ConfigInfo),
	    {{infra_dbase,ipaddr},IpAddrDbase}=lists:keyfind({infra_dbase,ipaddr},1,ConfigInfo),
	    %% 
	    application:load(sd),
	    application:start(sd),
	    true=sd:add_resource(dbase,IpAddrDbase,PortDbase), 
	    true=sd:add_resource(master,IpAddrMaster,PortMaster),
	    io:format("Application Started ~p~n",[{?MODULE}]),
	    InitResult={ok, #state{}};   
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

handle_call({build_release,Release,DevAreaDir},_From, State) ->
    Reply=repo_mgr_lib:build_release(Release,DevAreaDir),
    {reply, Reply, State};

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
handle_call({delete_service,Service},_From, State) ->
    Reply=repo_mgr_lib:delete_service(Service),
    {reply, Reply, State};

handle_call({create_update_service,ServiceDir,DevAreaDir},_From, State) ->
    Reply=repo_mgr_lib:create_update_service(ServiceDir,DevAreaDir),
    {reply, Reply, State};

handle_call({update_service,ServiceDir,DevAreaDir},_From, State) ->
    Reply=repo_mgr_lib:update_service(ServiceDir,DevAreaDir),
    {reply, Reply, State};

handle_call({read_service,Item,Service},_From, State) ->
    Reply=repo_mgr_lib:read_service(Item,Service),
    {reply, Reply, State};

% --------------------------------------------------------------------
%% Function: store release 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
handle_call({latest_release,ServiceStr},_From, State) ->
    Reply=repo_mgr_lib:latest_release(ServiceStr),
    {reply, Reply, State};

handle_call({get_service_releases,Service},_From, State) ->
    Reply=sd:call(dbase,infra_dbase,get_service_releases,[Service],5000),
    {reply, Reply, State};

handle_call({get_service,Service,ReleaseStr},_From, State) ->
    KeyServiceData={ReleaseStr,Service},
    Reply=sd:call(dbase,infra_dbase,get,[KeyServiceData],5000),
    {reply, Reply, State};

handle_call({remove_service,Service,ReleaseStr},_From, State) ->
    KeyServiceData={ReleaseStr,Service},
    Reply=sd:call(dbase,infra_dbase,remove,[KeyServiceData],5000),
    {reply, Reply, State};

% --------------------------------------------------------------------
%% Function: store service
%% Description:
%% Returns: non
%% Dbase structure
%% service_info:
%% Key= {service,release}
%% Value={TarFileName,TarFile}
%% --------------------------------------------------------------------
handle_call({store_service,Service,ReleaseStr,RepoDir},_From, State) ->
    Reply=repo_mgr_lib:store_service(Service,ReleaseStr,RepoDir),
    {reply, Reply, State};

handle_call({tabort____FFF______store_service,Service,ReleaseStr,RepoDir},_From, State) ->
  %% get tar file
    case sd:call(infra_dbase,infra_dbase,get,[latest_release],5000) of
	{ok,[]}->
	    Reply=repo_mgr_lib:store_service(Service,ReleaseStr,RepoDir);
	{ok,[{latest_release,LatestReleaseStr}]}->
	    
	    %={ list_to_integer(ReleaseStr),list_to_integer(LatestReleaseStr)},
	    %glurk= list_to_integer(ReleaseStr)<list_to_integer(LatestReleaseStr),

	    case list_to_integer(ReleaseStr)<list_to_integer(LatestReleaseStr) of
		true->
		    Reply={error,{?MODULE,?LINE,
				  tried_to_store_old_release,
				  {ReleaseStr,"<",LatestReleaseStr}}};
		false ->
		     Reply=repo_mgr_lib:store_service(Service,ReleaseStr,RepoDir)
	    end
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
   
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
