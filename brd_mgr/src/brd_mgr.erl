%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(brd_mgr).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
% -include("../infra_master/include/infra.hrl").
%% --------------------------------------------------------------------
-define(CONFIGFILE,"service.config").


%% External exports
-export([hello_world/0]).


-export([connect/0,activate/0,
	 start_server/2,
	 start/0,stop/0]).
-export([test/0]).
-export([start_brd_mgr/0,stop_brd_mgr/0]).


%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {status,listner_port,server_setup,lsock}).

%% ====================================================================
%% External support functions
%% ====================================================================


start_brd_mgr()->
    Load=application:load(?MODULE),
    Start=application:start(?MODULE),
    {Load,Start}.

stop_brd_mgr()->
    Stop=application:stop(infra_dbase),
    UnLoad=application:unload(infra_dbase),
    {Stop,UnLoad}.



test()->
    Reply=test_brd_mgr:test(),
    Reply.

%% ====================================================================
%% External Server functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
 

connect()->
    gen_server:call(?MODULE, {connect},infinity).    
activate()->
    gen_server:call(?MODULE, {activate},infinity).    

hello_world()-> 
    gen_server:call(?MODULE, {hello_world},infinity).
start_server(Port,ServerSetUp)-> 
    gen_server:call(?MODULE, {start_server,Port,ServerSetUp},infinity).
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
	    {{brd_mgr,port},PortBrdMgr}=lists:keyfind({brd_mgr,port},1,ConfigInfo),
	    {{tcp_setup,server},ServerSetup}=lists:keyfind({tcp_setup,server},1,ConfigInfo),    
	    {ok,LSock}=gen_tcp:listen(PortBrdMgr,ServerSetup),
	    spawn(fun()->par_connect(LSock) end),
	    io:format("Application Starting ~p~n",[{?MODULE}]),
	    InitResult={ok, #state{status=standby,
				   listner_port=PortBrdMgr,
				   server_setup=ServerSetup,
				   lsock=LSock}};   
	 Err ->
	    io:format("Application crasched ~p~n",[{?MODULE,Err}]),
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
handle_call({start_server,BrdPort,ServerSetUp}, _From, State) ->
    {ok,LSock}=gen_tcp:listen(BrdPort,ServerSetUp),  
    spawn(fun()->par_connect(LSock) end),
    Reply={ok,BrdPort,ServerSetUp},
    {reply, Reply, State};


handle_call({connect}, From, State) ->
  %  io:format("~p~n",[{node(),time(),connect,From}]),
    Reply=State#state.status,
    {reply, Reply, State};

handle_call({activate},From, State) ->
 %   io:format("~p~n",[{node(),time(),activate,From}]),
    NewState=State#state{status=active},
    Reply=active,
    {reply, Reply, NewState};



handle_call({hello_world}, _From, State) ->
    Reply = {?MODULE,?LINE,hello_world},
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
	    {MsgRef,Service,Request,Args}=binary_to_term(Bin),
	    Result=rpc:call(node(),Service,Request,Args),
	    ReplyBin=term_to_binary({MsgRef,Result}),
	    gen_tcp:send(Socket,ReplyBin),
	    loop(Socket);
	{tcp_closed,Socket} ->
	    tcp_closed;
	   % io:format("Socket closed ~p~n",[{?MODULE,?LINE,Socket}]);
	_Err->
	    loop(Socket)
    %after 1000*60*10 ->       % Kill if inactive in 10 mins
%	    inactive_too_long
    end.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
