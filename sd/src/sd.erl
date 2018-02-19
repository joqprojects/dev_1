%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sd).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../infra_master/include/infra.hrl").
%% --------------------------------------------------------------------
%% External exports

-export([call/5,add_resource/3,delete_resource/3,
	resources/0,resources/1]).

-export([test/0]).

-export([start/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {resources}).

%% ====================================================================
%% External support functions
%% ====================================================================

test()->
    Reply=test_sd:test(),
    Reply.
	

call(Service,Module,Fun,Args,TimeOut) ->
    case sd:resources(Service) of
	[]->
	    Reply={error,{?MODULE,?LINE,service_not_available,Service}};
	Services->
	    [{Service,Ip,Port}|_T]=Services,
	    [{{tcp_setup,client},ClientSetUp}]=sd:resources({tcp_setup,client}),
	  %  io:format("~p~n",[{?MODULE,?LINE,Service,Ip,Port,ClientSetUp}]),
	    case gen_tcp:connect(Ip,Port,ClientSetUp) of
		{ok,Socket}->
		 %   io:format("~p~n",[{?MODULE,?LINE,Socket}]),
		   Ref=erlang:timestamp(),
		    case send_receive(Socket,{Ref,Module,Fun,Args},TimeOut) of
			{Ref,Result}->
			    Reply=Result;
			Err->
			    Reply={error,{?MODULE,?LINE,Err}}
		    end,
		    gen_tcp:close(Socket);
		Err->
		    Reply={error,{?MODULE,?LINE,Err}}
	    end	
    end,
    Reply.
 							       
%% ====================================================================
%% External Server functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
 

add_resource(Service,Ip,Port)-> 
    gen_server:call(?MODULE, {add_resource,Service,Ip,Port},infinity).
delete_resource(Service,Ip,Port)-> 
    gen_server:call(?MODULE, {delete_resource,Service,Ip,Port},infinity).

resources(Service)-> 
    gen_server:call(?MODULE, {resources,Service},infinity).
resources()-> 
    gen_server:call(?MODULE, {resources},infinity).

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
    Tab=ets:new(sd,[bag]),
    true=ets:insert_new(Tab,[{{tcp_setup,client},[binary,{packet,4}]},
			 {{tcp_setup,server},[binary,{packet,4},{reuseaddr,true},{active,true}]}]),
    io:format("Application Starting ~p~n",[{?MODULE}]),
    {ok, #state{resources=Tab}}.

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
handle_call({resources,Service},_From,State) ->
    Reply=sd_lib:resources(Service,State),
    {reply, Reply, State};

handle_call({resources},_From,State) ->
    Reply=sd_lib:resources(State),
    {reply, Reply, State};

handle_call({add_resource,Service,Ip,Port},_From,State) ->
    Reply=sd_lib:add_resource(Service,Ip,Port,State),
    {reply, Reply, State};

handle_call({delete_resource,Service,Ip,Port},_From,State) ->
    Reply=sd_lib:delete_resource(Service,Ip,Port,State),
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
send_receive(Socket,Msg,TimeOut)->
    gen_tcp:send(Socket,term_to_binary(Msg)),
    receive
	{tcp,Socket,Bin}->
	    Result=binary_to_term(Bin);
	{tcp_closed,Socket} ->
	    Result={error,{?MODULE,?LINE,tcp_closed,Socket}};
	Err->
	    Result={error,{?MODULE,?LINE,Err}}
    after TimeOut ->
	    Result={error,{?MODULE,?LINE,timeout}}
    end,
    Result.
