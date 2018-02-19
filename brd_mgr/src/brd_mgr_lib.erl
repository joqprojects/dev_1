%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(brd_mgr_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------

%% External exports
-export([start/0,
	 test/0]).

-export([store_board_info_dict/1,
	 upgrade/2,
	 start_worker/4,latest_rel/0]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================
test()->
    {ok,?MODULE}.

start()->
    ok=application:load(brd_mgr),
    ok=application:start(brd_mgr).
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
upgrade(ReleaseDir,ListTarFiles)->
    case filelib:is_dir(ReleaseDir) of
	true->
	    RemoveCmd="rm -r "++ReleaseDir,
	    []=os:cmd(RemoveCmd);
	false ->
	    ok
    end,
% Create release dir and un tar files
    ok=file:make_dir(ReleaseDir),
    ok=untar_files(ReleaseDir,ListTarFiles),

% Test release
    PathLatestRelease=ReleaseDir++"/*/ebin",
    TestNodeStr="tmp_test_node"++"@"++net_adm:localhost(),
    TestNode=list_to_atom(TestNodeStr),
    ErlCmd="erl -pa "++PathLatestRelease++" "++"-name "++TestNodeStr++" "++"-detached",
    []=rpc:call(node(),os,cmd,[ErlCmd]),
    timer:sleep(400), % Wait for Linux to start the VM

    {ok,ListServicesStr}=file:list_dir(ReleaseDir),
    case test_services(ListServicesStr,TestNode,ok) of
	ok->
	    Reply=ok;
	Err->
	    Reply=Err
    end,
    rpc:cast(TestNode,erlang,halt,[]),
    timer:sleep(300),
    Reply.
    
test_services([],_TestNode,ok)->
    ok;
test_services([],_TestNode,{error,Service})->
    {error,Service};
test_services(_ListServicesStr,_TestNode,{error,Service}) ->
    {error,Service};
test_services([ServiceStr|T],TestNode,_Acc) ->
    Service=list_to_atom(ServiceStr),
    R1=rpc:call(TestNode,application,load,[Service]),
    R2=rpc:call(TestNode,application,start,[Service]),
    R3=rpc:call(TestNode,Service,test,[]),
    case {R1,R2,R3} of
	{ok,ok,ok}->
	    NewAcc=ok;
	_ ->
	    NewAcc={error,Service}
    end,
    test_services(T,TestNode,NewAcc).
    
		     

untar_files(_ReleaseDir,[])->
    ok;			
untar_files(ReleaseDir,[{Service,{TarFileName,TarfFileBin}}|T])->
    Dest=filename:join(ReleaseDir,Service),    
    case file:make_dir(Dest) of
	 ok->
	    ok=file:write_file(TarFileName,TarfFileBin),
	    TarCmd="tar -xvf "++TarFileName++" -C "++Dest,
	    os:cmd(TarCmd);
	{error,eexist} ->
	    ok=file:write_file(TarFileName,TarfFileBin),
	    TarCmd="tar -xvf "++TarFileName++" -C "++Dest,
	    os:cmd(TarCmd);
	_Err->
	    error
    end,
    untar_files(ReleaseDir,T).
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

start_worker(LatestNameStr,PathLatestRelease,ServicePort,ServerSetUp)->
    ErlCmd="erl -pa "++PathLatestRelease++" "++"-name "++LatestNameStr++" "++"-detached",
    []=rpc:call(node(),os,cmd,[ErlCmd]),
    timer:sleep(400), % Wait for Linux to start the VM
    ok=rpc:call(list_to_atom(LatestNameStr),application,load,[q_mgr]),
    ok=rpc:call(list_to_atom(LatestNameStr),application,start,[q_mgr]),
    ok=rpc:call(list_to_atom(LatestNameStr),q_mgr,start_server,[ServicePort,ServerSetUp]),
    ok.
	    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
latest_rel()->
    case file:list_dir(".") of
	{ok,FileNames}->
	    case find_latest(FileNames,[]) of
	        []->
		    Reply={error,{?MODULE,?LINE,no_release_dir_found}};
	        [Latest,Current] ->
		    Reply={ok,[Latest,Current]};
		 [Latest]->
		    Reply={ok,[Latest]}
	    end; 
	{error,Err} ->
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.

find_latest([],Acc)->
    List=sort_and_convert_to_filenames(Acc),
    SubList=lists:sublist(List,2),
    SubList;
 
    
find_latest([FileName|T],Acc)->
    case filelib:is_dir(FileName) of
	true->
	    case rpc:call(node(),erlang,list_to_integer,[FileName]) of
		{badrpc,_Err}->
		    NewAcc=Acc;
		DirAsInteger ->
		    NewAcc=[DirAsInteger|Acc]
	    end;
	false ->
	    NewAcc=Acc
    end,
    find_latest(T,NewAcc).

num_elements([],Acc)->
    Acc;
num_elements([_Element|T],Acc) ->
    if
	Acc > 2 ->
	    NewT=[], %stop 
	    NewAcc=Acc;
	true ->
	    NewAcc=Acc+1,
	    NewT=T
    end,
    num_elements(NewT,NewAcc).


sort_and_convert_to_filenames(Acc)->
    SortedIntegers=lists:sort(Acc),
    Result=convert_to_filenames(SortedIntegers,[]),
    Result.

convert_to_filenames([],Acc) ->
    Acc;
convert_to_filenames([Int|T],Acc) ->
    NewAcc=[integer_to_list(Int)|Acc],
    convert_to_filenames(T,NewAcc).     

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
store_board_info_dict(BoardConfigFileBin)->
    ok=file:write_file("temp.config",BoardConfigFileBin),
    {ok,BrdConfig}=file:consult("temp.config"),
    {infra_master,InfraInfo}=lists:keyfind(infra_master,1,BrdConfig),
    {infra_ip_addr,InfraIp}=lists:keyfind(infra_ip_addr,1,InfraInfo),
    {infra_port,InfraPort}=lists:keyfind(infra_port,1,InfraInfo),
    
    {brd_info,BrdInfo}= lists:keyfind(brd_info,1,BrdConfig),
    {brd_id,BrdId}=lists:keyfind(brd_id,1,BrdInfo),
    {brd_port,BrdPort}=lists:keyfind(brd_port,1,BrdInfo),
    {worker_port,ServicePort}=lists:keyfind(worker_port,1,BrdInfo),
    {connect_tries,ConnTries}=lists:keyfind(connect_tries,1,BrdInfo),
    {period,Period}=lists:keyfind(period,1,BrdInfo),
    {timeout,Timeout}=lists:keyfind(timeout,1,BrdInfo),

    {tcp_setup,TcpInfo}=lists:keyfind(tcp_setup,1,BrdConfig),
    {client_setup,ClientSetUp}=lists:keyfind(client_setup,1,TcpInfo),
    {server_setup,ServerSetUp}=lists:keyfind(server_setup,1,TcpInfo),
    List=[{infra_ip_addr,InfraIp},{infra_port,InfraPort},
	  {brd_id,BrdId},{brd_port,BrdPort},{worker_port,ServicePort},
	  {connect_tries,ConnTries},{period,Period},{timeout,Timeout},
	   {client_setup,ClientSetUp},  {server_setup,ServerSetUp}
	 ],
    Dict=dict:from_list(List),
    ok=file:delete("temp.config"),
    {ok,Dict}.
    

	  

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
