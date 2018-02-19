%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(infra_master_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------

%% External exports

-export([ add_all_workers/0,
	 add_all_workers/2,
	 connect_workers/0,
	 poll_workers/1,
	 poll_workers/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions

 %% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
add_all_workers()->
    case sd:call(dbase,infra_dbase,id_all_workers,[],5000) of
	{ok,Workers}->
	    Reply=rpc:call(node(),?MODULE,add_all_workers,[Workers,ok],30000);
	Err->
	    Reply={error,{?MODULE,?LINE,Err}}
    end,   
    Reply.

add_all_workers([],Acc)->
    Acc;
add_all_workers([Worker|T],_Acc)->
     case sd:call(dbase,infra_dbase,read_object,[object,{worker,Worker}],5000) of
	 [{brd_ipaddr,IpAddr},
	  {brd_port,BrdMgrPort},
	  {worker_port,_}]->
	     true=sd:add_resource(Worker,IpAddr,BrdMgrPort),
	    % true=sd:add_resource(Wrk,IpAddr,WrkPort),
	     NewAcc=ok;
	 Err ->
	     NewAcc={error,{?MODULE,?LINE,Err}}
     end,
    add_all_workers(T,NewAcc).



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
poll_workers(start,Interval)->
 %   io:format("~p~n",[{?MODULE,?LINE,start,Interval}]),
    infra_master:connect_workers(Interval).
poll_workers(Interval)->
%    io:format("~p~n",[{?MODULE,?LINE,Interval}]),
    timer:sleep(Interval),
    infra_master:connect_workers(Interval).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

connect_workers()->
 %   io:format("~p~n",[{?MODULE,?LINE}]),
    case sd:call(dbase,infra_dbase,id_all_workers,[],5000) of
	{ok,Workers}->
%	    io:format("~p~n",[{?MODULE,?LINE}]),
	    %% Try to connect all workers
	    ConnectionWorkers=[connect_worker(Worker) || Worker<-Workers],
	    Active=[Worker||{active,Worker,_}<-ConnectionWorkers],
	    Standby=[Worker||{standby,Worker,_}<-ConnectionWorkers],
	    Disconnected=[{Worker,Err}||{error,Worker,Err}<-ConnectionWorkers],

%	    io:format("~p~n",[{?MODULE,?LINE,Active,Standby}]),
	    % Update local sd with the ones just started
	    LocalSdActive=[update_local_sd(Worker) || Worker<-Active],
%	    io:format("LocalSdActive ~p~n",[{?MODULE,?LINE,LocalSdActive}]),
	    LocalSdStandby=[update_local_sd(Worker) || Worker<-Standby],
%	    io:format("Standby ~p~n",[{?MODULE,?LINE,LocalSdStandby}]),
	   
            %load and test services
	    LoadedServices=[load_services(Worker) || Worker<-Standby],
	    LoadedServicesList=lists:append(LoadedServices),
	    [load_start_service(ServiceStrNoUpdate,VsnNoUpdate,WorkerNoUpdate) ||{no_update,ServiceStrNoUpdate,VsnNoUpdate,WorkerNoUpdate}<-LoadedServicesList],
	    
	    io:format("LoadedServices ~p~n",[{?MODULE,?LINE,LoadedServices}]),
	    TestedServices=[do_test_service(ServiceStr2Test,Vsn2Test,Worker2Test) ||{updated,ServiceStr2Test,Vsn2Test,Worker2Test}<-LoadedServicesList],

%	    OkServices=[{Worker,ServiceStr,Vsn} ||{ok,Worker,ServiceStr,Vsn}<-Loaded_Tested],
%	    FailedServices=[{Worker,[Err]} ||{error,Worker,[Err]}<-Loaded_Tested],
	    
	    %remove_sd(UpdatedDisconnected),
	    Reply=[{active,Active},
		   {standby,Standby},
		   {disconnected,Disconnected},
		   {loaded_services,LoadedServicesList},
		   {tested_services,TestedServices}];
	%	   {ok_services,OkServices},{failed_services,FailedServices}]; 
	  %  Reply={[Active|UpdatedActive],[Disconnected|UpdatedDisconnected]};
	Err->
	    Reply={error,{?MODULE,?LINE,Err}}
    end,      
    Reply.


stop_unload_service(ServiceStr,Vsn,Worker)->
    Ebin=filename:join(ServiceStr++"-"++Vsn,"ebin"),
    ServiceAtom=list_to_atom(ServiceStr),
    Stop=sd:call({Worker,brd_mgr},application,stop,[ServiceAtom],5000),
    Unload=sd:call({Worker,brd_mgr},application,unload,[ServiceAtom],5000),
    true=sd:call({Worker,brd_mgr},code,del_path,[Ebin],5000),
    case sd:call({Worker,brd_mgr},file,list_dir,[Ebin],5000) of
	{ok,BaseNames}->
	    Reply=[purge_file(BaseName,Worker,Ebin) || BaseName<-BaseNames];
	{error,Err}->
	    Reply={error,[?MODULE,?LINE,ServiceStr,Vsn,Worker,Err]}
    end,
    Reply.

load_start_service(ServiceStr,Vsn,Worker)->
    Ebin=filename:join(ServiceStr++"-"++Vsn,"ebin"),
    ServiceAtom=list_to_atom(ServiceStr),
    case sd:call({Worker,brd_mgr},application,unload,[ServiceAtom],5000) of
	{error,{running,ServiceAtom}}->
	    Reply={error,[?MODULE,?LINE,already_started,ServiceStr,Vsn,Worker]};
	{error,{not_loaded,ServiceAtom}}->
	    true=sd:call({Worker,brd_mgr},code,add_patha,[Ebin],5000),
	    case sd:call({Worker,brd_mgr},file,list_dir,[Ebin],5000) of
		{ok,BaseNames}->
		    [load_file(BaseName,Worker,Ebin) || BaseName<-BaseNames],
		    Load=sd:call({Worker,brd_mgr},application,load,[ServiceAtom],5000),
		    Start=sd:call({Worker,brd_mgr},application,start,[ServiceAtom],5000),
		    Reply={Load,Start,ServiceStr,Vsn,Worker};
		{error,Err}->
		    Reply={error,[?MODULE,?LINE,ServiceStr,Vsn,Worker,Err]}
	    end
    end,
    Reply.

load_file(BaseName,Worker,Ebin) ->
    case filename:extension(BaseName) of
	".beam"->
	    ModuleStr=filename:basename(BaseName,".beam"),
	    Module=list_to_atom(ModuleStr),
%	    io:format("load Module ~p~n",[{?MODULE,?LINE,Module}]),
	    Reply=sd:call(Worker,code,load_file,[Module],5000);
	NoBeamFile ->
	    Reply={do_nothing,NoBeamFile}
    end,
    Reply.

purge_file(BaseName,Worker,Ebin) ->
    case filename:extension(BaseName) of
	".beam"->
	    ModuleStr=filename:basename(BaseName,".beam"),
	    Module=list_to_atom(ModuleStr),
%	    io:format("Purge  Module ~p~n",[{?MODULE,?LINE,Module}]),
	    Reply={sd:call(Worker,code,purge,[Module],5000),BaseName};
	NoBeamFile->
	   % io:format("do nothing ~p~n",[{?MODULE,?LINE,Err}]),
	    Reply={do_nothing,NoBeamFile}
    end,
    Reply.

    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
 do_test_service(ServiceStr,VsnNew,Worker)->
    
    LoadedApplications=sd:call({Worker,brd_mgr},application,loaded_applications,[],5000),
    ServiceAtom=list_to_atom(ServiceStr),
    case lists:keyfind(ServiceAtom,1,LoadedApplications) of
	false->
	    load_start_service(ServiceStr,VsnNew,Worker),
	    case sd:call({Worker,brd_mgr},ServiceAtom,test,[],5000) of
		ok->
		    Reply={ok,ServiceStr,Worker};
		Err ->
		    stop_unload_service(ServiceStr,VsnNew,Worker),
		    ServiceDirNew=ServiceStr++"-"++VsnNew,
		    sd:call({Worker,brd_mgr},os,cmd,["rm -r "++ServiceDirNew],5000),
		    Reply={failed,ServiceStr,Worker,Err}
	    end;
	{ServiceAtom,_AppInfo,VsnNew}-> %% Already latest
	    Reply={ok,ServiceStr,Worker};
	{ServiceAtom,_AppInfo,Vsn}->
	    stop_unload_service(ServiceStr,Vsn,Worker),
	    load_start_service(ServiceStr,VsnNew,Worker),
	    case sd:call({Worker,brd_mgr},ServiceAtom,test,[],5000) of
		ok->
		    ServiceDirCurrent=ServiceStr++"-"++Vsn,
		     io:format("ServiceDirCurrent~p~n",[{?MODULE,?LINE,ServiceDirCurrent}]),
		    sd:call({Worker,brd_mgr},os,cmd,["rm -r "++ServiceDirCurrent],5000),
		    Reply={ok,ServiceStr,Worker};
		Err ->
		    stop_unload_service(ServiceStr,VsnNew,Worker),
		    load_start_service(ServiceStr,Vsn,Worker),
		    ServiceDirNew=ServiceStr++"-"++VsnNew,
		    sd:call({Worker,brd_mgr},os,cmd,["rm -r "++ServiceDirNew],5000),
		    Reply={failed,ServiceStr,Worker,Err}
	    end
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
update_local_sd(Worker)->
    [{brd_ipaddr,IpAddr},
     {brd_port,BrdPort},
     {worker_port,_}]=sd:call(dbase,infra_dbase,read_object,[object,{worker,Worker}],5000),
 %   Resource=Worker++"-"++"brd_mgr",
    true=sd:add_resource({Worker,brd_mgr},IpAddr,BrdPort),
    {Worker,IpAddr,BrdPort}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_services(Worker) ->
%    io:format("~p~n",[{?MODULE,?LINE,Worker}]),
    case sd:call(dbase,infra_dbase,read_object,[services,{deployment,Worker}],5000) of
	{error,Err}->
	    % Worker has no Services to deploy
%	    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
	    Reply={error,Worker,[Err]};
	ServicesToDeploy ->
	    io:format("~p~n",[{?MODULE,?LINE,ServicesToDeploy}]),
	    LoadedServices=[load_service(ServiceStr,Worker) || {ServiceStr,_Num}<-ServicesToDeploy],
	    Reply=LoadedServices
	    %Reply=[deploy_service(ServiceStr,Worker) || {ServiceStr,_Num}<-ServicesToDeploy]
    end,
    Reply.

load_service(ServiceStr,Worker)->	    
    case sd:call(dbase,infra_dbase,read_object,[vsn,{latest,ServiceStr}],5000) of 
	{error,Err}->
%	    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
	    Reply={error,[Worker,ServiceStr,Err]};
	VsnLatest ->
	    case service_exists(ServiceStr,Worker) of
		false->
		    io:format("not exists ~p~n",[{?MODULE,?LINE,ServiceStr,Worker}]),
		    Reply=load_sw(ServiceStr,VsnLatest,Worker);
		AppFiles ->
	%	    io:format(" exists ~p~n",[{?MODULE,?LINE,ServiceStr,Worker,AppFiles}]),
		    Reply=load_sw(ServiceStr,Worker,VsnLatest,AppFiles)
	    end
    end,
    Reply.
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

load_sw(ServiceStr,Worker,VsnLatest,AppFiles)->
    Compared=[compare_vsn(VsnLatest,ServiceStr,DirFile,Binary)||{ServiceStr,DirFile,Binary,Worker}<-AppFiles],
    io:format(" compared ~p~n",[{?MODULE,?LINE,ServiceStr,Worker,Compared}]),
    case {lists:keymember(less,1,Compared),lists:keymember(equal,1,Compared)} of
	{false,false}->
%	    io:format(" {false,false} ~p~n",[{?MODULE,?LINE}]),
	    Reply=load_sw(ServiceStr,VsnLatest,Worker);	    
	_->
	   % io:format(" {A,B} ~p~n",[{?MODULE,?LINE,A,B}]),
	    Reply={no_update,ServiceStr,VsnLatest,Worker}
    end,
    Reply.

load_sw(ServiceStr,Vsn,Worker)->	
    [TarBaseName,Binary]=sd:call(dbase,infra_dbase,read_object,[release,{service,ServiceStr,Vsn}],5000),
%    io:format(" exists ~p~n",[{?MODULE,?LINE,TarBaseName}]),
    %% Store tar file	    
    ok=sd:call({Worker,brd_mgr},file,write_file,[TarBaseName,Binary],5000),
    ok=sd:call({Worker,brd_mgr},erl_tar,extract,[TarBaseName],5000),
    ok=sd:call({Worker,brd_mgr},file,delete,[TarBaseName],5000),
    Reply={updated,ServiceStr,Vsn,Worker},
    Reply.




%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
compare_vsn(VsnLatest,ServiceStr,DirFile,Binary)->
    Time=erlang:system_time(),
    TempBaseName=integer_to_list(Time)++".app",
    ok=file:write_file(TempBaseName,Binary),
    {ok,[{application,ServiceAtom,AppInfo}]}=file:consult(TempBaseName),
  %  io:format("~p~n",[{?MODULE,?LINE,file:consult(TempBaseName)}]),
    {vsn,Vsn}=lists:keyfind(vsn,1,AppInfo),
    ok=file:delete(TempBaseName),
    Reply={cmp_vsn_strings(VsnLatest,Vsn),ServiceStr},
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
service_exists(ServiceStr,Worker)->
    ServiceModule=list_to_atom(ServiceStr),
    case sd:call({Worker,brd_mgr},file,list_dir,["."],5000) of
	{ok,DirFiles}->
	    AppFiles= [find_app_file(DirFile,ServiceStr,Worker) ||DirFile<-DirFiles],
	    case [{ServiceStr,AppFileName,Binary,Worker} || {ok,ServiceStr,AppFileName,Binary,Worker}<-AppFiles] of
		[]->
		    Reply=false;
		FoundAppFiles->
		    Reply=FoundAppFiles
	    end;
	{error,Err} ->
	    Reply={error,[?MODULE,?LINE,Worker,ServiceStr,Err]}
    end,
    Reply.


find_app_file(DirFile,ServiceStr,Worker)->
    AppFileName=filename:join([DirFile,"ebin",ServiceStr++".app"]),
    case sd:call({Worker,brd_mgr},file,read_file,[AppFileName],5000) of
	{error,Err}->
	    Reply={noent,DirFile,ServiceStr,Worker};
	{ok,Binary}->
	    Reply={ok,ServiceStr,DirFile,Binary,Worker}
    end,
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

cmp_vsn_strings(Vsn_A,Vsn_B)->
    % Vsn="2.42.120"
    % -> equal,A less B ,A larger B
    [P2Str_A,P1Str_A,P0Str_A]=string:tokens(Vsn_A,"."),
    [P2Str_B,P1Str_B,P0Str_B]=string:tokens(Vsn_B,"."),
    P2_A=list_to_integer(P2Str_A),
    P2_B=list_to_integer(P2Str_B),
    case {(P2_A<P2_B),(P2_A>P2_B)} of
	{false,false}->
	    P1_A=list_to_integer(P1Str_A),
	    P1_B=list_to_integer(P1Str_B),
	    case {(P1_A<P1_B),(P1_A>P1_B)} of
		{false,false}->
		    P0_A=list_to_integer(P0Str_A),
		    P0_B=list_to_integer(P0Str_B),
		    case {(P0_A<P0_B),(P0_A>P0_B)} of
			{false,false}->
			    Reply=equal;
			{true,false}->
			    Reply=less;
			{false,true} ->
			    Reply=larger
		    end;
		{true,false}->
		    Reply=less;
		{false,true} ->
		    Reply=larger
	    end;
	{true,false}->
	    Reply=less;
	{false,true} ->
	    Reply=larger
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

deploy_service(ServiceStr,Worker) ->
 %   io:format("~p~n",[{?MODULE,?LINE,Worker,ServiceStr}]),
    case sd:call(dbase,infra_dbase,read_object,[vsn,{latest,ServiceStr}],5000) of 
	{error,Err}->
%	    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
	    Reply={error,[Worker,ServiceStr,Err]};
	VsnLatest ->
	    % Check if need to update  latest is newer then currant
	    ServiceModule=list_to_atom(ServiceStr),
	    LoadedApplications=sd:call({Worker,brd_mgr},application,loaded_applications,[],5000),
	    case lists:keyfind(ServiceModule,1,LoadedApplications) of
		false->
		    case load_test_service(ServiceStr,VsnLatest,Worker,false) of
			{error,Err}->
%			    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
			    Reply={error,[Worker,ServiceStr,Err]};
			ok->
			    Reply={ok,Worker,ServiceStr,VsnLatest}
		    end;
		{ServiceModule,_AppInfo,Vsn}->
		    case cmp_vsn_strings(VsnLatest,Vsn) of
			larger->
			    case load_test_service(ServiceStr,VsnLatest,Worker,true) of
				{error,Err}->
				    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
				    Reply={error,[Worker,ServiceStr,Err]};
				ok->
				    Reply={ok,Worker,ServiceStr,VsnLatest}
			    end;
			less ->
			    Reply={ok,Worker,ServiceStr,Vsn};
			equal ->
			    Reply={ok,Worker,ServiceStr,Vsn}
		    end
	    end
    end,
    Reply.

load_test_service(ServiceStr,VsnLatest,Worker,false)->		    
    ok.
		    
remove_service(_ServiceStr,_Worker)->
    ok.
load_start_service(ServiceStr,Worker)->
    


    ok.  
stop_unload_service(ServiceStr,Worker)->
    ServiceModule=list_to_atom(ServiceStr),
    io:format("~p~n",[{?MODULE,?LINE,ServiceStr,sd:call({Worker,brd_mgr},application,stop,[ServiceModule],5000)}]),
    io:format("~p~n",[{?MODULE,?LINE,ServiceStr,sd:call({Worker,brd_mgr},application,unload,[ServiceModule],5000)}]),
    ok.
    

load_test_service(ServiceStr,Worker)->
    VsnLatest=glurk,
    NewServiceDir=ServiceStr++"-"++VsnLatest,
 %   io:format("NewServiceDir = ~p~n",[{?MODULE,?LINE,NewServiceDir}]), 	 
    NewEbinPath=filename:join(NewServiceDir,"ebin"),
    true=sd:call({Worker,brd_mgr},code,add_patha,[NewEbinPath],5000),
 %   true=sd:call({Worker,brd_mgr},code,add_path,[NewEbinPath],5000),
    ServiceModule=glurk,
    io:format("NewDir = ~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},code,lib_dir,[ServiceModule],5000)}]),
    {ok,NewBaseNames}= sd:call({Worker,brd_mgr},file,list_dir,[NewEbinPath],5000),
    load_files(NewBaseNames,{Worker,brd_mgr},NewEbinPath),

    io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,load,[ServiceModule],5000)}]),
    io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,start,[ServiceModule],5000)}]),
    case sd:call({Worker,brd_mgr},ServiceModule,test,[],5000) of
	ok->
	    Reply=ok;
	Err ->
	    % Remove NewService
	  %  io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,stop,[ServiceModule],5000)}]),
	  %  io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,unload,[ServiceModule],5000)}]),
	  %  purge_files(NewBaseNames,{Worker,brd_mgr},NewEbinPath),
	  %  sd:call({Worker,brd_mgr},code,del_path,[NewServiceDir],5000),
	    
	  %  true=sd:call({Worker,brd_mgr},code,add_patha,[EbinCurrentServicePath],5000),
	  %  {ok,NewBaseNames}= sd:call({Worker,brd_mgr},file,list_dir,[NewEbinPath],5000),
	  %  load_files(NewBaseNames,{Worker,brd_mgr},NewEbinPath),
	    
	  %  io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,load,[ServiceModule],5000)}]),
	  %  io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,start,[ServiceModule],5000)}]),		
	%	true=sd:call({Worker,brd_mgr},os,cmd,["rm -r "++ NewServiceDir],5000),
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.   


	    
%	    CurrentServiceDir=filename:basename(CurrentPath),	   
%	    io:format("CurrentServiceDir = ~p~n",[{?MODULE,?LINE,CurrentServiceDir}]), 	    	    
%	    EbinCurrentServicePath=filename:join(CurrentPath,"ebin"),
%	    io:format("~p~n",[{?MODULE,?LINE,EbinCurrentServicePath}]),
%	    {ok,CurrentBaseNames}= sd:call({Worker,brd_mgr},file,list_dir,[EbinCurrentServicePath],5000),
%	    io:format("CurrentBaseNames = ~p~n",[{?MODULE,?LINE,CurrentBaseNames}]), 	 
%	    purge_files(CurrentBaseNames,{Worker,brd_mgr},EbinCurrentServicePath),
%	    sd:call({Worker,brd_mgr},code,del_path,[CurrentServiceDir],5000)
 %   end,
    
  

load_files([],_,_)->
  %  io:format("load files ~p~n",[{?MODULE,?LINE,[]}]),
    ok;
load_files([BaseName|T],Resource,EbinCurrentServicePath) ->
    _RelativeFileName=filename:join(EbinCurrentServicePath,BaseName),
    case filename:extension(BaseName) of
	".beam"->
	    ModuleStr=filename:basename(BaseName,".beam"),
	    Module=list_to_atom(ModuleStr),
%	    io:format("load Module ~p~n",[{?MODULE,?LINE,Module}]),
	    {module,Module}=sd:call(Resource,code,load_file,[Module],5000);
	_ ->
	    do_nothing
    end,
    load_files(T,Resource,EbinCurrentServicePath).

purge_files([],_,_)->
 %   io:format("Purge  Module ~p~n",[{?MODULE,?LINE,[]}]),
    ok;
purge_files([BaseName|T],Resource,EbinCurrentServicePath) ->
  %  io:format("Purge  Module ~p~n",[{?MODULE,?LINE,BaseName,EbinCurrentServicePath}]),
    _RelativeFileName=filename:join(EbinCurrentServicePath,BaseName),
    case filename:extension(BaseName) of
	".beam"->
	    ModuleStr=filename:basename(BaseName,".beam"),
	    Module=list_to_atom(ModuleStr),
%	    io:format("Purge  Module ~p~n",[{?MODULE,?LINE,Module}]),
	    sd:call(Resource,code,purge,[Module],5000);
	Err->
	    io:format("do nothing ~p~n",[{?MODULE,?LINE,Err}]),
	    do_nothing
    end,
    purge_files(T,Resource,EbinCurrentServicePath).



connect_worker(Worker) ->
    [{brd_ipaddr,IpAddr},
     {brd_port,BrdPort},
     {worker_port,_}]=sd:call(dbase,infra_dbase,read_object,[object,{worker,Worker}],5000),
    true=sd:add_resource({Worker,brd_mgr},IpAddr,BrdPort),
 %   io:format("~p~n",[{?MODULE,?LINE,Worker}]),
    case sd:call({Worker,brd_mgr},brd_mgr,connect,[],5000) of
	active->
%	    io:format("~p~n",[{?MODULE,?LINE,Worker}]),
	    Reply={active,Worker,[]};   
	standby ->
	   sd:delete_resource({Worker,brd_mgr},IpAddr,BrdPort),
%	    io:format("~p~n",[{?MODULE,?LINE,Worker}]),
	    Reply={standby,Worker,[]};  
	{error,Err}->
	    sd:delete_resource({Worker,brd_mgr},IpAddr,BrdPort),
%	    io:format("~p~n",[{?MODULE,?LINE,Worker,error,Err}]),
	    Reply={error,Worker,[Err]};   
	Err ->
	    sd:delete_resource({Worker,brd_mgr},IpAddr,BrdPort),
%	    io:format("~p~n",[{?MODULE,?LINE,Worker,Err}]),
	    Reply={error,Worker,[Err]}   
    end,				
 %   io:format("~p~n",[{?MODULE,?LINE}]),  
    Reply.



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% active = services to run
%% canditate = temporary dir where new load is loaded and teast befor update
%% rollback = active -> rollback when updating
%% --------------------------------------------------------------------
load_services(latest_release,Worker)->
    %% Preventive action stop candidate vm and  delete candidate dir
    Host=sd:call(Worker,net_adm,localhost,[],5000),
    CandidateNodeStr="candidate@"++Host,
    CandidateNode=list_to_atom(CandidateNodeStr),
    sd:call(Worker,rpc,call,[CandidateNode,erlang,halt,[]],5000),
    sd:call(Worker,os,cmd,["rm -r candidate"],5000),
    timer:sleep(500),
    
    %% Create candidate dir and  services dirs
    
    CandidateDir="candidate",
    CandidateNodeName="candidate",
    case start_node(Worker,CandidateDir,CandidateNodeName) of
	{ok,CandidateNode}->
	    ok=sd:call(Worker,file,make_dir,[CandidateDir],5000),
	    MandatoryServices=sd:call(dbase,infra_dbase,read_object,[services,{deployment,"mandatory"}],5000),
	    ResultMandatory=store_test_tar_files(MandatoryServices,Worker,CandidateDir,CandidateNode,[]),
	  %  io:format("~p~n",[{?MODULE,?LINE,time(),self(),ResultMandatory}]),	   
	    WorkerServices=sd:call(dbase,infra_dbase,read_object,[services,{deployment,Worker}],5000),
	    ResultWorkers=store_test_tar_files(WorkerServices,Worker,CandidateDir,CandidateNode,[]),
	 %   io:format("~p~n",[{?MODULE,?LINE,time(),self(),ResultWorkers}]),	
	  %  ok=stop_node( sd:call(Worker,rpc,call,[CandidateNode,erlang,halt,[]],5000),
	    case {ResultMandatory,ResultWorkers} of
		{ok,ok}->
		    Reply=ok;
		_->
		    Reply={error,{?MODULE,?LINE,ResultMandatory,ResultWorkers}}
	    end,
	    sd:call(Worker,rpc,call,[CandidateNode,erlang,halt,[]],5000);
	{error,Err}->
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.

store_test_tar_files([],_Worker,_CandidateDir,_CandidateNode,Acc)->
    Acc;
store_test_tar_files([{Service,_NumInstances}|T],Worker,CandidateDir,CandidateNode,_Acc)->
    Key={service,Service},
    case sd:call(dbase,infra_dbase,read_object,[latest_release,Key],5000) of
	{error,Err}->
	    NewAcc={error,{?MODULE,?LINE,Err}},
	    NewT=[];
	LatestStr->
	    {TarFileName,Binary}=sd:call(dbase,infra_dbase,read_object,[LatestStr,Key],5000),
	    ServiceDir=filename:join(CandidateDir,Service),    
	    case sd:call(Worker,file,make_dir,[ServiceDir],5000) of
		ok->
		    case sd:call(Worker,file,write_file,[TarFileName,Binary],5000) of
			ok->
			    TarCmd="tar -xvf "++TarFileName++" -C "++ServiceDir,
			    sd:call(Worker,os,cmd,[TarCmd],5000),
			   % timer:sleep(1000),
			    Ebin=filename:join(ServiceDir,"ebin"),
		%	     io:format("~p~n",[{?MODULE,?LINE,time(),self(),sd:call(Worker,rpc,call,[CandidateNode,code,add_patha,[Ebin]],5000)}]),
			    sd:call(Worker,rpc,call,[CandidateNode,code,add_patha,[Ebin]],5000),
			    case test_service(Service,CandidateNode,Worker) of
				ok->
				    NewAcc=ok,
				    NewT=T;
				{error,Err}->
				    NewAcc={error,{?MODULE,?LINE,Err}},
				    NewT=[]
			    end;
			{error,Err}->
			    NewAcc={error,{?MODULE,?LINE,Err}},
			    NewT=[]
		    end;
		{error,eexist} ->
		    case sd:call(Worker,file,write_file,[TarFileName,Binary],5000) of
			ok->
			    TarCmd="tar -xvf "++TarFileName++" -C "++ServiceDir,
			    sd:call(Worker,os,cmd,[TarCmd],5000),
			    %timer:sleep(1000),
			    _Ebin=filename:join(ServiceDir,"ebin"),
%			    io:format("~p~n",[{?MODULE,?LINE,sd:call(Worker,rpc,call,[CandidateNode,code,add_patha,[Ebin]],5000)}]),
			    case test_service(Service,CandidateNode,Worker) of
				ok->
				    NewAcc=ok,
				    NewT=T;
				{error,Err}->
				    NewAcc={error,{?MODULE,?LINE,Err}},
				    NewT=[]
			    end;
			{error,Err}->
			    NewAcc={error,{?MODULE,?LINE,Err}},
			    NewT=[]
		    end;
		Err->
		    NewAcc={error,{?MODULE,?LINE,Err}},
		    NewT=[]
	    end
    end,    
    store_test_tar_files(NewT,Worker,CandidateDir,CandidateNode,NewAcc).

start_node(Worker,Dir,NodeName)->
 %   io:format("~p~n",[{?MODULE,?LINE,sd:call(Worker,file,get_cwd,[],5000)}]),
    
    EbinDirs=filename:join([Dir,"*","ebin"]),
%    io:format("~p~n",[{?MODULE,?LINE,EbinDirs}]),
    Host=sd:call(Worker,net_adm,localhost,[],5000),
    NodeStr=NodeName++"@"++Host,
    Node=list_to_atom(NodeStr),
    ErlCmd="erl -pa "++EbinDirs++" "++"-sname "++NodeStr++" "++"-detached",
    
    case sd:call(Worker,os,cmd,[ErlCmd],5000) of
	[]->
	    Reply={ok,Node},
	    timer:sleep(3000); % Wait for Linux to start the VM
	Err->

	    Reply={error,{?MODULE,?LINE,couldnt_start_node,Err}}
    end,
    Reply.


test_service(ServiceStr,Node,Worker) ->
    Service=list_to_atom(ServiceStr),
    _Ping=sd:call(Worker,net_adm,ping,[Node],5000),
    _Path=sd:call(Worker,rpc,call,[Node,code,get_path,[]],5000),
    R1=sd:call(Worker,rpc,call,[Node,application,load,[Service]],5000),
    R2=sd:call(Worker,rpc,call,[Node,application,start,[Service]],5000),
    R3=sd:call(Worker,rpc,call,[Node,Service,test,[]],5000),
    case {R1,R2,R3} of
	{ok,ok,ok}->
	    Reply=ok;
	 _->
	    Reply={error,{?MODULE,?LINE,Service,R1,R2,R3}}
    end,
    Reply.




%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
  
test_services([],_Worker,_Node,ok)->
    ok;
test_services([],_Worker,_Node,{error,Service})->
    {error,Service};
test_services(_ListServicesStr,_Worker,_Node,{error,Service}) ->
    {error,Service};
test_services([ServiceStr|T],Worker,Node,_Acc) ->
    io:format("Testing service ~p~n",[{Worker,ServiceStr}]),
    Service=list_to_atom(ServiceStr),
    R1=sd:call(Worker,rpc,call,[Node,application,load,[Service]],5000),
    R2=sd:call(Worker,rpc,call,[Node,application,start,[Service]],5000),
    R3=sd:call(Worker,rpc,call,[Node,Service,test,[]],5000),
    case {R1,R2,R3} of
	{ok,ok,ok}->
	    NewAcc=ok;
	_ ->
	    NewAcc={error,Service}
    end,
    test_services(T,Worker,Node,NewAcc).
    
		     

	    
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
