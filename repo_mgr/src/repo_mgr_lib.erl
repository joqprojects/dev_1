%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(repo_mgr_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).


%-export([create_update_service/2,
%	 update_service/2,read_service/2,delete_service/1]).


%export([build_release/2,
%	 get_services_tar/1,get_services_tar/2]).





%% ====================================================================
%% External functions
%% ====================================================================

unix_tar(BaseFileName,Dir)->
    BaseDir=filename:basename(Dir),
    case filelib:is_dir(BaseDir) of
	true->
	    os:cmd("tar -czf "++BaseFileName++" "++Dir);
	false->
	    io:format("~p~n",[{?MODULE,?LINE,BaseFileName,Dir,BaseDir}]),
	    []=os:cmd("cp -R "++Dir++" "++"."),
	    []=os:cmd("tar -czf "++BaseFileName++" "++BaseDir),
	    []=os:cmd("rm -r "++BaseDir)
	    
    end,
    case filelib:is_file(BaseFileName) of
	true->
	    Reply={ok,BaseFileName};
	 false->
	    Reply={error,{?MODULE,?LINE,failed_to_create_tar_file,BaseFileName}}
    end,
    Reply.

unix_untar(BaseFileName,Dir)->
    os:cmd("tar -xzf "++BaseFileName++" -C "++Dir),
    ok.



%tar -czf web.tar web
%tar -xzvf archive.tar.gz

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
latest_release(ServiceStr)->
    KeyLatest={latest,ServiceStr},
    LatestVsn=sd:call(dbase,infra_dbase,read_object,[vsn,KeyLatest],5000),
    LatestVsn.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
build_release(Release,DevAreaDir)->
    case file:list_dir(DevAreaDir) of
	{ok,ServiceDirs}->
	    {Result,List}=build(ServiceDirs,DevAreaDir,{ok,[]}),
	    case Result of
		ok->
		    Key={main_release,Release},
		    case sd:call(dbase,infra_dbase,member_object,[Key],5000) of
			false->
			    NewObjectList=[{services,List}],
			    Reply=sd:call(dbase,infra_dbase,create_object,[Key,NewObjectList],5000);
			true->
			    NewObjectList={services,List},
			    Reply=sd:call(dbase,infra_dbase,update_object,[{service,NewObjectList},Key],5000)
		    end;
		build_failed->
		    Reply={Result,Release,List}
	    end;
	{error,Err}->		
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.

build([],_DevAreaDir,Acc)->
    Acc;

build([ServiceStr|T],DevAreaDir,{Result,List})->
    Key={service,ServiceStr},
    case sd:call(dbase,infra_dbase,member_object,[Key],5000) of
	false->
	    case create_update_service(ServiceStr,DevAreaDir) of
		ok->
		    NewResult=Result,
		    NewList=[ServiceStr|List],
		    NewT=T;
		{error,Err}->
		    NewResult={error,Err},
		    NewList=[ServiceStr|List],
		    NewT=[]
	    end;
	true->
	    case update_service(ServiceStr,DevAreaDir) of 
		ok->
		    NewResult=Result,
		    NewList=[ServiceStr|List],
		    NewT=T;
		{error,Err}->
		    NewResult={error,Err},
		    NewList=[ServiceStr|List],
		    NewT=[]
	    end
    end,
    build(NewT,DevAreaDir,{NewResult,NewList}).



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%% service_information
%% Key={service,Id}
%% Value for service=[{latest_release,RelNum,},{relNum1,TarFileName,TarFileBin},{relNumN,TarFileName,TarFileBin}]
create_update_service(ServiceStr,DevAreaDir)->
    Service=list_to_atom(ServiceStr),
    SrcEbinDir=filename:join([DevAreaDir,ServiceStr,"ebin"]),
    AppBaseName=ServiceStr++".app",
    AppFileName=filename:join(SrcEbinDir,AppBaseName),
    case file:consult(AppFileName) of
	{ok,[{application,Service,AppInfo}]}->
	    case update_release_info(AppInfo,ServiceStr,SrcEbinDir) of
		{error,Err}->
		    Reply={error,Err};
		{ok,Vsn}->
		    Reply=update_latest(Vsn,ServiceStr)
	    end;
	{error,Err}->
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.

update_latest(Vsn,ServiceStr)->
    KeyLatest={latest,ServiceStr},
    case sd:call(dbase,infra_dbase,member_object,[KeyLatest],5000) of
	false->
	    Reply=sd:call(dbase,infra_dbase,create_object,[KeyLatest,[{vsn,Vsn}]],5000);	
	true->
	    LatestVsn=sd:call(dbase,infra_dbase,read_object,[vsn,KeyLatest],5000),
	    case cmp_vsn_strings(Vsn,LatestVsn) of
		less->
		    Reply={error,{?MODULE,?LINE,not_update}};
		equal->
		    Reply=sd:call(dbase,infra_dbase,update_object,[{vsn,Vsn},KeyLatest],5000);
		larger->
		    Reply=sd:call(dbase,infra_dbase,update_object,[{vsn,Vsn},KeyLatest],5000)
	    end
    end,
    Reply.




update_release_info(AppInfo,ServiceStr,SrcEbinDir)->
    {Vsn,_Modules}=extract_vsn_modules(AppInfo),
    {TarBaseName,Binary}=create_tar_file(Vsn,ServiceStr,SrcEbinDir),
    KeyReleaseData={service,ServiceStr,Vsn},
    case sd:call(dbase,infra_dbase,member_object,[KeyReleaseData],5000) of
	false->
	    NewReleaseData=[{release,[TarBaseName,Binary]}],
	    _Reply=sd:call(dbase,infra_dbase,create_object,[KeyReleaseData,NewReleaseData],5000);	
	true->
	    NewReleaseData=[TarBaseName,Binary],
	    _Reply=sd:call(dbase,infra_dbase,update_object,[{release,NewReleaseData},KeyReleaseData],5000)
    end,
    ok=file:delete(TarBaseName),
    timer:sleep(500),
    {ok,Vsn}.


extract_vsn_modules(AppInfo)->
    {vsn,Vsn}=lists:keyfind(vsn,1,AppInfo),
    {modules,Modules}=lists:keyfind(modules,1,AppInfo),
    {Vsn,Modules}.

create_tar_file(Vsn,ServiceStr,SrcEbinDir)->
    case file:list_dir(SrcEbinDir) of
	{error,Err} ->
	    Reply={error,Err};
	{ok,FilesBaseNames}->
	    RelativeDir=ServiceStr++"-"++Vsn,
	    os:cmd("rm -r "++RelativeDir),
	    timer:sleep(300),
	    ok=file:make_dir(RelativeDir),
	    RelativeEbin=filename:join(RelativeDir,"ebin"),
	    ok=file:make_dir(RelativeEbin),
	    TarBaseName=ServiceStr++"-"++Vsn++".tar",
	    FilesRelativeNames=create_tar_file(FilesBaseNames,SrcEbinDir,RelativeEbin,[]),
	    ok=erl_tar:create(TarBaseName,FilesRelativeNames),
	    {ok,Binary}=file:read_file(TarBaseName),
	    os:cmd("rm -r "++ServiceStr),
	    timer:sleep(300),
	    Reply={TarBaseName,Binary}
    end,
    Reply.

create_tar_file([],_SrcEbinDir,_RelativeEbin,Acc)->
    Acc;
create_tar_file([FileBaseName|T],SrcEbinDir,RelativeEbin,Acc)->
    SrcFullName=filename:join(SrcEbinDir,FileBaseName),
    DestRelativeName=filename:join(RelativeEbin,FileBaseName),
    {ok,Binary}=file:read_file(SrcFullName),
    ok=file:write_file(DestRelativeName,Binary),
    NewAcc=[DestRelativeName|Acc],
    create_tar_file(T,SrcEbinDir,RelativeEbin,NewAcc).
    
cmp_vsn_strings(Vsn_A,Vsn_B)->
    % Vsn="2.42.120"
    % -> equal,A lesser B ,A larger B
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
update_service(ServiceDir,DevAreaDir)->
    TarFileName=ServiceDir++".tar",
    FullTarFileName=filename:join([DevAreaDir,ServiceDir,TarFileName]),
    case file:read_file(FullTarFileName) of
	{ok,Binary}->
	    {ok,{ServiceDir,{Release,TarFaleName,Binary}}}=get_services_tar(ServiceDir,DevAreaDir),
	    Key={service,ServiceDir},
	    NewServiceTuple={Release,{TarFaleName,Binary}},
	    NewLatestTuple={latest_release,Release},
	    case sd:call(dbase,infra_dbase,member_object,[Key],5000) of
		false->
		    NewObjectList=[NewLatestTuple,NewServiceTuple],
		    Reply=sd:call(dbase,infra_dbase,create_object,[Key,NewObjectList],5000);
		true->
		    LatestStr=sd:call(dbase,infra_dbase,read_object,[latest_release,Key],5000),
		    Latest=list_to_integer(LatestStr),
		    NewRel=list_to_integer(Release),
		    if
			NewRel>=Latest ->
			    R1=sd:call(dbase,infra_dbase,update_object,[NewServiceTuple,Key],5000),
			    R2=sd:call(dbase,infra_dbase,update_object,[NewLatestTuple,Key],5000),
			    case {R1,R2} of
				{ok,ok}->
				    Reply=ok;
				{{error,Err},_}->
				    Reply={error,Err};
				{_,{error,Err}}->
				     Reply={error,Err}
			    end;
			true ->
			    Reply={error,{?MODULE,?LINE,new_release_to_old,Latest,">",NewRel}}
		    end;
		{error,Err} ->
		    Reply={error,{?MODULE,?LINE,Err}}
	    end
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
read_service(Vsn,ServiceStr)->
    case Vsn of
	latest_release->
	    case latest_release(ServiceStr) of
		{error,Err}->
		    Reply={error,{?MODULE,?LINE,Err}};
		LatestVsn->
		    KeyReleaseData={service,ServiceStr,LatestVsn},
		    Reply=sd:call(dbase,infra_dbase,read_object,[release,KeyReleaseData],5000)
	    end;
	Vsn->
	    KeyReleaseData={service,ServiceStr,Vsn},
	    Reply=sd:call(dbase,infra_dbase,read_object,[release,KeyReleaseData],5000)
    end,
    Reply. 

delete_service(Service)->
    Key={service,Service},
    Reply=sd:call(dbase,infra_dbase,delete_object,[Key],5000),
    Reply. 
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_services_tar(ServiceDir,DevAreaDir)->
    FullNameServiceDir=filename:join(DevAreaDir,ServiceDir),
    case filelib:is_dir(FullNameServiceDir) of
	false->
	    Reply={error,{?MODULE,?LINE,ServiceDir,enoent}};
	true ->
	    AppFile=ServiceDir++".app",
	    FullFileName=filename:join([DevAreaDir,ServiceDir,"ebin",AppFile]),
	    case file:consult(FullFileName) of
		{error,Err}->
		    Reply={error,{?MODULE,?LINE,ServiceDir,Err}};
		{ok,[{application,_Service,Info}]}->
		    TarFile=filename:join([DevAreaDir,ServiceDir,ServiceDir++".tar"]),
		    case file:read_file(TarFile) of
			{error,Err}->
			    Reply={error,{?MODULE,?LINE,ServiceDir,ServiceDir++".tar",Err}};
			{ok,Binary}->
			    {vsn,Rev}=lists:keyfind(vsn,1,Info),
			    Reply={ok,{ServiceDir,{Rev,ServiceDir++".tar",Binary}}}
		    end
	    end
    end,
    Reply.


get_services_tar(DevAreaDir)->
    %DevAreDir   service/ebin/service.app
    case file:list_dir(DevAreaDir) of
	{ok,ListOfServiceDirs}->
	    ServiceVsnList=extract_service_app(ListOfServiceDirs,DevAreaDir,[]),
	    Reply={ok,ServiceVsnList};
	{error,Err}->
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.

extract_service_app([],_DevAreaDir,Acc)->
    Acc;
extract_service_app([ServiceDir|T],DevAreaDir,Acc)->
    FullNameServiceDir=filename:join(DevAreaDir,ServiceDir),
    case filelib:is_dir(FullNameServiceDir) of
	false->
	    NewAcc=[{error,{?MODULE,?LINE,ServiceDir,enoent}}|Acc];
	true ->
	    AppFile=ServiceDir++".app",
	    FullFileName=filename:join([DevAreaDir,ServiceDir,"ebin",AppFile]),
	    case file:consult(FullFileName) of
		{error,Err}->
		    NewAcc=[{error,{?MODULE,?LINE,ServiceDir,Err}}|Acc];
		{ok,[{application,_Service,Info}]}->
		    TarFile=filename:join([DevAreaDir,ServiceDir,ServiceDir++".tar"]),
		    case file:read_file(TarFile) of
			{error,Err}->
			    NewAcc=[{error,{?MODULE,?LINE,ServiceDir,ServiceDir++".tar",Err}}|Acc];
			{ok,Binary}->
			    {vsn,Rev}=lists:keyfind(vsn,1,Info),
			    NewAcc=[{ok,{ServiceDir,{Rev,ServiceDir++".tar",Binary}}}|Acc]
		    end
	    end
    end,
    extract_service_app(T,DevAreaDir,NewAcc).
	    
		    



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
%% -------------------------------------------------------------------

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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%%-------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
	  


