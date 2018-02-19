%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(infra_dbase_lib).
 

%% --------------------------------------------------------------------
%% Data structure
%% --------------------------------------------------------------------
%  service_release: {service_release,Service,Rel,Latest},{TarFileName,TarFileBin}
%% board_object: {board_object,BoardId},[{brd_ipaddr,IpAddr},{brd_port,BrdPort},{worker_port,WorkerPort}]
%% main_release: {main_release,Rel},[{Service,Rel},{Service,Rel}]
%% master_release:{master_release,Service,Rel,Latest},{TarFileName,TarFileBin}
%% master_object: {master_object,BoardId},[{brd_ipaddr,IpAddr},{brd_port,BrdPort}]
%% dbase_release:{dbase_release,Service,Rel,Latest},{TarFileName,TarFileBin}
%% dbase_object: {dbase_object,BoardId},[{brd_ipaddr,IpAddr},{brd_port,BrdPort}]
%% capability: {capability,Service},[BoardId_1,BoardId_2]
%% log:{log,Service},[{Time,Module,Line,Type(info,event,error),[Info]}, ...]
%% tcp_setup:{tcp_setup,server},[ ]
%% tcp_setup:{tcp_setup,client},[ ]

% 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([member_object/2,
	 create_object/3,
	 delete_object/2,
	 update_object/3,
	 read_object/3,
	 master_object/2,
	 dbase_object/2,
	 board_object/2,
	 latest_release/2,
	 create_initial_dbase/2,
	 id_all_workers/1]).
-export([delete/1,exists/1,create/2,get/2,store/3,all/1,remove/2]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% {board_object,BoardId},[{brd_ipaddr,IpAddr},{brd_port,BrdPort},{worker_port,WorkerPort}]
%% ------------------------------------------------------------------
member_object(Key,DbaseFile)->
    Reply=member(Key,DbaseFile),
    Reply.


create_object(Key,Value,DbaseFile)->
    case get(Key,DbaseFile) of
	{ok,[]}->
	    Reply=infra_dbase_lib:store(Key,Value,DbaseFile);
	_->
	    Reply={error,{?MODULE,?LINE,object_already_exista,Key}}
    end,
    Reply.

delete_object(Key,DbaseFile)->
  %  Key={ObjectType,Id},
    Reply=infra_dbase_lib:remove(Key,DbaseFile),
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% ------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
update_object(NewValue,Key,DbaseFile)->
    {Item,NewObjectValue}=NewValue,
    case Item of
	object ->
	    Reply=store(Key,NewObjectValue,DbaseFile);
	Item->
	    case get(Key,DbaseFile) of
		{ok,[]}->
		    Reply={error,{?MODULE,?LINE,object_not_exist,Key}};
		{ok,[{Key,ObjectData}]}->
		    NewObjectData=lists:keystore(Item,1,ObjectData,{Item,NewObjectValue}),
		    Reply=store(Key,NewObjectData,DbaseFile)
	    end
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% {board_object,BoardId},[{brd_ipaddr,IpAddr},{brd_port,BrdPort},{worker_port,WorkerPort}]
%% --------------------------------------------------------------------
read_object(Item,Key,DbaseFile)->
    case Item of
	object ->
	    case get(Key,DbaseFile) of
		{ok,[]}->
		    Reply={error,{?MODULE,?LINE,object_not_exist,Key}};
		{ok,[{Key,ObjectData}]}->
		    Reply=ObjectData
	    end;
	Item->
	    case get(Key,DbaseFile) of
		{ok,[]}->
		    Reply={error,{?MODULE,?LINE,object_not_exist,Key}};
		{ok,[{Key,ObjectData}]}->
		    case lists:keyfind(Item,1,ObjectData) of
			false->
			    Reply={error,{?MODULE,?LINE,item_not_exist,Item}};
			{Item,Value} ->
			    Reply=Value;
			X ->
			    Reply={error,{?MODULE,?LINE,X}}
		    end;
		{ok,[ObjectData]}->
		    case lists:keyfind(Item,1,ObjectData) of
			false->
			    Reply={error,{?MODULE,?LINE,item_not_exist,Item}};
			{Item,Value} ->
			    Reply=Value
		    end;
		Err ->
		    Reply={error,{?MODULE,?LINE,programming_error,Err}}
	    end
    end,
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% {board_object,BoardId},[{brd_ipaddr,IpAddr},{brd_port,BrdPort},{worker_port,WorkerPort}]
%% --------------------------------------------------------------------
board_object({Item,Id},DbaseFile)->
    {ok,[{{board_object,Id},[{brd_ipaddr,IpAddr},{brd_port,BrdPort},
                 {worker_port,WorkerPort}]}]}=get({board_object,Id},DbaseFile),
    case Item of
	brd_ipaddr->
	    Reply=IpAddr;
	brd_port ->
	    Reply=BrdPort;
	worker_port->
	    Reply=WorkerPort;
	board_object->
	    {ok,BoardObject}=get({board_object,Id},DbaseFile),
	    Reply=BoardObject;
	Err->
	    Reply={error,{?MODULE,?LINE,item_not_exist,Err}}
    end,	
    Reply.
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%%{{master_object,infra_master},[{brd_ipaddr,"80.216.90.159"},{brd_port,10010}]}.
%% --------------------------------------------------------------------


master_object({Item,Id},DbaseFile)->
    {ok,[{{master_object,Id},[{brd_ipaddr,IpAddr},{brd_port,BrdPort}]}]}=get({master_object,Id},DbaseFile),
    case Item of
	brd_ipaddr->
	    Reply=IpAddr;
	brd_port ->
	    Reply=BrdPort;
	master_object->
	    {ok,MasterObject}=get({master_object,Id},DbaseFile),
	    Reply=MasterObject;
	Err->
	    Reply={error,{?MODULE,?LINE,item_not_exist,Err}}
    end,	
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% {{dbase_object,infra_dbase},[{brd_ipaddr,"80.216.90.159"},{brd_port,10000},{dets_config,[type,set]}]}.
%% --------------------------------------------------------------------
dbase_object({Item,Id},DbaseFile)->
    {ok,[{{dbase_object,Id},[{brd_ipaddr,IpAddr},{brd_port,BrdPort},
                 {dets_config,DestConfig}]}]}=get({dbase_object,Id},DbaseFile),
    case Item of
	brd_ipaddr->
	    Reply=IpAddr;
	brd_port ->
	    Reply=BrdPort;
	dets_config->
	    Reply=DestConfig;
	dbase_object->
	    {ok,DbaseObject}=get({dbase_object,Id},DbaseFile),
	    Reply=DbaseObject;
	Err->
	    Reply={error,{?MODULE,?LINE,item_not_exist,Err}}
    end,	
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
latest_release(ServiceStr,File)->
    case dets:open_file(File) of
	{error,Err}->
	    Reply={error,{?MODULE,?LINE,Err}};
	{ok,R}->
	    Reply=dets:match(R,{{'$1',ServiceStr},'$2'}),
	    dets:close(R)
    end,
    Reply.

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
create_initial_dbase(DbaseFile,InitialValuesFile)->
    case exists(DbaseFile) of
	false->
	    {ok,InitialValues}=file:consult(InitialValuesFile),
	   {{infra_dbase,capabilities},
	    [{infra_dbase,[{type,Type}]}]}=lists:keyfind({infra_dbase,capabilities},1,InitialValues),
	    case dets:open_file(DbaseFile,[{type,Type}]) of
		{badrpc,Err}->
		    Reply={error,{?MODULE,?LINE,badrpc,Err}};
		{ok,File}->	    
		    true=dets:insert_new(DbaseFile,InitialValues),
		    ok=dets:close(File),
		    Reply=ok
	    end;
	true->
	    Reply=ok
    end,
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
id_all_workers(DbaseFile)->
    {ok,R}=dets:open_file(DbaseFile),
    WorkersDetsList=dets:match(R,{{worker,'$1'},'$2'}),
    WorkersList=fix_worker_list(WorkersDetsList,[]),
    {ok,WorkersList}.

fix_worker_list([],Acc)->
    Acc;
fix_worker_list([[IdWorker|_]|T],Acc) ->
    NewAcc=[IdWorker|Acc],
    fix_worker_list(T,NewAcc).

    


%%-----------------------------------------------
delete(File) ->
    case filelib:is_file(File) of 
	true->
	    file:delete(File),
	    Reply={ok,file_deleted};
	false->
	   Reply={error,{?MODULE,?LINE,file_not_exist}}
    end,
    Reply.

exists(File) ->
    Reply=filelib:is_file(File),
    Reply.

create(Config,File) ->
    case filelib:is_file(File) of 
	true->
	    Reply={ok,file_already_exsist};
	false->
	    {ok,Descriptor}=dets:open_file(File,Config),
	    dets:close(Descriptor),
	    Reply={ok,file_created}
    end,
    Reply.

member(Key,File) ->
    {ok,Descriptor}=dets:open_file(File),
    Reply=dets:member(Descriptor,Key),
    dets:close(Descriptor),
    Reply.


store(Key,Value,File) ->
    case filelib:is_file(File) of 
	true->
	    {ok,Descriptor}=dets:open_file(File),
	    ok=dets:insert(Descriptor, {Key,Value}),
	    dets:close(Descriptor),
	    Reply={ok,store};
	false->
	    Reply = {error,{?MODULE,?LINE,file_not_exist}}
    end,
    Reply.


get(Key,File) ->
    Reply=l_get(Key,File),
    Reply.

all(File) ->
    case filelib:is_file(File) of 
	true->
	    {ok,Descriptor}=dets:open_file(File),
	    Key=dets:first(Descriptor),
	    Reply=get_all(Descriptor,Key,[]),
	    dets:close(Descriptor);
	false->
	    Reply = {error,{?MODULE,?LINE,file_not_exist}}
    end,
    Reply.

remove(Key,File) ->
    case filelib:is_file(File) of 
	true->
	    {ok,Descriptor}=dets:open_file(File),
	    case dets:lookup(Descriptor, Key) of
		[]->
		    Reply = {error,{?MODULE,?LINE,no_entry}};
		X->
		    ok=dets:delete(Descriptor, Key),
		    [{Key,_Value}]=X,
		    Reply={ok,Key}
	    end,
	    dets:close(Descriptor);
	false->
	    Reply = {error,{?MODULE,?LINE,file_not_exist}}
    end,
    Reply.


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: get_all/0
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
get_all(_Desc,'$end_of_table',Acc)->
    {ok,Acc};
get_all(Desc,Key,Acc)->  
    Status=dets:lookup(Desc, Key),
    Acc1=lists:append(Status,Acc),
    Key1=dets:next(Desc,Key),
    get_all(Desc,Key1,Acc1).

%% Function: l_get/0
%% Description:local get funtion used by several server functions
%% Returns: {ok,Value}|{error,Errcode}
%% --------------------------------------------------------------------
l_get(Key,File)->
    case filelib:is_file(File) of 
	true->
	    {ok,Descriptor}=dets:open_file(File),
	    Value=dets:lookup(Descriptor, Key),
	    Reply={ok,Value},
	    dets:close(Descriptor);
	false->
	    Reply = {error,{?MODULE,?LINE,file_not_exist}}
    end,
    Reply.
