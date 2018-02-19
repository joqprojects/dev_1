%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% 
%%% -------------------------------------------------------------------
-module(test_infra_dbase).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).
%%
%% API Functions
%%


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
start_test()->
    {ok,ok}=infra_dbase:start_dbase(),
    % start sd for test file
    ok=application:load(sd),
    ok=application:start(sd),
    true=sd:add_resource(infra_dbase,"80.216.90.159",10000),
   [{infra_dbase,"80.216.90.159",10000}]=sd:resources(infra_dbase),
    ok.

start_again_test()->
    {{error,{already_loaded,infra_dbase}},
     {error,{already_started,infra_dbase}}}=infra_dbase:start_dbase(),
    ok.

not_initiated_test()->
    {badrpc,Err}=sd:call(infra_dbase,infra_dbase,id_all_workers,[],1000),
    false=sd:call(infra_dbase,infra_dbase,initiated,[],1000),
    ok.

initiate_test()->
    {ok,InitialValues}=file:consult("../infra_master/include/initial.config"),
    ok =sd:call(infra_dbase,infra_dbase,init_dbase,[InitialValues],1000),
    true=sd:call(infra_dbase,infra_dbase,initiated,[],1000),
    ok.


%%---------------------------------
%% CRUD Create,Read,Update,Delete 
%%---------------------------------
%%
%% Board_information
%% Key={ObjectType,Id}
%% Value for worker r=[{brd_ipaddr,IpAddr},{brd_port,BrdPort},{worker_port,WorkerPort}]
%% Value for infra_master =[{brd_ipaddr,IpAddr},{brd_port,BrdPort}]
%% Value for infra_dbase = [{brd_ipaddr,IpAddr},{brd_port,BrdPort},{dbase_config,[{type,set}]}]
%%
%% 

create_object_dbase_test()->
    Key={worker,"gen_worker_1"},
    IpAddr="80.216.90.159",
    BrdPort=10601,
    WorkerPort=10602,
    Object=[{brd_ipaddr,IpAddr},{brd_port,BrdPort},
				      {worker_port,WorkerPort}],
    false=infra_dbase:member_object(Key), 
    ok=infra_dbase:create_object(Key,Object),
    true=infra_dbase:member_object(Key),
    {error,{infra_dbase_lib,_,
	    object_already_exista,
	    {worker,"gen_worker_1"}}}=infra_dbase:create_object(Key,Object),
    ok.

% Read
read_object_dbase_test()->
    Key={worker,"gen_worker_1"},
    Item=object,
    [{brd_ipaddr,"80.216.90.159"},
     {brd_port,10601},
     {worker_port,10602}]=infra_dbase:read_object(Item,Key),
    "80.216.90.159"=infra_dbase:read_object(brd_ipaddr,Key),
    10601=infra_dbase:read_object(brd_port,Key),
    10602=infra_dbase:read_object(worker_port,Key),
    ok.

%% Update
update_object_dbase_test()->
    Key={worker,"gen_worker_1"},
    Item=object,
    [{brd_ipaddr,"80.216.90.159"},
     {brd_port,10601},
     {worker_port,10602}]=infra_dbase:read_object(Item,Key),
    
    ok=infra_dbase:update_object({brd_port,12345},Key),
    
    [{brd_ipaddr,"80.216.90.159"},
     {brd_port,12345},
     {worker_port,10602}]=infra_dbase:read_object(Item,Key),
    
    ok.
delete_object_dbase_test()->
    Key={worker,"gen_worker_1"},
    Item=object,
    [{brd_ipaddr,"80.216.90.159"},
     {brd_port,12345},
     {worker_port,10602}]=infra_dbase:read_object(Item,Key),
    
    ok=infra_dbase:delete_object(Key),
    
   {error,{infra_dbase_lib,_,
	   object_not_exist,
	   {worker,"gen_worker_1"}}}=infra_dbase:read_object(Item,Key),
    ok.

%%----------------------------------------------------------------------------------
%% CRUD service object
%% service_information
%% Key={service,Id}
%% Value for service=[{latest_release,RelNum,},{relNum1,TarFileName,TarFileBin},{relNumN,TarFileName,TarFileBin}]
crud_service_object_test()->
    Key={service,"myadd"},
    Value=[{latest_release,"102"},
	   {"100",{"myadd.tar",myadd_tar_bin_100}},
	   {"102",{"myadd.tar",myadd_tar_bin_102}},
	   {"101",{"myadd.tar",myadd_tar_bin_101}}],
    ok=infra_dbase:create_object(Key,Value),
    "102"=infra_dbase:read_object(latest_release,Key),
    [{latest_release,"102"},
     {"100",{"myadd.tar",myadd_tar_bin_100}},
     {"102",{"myadd.tar",myadd_tar_bin_102}},
     {"101",{"myadd.tar",myadd_tar_bin_101}}]=infra_dbase:read_object(object,Key),
    NewRelNum="103",
    TarFileName="myadd.tar",
    NewTarFileBin=myadd_tar_bin_103,
    NewRelease={NewRelNum,TarFileName,NewTarFileBin},
    ok=infra_dbase:update_object({NewRelNum,NewRelease},Key),
    [{latest_release,"102"},
     {"100",{"myadd.tar",myadd_tar_bin_100}},
     {"102",{"myadd.tar",myadd_tar_bin_102}},
     {"101",{"myadd.tar",myadd_tar_bin_101}},
     {"103",{"103","myadd.tar",myadd_tar_bin_103}}]=infra_dbase:read_object(object,Key),

    ok.


%%----------------------------------------------------------------------------------
%% CRUD generic object
%% Release_information
%% Key={release,Id}
%% Value for Release=[{service_1,RelNum},{service_N,RelNum}]
crud_gen_test()->
    Key={release,"100"},
    Value=[{s1,"103"},{s2,"221"},{s3,"303"},{s4,"401"}],
    ok=infra_dbase:create_object(Key,Value),
    "303"=infra_dbase:read_object(s3,Key),
    [{s1,"103"},{s2,"221"},
     {s3,"303"},{s4,"401"}]=infra_dbase:read_object(object,Key),
     {error,{infra_dbase_lib,_,item_not_exist,glurk}}=infra_dbase:read_object(glurk,Key),
    ok.



stop_test()->    
    file:delete("infra_dbase.file"),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.

%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
