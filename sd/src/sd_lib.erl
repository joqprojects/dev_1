%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sd_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([resources/2,resources/1,
	add_resource/4,
	delete_resource/4]).

-record(state, {resources}).
%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
resources(Service,State)->
    Reply=ets:lookup(State#state.resources,Service),
    Reply.

resources(State)->
    Reply=ets:tab2list(State#state.resources),
    Reply.

add_resource(Service,Ip,Port,State)->
    Reply=ets:insert(State#state.resources,{Service,Ip,Port}),
    Reply.

delete_resource(Service,Ip,Port,State)->
    Reply=ets:delete_object(State#state.resources,{Service,Ip,Port}),
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

%% Function: l_get/0
%% Description:local get funtion used by several server functions
%% Returns: {ok,Value}|{error,Errcode}
%% --------------------------------------------------------------------
