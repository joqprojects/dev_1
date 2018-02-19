%% This is the application resource file (.app file) for the 'base'
%% application.
{application, brd_mgr,
[{description, "brd_mgr server using dets " },
{vsn, "1.0.0" },
{modules, 
	  [brd_mgr_app,brd_mgr_sup,brd_mgr,brd_mgr_lib,test_brd_mgr]},
{registered,[brd_mgr]},
{applications, [kernel,stdlib]},
{mod, {brd_mgr_app,[]}},
{start_phases, []}
]}.
