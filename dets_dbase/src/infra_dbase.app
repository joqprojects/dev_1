%% This is the application resource file (.app file) for the 'base'
%% application.
{application, infra_dbase,
[{description, "infra_dbase server using dets " },
{vsn, "1.0.0" },
{modules, 
	  [infra_dbase_app,infra_dbase_sup,infra_dbase,infra_dbase_lib,test_infra_dbase]},
{registered,[infra_dbase]},
{applications, [kernel,stdlib]},
{mod, {infra_dbase_app,[]}},
{start_phases, []}
]}.
