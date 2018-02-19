%% This is the application resource file (.app file) for the 'base'
%% application.
{application, infra_master,
[{description, "infra_master manager " },
{vsn, "1.0.0" },
{modules, 
	  [infra_master_app,infra_master_sup,infra_master]},
{registered,[infra_master]},
{applications, [kernel,stdlib]},
{mod, {infra_master_app,[]}},
{start_phases, []}
]}.
