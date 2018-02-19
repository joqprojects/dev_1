%% This is the application resource file (.app file) for the 'base'
%% application.
{application, sd,
[{description, "sd server using dets " },
{vsn, "1.0.0" },
{modules, 
	  [sd_app,sd_sup,sd,sd_lib,test_sd]},
{registered,[sd]},
{applications, [kernel,stdlib]},
{mod, {sd_app,[]}},
{start_phases, []}
]}.
