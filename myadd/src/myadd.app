%% This is the application resource file (.app file) for the 'base'
%% application.
{application, myadd,
[{description, "myadd server using dets " },
{vsn, "3.0.0" },
{modules, 
	  [myadd_app,myadd_sup,myadd,myadd_lib,test_myadd]},
{registered,[myadd]},
{applications, [kernel,stdlib]},
{mod, {myadd_app,[]}},
{start_phases, []}
]}.
