%% This is the application resource file (.app file) for the 'base'
%% application.
{application, mem_dbase,
[{description, "mem_dbase server using mem " },
{vsn, "1.0.1" },
{modules, 
	  [mem_dbase_app,mem_dbase_sup,mem_dbase,mem_dbase_lib,test_mem_dbase]},
{registered,[mem_dbase]},
{applications, [kernel,stdlib]},
{mod, {mem_dbase_app,[]}},
{start_phases, []}
]}.
