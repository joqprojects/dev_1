%% This is the application resource file (.app file) for the 'base'
%% application.
{application, repo_mgr,
[{description, "repo_mgr manager " },
{vsn, "1.0.0" },
{modules, 
	  [repo_mgr_app,repo_mgr_sup,repo_mgr,test_repo_mgr]},
{registered,[repo_mgr]},
{applications, [kernel,stdlib]},
{mod, {repo_mgr_app,[]}},
{start_phases, []}
]}.
