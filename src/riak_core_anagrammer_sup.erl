-module(riak_core_anagrammer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { riak_core_anagrammer_vnode_master,
                  {riak_core_vnode_master, start_link, [riak_core_anagrammer_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},
	
	SolveFSMs = {riak_core_anagrammer_solve_fsm_sup,
               {riak_core_anagrammer_solve_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [riak_core_anagrammer_solve_fsm_sup]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, SolveFSMs]}}.
