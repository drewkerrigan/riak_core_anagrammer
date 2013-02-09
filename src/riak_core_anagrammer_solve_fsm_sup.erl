%% @doc Supervise the rts_get FSM.
-module(riak_core_anagrammer_solve_fsm_sup).
-behavior(supervisor).

-export([start_solve_fsm/1,
         start_link/0]).
-export([init/1]).

start_solve_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SolveFsm = {undefined,
              {riak_core_anagrammer_solve_fsm, start_link, []},
              temporary, 5000, worker, [riak_core_anagrammer_solve_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [SolveFsm]}}.