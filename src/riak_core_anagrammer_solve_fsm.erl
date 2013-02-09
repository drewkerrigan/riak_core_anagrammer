%% @doc The coordinator for solve operations.
-module(riak_core_anagrammer_solve_fsm).
-behavior(gen_fsm).
-include("riak_core_anagrammer.hrl").

%% API
-export([start_link/4, solve/2]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2]).

-record(state, {req_id,
                from,
                client,
                word,
                preflist,
                num_r=0,
                replies=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqID, From, Client, StatName) ->
    gen_fsm:start_link(?MODULE, [ReqID, From, Client, StatName], []).

solve(Client, Word) ->
    ReqID = mk_reqid(),
    riak_core_anagrammer_solve_fsm_sup:start_solve_fsm([ReqID, self(), Client, Word]),
    {ok, ReqID}.

%%%===================================================================
%%% States
%%%===================================================================

%% Intiailize state data.
init([ReqId, From, Client, Word]) ->
    SD = #state{req_id=ReqId,
                from=From,
                client=Client,
                word=Word},
    {ok, prepare, SD, 0}.

%% @doc Calculate the Preflist.
prepare(timeout, SD0=#state{client=Client,
                            word=Word}) ->
    %%get all vnodes here!
	%%AllVnodes = riak_core_vnode_manager:all_vnodes(riak_core_anagrammer_vnode),
	{ok,{_,_,_,{_,Prelist},_,_,_,_,_,_,_}} = riak_core_ring_manager:get_my_ring(),
	DocIdx = riak_core_util:chash_key({list_to_binary(Client), list_to_binary(Word)}),
%%    Prelist = riak_core_apl:get_apl(DocIdx, 1, riak_core_anagrammer),
    SD = SD0#state{preflist=Prelist},
    {next_state, execute, SD, 0}.

%% @doc Execute the get reqs.
execute(timeout, SD0=#state{req_id=ReqId,
                            word=Word,
                            preflist=Prelist}) ->
    riak_core_anagrammer_vnode:solve(Prelist, ReqId, Word),
    {next_state, waiting, SD0}.

%% @doc Wait for R replies and then respond to From (original client
%% that called `rts:get/2').
%% TODO: read repair...or another blog post?
waiting({ok, ReqID, Val}, SD0=#state{from=From, num_r=NumR0, replies=Replies0}) ->
    NumR = NumR0 + 1,
    Replies = [Val|Replies0],
    SD = SD0#state{num_r=NumR,replies=Replies},
    if
        NumR =:= ?R ->
            Reply =
                case lists:any(different(Val), Replies) of
                    true ->
                        Replies;
                    false ->
                        Val
                end,
            From ! {ReqID, ok, Reply},
            {stop, normal, SD};
        true -> {next_state, waiting, SD}
    end.

handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

different(A) -> fun(B) -> A =/= B end.

mk_reqid() -> erlang:phash2(erlang:now()).