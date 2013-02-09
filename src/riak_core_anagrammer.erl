-module(riak_core_anagrammer).
-include("riak_core_anagrammer.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
		 solve/1
        ]).

-define(TIMEOUT, 5000).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_core_anagrammer),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, riak_core_anagrammer_vnode_master).

%% @doc Picks a random vnode and sends a request to solve for a word
solve(Word) ->
	{ok, ReqID} = riak_core_anagrammer_solve_fsm:solve("anagrammer", Word),
    wait_for_reqid(ReqID, ?TIMEOUT).
%% 	AllVnodes = riak_core_vnode_manager:all_vnodes(riak_core_anagrammer_vnode)
%% %%	{riak_core_anagrammer_vnode,Chash,Pid} =
%% 
%%     DocIdx = riak_core_util:chash_key({<<"solve">>, term_to_binary(now())}),
%% 
%%     PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_core_anagrammer),
%%     [{IndexNode, _Type}] = PrefList,
%%     riak_core_vnode_master:sync_spawn_command(IndexNode, {solve,Word}, riak_core_anagrammer_vnode_master).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

wait_for_reqid(ReqID, Timeout) ->
    receive
	{ReqID, ok} -> ok;
        {ReqID, ok, Val} -> {ok, Val}
    after Timeout ->
	    {error, timeout}
    end.