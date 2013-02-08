-module(riak_core_anagrammer).
-include("riak_core_anagrammer.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
		 solve/1
        ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_core_anagrammer),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, riak_core_anagrammer_vnode_master).

%% @doc Picks a random vnode and sends a request to solve for a word
solve(Word) ->
    DocIdx = riak_core_util:chash_key({<<"solve">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_core_anagrammer),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {solve,Word}, riak_core_anagrammer_vnode_master).