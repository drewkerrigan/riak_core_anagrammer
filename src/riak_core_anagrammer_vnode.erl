-module(riak_core_anagrammer_vnode).
-behaviour(riak_core_vnode).
-include("riak_core_anagrammer.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-export([
         solve/3
        ]).

-define(MASTER, riak_core_anagrammer_vnode_master).

-record(state, {partition, words}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

solve(Preflist, ReqID, Word) ->
    riak_core_vnode_master:command(Preflist,
                                   {solve, ReqID, Word},
                                   {fsm, undefined, self()},
                                   ?MASTER).

init([Partition]) ->
	Words = readlines("/Users/dkerrigan/src/riak_core_anagrammer/resources/words_small.txt"),
	%%iterate over list, sort characters of words, store that as key... or something
	%%lists:sort("Hello").
%% 	Words = [],
%%  	_A = for_each_line_in_file("/Users/dkerrigan/src/riak_core_anagrammer/resources/words_small.txt",
%% %% 		fun(X, Count) -> io:fwrite("~10B: ~s", [Count, X]), Count + 1 end, [read], 0),
%%  		fun(X, Count) -> io:fwrite("~10B: ~s", [Count, X]), Words = lists:append([Words, list_to_binary(X)]]), Count + 1 end, [read], 0),
	{ok, #state { partition=Partition, words=Words }}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

%%handle_command({solve,Word}, _Sender, State) ->
%%	A = for_each_line_in_file("/Users/dkerrigan/src/riak_core_anagrammer/resources/words.txt",
%%	fun(X, Count) -> io:fwrite("~10B: ~s", [Count, X]), Count + 1 end, [read], 0),
%%	{reply, {Word, State#state.partition}, State};

handle_command({solve, ReqID, Word}, _Sender, #state{words=Words}=State) ->
	Reply = lists:filter(fun(X) -> string:equal(lists:sort(X), lists:sort(Word)) end, Words),
	{reply, {ok, ReqID, Reply}, State};

handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.