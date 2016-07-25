-module(worker).
%% Export public API
-export([start/1, stop/0, assign_map/2, assign_reduce/1, begin_map/2, begin_reduction/1]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, terminate/3,
        % worker states
         wait/2, mapper/2, reducer/2]).
-include("staterec.hrl").

%%% Public API
start(State) ->
  gen_fsm:start_link(?MODULE, State, []).

stop() ->
  gen_fsm:send_all_state_event(worker, stop).

init(State) ->
  {ok, wait, State}.

%% Assign worker to map given Data
assign_map(MapperPid, Data) ->
  gen_fsm:send_event(MapperPid, {assign_map, Data}).

%% Assign worker to reduce data
assign_reduce(ReducerPid) ->
  gen_fsm:send_event(ReducerPid, assign_reduce).

%% Tell worker to begin mapping (Might be redundant?)
begin_map(MapperPid, ReducerPid) ->
  gen_fsm:send_event(MapperPid, {begin_map, ReducerPid}).

%% Notify reducer to begin reduction, call comes from Master
begin_reduction(ReducerPid) ->
  gen_fsm:send_event(ReducerPid, reduce_data).

%% Send mapped data to the reducer
send_mapped_data(Data, ReducerPid) ->
  gen_fsm:send_event(ReducerPid, {receive_data, Data}).

%% Notify the master process that the reducing is done
notify_reduce_done(MasterPid, Result) ->
  gen_fsm:send_event(MasterPid, {reduce_done, Result}).

%% Notify the master process that the mapping is done
notify_map_done(MasterPid) ->
  gen_fsm:send_event(MasterPid, map_done).

%%% Worker states
%% Wait to be assigned as mapper
wait({assign_map, Data}, State) ->
  io:format("mapper assigned ~n"),
  {next_state, mapper, {Data, State}};
%% Wait to be assigned as reducer
wait(assign_reduce, State) ->
  io:format("reducer assigned ~n"),
  {next_state, reducer, {[], State}};
%% Unexpected event while waiting
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.

%% Map given Data, send it to the reducer and return to wait state
mapper({begin_map, ReducerPid}, {Data, State}) ->
  MappedData = lists:map(State#state.mapfun, Data),
  %State#state.master ! MappedData,
  notify_map_done(State#state.master),
  send_mapped_data(MappedData, ReducerPid),
  {next_state, wait, State};
%% Unexpected event while waiting for map
mapper(Event, Data) ->
  unexpected(Event, wait),
  {next_state, mapper, Data}.

%% Collect data that is to be reduced
reducer({receive_data, Data}, {Collected, State}) ->
  % Collect data here untill all mappers delivered
    % MAKE CASE WHERE LENGTH(NEWCOLLECTED) == WORKERCOUNT? IF TRUE SEND MSG TO MASTER TO REDUCE THAT SHIT
  NewCollected = [Data | Collected],
  {next_state, reducer, {NewCollected, State}};
%% Given cue from Master, reduce data and send it to the master process
reducer(reduce_data, {Collected, State}) ->
  % Master will tell us when to reduce
  Collapsed = lists:merge(Collected),
  RedFun = State#state.redfun,
  Result = lists:foldl(RedFun, State#state.initial, Collapsed),
  %State#state.master ! Result,
  notify_reduce_done(State#state.master, Result),
  {next_state, wait, State};
%% Unexpected event while waiting for reduce
reducer(Event, Data) ->
  unexpected(Event, wait),
  {next_state, reducer, Data}.

terminate(_Reason, _StateName, _StateData) ->
  io:format("terminating ~n").

%%% Handle stopping
handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

%%% Utility functions
unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n",
            [self(), Msg, State]).
