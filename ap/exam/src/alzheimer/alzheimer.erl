-module(alzheimer).
-behavior(gen_server).
-export([start/0, upsert/3, query/2, testFun/1]).
-export([init/1,  handle_call/3]).

-define(SERVER, ?MODULE).
%%%=========================================================================
%%%  API
%%%=========================================================================


% Client Interface Functions
start() ->
  gen_server:start_link(?MODULE, [], []).

query(Aid, P) ->
  gen_server:call(Aid, {read, P}).

upsert(Aid, Id, F) ->
  gen_server:call(Aid, {write, Id, F}).

% Helper Function
filterWithError(_, List, Rows) when length(List) == 0 -> {ok, Rows};
filterWithError(P, [Elem | List], Rows) ->
  Elem = lists:nth(1, List),
  try P(Elem) of
    true  -> filterWithError(P, List, lists:append([Rows, [Elem]]));
    false -> filterWithError(P, List, Rows)
  catch
    _ : _ -> filterWithError(P, [], lists:append([[{error, element(2, Elem)}], Rows]))
  end.

% Callback Functions
init(_Args) ->
  {ok, dict:new()}.

handle_call(Req, From, State) ->
  case Req of
    {read, P} -> case filterWithError(P, dict:to_list(State), []) of
                   {ok, Rows} -> case element(1, Rows) of
                                   {error, Row} -> {error, Row};
                                   _            -> {ok, Rows}
                                 end
                 end;
    {write, Id, F} -> case dict:find(Id, State) of
                        {ok, Value} -> try F({existing, {Id, Value}}) of
                                         {modify, NewData} -> dict:store(Id, NewData, State),
                                                              {modify, NewData};
                                         ignore            -> ignore
                                       catch
                                         throw : Reason    -> throw(Reason)
                                       end;
                        error       -> try F({new, Id}) of
                                         {modify, NewData} -> dict:store(Id, NewData, State),
                                                              {modify, NewData};
                                         ignore            -> ignore
                                       catch
                                         throw : Reason    -> throw(Reason)
                                       end
                      end
  end.

% Test Functions
testFun(X) ->
  case X of
    {new, _}             -> {modify, 99};
    {existing, {_, _}} -> ignore
  end.
