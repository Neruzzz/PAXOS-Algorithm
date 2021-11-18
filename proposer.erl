-module(proposer).
-export([start/6]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, ..., ..., ..., PanelId) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(...),
      round(Name, (2*Backoff), ..., Proposal, Acceptors, PanelId)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  prepare(..., ...),
  Quorum = (length(...) div 2) + 1,
  MaxVoted = order:null(),
  case collect(..., ..., ..., ...) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(..., ..., ...),
      case vote(..., ...) of
        ok ->
          {ok, ...};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(0, _, _, Proposal) ->
  {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal) ->
  receive 
    {promise, Round, _, na} ->
      collect(..., ..., ..., ...);
    {promise, Round, Voted, Value} ->
      case order:gr(..., ...) of
        true ->
          collect(..., ..., ..., ...);
        false ->
          collect(..., ..., ..., ...)
      end;
    {promise, _, _,  _} ->
      collect(N, Round, MaxVoted, Proposal);
    {sorry, {prepare, Round}} ->
      collect(..., ..., ..., ...);
    {sorry, _} ->
      collect(N, Round, MaxVoted, Proposal)
  after ?timeout ->
    abort
  end.

vote(0, _) ->
  ok;
vote(N, Round) ->
  receive
    {vote, Round} ->
      vote(..., ...);
    {vote, _} ->
      vote(N, Round);
    {sorry, {accept, Round}} ->
      vote(..., ...);
    {sorry, _} ->
      vote(N, Round)
  after ?timeout ->
    abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {prepare, self(), Round}) 
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {accept, self(), Round, Proposal}) 
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.
