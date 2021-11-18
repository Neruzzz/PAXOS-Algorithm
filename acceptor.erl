-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(..., ...) of
        true ->
          ... ! {promise, ..., ..., ...},               
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, ..., ..., Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [...]), Colour},
          acceptor(Name, ..., Voted, Value, PanelId);
        false ->
          ... ! {sorry, {prepare, ...}},
          acceptor(Name, ..., Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(..., ...) of
        true ->
          ... ! {vote, ...},
          case order:goe(..., ...) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, ..., ...]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [...]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), ...},
              acceptor(Name, Promised, ..., ..., PanelId);
            false ->
              acceptor(Name, Promised, ..., ..., PanelId)
          end;                            
        false ->
          ... ! {sorry, {accept, ...}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
