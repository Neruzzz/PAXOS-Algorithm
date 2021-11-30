-module(paxy).
%-export([start/1, stop/0, stop/1]).
-export([start/3, remoteAcceptors/2, remoteAcceptors/0, stop/0, stop/1]).
%-export([remoteProposers/2, remoteProposers2/2, remoteAcceptors/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

start(Sleep, AccepNode, PropNode) ->
	spawn(PropNode, fun() -> remoteProposers(Sleep, AccepNode) end).
	spawn(AccepNode, fun() -> remoteAcceptors() end).

% Sleep is a list with the initial sleep time for each proposer
remoteProposers(Sleep, AccepNode) ->
	ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
                   {"Proposer willard", ?BLUE}],		   
	PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
	AccRegisterRemote = [{a, AccepNode}, {b, AccepNode}, {c, AccepNode}, {d, AccepNode}, {e, AccepNode}],
	PropPanelHeight = length(Proposers)*?InSizerMinHeight + 10,
	register(gui, spawn(fun() -> gui:start_proposers(ProposerNames, PropPanelHeight) end)),
	gui_proposers ! {reqState, self()},
	 receive
        {reqState, State} ->
          {PropIds} = State,
          start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
		  wait_proposers(length(PropIds))
      end.
  
remoteAcceptors(PropNode) ->
	AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
                   "Acceptor e"],
	AccRegister = [a, b, c, d, e],
	AccPanelHeight = length(Acceptors)*?InSizerMinHeight + 10, 
	register(gui_acceptors, spawn(fun() -> gui:start_acceptors(AcceptorNames, AccPanelHeight) end)),
	gui_acceptors ! {reqStateAccep, self()},
	receive
    {reqState, State} ->
      {AccIds} = State,
      start_acceptors(AccIds, AccRegister)
	end.


start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  stop(a),
  stop(b),
  stop(c),
  stop(d),
  stop(e),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.

 
