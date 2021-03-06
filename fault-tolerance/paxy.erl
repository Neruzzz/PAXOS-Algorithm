-module(paxy).
-export([start/1, stop/0, stop/1, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
                   "Acceptor e"],
  AccRegister = [a, b, c, d, e],
  ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
                   {"Proposer willard", ?BLUE}],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() -> 
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
		timer:sleep(2000),
		crash(a),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed]),
		stop()
      end)
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
      case Name == gui of
        false->
          pers:delete(Name)
        end,
      
      Pid ! stop
  end.

crash(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->timer:sleep(1000),
      pers:open(Name),
	  {_, _, _, Pn} = pers:read(Name),
	  Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
	  pers:close(Name),
	  unregister(Name),
	  exit(Pid, "crash"),
	  
	  timer:sleep(3000),
	  register(Name, acceptor:start(Name, na))
  end.