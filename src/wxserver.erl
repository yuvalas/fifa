%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2020 13:45
%%%-------------------------------------------------------------------
-module(wxserver).
-author("Moshe.Yuval").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../params.hrl").

-export([startme/0, applyChangesInDataBase/4, linkedMonitor/2, finishedGame/1, serverDown/1, kick/1]).
-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(wXRecord, {wXPanel, wxFrame, wxBall, self, round}).


startme() ->
  compile:file(ball),
  compile:file(controlledplayer),
  compile:file(computerplayer),
  compile:file(common),
  compile:file(utils),
  compile:file(etsutils),
  WX = wx:new(),
  wx_object:start_link({global, main}, ?MODULE, WX, []).

applyChangesInDataBase(EntriesToChange, WhatToDelete, MonitorName, ID) ->
  gen_server:cast(ID, {applyChangesInDataBase, EntriesToChange, WhatToDelete, MonitorName}).

linkedMonitor(ID, MonitorName) ->
  gen_server:cast(ID, {linkedMonitor, MonitorName}).

finishedGame(ID) ->
  gen_server:cast(ID, {finishedGame}).

serverDown(ID) ->
  gen_server:cast(ID, {serverDown}).

init(ID) ->
  FunToBatch = fun() -> initialize(ID) end,
  wx:batch(FunToBatch).

initialize(ID) ->
  IsMainDefined = whereis(main),
  if IsMainDefined /= undefined ->
    unregister(main),
    rpc:multicall(?MONITOR_PROCESSES, global, unregister, [main]);
    true -> continue
  end,
  global:register_name(main, self()),
  setUpConnectionWithAllServers(),
  etsutils:generateEts(),
  FunToSpawn1 = fun() ->
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [controlledplayer]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [computerplayer]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [monitor]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [wxserver]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [utils]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [etsutils]),
    rpc:call(?MONITOR_LONG_NAME_A, monitor, startme, [?MONITORA, 1, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_B, monitor, startme, [?MONITORB, 2, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_C, monitor, startme, [?MONITORC, 3, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_D, monitor, startme, [?MONITORD, 4, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]) end,
  FunToSpawn2 = fun() -> monitorNodes() end,
  spawn(FunToSpawn1),
  spawn(FunToSpawn2),
  WXFrame = wxFrame:new(ID, -1, "FIFA Distrabuted Game", [{size, ?SCREEN_SIZE}]),
  WXPanel = wxPanel:new(WXFrame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
  wxFrame:createStatusBar(WXFrame),
  wxFrame:connect(WXPanel, motion),
  wxFrame:connect(WXFrame, command_menu_selected, []),
  wxFrame:connect(WXPanel, left_up),
  wxWindow:setBackgroundColour(WXFrame, ?BACKGROUND_COLOR),
  wxFrame:show(WXFrame),
  WXBall = wxImage:new(?BALL_IMAGE),
  WXRecord = #wXRecord{wXPanel = WXPanel, wxFrame = WXFrame, wxBall = WXBall, self = self(), round = 0},
  WXIMGBackground = wxImage:new(?STAGE_IMAGE),
  FunForConnectwxFrame = fun(#wx{event = #wxPaint{}}, _wxObj) ->
    WxBufferedPaint = wxBufferedPaintDC:new(WXPanel),
    ets:insert(statistics, {180, WxBufferedPaint}),
    WXBitmap = wxBitmap:new(WXIMGBackground),
    wxDC:drawBitmap(WxBufferedPaint, WXBitmap, {0, 0}),
    wxBitmap:destroy(WXBitmap),
    wxDC:setTextForeground(WxBufferedPaint, ?wxBLACK),
    mainLoop(WxBufferedPaint),
    wxBufferedPaintDC:destroy(WxBufferedPaint) end,
  wxFrame:connect(WXPanel, paint, [{callback, FunForConnectwxFrame}]),
  timer:send_interval(?SCREEN_REFRESH_TIME, self(), refreshWindow),
  {WXPanel, WXRecord}.

handle_event(#wx{obj = _Panel, event = #wxMouse{type = motion, x = MouseX, y = MouseY}}, State) ->
  case ets:lookup(playersETS, controlledplayer) of
    [] ->
      continue;
    [{_, {_, {_, _}, {MonitorName}}, _}] ->
      DistanceLowerX = ?X_Lower_Limit + ?CHARACTER_WIDTH,
      DistanceUpperX = ?X_Upper_Limit - ?CHARACTER_WIDTH,
      DistanceLowerY = ?Y_Lower_Limit + ?CHARACTER_WIDTH,
      DistanceUpperY = ?MAX_PLAYER_Y - ?CHARACTER_WIDTH,
      case MouseX < DistanceLowerX of
        false -> X1Coordinate = MouseX;
        _ -> X1Coordinate = DistanceLowerX
      end,
      X2Coordinate = X1Coordinate,
      case X2Coordinate > DistanceUpperX of
        false -> X3Coordinate = X2Coordinate;
        _ -> X3Coordinate = DistanceUpperX
      end,
      case MouseY < DistanceLowerY of
        false -> Y1Coordinate = MouseY;
        _ -> Y1Coordinate = DistanceLowerY
      end,
      Y2Coordinate = Y1Coordinate,
      case Y2Coordinate > DistanceUpperY of
        false -> Y3Coordinate = Y2Coordinate;
        _ -> Y3Coordinate = DistanceUpperY
      end,
      {X4Coordinate, Y4Coordinate} = {X3Coordinate, Y3Coordinate},
      monitor:refreshLocation(MonitorName, controlledplayer, {X4Coordinate, Y4Coordinate})
  end,
  {noreply, State};

handle_event(#wx{event = #wxMouse{type = left_up}}, Data) ->
  [{_, Status}] = ets:lookup(generalState, status),
  case Status of
    startGame ->
      startRound(1, true),
      ets:insert(generalState, {status, idle});
    resetRound ->
      resetRound(),
      ets:insert(generalState, {status, idle});
    finishedGame ->
      stay;
    idle ->
      kick(Data#wXRecord.wxBall);
    _ -> ignore
  end,
  {noreply, Data};

handle_event(#wx{}, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({applyChangesInDataBase, EntriesToChange, WhatToDelete, _}, State) ->
  [{_, Status}] = ets:lookup(generalState, status),
  case Status of
    idle ->
      FunInsertPlayers = fun() ->
        lists:foreach(
          fun({ID, Args}) -> [{_, Status}] = ets:lookup(generalState, status),
            if Status =:= idle ->
              ets:insert(playersETS, {ID, Args, ID})
            end
          end, EntriesToChange) end,
      FunDelPlayers = fun() -> lists:foreach(fun({ID, _}) ->
        ets:delete(playersETS, ID) end, WhatToDelete) end,
      spawn(FunDelPlayers),
      spawn(FunInsertPlayers);
    _ -> continue
  end,
  {noreply, State};

handle_cast({linkedMonitor, MonitorName}, Data) ->
  case MonitorName of
    ?MONITORA ->
      insertMonitor(?MONITORA);
    ?MONITORB ->
      insertMonitor(?MONITORB);
    ?MONITORC ->
      insertMonitor(?MONITORC);
    ?MONITORD ->
      insertMonitor(?MONITORD)
  end,
  {noreply, Data};

handle_cast({serverDown}, State) ->
  {stop, shutdown, State};

handle_cast(finishedGame, State) ->
  ets:insert(generalState, {status, finishedGame}),
  getMonitorsAndDeleteAll(),
  ets:delete_all_objects(playersETS),
  {noreply, State};

handle_cast(_Cast, State) ->
  {noreply, State}.

insertMonitor(MonitorName) ->


  case ets:lookup(monitors, MonitorName) of
    [] ->
      ets:insert(monitors, {MonitorName, {true}}),
      List = ets:tab2list(monitors),
      case length(List) of
        4 -> ets:insert(generalState, {status, startGame});
        _ -> ignore
      end;
    [{MonitorName, {false}}] ->
      regainConnectionToMonitor(MonitorName),
      ets:insert(monitors, {MonitorName, {true}})
  end.

handle_info(refreshWindow, Data) ->
  wxWindow:refresh(Data#wXRecord.wXPanel, [{eraseBackground, false}]),
  {noreply, Data};

handle_info(_, Data) ->
  {noreply, Data}.

code_change(_, _, Data) ->
  {stop, ignore, Data}.

terminate(_, Data) ->
  FunGetMonitors = fun({_, {A}}) -> A end,
  MList = ets:tab2list(monitors),
  ListOfMonitors = lists:filter(FunGetMonitors, MList),
  lists:foreach(fun({Monitor, _}) ->
    monitor:shutDown(Monitor) end, ListOfMonitors),
  PIDA = whereis(?MONITORA),
  PIDB = whereis(?MONITORB),
  PIDC = whereis(?MONITORC),
  PIDD = whereis(?MONITORD),
  if PIDA /= undefined ->
    PIDA ! kill;
    true -> continue
  end,
  if PIDB /= undefined ->
    PIDB ! kill;
    true -> continue
  end,
  if PIDC /= undefined ->
    PIDC ! kill;
    true -> continue
  end,
  if PIDD /= undefined ->
    PIDD ! kill;
    true -> continue
  end,
  wxPanel:destroy(Data#wXRecord.wXPanel),
  wx:destroy(),
  ok.

setUpConnectionWithAllServers() ->
  net_adm:ping(?MONITOR_LONG_NAME_A),
  net_adm:ping(?MONITOR_LONG_NAME_B),
  net_adm:ping(?MONITOR_LONG_NAME_C),
  net_adm:ping(?MONITOR_LONG_NAME_D).

monitorNodes() ->
  MonitorA = spawn(fun() -> erlang:monitor_node(?MONITOR_LONG_NAME_A, true),
    receiveBlock(?MONITOR_LONG_NAME_A) end),
  MonitorB = spawn(fun() -> erlang:monitor_node(?MONITOR_LONG_NAME_B, true), receiveBlock(?MONITOR_LONG_NAME_B) end),
  MonitorC = spawn(fun() -> erlang:monitor_node(?MONITOR_LONG_NAME_C, true), receiveBlock(?MONITOR_LONG_NAME_C) end),
  MonitorD = spawn(fun() -> erlang:monitor_node(?MONITOR_LONG_NAME_D, true), receiveBlock(?MONITOR_LONG_NAME_D) end),
  register(monitorA, MonitorA),
  register(monitorB, MonitorB),
  register(monitorC, MonitorC),
  register(monitorD, MonitorD).

receiveBlock(Monitor) ->
  receive
    {nodedown, Monitor} ->
      case Monitor of
        ?MONITOR_LONG_NAME_A ->
          MonitorName = ?MONITORA;
        ?MONITOR_LONG_NAME_B ->
          MonitorName = ?MONITORB;
        ?MONITOR_LONG_NAME_C ->
          MonitorName = ?MONITORC;
        ?MONITOR_LONG_NAME_D ->
          MonitorName = ?MONITORD
      end,
      io:fwrite("~nLost Connection With ~p~n", [MonitorName]),
      lostConnectionWithMonitor(MonitorName),
      connectToMonitor(Monitor, MonitorName);
    kill -> kill;
    _ -> receiveBlock(Monitor)
  end.

mainLoop(WXPaint) ->
  [{_, Status}] = ets:lookup(generalState, status),
  Font = wxFont:new(20, ?wxFONTFAMILY_ROMAN, ?wxFONTSTYLE_NORMAL, ?wxBOLD),
  wxDC:setFont(WXPaint, Font),
  case Status of
    idle ->
      WXIMGBackground = wxImage:new(?FIELD_IMAGE),
      WXBitmap = wxBitmap:new(WXIMGBackground),
      wxDC:drawBitmap(WXPaint, WXBitmap, {0, 0}),
      wxBitmap:destroy(WXBitmap),
      List = ets:tab2list(playersETS),
      displayCharacters(WXPaint, List),
      ShouldPrint = false;
    startGame ->
      wxDC:drawLabel(WXPaint, "Welcome to Fifa Distrebuted Game!", {?PRINT_X_START, 300, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "Click left mouse button to start", {?PRINT_X_START, 300 + ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      ShouldPrint = false;
    resetRound ->
      WXIMGStage = wxImage:new(?STAGE_IMAGE),
      WXBitmap = wxBitmap:new(WXIMGStage),
      wxDC:drawBitmap(WXPaint, WXBitmap, {0, 0}),
      wxBitmap:destroy(WXBitmap),
      [{_, Round}] = ets:lookup(generalState, round),
      wxDC:drawLabel(WXPaint, "Round " ++ integer_to_list(Round), {?PRINT_X_START + 10 * ?TAB, 50, ?PRINT_W_H, ?PRINT_W_H}),
      ShouldPrint = true;
    finishedGame ->
      WXIMGfinishedgame = wxImage:new(?FINISHED_IMAGE),
      WXBitmap = wxBitmap:new(WXIMGfinishedgame),
      wxDC:drawBitmap(WXPaint, WXBitmap, {0, 0}),
      wxBitmap:destroy(WXBitmap),
      wxDC:setTextForeground(WXPaint, ?wxWHITE),
      wxDC:drawLabel(WXPaint, "Game Over!", {?PRINT_X_START, 50, ?PRINT_W_H, ?PRINT_W_H}),
      ShouldPrint = true;
    _ -> ShouldPrint = false
  end,
  utils:printToScreen(ShouldPrint, WXPaint, Status).

locateBall(WhichTeam, PlayerX, PlayerY, BallX, BallY) ->
  HowFarIsBall = common:getDistance({PlayerX, PlayerY}, {BallX, BallY}),
  if ((WhichTeam == 2) and (HowFarIsBall < 26)) or ((WhichTeam == 1) and (HowFarIsBall < 26)) ->
    IsNear = near;
    true -> IsNear = far
  end,
  IsNear.

insertBallsOwnerAndPosition(ID, _, Paint, _, Pic, _, NewX, NewY, RefereePic) ->
  [{_, PrevStatsUpdate}] = ets:lookup(statistics, prevStatupdate),
  if ((ID /= controlledplayer) and (PrevStatsUpdate /= ID)) ->
    ets:insert(isOwner, {kickDirection, 0}),
    ets:insert(ball, {flag, false}),
    ets:insert(statistics, {prevStatupdate, ID}),
    updateStatistics(ID, 1);
    true -> skip
  end,
  updateBallEts(Paint, Pic, NewX, NewY, RefereePic),
  ets:insert(ball, {previousOwner, ID}).

displayCharacters(_, []) -> continue;
displayCharacters(WXPaint, [{ID, {Type, Location, Args}, _} | T]) ->
  mainLogic(WXPaint, ID, Type, Location, Args),
  displayCharacters(WXPaint, T).

updateStatistics(ID, Offset) ->
  [{_, Amount}] = ets:lookup(statistics, ID),
  ets:insert(statistics, {ID, Amount + Offset}).


controlledPlayerStatsUpdate(IsNear, BallPreviousOwner) ->
  if IsNear == near ->
    ets:insert(ball, {flag, false}),
    if BallPreviousOwner == controlledplayer -> continue;
      true ->
        ets:insert(isOwner, {kickDirection, 0}),
        updateStatistics(controlledPlayer, 1)
    end;
    true -> continue
  end.

computerLogic(Id, GivenX, GivenY, BallX, BallY, BallpreviousOwner, BallWidth, GivenX, _, IsBallBelongToCntrPlayer, WXPaint, BallImg, RefereePic) ->
  IsTeam1Near = locateBall(1, GivenX, GivenY, BallX, BallY),
  IsTeam2Near = locateBall(2, GivenX, GivenY, BallX, BallY),
  [{_, PreviousX}] = ets:lookup(playerDirection, Id),
  ets:insert(playerDirection, {Id, GivenX}),
  NewDir = GivenX - PreviousX,
  {X_img, DirPic} = utils:loadPlayerImage(Id, NewDir, BallWidth, GivenX),
  XLocation = X_img,
  if (IsBallBelongToCntrPlayer == 0) ->
    if
      ((IsTeam2Near == near) and (Id >= 2.0) and (Id < 3)) ->
        insertBallsOwnerAndPosition(Id, GivenY, WXPaint, XLocation, BallImg, 0, GivenX , GivenY, RefereePic),
        ets:insert(isOwner, {owned, 1}),
        if ((NewDir < 0) or ((Id == 2.0) and (NewDir == 0)) ) ->
          ets:insert(isOwner, {kickDirection, 1});
          true -> continue
        end;
      ((IsTeam1Near == near) and (Id >= 1.0) and (Id < 2)) ->
        insertBallsOwnerAndPosition(Id, GivenY, WXPaint, XLocation, BallImg, 0, GivenX , GivenY, RefereePic),
        ets:insert(isOwner, {owned, 1}),
        if ((NewDir > 0) or ((Id == 1.0) and (NewDir == 0))) ->
          ets:insert(isOwner, {kickDirection, 2});
          true -> continue
        end;
      true ->
        ets:insert(isOwner, {owned, 0}),
        ets:insert(ball, {previousOwner, BallpreviousOwner})
    end;
    true -> continue
  end,
  Dpic = DirPic,
  Dpic.


ballLogic(Id, GivenX,BallWidth) ->
  [{_, PreviousX}] = ets:lookup(playerDirection, Id),
  ets:insert(playerDirection, {Id, GivenX}),
  NewDir = GivenX - PreviousX,
  {_, DirPic} = utils:loadPlayerImage(Id, NewDir, BallWidth, GivenX),
  Dpic = DirPic,
  Dpic.

controlledLogic(GivenX, GivenY, BallX, BallY, BallpreviousOwner, BallWidth, WXPaint, BallImg, RefereePic, Id) ->
  IsNear = locateBall(2, GivenX, GivenY, BallX, BallY),
  controlledPlayerStatsUpdate(IsNear, BallpreviousOwner),
  [{_, ContPlayerX}] = ets:lookup(controlledplayerX, xCoordinate),
  ets:insert(controlledplayerX, {xCoordinate, GivenX}),
  ContPlayerNewDir = GivenX - ContPlayerX,
  [{_, LastPos}] = ets:lookup(controlledplayerDir, direction),
  if ContPlayerNewDir == 0 -> ets:insert(controlledplayerDir, {direction, LastPos});
    true -> ets:insert(controlledplayerDir, {direction, ContPlayerNewDir})
  end,
  {XLocation, Dpic} = utils:loadImageControlledPlayer(ContPlayerNewDir, BallWidth, GivenX, LastPos),
  case IsNear of
    near ->
      insertBallsOwnerAndPosition(Id, GivenY, WXPaint, XLocation, BallImg, 1, XLocation, GivenY, RefereePic),
      ets:insert(isOwner, {ball, 1});
    _ ->
      ets:insert(isOwner, {ball, 0}),
      ets:insert(ball, {previousOwner, BallpreviousOwner})
  end,
  Dpic.

mainLogic(WXPaint, Id, Type, {GivenX, GivenY}, Args) ->
  [{_, BallX}] = ets:lookup(ball, ballX),
  [{_, BallY}] = ets:lookup(ball, ballY),
  [{_, IsBallBelongToCntrPlayer}] = ets:lookup(isOwner, ball),
  IsFinishedGame = isFinishedGame(BallX, BallY),
  BallImg = wxImage:new(?BALL_IMAGE),
  RefereePic = wxImage:new(?REFEREE_IMAGE),
  BallWidth = wxImage:getWidth(BallImg),
  [{_, BallPreviousOwner}] = ets:lookup(ball, previousOwner),
  if IsFinishedGame == 1 ->
    getMonitorsAndDeleteAll(),
    ets:delete_all_objects(playersETS),
    ets:insert(generalState, {status, finishedGame});
    true ->
      if (Id < 3) or (Id == controlledplayer) ->
        case Type of
          computerplayer ->
            PicToDraw = computerLogic(Id, GivenX, GivenY, BallX, BallY, BallPreviousOwner, BallWidth, GivenX, Args, IsBallBelongToCntrPlayer, WXPaint, BallImg, RefereePic);
          controlledplayer ->
            PicToDraw = controlledLogic(GivenX, GivenY, BallX, BallY, BallPreviousOwner, BallWidth, WXPaint, BallImg, RefereePic, Id);
          ball ->
            PicToDraw = ballLogic(Id, GivenX,BallWidth)
        end,
        Width = wxImage:getWidth(PicToDraw),
        CalcX = GivenX - Width / 2,
        Height = wxImage:getHeight(PicToDraw),
        CalcY = GivenY - Height / 2,
        paintImage(WXPaint, PicToDraw, {CalcX, CalcY}),
        wxImage:destroy(PicToDraw);
        true ->
          [{_, KickDirection}] = ets:lookup(isOwner, kickDirection),
          [{_, Owned}] = ets:lookup(isOwner, owned),
          if (IsBallBelongToCntrPlayer == 0) ->
            if
              ((KickDirection == 0) and (Owned == 0)) ->
                updateBallEts(WXPaint, BallImg, BallX, BallY, RefereePic);
              (KickDirection == 1) ->
                {NewPosX, NewPosY} = common:ballMovementDrawing({BallX, BallY}, randomNetDest(left), 40),
                updateBallEts(WXPaint, BallImg, NewPosX, NewPosY, RefereePic);
              (KickDirection == 2) ->
                {NewPosX, NewPosY} = common:ballMovementDrawing({BallX, BallY}, randomNetDest(right), 40),
                updateBallEts(WXPaint, BallImg, NewPosX, NewPosY, RefereePic);
              true -> continue
            end;
            true -> continue
          end
      end
  end.

paintImage(WXPaint, Image, {X, Y}) ->
  {RoundedX, RoundedY} = {round(X), round(Y)},
  Bitmap = wxBitmap:new(Image),
  wxDC:drawBitmap(WXPaint, Bitmap, {RoundedX, RoundedY}),
  wxBitmap:destroy(Bitmap).

updateBallEts(WXPaint, Pic, NewX, NewY, RefereePic) ->
  paintImage(WXPaint, RefereePic, {NewX + 50, NewY + 50}),
  wxImage:destroy(RefereePic),
  paintImage(WXPaint, Pic, {NewX, NewY}),
  wxImage:destroy(Pic),
  ets:insert(ball, {ballX, NewX}),
  ets:insert(ball, {ballY, NewY}).

restartMonitors() ->
  monitor:shutDown(?MONITOR_LONG_NAME_A),
  monitor:shutDown(?MONITOR_LONG_NAME_B),
  monitor:shutDown(?MONITOR_LONG_NAME_C),
  monitor:shutDown(?MONITOR_LONG_NAME_D),
  whereis(?MONITORA) ! kill,
  whereis(?MONITORB) ! kill,
  whereis(?MONITORC) ! kill,
  whereis(?MONITORD) ! kill,
  setUpConnectionWithAllServers(),
  FunToSpawn1 = fun() ->
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [controlledplayer]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [computerplayer]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [monitor]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [wxserver]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [utils]),
    rpc:call(?MONITOR_LONG_NAME_A, monitor, startme, [?MONITORA, 1, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_B, monitor, startme, [?MONITORB, 2, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_C, monitor, startme, [?MONITORC, 3, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_D, monitor, startme, [?MONITORD, 4, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]) end,
  FunToSpawn2 = fun() -> monitorNodes() end,
  spawn(FunToSpawn1),
  spawn(FunToSpawn2).

startRound(RoundNum, ShouldInsertControlledPlayer) ->
  if (RoundNum /= 1) ->
    restartMonitors();
    true -> cont
  end,
  initializeBallEts(),
  ComponentsLayoutList = utils:getComponentsLayoutList(),
  [{_, Round}] = ets:lookup(generalState, round),
  ets:insert(generalState, {round, Round + 1}),
  NextMonitor = pickNextMonitor(720),
  case ShouldInsertControlledPlayer of
    false ->
      [{ID, {_, Location, {_}}}] = ets:lookup(generalState, controlledplayer),
      monitor:addControlledplayer(NextMonitor, {ID, Location, old});
    _ -> monitor:addControlledplayer(NextMonitor, {controlledplayer, {720, 600}, new})
  end,
  FunToMap = fun({ID, Loc, Dest}) -> {ID, {computerplayer, Loc, {Dest}}} end,
  AllPlayers = lists:map(FunToMap, ComponentsLayoutList),
  MonitorAPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?MONITORA) end, AllPlayers),
  MonitorBPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?MONITORB) end, AllPlayers),
  MonitorCPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?MONITORC) end, AllPlayers),
  MonitorDPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?MONITORD) end, AllPlayers),
  monitor:addBall(pickNextMonitor(600), {ball, ?BALL_INITIAL_COORDINATES, ?BALL_INITIAL_COORDINATES, new}),
  monitor:addListOfPlayers(?MONITORA, MonitorAPlayers),
  monitor:addListOfPlayers(?MONITORB, MonitorBPlayers),
  monitor:addListOfPlayers(?MONITORC, MonitorCPlayers),
  monitor:addListOfPlayers(?MONITORD, MonitorDPlayers).

initializeBallEts() ->
  ets:insert(ball, {ballX, ?BALL_INIT_X}),
  ets:insert(ball, {ballY, ?BALL_INIT_Y}),
  ets:insert(isOwner, {kickDirection, 0}),
  ets:insert(isOwner, {ball, 0}),
  ets:insert(isOwner, {owned, 0}),
  ets:insert(ball, {destX, 0}),
  ets:insert(ball, {destY, 0}),
  ets:insert(ball, {togg, down}),
  ets:insert(ball, {flag, false}).

resetRound() ->
  initializeBallEts(),
  ComponentsLayoutList = utils:getComponentsLayoutList(),
  [{_, Round}] = ets:lookup(generalState, round),
  ets:insert(generalState, {round, Round + 1}),
  FunToMap = fun({ID, Location, Dest}) ->
    {ID, {computerplayer, Location, {Dest}}} end,
  AllPlayers = lists:map(FunToMap, ComponentsLayoutList),
  MonitorAPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?MONITORA) end, AllPlayers),
  MonitorBPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?MONITORB) end, AllPlayers),
  MonitorCPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?MONITORC) end, AllPlayers),
  MonitorDPlayers = lists:filter(fun({_, {_, {X, _}, _}}) -> (pickNextMonitor(X) == ?MONITORD) end, AllPlayers),
  FunRefreshPlayerInMonitorA = fun({ID, {computerplayer, Location, {Dest}}}) ->
    monitor:refreshInfo(?MONITORA, ID, {computerplayer, Location, {Dest}}) end,
  FunRefreshPlayerInMonitorB = fun({ID, {computerplayer, Location, {Dest}}}) ->
    monitor:refreshInfo(?MONITORB, ID, {computerplayer, Location, {Dest}}) end,
  FunRefreshPlayerInMonitorC = fun({ID, {computerplayer, Location, {Dest}}}) ->
    monitor:refreshInfo(?MONITORC, ID, {computerplayer, Location, {Dest}}) end,
  FunRefreshPlayerInMonitorD = fun({ID, {computerplayer, Location, {Dest}}}) ->
    monitor:refreshInfo(?MONITORD, ID, {computerplayer, Location, {Dest}}) end,

  monitor:refreshInfo(pickNextMonitor(600), ball, {ball, ?BALL_INITIAL_COORDINATES, {?BALL_INITIAL_COORDINATES}}),
  lists:map(FunRefreshPlayerInMonitorA, MonitorAPlayers),
  lists:map(FunRefreshPlayerInMonitorB, MonitorBPlayers),
  lists:map(FunRefreshPlayerInMonitorC, MonitorCPlayers),
  lists:map(FunRefreshPlayerInMonitorD, MonitorDPlayers).

pickNextMonitor(X) ->
  EtsToList = ets:tab2list(monitors),
  FunForNumber = fun({_, {Act}}) -> Act end,
  FunForNames = fun({Monitor, _}) -> Monitor end,
  MonitorsList = lists:filter(FunForNumber, EtsToList),
  MonitorsNumber = length(MonitorsList),
  MonitorsNames = lists:map(FunForNames, MonitorsList),
  Ans = common:pickNextMonitor(X, MonitorsNumber, MonitorsNames),
  Ans.

lostConnectionWithMonitor(MonitorName) ->
  ets:insert(monitors, {MonitorName, {false}}),
  FunToFilter = fun({_, {Active}}) -> Active end,
  EtsToList = ets:tab2list(monitors),
  FilteredList = lists:filter(FunToFilter, EtsToList),
  case FilteredList of
    [] -> serverDown(global:whereis_name(main));
    _ -> ok
  end,
  refreshMonitorEts().

regainConnectionToMonitor(Name) ->
  ets:insert(monitors, {Name, {true}}),
  refreshMonitorEts().

refreshMonitorEts() ->
  PlayersETSList = ets:tab2list(playersETS),
  FunToFilter = fun({_, {FF}}) -> FF end,
  FunToMap = fun({DD, _}) -> DD end,
  EtsToList = ets:tab2list(monitors),
  FilteredList = lists:filter(FunToFilter, EtsToList),
  ets:delete_all_objects(playersETS),
  NumOfServers = length(FilteredList),
  NameOfServers = lists:map(FunToMap, FilteredList),
  updateScreenSizes(1, NumOfServers, NameOfServers, NameOfServers, PlayersETSList).

getMonitorsAndDeleteAll() ->
  Fun = fun({_, {Active}}) -> Active end,
  MList = ets:tab2list(monitors),
  Fun2 = fun({A, _}) ->
    monitor:deleteAll(A)
         end,
  MonitorsList = lists:filter(Fun, MList),
  lists:foreach(Fun2, MonitorsList).

connectToMonitor(MonitorLongName, MonitorName) ->
  receive
    kill -> kill
  after 9999 ->
    case net_adm:ping(MonitorLongName) of
      pong ->
        io:fwrite("~nRestablish connection with ~p~n", [MonitorName]),
        case rpc:call(MonitorLongName, compile, file, [monitor]) of
          {ok, _} ->
            compileAllFiles(MonitorLongName),
            ConnectedMonitorsList = lists:sort(namesConnectedMonitors() ++ [MonitorName]),
            NumConnectedMonitors = countConnectedMonitors() + 1,
            rpc:call(MonitorLongName, monitor, startme, [MonitorName, monitorLongNameToNum(MonitorLongName), NumConnectedMonitors,
              ConnectedMonitorsList]),
            erlang:monitor_node(MonitorLongName, true),
            receiveBlock(MonitorLongName);
          _ ->
            connectToMonitor(MonitorLongName, MonitorName)
        end;
      pang ->
        connectToMonitor(MonitorLongName, MonitorName)
    end
  end.

monitorLongNameToNum(MonitorLongName) ->
  case (MonitorLongName) of
    ?MONITOR_LONG_NAME_D -> MonitorId = 4;
    ?MONITOR_LONG_NAME_C -> MonitorId = 3;
    ?MONITOR_LONG_NAME_B -> MonitorId = 2;
    ?MONITOR_LONG_NAME_A -> MonitorId = 1;
    _ -> MonitorId = 0
  end,
  MonitorId.

countConnectedMonitors() ->
  AIsAlive = isMonitorAlive(?MONITORA),
  BIsAlive = isMonitorAlive(?MONITORB),
  CIsAlive = isMonitorAlive(?MONITORC),
  DIsAlive = isMonitorAlive(?MONITORD),
  AIsAlive + BIsAlive + CIsAlive + DIsAlive.

namesConnectedMonitors() ->
  AIsAlive = isMonitorAlive(?MONITORA),
  BIsAlive = isMonitorAlive(?MONITORB),
  CIsAlive = isMonitorAlive(?MONITORC),
  DIsAlive = isMonitorAlive(?MONITORD),
  if (AIsAlive == 1) ->
    OutA = [?MONITORA];
    true ->
      OutA = []
  end,
  if (BIsAlive == 1) ->
    OutB = [?MONITORB];
    true ->
      OutB = []
  end,
  if (CIsAlive == 1) ->
    OutC = [?MONITORC];
    true ->
      OutC = []
  end,
  if (DIsAlive == 1) ->
    OutD = [?MONITORD];
    true ->
      OutD = []
  end,
  Output = lists:append([OutA, OutB, OutC, OutD]),
  Output.

isMonitorAlive(MonitorName) ->
  [{_, {IsALive}}] = ets:lookup(monitors, MonitorName),
  if (IsALive == true) ->
    Res = 1;
    true ->
      Res = 0
  end,
  Res.

compileAllFiles(MonitorID) ->
  rpc:call(MonitorID, compile, file, [controlledplayer]),
  rpc:call(MonitorID, compile, file, [ball]),
  rpc:call(MonitorID, compile, file, [computerplayer]),
  rpc:call(MonitorID, compile, file, [wxserver]),
  rpc:call(MonitorID, compile, file, [utils]),
  rpc:call(MonitorID, compile, file, [etsutils]),
  rpc:call(MonitorID, compile, file, [monitor]),
  rpc:call(MonitorID, compile, file, [common]).

updateScreenSizes(N, N, [H], MonitorNames, AllPlayersAndBall) ->
  FunToFilter = fun({_, {_, {X, _}, _}, _}) -> (pickNextMonitor(X) == H) end,
  PlayersBelongToMonitor = lists:filter(FunToFilter, AllPlayersAndBall),
  monitor:refreshActiveMonitors(H, N, N, MonitorNames, PlayersBelongToMonitor);

updateScreenSizes(M, N, [H | T], MonitorNames, AllPlayersAndBall) ->
  FunToFilter = fun({_, {_, {X, _}, _}, _}) -> (pickNextMonitor(X) == H) end,
  PlayersBelongToMonitor = lists:filter(FunToFilter, AllPlayersAndBall),
  monitor:refreshActiveMonitors(H, M, N, MonitorNames, PlayersBelongToMonitor),
  updateScreenSizes(M + 1, N, T, MonitorNames, AllPlayersAndBall).

isFinishedGame(LocationX, LocationY) ->
  if (LocationX < 50) and ((LocationY < ?GATE_UPPER_LIMIT) and (LocationY > ?GATE_LOWER_LIMIT)) ->
    IsFinished = goalUpdate(teamTwoPoints),
    ets:insert(ball, {ballX, ?BALL_INIT_X}),
    ets:insert(ball, {ballY, ?BALL_INIT_Y}),
    IsFinished;
    (LocationX > 1384) and ((LocationY < ?GATE_UPPER_LIMIT) and (LocationY > ?GATE_LOWER_LIMIT)) ->
      IsFinished = goalUpdate(teamOnePoints),
      ets:insert(ball, {ballX, ?BALL_INIT_X}),
      ets:insert(ball, {ballY, ?BALL_INIT_Y}),
      IsFinished;
    true -> 0
  end.

goalUpdate(TeamNumAtom) ->
  [{_, CurrentPoints}] = ets:lookup(statistics, TeamNumAtom),
  NewRes = CurrentPoints + 1,
  ets:insert(statistics, {TeamNumAtom, NewRes}),
  if (NewRes >= 3) -> 1;
    true ->
      ets:insert(generalState, {status, resetRound}),
      0
  end.

kick(BallImg) ->
  [{_, IsBallAtPlayer}] = ets:lookup(isOwner, ball),
  if (IsBallAtPlayer == 1) ->
    [{_, WXPaint}] = ets:lookup(statistics, 180),
    [{_, BallX}] = ets:lookup(ball, ballX),
    [{_, BallY}] = ets:lookup(ball, ballY),
    ets:insert(isOwner, {ball, 0}),
    Height = wxImage:getHeight(BallImg) / 22,
    paintImage(WXPaint, BallImg, {BallX - 90, BallY - Height}),
    ets:insert(ball, {ballX, BallX - 90}),
    ets:insert(ball, {ballY, BallY - Height}),
    ets:insert(isOwner, {kickDirection, 1});
    true -> continue
  end.

randomNetDest(WhichNet) ->
  [{_, DestBallX}] = ets:lookup(ball, destX),
  [{_, DestBallY}] = ets:lookup(ball, destY),
  [{_, Flag}] = ets:lookup(ball, flag),
  [{_, Togg}] = ets:lookup(ball, togg),
  if (Flag == false) ->
    Random = rand:uniform(235),
    if (Togg == up) ->
      ets:insert(ball, {togg, down}),
      NewY = 250 - Random;
      true ->
        ets:insert(ball, {togg, up}),
        NewY = Random + 250
    end,
    if (WhichNet == left) ->
      Res = {45, NewY},
      ets:insert(ball, {destX, 45}),
      ets:insert(ball, {flag, true}),
      ets:insert(ball, {destY, NewY});
      (WhichNet == right) ->
        Res = {1390, NewY},
        ets:insert(ball, {destX, 1390}),
        ets:insert(ball, {flag, true}),
        ets:insert(ball, {destY, NewY})
    end;
    true -> Res = {DestBallX, DestBallY}
  end,
  Res.