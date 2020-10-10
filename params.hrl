%%%-------------------------------------------------------------------
%%% @author Moshe.Dahan
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2020 18:29
%%%-------------------------------------------------------------------
-author("Moshe.Dahan").

-define(TimeToWait1, 60).
-define(TimeToWait2, 150).
-define(REFRESH_RATE, 150).
-define(MONITOR_REFRESH_RATE, 100).


-define(MONITOR_PROCESSES, [?MONITOR_LONG_NAME_A,?MONITOR_LONG_NAME_B,?MONITOR_LONG_NAME_C,?MONITOR_LONG_NAME_D]).
-define(MONITORA, 'monitorA').
-define(MONITORB, 'monitorB').
-define(MONITORC, 'monitorC').
-define(MONITORD, 'monitorD').
-define(BACKGROUND_COLOR,	{200,200,200}).
-define(SCREEN_REFRESH_TIME,	30).
-define(SCREEN_SIZE,{1439, 819}).
-define(MONITOR_LONG_NAME_D, 'monitorD@IL01-PF1P52W').
-define(MONITOR_LONG_NAME_C, 'monitorC@IL01-PF1P52W').
-define(MONITOR_LONG_NAME_B, 'monitorB@IL01-PF1P52W').
-define(MONITOR_LONG_NAME_A, 'monitorA@IL01-PF1P52W').
-define(MAX_PLAYER_Y, 750).
-define(BALL_DIFF, 5).

% Utils
-define(TEAM1IMAGE, "Images/BAR.png").
-define(TEAM2IMAGE, "Images/T2.png").
-define(CONTROLLED_PLAYER_IMAGE, "Images/LEAD.png").
-define(BALL_WIDTH_MULTIPLY_FACTOR, 1.28).
-define(BALL_IMAGE, "Images/ball.png").
-define(REFEREE_IMAGE, "Images/referee.png").
-define(FINISHED_IMAGE, "Images/finishedgame.png").
-define(STAGE_IMAGE, "Images/stagefinish.png").
-define(FIELD_IMAGE,"Images/Field.jpg").


-define(PRINT_X_START, 400).
-define(PRINT_Y_START, 130).
-define(PRINT_W_H, 300).
-define(TAB, 30).
-define(BIGGER_INDENTATIONS,120 ).
-define(GATE_UPPER_LIMIT, 450).
-define(GATE_LOWER_LIMIT, 360).
-define(BALL_INIT_X, 698).
-define(BALL_INIT_Y, 390).
-define(PLAYER_STEP_SIZE, 5).
-define(BALL_INITIAL_COORDINATES,{600, 500}).
-define(GOALIE_Y_START, 350).

-define(X_Upper_Limit, 1439).
-define(X_Lower_Limit, 20).
-define(Y_Upper_Limit, 810).
-define(Y_Lower_Limit, 15).

-define(CHARACTER_WIDTH, 15).
