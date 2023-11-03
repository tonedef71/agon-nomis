10000 REM :::::::::::::::::::::::::::::::::::::::::::::
10010 REM :: NOMIS FOR AgonLight (BBC BASIC v3)      ::
10020 REM :: NOMIS Only May Impersonate Simon        ::
10030 REM :::::::::::::::::::::::::::::::::::::::::::::
10040 REM :: 20231103: V1.2 - Use new VDP MODEs      ::
10050 REM :::::::::::::::::::::::::::::::::::::::::::::
10060 REM :: This game won first place in the Olimex ::
10070 REM :: Weekend Programming Challenge Issue #1  ::
10080 REM :::::::::::::::::::::::::::::::::::::::::::::
10090 REM :: It is best experienced in a 40+ column, ::
10100 REM :: 16+ color display mode                  ::
10110 REM :::::::::::::::::::::::::::::::::::::::::::::
10120 CLEAR
10130 REPEAT CLS:SY$=FN_TO_UPPER(FN_PROMPT(0,0,"TARGET (A)gon or (B)BC B-SDL:","A")):UNTIL SY$ = "A" OR SY$ = "B"
10140 IF SY$ = "B" THEN PDLY% = 350:SDLY% = 20:MO% = 9:ELSE PDLY% = 125:SDLY% = 13:MO% = 13
10150 IF SY$ = "A" THEN REPEAT CLS:MO$=FN_PROMPT_FOR_NUMBERS(0,0,"MODE (0,3,4,8,9,12,13,...):",STR$(MO%),3):UNTIL VAL(MO$) > 0:MO% = VAL(MO$)
10160 MODE MO%
10170 PROC_SETUP
10180 ON ERROR PROC_HANDLE_ERROR:REM Handle ESC key
10190 PROC_WELCOME
10200 REPEAT
10210   PROC_DEFAULT_COLORS:PROC_NEW_GAME:PROC_DRAGNET
10220   PROC_MAIN_LOOP:REM Invoke main loop
10230   PROC_GAME_OVER
10240   Resp$ = FN_PLAY_AGAIN
10250   IF Resp$ = "Y" THEN CLS:VDU 31,0,0:ELSE PROC_GOODBYE(GameName$)
10260 UNTIL Resp$ <> "Y"
10270 END
10280 :
10290 REM ::::::::::::::::::::
10300 REM ::   Setup Game   ::
10310 REM ::::::::::::::::::::
10320 DEF PROC_SETUP
10330 LOCAL i%, n%
10340 MAXINT% = &3B9AC9FF:GameName$ = "NOMIS":MaxLevel% = 4:BestScore% = 6:BestRounds% = 8
10350 BLACK = 0:RED = 1:GREEN = 2:YELLOW = 3:BLUE = 4:MAGENTA = 5:CYAN = 6:WHITE = 7:BLANK = 32:LIT = 32:UNLIT = 35
10360 DIM DIFFICULTY_LEVELS(MaxLevel%), Sequence%(32), LIT_PAD$(4, 2), UNLIT_PAD$(4, 2), PAD%(4, 2), PAD_COLOR%(4), PAD_PITCH%(4), PAD_KEY$(4)
10370 IF SY$ = "A" THEN CW% = FN_getByteVDP(&13):CH% = FN_getByteVDP(&14):ELSE CW% = 40:CH% = 24
10380 RESTORE:FOR i% = 0 TO 3:READ DIFFICULTY_LEVELS(i%):NEXT i%
10390 DATA 8,14,20,31
10400 FOR i% = 0 TO 3
10410   READ PAD_COLOR%(i%),PAD_PITCH%(i%),PAD_KEY$(i%),PAD%(i%, 0),PAD%(i%, 1)
10420   READ n%:UNLIT_PAD$(i%, 0) = STRING$(n%, CHR$(UNLIT)):LIT_PAD$(i%, 0) = STRING$(n%, CHR$(LIT))
10430   READ n%:UNLIT_PAD$(i%, 1) = STRING$(n%, CHR$(UNLIT)):LIT_PAD$(i%, 1) = STRING$(n%, CHR$(LIT))
10440 NEXT i%
10450 REM PAD_COLOR_VALUE,PAD_SOUND_VALUE,PAD_KEY,PAD_X,PAD_Y,PAD_TOP_WIDTH,PAD_BOTTOM_WIDTH
10460 DATA GREEN,213,"R",0,0,10,5,RED,185,"I",11,0,10,5,YELLOW,201,"F",0,7,5,10,BLUE,165,"J",11,7,5,10
10470 PROC_HISCORE_READ(GameName$)
10480 ENDPROC
10490 :
10500 REM ::::::::::::::::::::::
10510 REM ::     New Game     ::
10520 REM ::::::::::::::::::::::
10530 DEF PROC_NEW_GAME
10540 Lost% = FALSE
10550 PROC_SELECT_DIFFICULTY(POS, VPOS)
10560 PROC_NEW_SEQUENCE(DifficultyLevel%)
10570 NumRounds% = DIFFICULTY_LEVELS(DifficultyLevel% - 1)
10580 CLS:PROC_HIDE_CURSOR
10590 PROC_DRAW_PLAYING_FIELD
10600 ENDPROC
10610 :
10620 REM ::::::::::::::::::::::
10630 REM ::     Main Loop    ::
10640 REM ::::::::::::::::::::::
10650 DEF PROC_MAIN_LOOP
10660 LOCAL index%, i%, n%, currentRound%, delayTime%, won%
10670 currentRound% = 0:won% = FALSE
10680 REPEAT
10690   PROC_GET_READY:PROC_DISPLAY_SCORES(currentRound%)
10700   FOR i% = 0 TO currentRound%:REM Present sequence revealed thus far
10710     delayTime% = SDLY% - (currentRound% >= 5) * -SDLY%/6 + (currentRound% >= 9) * -SDLY%/6 + (currentRound% >= 13) * -SDLY%/6
10720     n% = Sequence%(i%):PROC_PAINT_PAD(n%, TRUE, 20):PROC_PAD_SOUND(n%, delayTime%):PROC_PAINT_PAD(n%, FALSE, 20)
10730   NEXT i%
10740   i% = 0:IF SY$ <> "A" THEN PROC_EMPTY_KEYBOARD_BUFFER:REM Clear out any lingering extra presses
10750   REPEAT
10760     n% = Sequence%(i%)
10770     index% = FN_PAD_PRESS(PDLY%):REM Get pad pressed by player
10780     IF (index% = -1) OR (index% <> n%) THEN Lost% = TRUE:REM Wrong pad press or no pad press detected
10790     PROC_PAINT_PAD(n%, TRUE, 20):REM Illuminate the correct pad
10800     delayTime% = (Lost%) * -150 + (NOT Lost%) * -SDLY%
10810     IF Lost% THEN PROC_SOUND(1, 5, 30):PROC_SLEEP(delayTime%):ELSE PROC_PAD_SOUND(index%, delayTime%)
10820     PROC_PAINT_PAD(n%, FALSE, 20):REM Stop illuminating the pad
10830     i% = i% + 1:REM Next signal in sequence
10840   UNTIL i% > currentRound% OR Lost%
10850   currentRound% = currentRound% + 1
10860   IF NOT Lost% AND (currentRound% = NumRounds%) THEN PROC_UPDATE_BEST_SCORE(currentRound%):won% = TRUE
10870 UNTIL won% OR Lost%
10880 ENDPROC
10890 :
10900 REM ::::::::::::::::::::::::::::::::
10910 REM :: Generate A Random Sequence ::
10920 REM ::::::::::::::::::::::::::::::::
10930 DEF PROC_NEW_SEQUENCE(difficulty_level%)
10940 LOCAL i%
10950 IF difficulty_level% < 1 OR difficulty_level% > 4 THEN difficulty_level% = 1
10960 FOR i% = 0 TO DIFFICULTY_LEVELS(difficulty_level% - 1) - 1
10970   Sequence%(i%) = FN_RND_INT(0, 3)
10980 NEXT i%
10990 Sequence%(i%) = -1:REM Signal end of sequence with terminating value of -1
11000 ENDPROC
11010 :
11020 REM ::::::::::::::::::::::::
11030 REM :: Draw Playing Field ::
11040 REM ::::::::::::::::::::::::
11050 DEF PROC_DRAW_PLAYING_FIELD
11060 LOCAL i%, ox%, oy%
11070 FOR i% = 0 TO 3
11080   PROC_PAINT_PAD(i%, FALSE, 0)
11090 NEXT i%
11100 ox% = CW% DIV 2 - 10:oy% = CH% DIV 2 - 10
11110 PRINT TAB(8+ox%, 6+oy%)CHR$(17)CHR$(128+BLACK)CHR$(17)CHR$(WHITE)GameName$
11120 PRINT TAB(7+ox%, 3+oy%)PAD_KEY$(0)TAB(13+ox%, 3+oy%)PAD_KEY$(1)TAB(7+ox%, 9+oy%)PAD_KEY$(2)TAB(13+ox%, 9+oy%)PAD_KEY$(3)
11130 ENDPROC
11140 :
11150 REM ::::::::::::::::::::::
11160 REM :: Handle Pad Press ::
11170 REM ::::::::::::::::::::::
11180 DEF FN_PAD_PRESS(duration%)
11190 LOCAL br%, ch%, ch$
11200 IF SY$ = "A" THEN ch% = FN_GET_KEY(duration%):ELSE ch% = INKEY(duration%)
11210 IF ch% <> -1 THEN ch$ = FN_TO_UPPER(CHR$(ch%))
11220 br% = (ch$ = PAD_KEY$(0)) * -1 + (ch$ = PAD_KEY$(1)) * -2 + (ch$ = PAD_KEY$(2)) * -3 + (ch$ = PAD_KEY$(3)) * -4
11230 br% = br% - 1
11240 = br%
11250 :
11260 REM ::::::::::::::::::::::
11270 REM ::    Paint Pad     ::
11280 REM ::::::::::::::::::::::
11290 DEF PROC_PAINT_PAD(index%, is_lit%, pause%)
11300 LOCAL i%, ox%, oy%, x%, y%, p$
11310 ox% = CW% DIV 2 - 10:oy% = CH% DIV 2 - 10
11320 FOR i% = 0 TO 5
11330   x% = PAD%(index%, 0):y% = PAD%(index%, 1) + i%
11340   IF is_lit% THEN p$ = LIT_PAD$(index%, (i% > 2) * -1):ELSE p$ = UNLIT_PAD$(index%, (i% > 2) * -1)
11350   IF (LEN(p$) < 10) AND (index% MOD 2 = 1) THEN x% = x% + 5
11360   PRINT TAB(x% + ox%, y% + oy%)FN_RVS(PAD_COLOR%(index%) + -8 * (is_lit% <> 0), BLACK, p$)
11370 NEXT i%
11380 PROC_SLEEP(pause%)
11390 ENDPROC
11400 :
11410 REM ::::::::::::::::::::::
11420 REM ::  Play Pad Sound  ::
11430 REM ::::::::::::::::::::::
11440 DEF PROC_PAD_SOUND(index%, duration%)
11450 LOCAL p%, dummy$
11460 p% = PAD_PITCH%(index%)
11470 PROC_SOUND(2, p%, duration% DIV 5):REM 0.05 = DURATION; .01 = SLEEP
11480 ENDPROC
11490 :
11500 REM :::::::::::::::::::::::::::::
11510 REM :: Select Difficulty Level ::
11520 REM :::::::::::::::::::::::::::::
11530 DEF PROC_SELECT_DIFFICULTY(x%, y%)
11540 LOCAL text$:PROC_HIDE_CURSOR
11550 text$ = CHR$(17)+CHR$(CYAN)+"Difficulty Level (1 to 4):"
11560 REPEAT
11570   r$ = FN_PROMPT(FN_CENTER(text$) - 2, y%, text$, "1")
11580   r% = VAL(r$)
11590 UNTIL (r% >= 1 AND r% <= 4)
11600 DifficultyLevel% = r%
11610 ENDPROC
11620 :
11630 REM ::::::::::::::::::::::
11640 REM ::  Display Scores  ::
11650 REM ::::::::::::::::::::::
11660 DEF PROC_DISPLAY_SCORES(currentRound%)
11670 LOCAL sc$
11680 sc$ = CHR$(17)+CHR$(CYAN)+"ROUND "+CHR$(17)+CHR$(WHITE)+STR$(currentRound% + 1)+"/"+STR$(NumRounds%)
11690 PRINT TAB(0,0)sc$
11700 PROC_UPDATE_BEST_SCORE(currentRound%)
11710 ENDPROC
11720 :
11730 REM :::::::::::::::::::::::::
11740 REM ::  Update Best Score  ::
11750 REM :::::::::::::::::::::::::
11760 DEF PROC_UPDATE_BEST_SCORE(currentRound%)
11770 LOCAL hs$
11780 IF (BestScore% < currentRound%) THEN BestScore% = currentRound%:BestRounds% = NumRounds%:REM Check if best score has been surpassed and update if needed
11790 hs$ = CHR$(17)+CHR$(YELLOW)+"BEST "+CHR$(17)+CHR$(WHITE)+STR$(BestScore%)+"/"+STR$(BestRounds%)
11800 PRINT TAB(CW% - LEN(hs$) + 4, 0)hs$
11810 ENDPROC
11820 :
11830 REM :::::::::::::::::::::::
11840 REM ::  High Score Read  ::
11850 REM :::::::::::::::::::::::
11860 DEF PROC_HISCORE_READ(game$)
11870 LOCAL f0%, status%, val%
11880 status% = 0
11890 f0% = OPENIN(game$ + ".HI")
11900 IF f0% <> 0 THEN INPUT#f0%, val1%, val2%:ELSE status% = -1
11910 CLOSE#f0%
11920 IF status% = 0 THEN BestScore% = val1%:BestRounds% = val2%
11930 ENDPROC
11940 :
11950 REM ::::::::::::::::::::::::
11960 REM ::  High Score Write  ::
11970 REM ::::::::::::::::::::::::
11980 DEF PROC_HISCORE_WRITE(game$)
11990 LOCAL f0%
12000 f0% = OPENOUT(game$ + ".HI")
12010 PRINT#f0%, BestScore%, BestRounds%
12020 CLOSE#f0%
12030 ENDPROC
12040 :
12050 REM ::::::::::::::::::::::::::::::
12060 REM :: Display Get Ready Prompt ::
12070 REM ::::::::::::::::::::::::::::::
12080 DEF PROC_GET_READY
12090 VDU 17,YELLOW:VDU 31, 0, 0:PROC_CENTER("GET READY!"):REM Display GET READY! Message
12100 PROC_SLEEP(150):VDU 31, 0, 0:PROC_CENTER(STRING$(10, " ")):REM Erase GET READY! Message after 2 Seconds
12110 ENDPROC
12120 :
12130 REM :::::::::::::::::::
12140 REM ::    Welcome    ::
12150 REM :::::::::::::::::::
12160 DEF PROC_WELCOME
12170 LOCAL i%, pad$
12180 pad$ = ""
12190 FOR i% = 0 TO 3:pad$ = pad$ + " " + CHR$(17) + CHR$(PAD_COLOR%(i%)) + PAD_KEY$(i%):NEXT i%
12200 CLS:PROC_CENTER(CHR$(17)+CHR$(YELLOW)+"NOMIS Only May Impersonate Simon"):PRINT:PRINT
12210 PROC_CENTER(CHR$(17)+CHR$(WHITE)+"Welcome to "+CHR$(17)+CHR$(CYAN)+GameName$+CHR$(17)+CHR$(WHITE)+"..."):PRINT
12220 PROC_CENTER("A colorful and musical game"):PRINT
12230 PROC_CENTER("of 'Follow the Leader'"):PRINT:PRINT
12240 PROC_CENTER(FN_RVS(WHITE, BLACK, "Object Of The Game")):PRINT
12250 PROC_CENTER("  Correctly repeat a longer and"):PRINT
12260 PROC_CENTER("longer sequence of signals"):PRINT:PRINT
12270 PROC_CENTER(FN_RVS(WHITE, BLACK, "Controls")):PRINT
12280 PROC_CENTER("The following keys press the"):PRINT
12290 PROC_CENTER("sensor pad of the"):PRINT
12300 PROC_CENTER("corresponding color:" + pad$):PRINT
12310 PRINT CHR$(17)CHR$(WHITE)
12320 ENDPROC
12330 :
12340 REM :::::::::::::::::
12350 REM ::  Game Over  ::
12360 REM :::::::::::::::::
12370 DEF PROC_GAME_OVER
12380 LOCAL co%, msg$
12390 IF Lost% THEN co% = RED:msg$ = "So sorry! You lost.":PROC_WAH_WAH:ELSE co% = GREEN:msg$ = "Congratulations! You won.":PROC_CHARGE
12400 VDU 17,co%:VDU 31, 0, CH% DIV 2 + 4:PROC_CENTER(msg$)
12410 PROC_SLEEP(200)
12420 PROC_HISCORE_WRITE(GameName$)
12430 ENDPROC
12440 :
12450 REM ::::::::::::::::::::::::::::::
12460 REM :: Play Another Game Prompt ::
12470 REM ::::::::::::::::::::::::::::::
12480 DEF FN_PLAY_AGAIN
12490 LOCAL message$, r$
12500 message$ = "Play Again? (Y/N)"
12510 PROC_EMPTY_KEYBOARD_BUFFER:VDU 17,YELLOW
12520 REPEAT r$ = FN_PROMPT(FN_CENTER(message$), CH% DIV 2 + 6, message$, "") UNTIL INSTR("YN", r$) <> 0
12530 = r$
12540 :
12550 REM :::::::::::::::::::
12560 REM ::  Say Goodbye  ::
12570 REM :::::::::::::::::::
12580 DEF PROC_GOODBYE(game$)
12590 PROC_HIDE_CURSOR
12600 CLS:PROC_FULL_CENTER_TEXT("So long and thank you for playing...")
12610 FOR i% = 0 TO FN_CENTER(game$) - 1:PRINTTAB(0, CH% DIV 2 + 2)STRING$(i%, " ")CHR$(17)CHR$(i% MOD 7 + 1)game$:PROC_SLEEP(20):NEXT i%
12620 PROC_DEFAULT_COLORS
12630 PROC_SHOW_CURSOR
12640 ENDPROC
12650 :
12660 REM :::::::::::::::::
12670 REM ::  CHARGE!!!  ::
12680 REM :::::::::::::::::
12690 DEF PROC_CHARGE
12700 PROC_PLAY("129001149001165001177004165002177008"):REM PITCH,DURATION
12710 ENDPROC
12720 :
12730 REM :::::::::::::::
12740 REM ::  WAH-WAH  ::
12750 REM :::::::::::::::
12760 DEF PROC_WAH_WAH
12770 PROC_PLAY("081002081002081002069020073002073002073002061024"):REM PITCH,DURATION
12780 ENDPROC
12790 :
12800 REM :::::::::::::::
12810 REM ::  DRAGNET  ::
12820 REM :::::::::::::::
12830 DEF PROC_DRAGNET
12840 PROC_PLAY("149006157002161006149006-01016149006157002161006149006173014"):REM PITCH,DURATION
12850 ENDPROC
12860 :
12870 REM :::::::::::::::::::::::::::::::::
12880 REM :: Random Integer Within Range ::
12890 REM :::::::::::::::::::::::::::::::::
12900 DEF FN_RND_INT(lo%, hi%):= (RND(1) * (hi% - lo% + 1)) + lo%
12910 :
12920 REM ::::::::::::::::::::::
12930 REM :: Maximum of x & y ::
12940 REM ::::::::::::::::::::::
12950 DEF FN_MAX(x, y):= y + (x > y) * (y - x)
12960 :
12970 REM ::::::::::::::::::::::
12980 REM :: Minimum of x & y ::
12990 REM ::::::::::::::::::::::
13000 DEF FN_MIN(x, y):= y + (x < y) * (y - x)
13010 :
13020 REM ::::::::::::::::::::::::::
13030 REM ::  Bounded time ticks  ::
13040 REM ::::::::::::::::::::::::::
13050 DEF FN_INT_TIME:= TIME MOD MAXINT%
13060 :
13070 REM ::::::::::::::::::::::
13080 REM :: Has time reached ::
13090 REM :: target seconds?  ::
13100 REM ::::::::::::::::::::::
13110 DEF FN_IS_TIME(sec%, prevSec%, targetSec%):= (sec% MOD targetSec% = 0 AND sec% <> prevSec%)
13120 :
13130 REM :::::::::::::::::::::
13140 REM :: Retrieve a byte ::
13150 REM :: register value  ::
13160 REM :: from VDP        ::
13170 REM :::::::::::::::::::::
13180 DEF FN_getByteVDP(var%):A% = &A0:L% = var%:= USR(&FFF4)
13190 :
13200 REM ::::::::::::::::::::::
13210 REM :: Retrieve a word ::
13220 REM :: register value  ::
13230 REM :: from VDP        ::
13240 REM :::::::::::::::::::::
13250 DEF FN_getWordVDP(var%):= FN_getByteVDP(var%) + 256 * FN_getByteVDP(var% + 1)
13260 :
13270 REM ::::::::::::::::::::::
13280 REM :: Retrieve the     ::
13290 REM :: number of colors ::
13300 REM :: reported by VDP  ::
13310 REM ::::::::::::::::::::::
13320 DEF FN_COLORCOUNT:= FN_getByteVDP(&15)
13330 :
13340 REM :::::::::::::::::::::
13350 REM :: Retrieve the    ::
13360 REM :: ASCII key code  ::
13370 REM :: reported by VDP ::
13380 REM ::::::::::::::::::::::
13390 DEF FN_ASCII_KEYCODE:= FN_getByteVDP(&05)
13400 :
13410 REM ::::::::::::::::::::::
13420 REM :: Retrieve the     ::
13430 REM :: Virtual key code ::
13440 REM :: reported by VDP  ::
13450 REM ::::::::::::::::::::::
13460 DEF FN_VIRTUAL_KEYCODE:= FN_getByteVDP(&17)
13470 :
13480 REM :::::::::::::::::::::::::::::
13490 REM :: Retrieve the number of  ::
13500 REM :: keys as reported by VDP ::
13510 REM :::::::::::::::::::::::::::::
13520 DEF FN_ASCII_KEYCOUNT:= FN_getByteVDP(&19)
13530 :
13540 REM ::::::::::::::::::::::::::::::::
13550 REM :: Retrieve a keypress within ::
13560 REM :: the given timeout value    ::
13570 REM ::::::::::::::::::::::::::::::::
13580 DEF FN_GET_KEY(timeout%)
13590 LOCAL i%, keycount%, r%, sync%
13600 r% = -1
13610 keycount% = FN_ASCII_KEYCOUNT
13620 i% = 0
13630 REPEAT
13640   IF keycount% <> FN_ASCII_KEYCOUNT THEN r% = FN_ASCII_KEYCODE:IF r% = 0 THEN r% = FN_VIRTUAL_KEYCODE ELSE *FX 19
13650   i% = i% + 1
13660 UNTIL i% = timeout% OR r% > 0
13670 := r%
13680 :
13690 REM :::::::::::::::::::::::::::::::::::::::
13700 REM :: Pause execution of the program    ::
13710 REM :: for a number of ticks (1/100) sec ::
13720 REM :::::::::::::::::::::::::::::::::::::::
13730 DEF PROC_SLEEP(hundredth_seconds%):LOCAL t:hundredth_seconds% = hundredth_seconds% + (hundredth_seconds% < 0) * -hundredth_seconds%:t = TIME:REPEAT UNTIL ((TIME - t) > hundredth_seconds%):ENDPROC
13740 :
13750 REM ::::::::::::::::::::::
13760 REM ::   To Uppercase   ::
13770 REM ::::::::::::::::::::::
13780 DEF FN_TO_UPPER(ch$):LOCAL ch%:ch% = ASC(ch$):ch$ = CHR$(ch% + 32 * (ch% >= 97 AND ch% <= 122)):=ch$
13790 :
13800 REM :::::::::::::::::::::
13810 REM ::   Center text   ::
13820 REM :::::::::::::::::::::
13830 DEF FN_CENTER(text$):= (CW% - LEN(text$)) DIV 2 + 1
13840 :
13850 REM :::::::::::::::::::::::::::::::::::
13860 REM :: Display Text In Reverse Video ::
13870 REM :::::::::::::::::::::::::::::::::::
13880 DEF FN_RVS(fg%, bg%, text$):= CHR$(17)+CHR$(128+fg%)+CHR$(17)+CHR$(bg%)+text$+CHR$(17)+CHR$(fg%)+CHR$(17)+CHR$(128+bg%)
13890 :
13900 REM ::::::::::::::::::::::::::::
13910 REM :: Disable display of the ::
13920 REM :: cursor on the screen   ::
13930 REM ::::::::::::::::::::::::::::
13940 DEF PROC_HIDE_CURSOR:VDU 23,1,0;0;0;0;:ENDPROC
13950 :
13960 REM :::::::::::::::::::::::::::
13970 REM :: Enable display of the ::
13980 REM :: cursor on the screen  ::
13990 REM :::::::::::::::::::::::::::
14000 DEF PROC_SHOW_CURSOR:VDU 23,1,1;0;0;0;:ENDPROC
14010 :
14020 REM :::::::::::::::::::::::::
14030 REM :: Prompt For Response ::
14040 REM :::::::::::::::::::::::::
14050 DEF FN_PROMPT(x%, y%, text$, default$)
14060 LOCAL r$
14070 PRINT TAB(x%, y%)text$;" ";default$:PRINT TAB(x% + LEN(text$) + 1, y%);
14080 r$ = GET$:r$ = FN_TO_UPPER(r$):IF r$ = CHR$(13) THEN r$ = default$
14090 := r$
14100 :
14110 REM :::::::::::::::::::::::::::::::::
14120 REM :: Enter numbers from keyboard ::
14130 REM :::::::::::::::::::::::::::::::::
14140 DEF FN_PROMPT_FOR_NUMBERS(x%, y%, text$, default$, length%)
14150 LOCAL c$, r$
14160 r$ = "":PROC_EMPTY_KEYBOARD_BUFFER:PROC_SHOW_CURSOR
14170 PRINT TAB(x%, y%)text$;" ";default$:PRINT TAB(x% + LEN(text$) + 1, y%);
14180 REPEAT
14190   c$ = GET$
14200   IF ((c$ = CHR$(127) OR c$ = CHR$(8)) AND LEN(r$) > 0) THEN r$ = LEFT$(r$, LEN(r$) - 1):PRINT CHR$(127);
14210   IF (c$ >= "0" AND c$ <= "9") AND LEN(r$) < length% THEN r$ = r$ + c$:PRINT c$;
14220   IF c$ = CHR$(13) AND LEN(r$) = 0 THEN r$ = default$
14230 UNTIL (c$ = CHR$(13) AND LEN(r$) <= length%)
14240 PROC_HIDE_CURSOR
14250 :=r$
14260 :
14270 REM :::::::::::::::::::::::::::
14280 REM :: Empty Keyboard Buffer ::
14290 REM :::::::::::::::::::::::::::
14300 DEF PROC_EMPTY_KEYBOARD_BUFFER
14310 REPEAT UNTIL INKEY(0) = -1
14320 ENDPROC
14330 :
14340 REM :::::::::::::::::::::::::::::
14350 REM ::  Display Centered Text  ::
14360 REM :::::::::::::::::::::::::::::
14370 DEF PROC_CENTER(text$)
14380 LOCAL i%, n%, l%
14390 l% = 0
14400 FOR i% = 1 TO LEN(text$)
14410   IF ASC(MID$(text$, i%, 1)) >= BLANK THEN l% = l% + 1
14420 NEXT i%
14430 n% = FN_CENTER(STRING$(l%, CHR$(BLANK)))
14440 i% = VPOS:VDU 31, n%, i%
14450 FOR i% = 1 TO LEN(text$)
14460   VDU ASC(MID$(text$, i%, 1))
14470 NEXT i%
14480 ENDPROC
14490 :
14500 REM :::::::::::::::::::::::::::::::::
14510 REM :: Center text both vertically ::
14520 REM :: and horizontally            ::
14530 REM :::::::::::::::::::::::::::::::::
14540 DEF PROC_FULL_CENTER_TEXT(text$):VDU 31,FN_CENTER(text$), CH% DIV 2:PRINT text$;:ENDPROC
14550 :
14560 REM ::::::::::::::::::::::::::::
14570 REM :: Restore Default Colors ::
14580 REM ::::::::::::::::::::::::::::
14590 DEF PROC_DEFAULT_COLORS
14600 COLOUR 128+BLACK:COLOUR WHITE
14610 ENDPROC
14620 :
14630 REM :::::::::::::::::::::::
14640 REM :: Play Simple Sound ::
14650 REM :::::::::::::::::::::::
14660 DEF PROC_SOUND(channel%, tone%, duration%)
14670 IF channel% < 0 OR channel% > 2 THEN channel% = 1
14680 SOUND channel%, -12, tone%, duration%
14690 SOUND channel%, 0, tone%, 1:REM Stacatto the currently playing sound
14700 ENDPROC
14710 :
14720 REM :::::::::::::::::::::::::
14730 REM :: Play Musical Phrase ::
14740 REM :::::::::::::::::::::::::
14750 DEF PROC_PLAY(notes$)
14760 LOCAL d%, j%, l%, p%
14770 l% = LEN(notes$) DIV 3
14780 FOR j% = 1 TO l% STEP 2
14790   p% = VAL(MID$(notes$, 3 * (j% - 1) + 1, 3)):d% = VAL(MID$(notes$, 3 * (j% - 1) + 4, 3))
14800   IF p% >= 0 THEN SOUND 1, -10, p%, d%:ELSE SOUND 1, 0, 0, d%
14810   SOUND 1, 0, p%, 1:REM Stacatto the currently playing sound
14820 NEXT j%
14830 ENDPROC
14840 :
14850 REM ::::::::::::::::::::::::::::::
14860 REM ::  Error Handling Routine  ::
14870 REM ::::::::::::::::::::::::::::::
14880 DEF PROC_HANDLE_ERROR
14890 IF ERR <> 17 THEN PROC_DEFAULT_COLORS:PROC_SHOW_CURSOR:PRINT:REPORT:PRINT" @line #";ERL:STOP
14900 ENDPROC
