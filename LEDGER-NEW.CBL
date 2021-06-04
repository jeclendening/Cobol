       IDENTIFICATION DIVISION.
       PROGRAM-ID. WOLFESALES.
      *REMARKS AND MODIFICATIONS. 
      **************************************************************
      * REMARKS:
      * REPORT RETURNS FOR PAPER BY WEEK.
      *
      *
      *
      **************************************************************
      *  MODIFICATION HISTORY:
      *
      *    
      *   MODIFIED: 7/01/2018
      *   PROGRAMMER: Jeffrey Clendening.
      *   MODIFICATION: added bulldog 349 to copybook FIXIT-NEW and 
      *                 added a comment section as well.
      *                 REMOVED (CARRIAGE RETURN LINE FEEDS that cause warning 
      *                          and errors when compiled)
      *   
      *         
      *   MODIFIED: 7/12/2018
      *   PROGRAMMER: Jeffrey Clendening
      *   MODIFICATION: Added bipad 349 and 449 to program because of 
      *                 saturday becoming bulldog.
      *
      *
      *
      *   MODIFIED:
      *   PROGRAMMER:
      *   MODIFICATION:
      *
      *
      *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-RS6000.
       OBJECT-COMPUTER.  IBM-RS6000.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY WNKSEL.
       COPY WNRSEL.
       COPY WNXSEL.
       COPY WNOSEL.
       COPY WNASEL.
           SELECT PRLINE ASSIGN PRINT WS-FILENAME
           STATUS FL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  WNKFILE LABEL RECORD STANDARD DATA RECORD WNK.
       01  WNK.
       COPY WNK.
       FD  WNRFILE LABEL RECORD STANDARD DATA RECORD WNR.
       01  WNR.
       COPY WNR.
       FD  WNAFILE LABEL RECORD STANDARD DATA RECORD WNA.
       01  WNA.
       COPY WNA.
       FD  WNXFILE LABEL RECORD STANDARD DATA RECORD WNX.
       01  WNX.
       COPY WNX.
       FD  WNOFILE LABEL RECORD STANDARD DATA RECORD WNO.
       01  WNO.
       COPY WNO.
       FD  PRLINE LABEL RECORD OMITTED DATA RECORD LINEPR.
       01  LINEPR.
           05  LP-W-E-DATE.
               10  LP-MM               PIC 99.
               10  LP-SLASH            PIC X.
               10  LP-DD               PIC 99.
           05  FILLER-1                PIC X.
           05  LP-DEALER-NO            PIC 99999.
           05  FILLER-2                PIC X.
           05  LP-STORE-NAME           PIC X(30).
           05  FILLER-3                PIC X.
           05  LP-STORE-ADDR-2         PIC X(30).
           05  FILLER-4                PIC X.
           05  LP-STORE-ADDR-3         PIC X(30).
           05  FILLER-5                PIC X.
           05  LP-STORE-ZIP-HI-5       PIC  X(5).
           05  FILLER-6                PIC X.
           05  LP-TITLE-NO             PIC 999.
           05  FILLER-7                PIC X.
           05  LP-TITLE                PIC X(21).
           05  FILLER-8                PIC X.
           05  LP-MON-DRAW             PIC 99999.
           05  FILLER-9                PIC X.
           05  LP-MON-RETURNS          PIC 99999.
           05  FILLER-10               PIC X.
           05  LP-MON-NET              PIC -99999.
           05  FILLER-11               PIC X.
           05  LP-TUE-DRAW             PIC 99999.
           05  FILLER-12               PIC X.
           05  LP-TUE-RETURNS          PIC 99999.
           05  FILLER-13               PIC X.
           05  LP-TUE-NET              PIC -99999.
           05  FILLER-14               PIC X.
           05  LP-WED-DRAW             PIC 99999.
           05  FILLER-15               PIC X.
           05  LP-WED-RETURNS          PIC 99999.
           05  FILLER-16               PIC X.
           05  LP-WED-NET              PIC -99999.
           05  FILLER-17               PIC X.
           05  LP-THU-DRAW             PIC 99999.
           05  FILLER-18               PIC X.
           05  LP-THU-RETURNS          PIC 99999.
           05  FILLER-19               PIC X.
           05  LP-THU-NET              PIC -99999.
           05  FILLER-20               PIC X.
           05  LP-FRI-DRAW             PIC 99999.
           05  FILLER-21               PIC X.
           05  LP-FRI-RETURNS          PIC 99999.
           05  FILLER-22               PIC X.
           05  LP-FRI-NET              PIC -99999.
           05  FILLER-23               PIC X.
           05  LP-SAT-DRAW             PIC 99999.
           05  FILLER-24               PIC X.
           05  LP-SAT-RETURNS          PIC 99999.
           05  FILLER-25               PIC X.
           05  LP-SAT-NET              PIC -99999.
           05  FILLER-26               PIC X.
           05  LP-SUN-DRAW             PIC 99999.
           05  FILLER-27               PIC X.
           05  LP-SUN-RETURNS          PIC 99999.
           05  FILLER-28               PIC X.
Ðÿ         05  LP-SUN-NET              PIC -99999.
           05  FILLER-29               PIC X.
           05  LP-MARKET               PIC XXXXXXXXX.
           05  FILLER                  PIC X.
       WORKING-STORAGE SECTION.
       01  WS-FILENAME.
           05  FILLER PIC X(40) VALUE
               "/home/prb/f/ledger/send/Liberty-".
           05  WS-FILENAME-DATE PIC 9999.
           05  FILLER PIC X(04) VALUE ".csv".
       01  WORK-AREA.
           05  FIRST-WRITE    PIC XXX VALUE "YES".
           05  WS-FIELD       PIC 99.
           05  FL-STATUS     PIC XX.
           05  REPLYX        PIC X.
           05  FILE-ID       PIC XXX.
           05  EDIT-COST PIC ZZZ9.999.
           05  WS-DEALER-NO PIC 99999 VALUE 99999.
           05  WS-ISS-DATE.
               10  WS-MM PIC 99.
               10  WS-DD PIC 99.
       01  HEAD-1.
           05  FILLER                  PIC XXXXX VALUE "W/E  ".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "ACCT ".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(12) VALUE "ACCOUNT NAME".
           05  FILLER                  PIC X(19) VALUE SPACE.
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(07) VALUE "ADDRESS".
           05  FILLER                  PIC X(24) VALUE SPACE.
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(10) VALUE "CITY STATE".
           05  FILLER                  PIC X(21) VALUE SPACE.
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(5)  VALUE "ZIP  ".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXX   VALUE "NO ".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(10) VALUE "PAPER NAME".
           05  FILLER                  PIC X(12) VALUE SPACE.
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "MON D".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "MON-R".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(6)  VALUE "M-SALE".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "TUE-D".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "TUE-R".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(6)  VALUE "T-SALE".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "WED-D".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "WED-R".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(6)  VALUE "W-SALE".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "THU-D".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "THU-R".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(6)  VALUE "T-SALE".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "FRI-D".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "DRI-R".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(6)  VALUE "F-SALE".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "SAT-D".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "SAT-R".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(6)  VALUE "S-SALE".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "SUN-D".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC XXXXX VALUE "SUN-R".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(6)  VALUE "S-SALE".
           05  FILLER                  PIC X     VALUE ",".
           05  FILLER                  PIC X(6)  VALUE "MARKET".
       PROCEDURE DIVISION.
       START-PROGRAM.
           OPEN INPUT WNKFILE WNRFILE WNAFILE WNXFILE WNOFILE.
           INITIALIZE WNA.
           START WNAFILE KEY NOT LESS WNA-PRIMARY-KEY.
           INITIALIZE WNR.
           MOVE 1000 TO WNR-DEALER-NO.
           START WNRFILE KEY GREATER WNR-PRIMARY-KEY.
       READ-WNR.
           READ WNRFILE NEXT AT END GO TO READ-WNA.
           MOVE WNR-ISS-DATE (3) TO WS-FILENAME-DATE.
           OPEN OUTPUT PRLINE.
           MOVE SPACE TO LINEPR.
           WRITE LINEPR FROM HEAD-1 AFTER ADVANCING 0.
       CHECK-MONTH.
       READ-WNA.
           MOVE SPACE TO LINEPR.
           READ WNAFILE NEXT AT END GO TO END-JOB.
           INITIALIZE WNK.
           MOVE WNA-DEALER-NO TO WNK-DEALER-NO-ONE.
           START WNKFILE KEY NOT LESS WNK-ALTERNATE-KEY-ONE
           INVALID KEY DISPLAY WNA-DEALER-NO GO TO READ-WNA.
       READ-WNK.
           READ WNKFILE NEXT AT END GO TO READ-WNA.
           IF WNK-DEALER-NO NOT = WNA-DEALER-NO GO TO READ-WNA.
           IF  WNK-TITLE-NO NOT = 115
               AND WNK-TITLE-NO NOT = 116
               AND WNK-TITLE-NO NOT = 315
               AND WNK-TITLE-NO NOT = 316
               AND WNK-TITLE-NO NOT = 415
               AND WNK-TITLE-NO NOT = 416
               AND WNK-TITLE-NO NOT = 049
               AND WNK-TITLE-NO NOT = 149
               AND WNK-TITLE-NO NOT = 249
               AND WNK-TITLE-NO NOT = 349
               AND WNK-TITLE-NO NOT = 438
               AND WNK-TITLE-NO NOT = 439
               AND WNK-TITLE-NO NOT = 449
               AND WNK-TITLE-NO NOT = 515
               AND WNK-TITLE-NO NOT = 549
               GO TO READ-WNK.
           IF WNK-TITLE-NO LESS 200 AND WNK-ISS-DRAW-MON (3) NOT
           GREATER 0
              AND WNK-ISS-RET-MON (3) NOT
              GREATER 0 GO TO READ-WNK.
           IF WNK-ISS-DRAW-MON (3) = 0 AND
              WNK-ISS-DRAW-TUE (3) = 0 AND
              WNK-ISS-DRAW-WED (3) = 0 AND
              WNK-ISS-DRAW-THU (3) = 0 AND
              WNK-ISS-DRAW-FRI (3) = 0 AND
              WNK-ISS-DRAW-SAT (3) = 0 AND
              WNK-ISS-RET-MON (3) = 0 AND
              WNK-ISS-RET-TUE (3) = 0 AND
              WNK-ISS-RET-WED (3) = 0 AND
              WNK-ISS-RET-THU (3) = 0 AND
              WNK-ISS-RET-FRI (3) = 0 AND
              WNK-ISS-RET-SAT (3) = 0
              GO TO READ-WNK.
           INITIALIZE LINEPR.
           MOVE WNA-NAME TO LP-STORE-NAME
           MOVE WNR-ISS-MM (3) TO LP-MM.
           MOVE WNR-ISS-DD (3) TO LP-DD.
           MOVE "/" TO LP-SLASH.
           MOVE WNA-DEALER-NO TO LP-DEALER-NO.
           MOVE WNA-ADDR-LINE-2 TO  LP-STORE-ADDR-2
           MOVE WNA-ADDR-LINE-3 TO  LP-STORE-ADDR-3
           MOVE WNA-ZIP-HI-5 TO LP-STORE-ZIP-HI-5
           MOVE "PRUSSIA  " TO LP-MARKET.
           IF WNA-SEQ-MISC-D = 2 MOVE "EDISON   " TO LP-MARKET.
           IF WNA-SEQ-MISC-D = 3 MOVE "WALL     " TO LP-MARKET.
           IF WNA-SEQ-MISC-D = 4 MOVE "AREA4    " TO LP-MARKET.
           INSPECT LINEPR REPLACING ALL "'" BY " ".
           INSPECT LINEPR REPLACING ALL "," BY " ".
           MOVE WNK-TITLE-NO TO LP-TITLE-NO
             WNX-TITLE-NO.
           READ WNXFILE INVALID KEY MOVE "INVALID TITLE"
                   TO WNX-TITLE.
           MOVE WNX-TITLE TO LP-TITLE.
           IF WNK-TITLE-NO GREATER 199
              MOVE WNK-ISS-DRAW-MON (3) TO LP-MON-DRAW
              MOVE WNK-ISS-DRAW-TUE (3) TO LP-TUE-DRAW
              MOVE WNK-ISS-DRAW-WED (3) TO LP-WED-DRAW
              MOVE WNK-ISS-DRAW-THU (3) TO LP-THU-DRAW
              MOVE WNK-ISS-DRAW-FRI (3) TO LP-FRI-DRAW
              MOVE WNK-ISS-DRAW-SAT (3) TO LP-SAT-DRAW
              MOVE WNK-ISS-RET-MON (3) TO LP-MON-RETURNS
              MOVE WNK-ISS-RET-TUE (3) TO LP-TUE-RETURNS
              MOVE WNK-ISS-RET-WED (3) TO LP-WED-RETURNS
              MOVE WNK-ISS-RET-THU (3) TO LP-THU-RETURNS
              MOVE WNK-ISS-RET-FRI (3) TO LP-FRI-RETURNS
              MOVE WNK-ISS-RET-SAT (3) TO LP-SAT-RETURNS
              SUBTRACT WNK-ISS-RET-MON (3) FROM WNK-ISS-DRAW-MON (3)
              GIVING LP-MON-NET
              SUBTRACT WNK-ISS-RET-TUE (3) FROM WNK-ISS-DRAW-TUE (3)
              GIVING LP-TUE-NET
              SUBTRACT WNK-ISS-RET-WED (3) FROM WNK-ISS-DRAW-WED (3)
              GIVING LP-WED-NET
              SUBTRACT WNK-ISS-RET-THU (3) FROM WNK-ISS-DRAW-THU (3)
              GIVING LP-THU-NET
              SUBTRACT WNK-ISS-RET-FRI (3) FROM WNK-ISS-DRAW-FRI (3)
              GIVING LP-FRI-NET
              SUBTRACT WNK-ISS-RET-SAT (3) FROM WNK-ISS-DRAW-SAT (3)
              GIVING LP-SAT-NET
           ELSE
                MOVE WNK-ISS-DRAW-MON (3) TO LP-SUN-DRAW
                MOVE WNK-ISS-RET-MON (3) TO LP-SUN-RETURNS
                SUBTRACT WNK-ISS-RET-MON (3) FROM WNK-ISS-DRAW-MON (3)
                GIVING LP-SUN-NET.
      *     MOVE WNK-ISS-COST-DLR (3) TO LP-COST.
*******     MOVE WNK-DEALER-NO TO WNO-DEALER-NO.
      *     MOVE WNK-TITLE-NO TO WNO-TITLE-NO.
      *     READ WNOFILE INVALID KEY GO TO NO-WNOFILE.
      *     MOVE WNO-SPEC-PRICE TO LP-COST.
       NO-WNOFILE.
           MOVE ALL "," TO
           FILLER-1
           FILLER-2
           FILLER-3
           FILLER-4
           FILLER-5
           FILLER-6
           FILLER-7
           FILLER-8
           FILLER-9
           FILLER-10
           FILLER-11
           FILLER-12
           FILLER-13
           FILLER-14
           FILLER-15
           FILLER-16
           FILLER-17
           FILLER-18
           FILLER-19
           FILLER-20
           FILLER-21
           FILLER-22
           FILLER-23
           FILLER-24
           FILLER-25
           FILLER-26
           FILLER-27
           FILLER-28
           FILLER-29.
           IF FIRST-WRITE = "YES"
              WRITE LINEPR AFTER ADVANCING 1
              MOVE "   " TO FIRST-WRITE
           ELSE
              WRITE LINEPR AFTER ADVANCING 1.
              GO TO READ-WNK.
       END-JOB.                                                                 
           CLOSE WNKFILE WNAFILE PRLINE WNXFILE WNRFILE WNOFILE.
           STOP RUN.

