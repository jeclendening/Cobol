       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEWTRENTONFILER.
       AUTHOR. JEFF BLACK
      *REMARKS AND MODIFICATIONS.
      **********************************************************
      * REMARKS:
      * CREATES TRENTON RETURNS REPORT.
      *
      **********************************************************
      *  MODIFICATION HISTORY:
      *
      *   MODIFIED: 04/04/2018
      *   PROGRAMMER: Jeffrey Clendening
      *   MODIFICATION: 
      *   JEC1. added missing Bipads, 
      *  
      *   JEC2. changed so sunday and monday 
      *   process separately.
      *
      *
      *   MODIFIED: 7/11/2018
      *   PROGRAMMER: Jeffrey Clendening
      *   MODIFICATION: added 349 and 449 new bulldogs  
      *                 for Trenton times
      *
      *   MODIFIED:
      *   PROGRAMMER:
      *   MODIFICATION:
      *
      **********************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TI-990.
       OBJECT-COMPUTER. TI-990.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY WNRSEL.
       COPY WNASEL.
       COPY WNKSEL.
           SELECT PRLINE ASSIGN PRINT WS-FILENAME
           STATUS FL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  WNRFILE LABEL RECORD STANDARD DATA RECORD WNR.
       01  WNR.
       COPY WNR.
   
       FD  WNAFILE LABEL RECORD STANDARD DATA RECORD WNA.
       01  WNA.
       COPY WNA.

       FD  WNKFILE LABEL RECORD STANDARD DATA RECORD WNK.
       01  WNK.
       COPY WNK.

       FD  PRLINE LABEL RECORD OMITTED DATA RECORD LINEPR.
       01  LINEPR.
          05  LP-MISC-SEQ     PIC 9.
          05  FILLER1         PIC X.
          05  LP-DEALER-NO    PIC 99999.
          05  FILLER2         PIC X.
          05  LP-SEQ          PIC 99999.
          05  FILLER3         PIC X.
          05  LP-NAME         PIC X(30).
          05  FILLER4        PIC X.
          05  LP-ADDR-LINE-2  PIC X(30).
          05  FILLER5         PIC X.
          05  LP-ADDR-LINE-3  PIC X(30).
          05  FILLER6         PIC X.
          05  LP-ZIP-HI-5     PIC XXXXX.
          05  FILLER7         PIC XX.
          05  LP-1 PIC X.
          05  LP-TELE-AREA-CODE PIC XXX.
          05  LP-2 PIC X.
          05  LP-TELE-NUMBER    PIC XXXXXXX.
          05  FILLER8           PIC X.
          05  LP-MON            PIC ZZZZ9.
          05  FILLER9           PIC XX.
          05  LP-TUE            PIC ZZZZ9.
          05  FILLER10          PIC XX.
          05  LP-WED            PIC ZZZZ9.
          05  FILLER11          PIC XX.
          05  LP-THU            PIC ZZZZ9.
          05  FILLER12          PIC XX.
          05  LP-FRI            PIC ZZZZ9.
          05  FILLER13          PIC XX.
          05  LP-SAT            PIC ZZZZ9.
          05  FILLER14          PIC XX.
          05  LP-SUN            PIC ZZZZ9.
          05  FILLER15          PIC X.

       WORKING-STORAGE SECTION.
       01  WS-FILENAME.
          05  FILLER PIC X(22) VALUE "/u/prb/f/trenton/send/".
      *   05  FILLER PIC X(22) VALUE "/u/prb/f/trenton/test/". 
          05  FILLER PIC X(16) VALUE "TRENTON-RETURNS-".   
      *   05  FILLER PIC X(20) VALUE "TRENTON-RETURNS-NEW-".
          05  WS-FILENAME-DATE PIC 9999.
          05  FILLER PIC XXXX VALUE ".csv".

       01  HEAD-ONE. 
          05  FILLER PIC X(42) VALUE "TTL DEALER   DEALER NAME".
          05  FILLER PIC X(06) VALUE "   MON".
          05  FILLER PIC X(24) VALUE "   TUE   WED   THU   FRI".
          05  FILLER PIC X(06) VALUE "   SAT".
          05  FILLER PIC X(06) VALUE "   SUN". 

       01  WRK.
          05  WS-FIELD     PIC 99     VALUE 0.
          05  FL-STATUS    PIC XX     VALUE SPACE.
          05  REPLYX       PIC X      VALUE SPACE.
          05  FILE-ID      PIC XXX    VALUE SPACE.
          05  EDIT-X       PIC ZZZ.
          05  PAPER-NO     PIC 9(3)  VALUE 0.
          05  TOT-MON      PIC 9(5)  VALUE 0.
          05  TOT-TUE      PIC 9(5)  VALUE 0.
          05  TOT-WED      PIC 9(5)  VALUE 0.
          05  TOT-THU      PIC 9(5)  VALUE 0.
          05  TOT-FRI      PIC 9(5)  VALUE 0.
          05  TOT-SAT      PIC 9(5)  VALUE 0.
          05  TOT-SUN      PIC 9(5)  VALUE 0.
          05  TOT-DLR      PIC 9(5)  VALUE 0.
          05  XX-DEALER-NO PIC 9(5)  VALUE 0.
          05  WS-DEALER-NO PIC 9(5)  VALUE 0.
          05  WS-TITLE-NO  PIC 999   VALUE 0.
          05  TOTAL-MON    PIC 9(5)  VALUE 0.
          05  TOTAL-TUE    PIC 9(5)  VALUE 0.
          05  TOTAL-WED    PIC 9(5)  VALUE 0.
          05  TOTAL-THU    PIC 9(5)  VALUE 0.
          05  TOTAL-FRI    PIC 9(5)  VALUE 0.
          05  TOTAL-SAT    PIC 9(5)  VALUE 0. 
          05  TOTAL-SUN    PIC 9(5)  VALUE 0.   
          05  TOTAL-DLR    PIC 9(5)  VALUE 0.
          05  WS-SEQ       PIC 9(5).
          05  WS-SEQ-R1 REDEFINES WS-SEQ.
             10  SEQ-ROUTE   PIC XX.
             10  FILLER      PIC XXX.
          05  WS-ROUTE        PIC XX VALUE LOW-VALUE.
       PROCEDURE DIVISION.
       BEGIN-BEGIN.
           OPEN INPUT WNRFILE.
           OPEN INPUT WNAFILE WNKFILE.
           INITIALIZE WNR.
           MOVE 1000 TO WNR-DEALER-NO.
           START WNRFILE KEY GREATER WNR-PRIMARY-KEY.
       READ-WNR.
           READ WNRFILE NEXT AT END GO TO END-READ-WNR.
       END-READ-WNR.
           MOVE WNR-ISS-DATE (3) TO WS-FILENAME-DATE.
           OPEN OUTPUT PRLINE.
       ACCEPT-TITLE-NO.
           MOVE 0 TO TOT-MON TOT-TUE TOT-WED TOT-THU TOT-FRI TOT-SAT
           TOT-SUN TOTAL-MON TOTAL-TUE TOTAL-WED TOTAL-THU TOTAL-FRI
           TOTAL-SAT TOTAL-SUN TOT-DLR TOTAL-DLR.
           MOVE HEAD-ONE TO LINEPR.
           MOVE SPACE TO LINEPR.
           INITIALIZE WNK.
       START-WNK.
           START WNKFILE KEY GREATER WNK-ALTERNATE-KEY-ONE
           INVALID KEY GO TO END-JOB.
           MOVE 0 TO XX-DEALER-NO.

       READ-WNK.
           READ WNKFILE NEXT AT END
           PERFORM TEST-TEST THRU END-TEST-TEST
           GO TO END-JOB.
           IF XX-DEALER-NO = 0 MOVE WNK-DEALER-NO-ONE
              TO XX-DEALER-NO.
           IF XX-DEALER-NO NOT = WNK-DEALER-NO-ONE
              PERFORM TEST-TEST THRU END-TEST-TEST
              MOVE WNK-DEALER-NO-ONE TO XX-DEALER-NO.
*******    JEC1.
           IF WNK-TITLE-NO-ONE = 049 OR
              WNK-TITLE-NO-ONE = 149 OR
              WNK-TITLE-NO-ONE = 249 OR
              WNK-TITLE-NO-ONE = 349 OR
              WNK-TITLE-NO-ONE = 449 OR
              WNK-TITLE-NO-ONE = 549
              PERFORM USE-IT THRU END-USE-IT.
              GO TO READ-WNK.
*******   JEC2. 
 
       USE-IT.
           MOVE WNK-DEALER-NO-ONE TO WS-DEALER-NO.
           IF WNK-TITLE-NO = 049 OR WNK-TITLE-NO = 149
              ADD WNK-ISS-RET-MON (3) TO TOT-SUN
           ELSE
             IF WNK-TITLE-NO = 349 OR WNK-TITLE-NO = 449
                ADD WNK-ISS-RET-SAT (3) TO TOT-SAT
             ELSE
               ADD WNK-ISS-RET-MON (3) TO TOT-MON
               ADD WNK-ISS-RET-TUE (3) TO TOT-TUE
               ADD WNK-ISS-RET-WED (3) TO TOT-WED
               ADD WNK-ISS-RET-THU (3) TO TOT-THU
               ADD WNK-ISS-RET-FRI (3) TO TOT-FRI
             END-IF
           END-IF.
       END-USE-IT. EXIT.
           
       TEST-TEST.
           IF TOT-MON = 0 AND
              TOT-TUE = 0 AND
              TOT-WED = 0 AND
              TOT-THU = 0 AND
              TOT-FRI = 0 AND
              TOT-SAT = 0 AND
              TOT-SUN = 0 GO TO END-TEST-TEST.
           MOVE SPACE TO LINEPR.
           MOVE WS-DEALER-NO TO WNA-DEALER-NO.
           READ WNAFILE INVALID KEY INITIALIZE WNA.
           MOVE 0 TO LP-MON LP-TUE LP-WED LP-THU LP-FRI
                LP-SAT LP-SUN.
           MOVE WNA-NAME TO LP-NAME.
           MOVE WNA-SEQ-NSP-DAY TO LP-SEQ.
           MOVE WNA-DEALER-NO  TO LP-DEALER-NO.
           MOVE WNA-ADDR-LINE-2 TO LP-ADDR-LINE-2.
           MOVE 0 TO WS-FIELD.
           MOVE WNA-ADDR-LINE-3 TO LP-ADDR-LINE-3.
           MOVE WNA-ZIP-HI-5    TO LP-ZIP-HI-5.
           MOVE WNA-TELE-AREA-CODE    TO LP-TELE-AREA-CODE.
           MOVE WNA-TELE-NUMBER       TO LP-TELE-NUMBER.
           MOVE "(" TO LP-1.
           MOVE ")" TO LP-2.
HERE       MOVE TOT-MON TO LP-MON
HERE       MOVE TOT-TUE TO LP-TUE.
HERE       MOVE TOT-WED TO LP-WED.
HERE       MOVE TOT-THU TO LP-THU.
HERE       MOVE TOT-FRI TO LP-FRI.
HERE       MOVE TOT-SAT TO LP-SAT.
HERE       MOVE TOT-SUN TO LP-SUN.
           IF WNA-SEQ-MISC-D = 1 MOVE 1 TO LP-MISC-SEQ.
           IF WNA-SEQ-MISC-D = 2 MOVE 2 TO LP-MISC-SEQ.
           IF WNA-SEQ-MISC-D = 3 MOVE 3 TO LP-MISC-SEQ.
           IF WNA-SEQ-MISC-D = 4 MOVE 4 TO LP-MISC-SEQ.
           INSPECT LINEPR REPLACING ALL "," BY " ".
           INSPECT LINEPR REPLACING ALL "." BY " ".
           INSPECT LINEPR REPLACING ALL "'" BY " ".
           MOVE "," TO FILLER1.
           MOVE "," TO FILLER2.
           MOVE "," TO FILLER3.
           MOVE "," TO FILLER4.
           MOVE "," TO FILLER5.
           MOVE "," TO FILLER6.
           MOVE "," TO FILLER7.
           MOVE "," TO FILLER8.
           MOVE "," TO FILLER9.
           MOVE "," TO FILLER10.
           MOVE "," TO FILLER11.
           MOVE "," TO FILLER12.
           MOVE "," TO FILLER13.
           MOVE "," TO FILLER14.
           MOVE "," TO FILLER15.
           WRITE LINEPR AFTER ADVANCING 1. 
*******    DISPLAY LINEPR.
           MOVE SPACE TO LINEPR. 
           MOVE 0 TO TOT-MON TOT-TUE TOT-WED TOT-THU TOT-FRI.
           MOVE 0 TO TOT-SAT TOT-SUN.
       END-TEST-TEST. EXIT.
       END-JOB.
           CLOSE WNKFILE WNAFILE PRLINE WNRFILE.
           STOP RUN.
