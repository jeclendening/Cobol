       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. SHORTTRENTON-NW.
       AUTHOR. Jeff Black
      **********************************************************
      * REMARKS: CREATES SHORT REPORT FOR TRENTON
      * 
      *
      *
      *
      ****************JEC2**************************************
      *  MODIFICATION HISTORY:
      *
      *   MODIFIED: 04/04/2018
      *   PROGRAMMER: Jeffrey Clendening
      *   MODIFICATION: 
      *           MOD1. added missing Bipads 149 AND 549.
      *           MOD2. ADDED MODIFICATION HISTORY TO PROGRAM  
      *  
      *   MODIFIED: 07/11/2018
      *   PROGRAMMER: Jeffrey Clendening
      *   MODIFICATION: 
      *           MOD3:added bulldog 349 and 449 
      *   
      *   
      *   
      *   
      *   MODIFIED:
      *   PROGRAMMER:
      *   MODIFICATION:
      *
      *
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  TI-990-10.
       OBJECT-COMPUTER.  TI-990-10.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT RPTFILE ASSIGN RANDOM "/home/prb/f/RPTFL1.KEY"
            ORGANIZATION INDEXED
            ACCESS DYNAMIC RECORD RPT-PRIMARY-KEY OF RPT
            ALTERNATE RECORD RPT-ALTERNATE-KEY-ONE OF RPT DUPLICATES
            ALTERNATE RECORD RPT-ALTERNATE-KEY-TWO OF RPT DUPLICATES
            STATUS FL-STATUS.

            SELECT PRLINE ASSIGN PRINT WS-FILENAME
            STATUS FL-STATUS.

            COPY WNASEL.

            COPY WOJSEL.

       DATA DIVISION.
       FILE SECTION.
       FD  RPTFILE LABEL RECORD STANDARD DATA RECORD RPT.
       01  RPT.
           05  RPT-PRIMARY-KEY.
               10  RPT-DATE PIC 9(6).
               10  RPT-TITLE-NO PIC 9(5).
               10  RPT-DEALER-NO PIC 9(5).
               10  RPT-SEQ PIC 999.
           05  RPT-ALTERNATE-KEY-ONE.
               10  RPT-DEALER-NO-R PIC 9(5).
               10  RPT-TITLE-NO-R PIC 9(5).
022896     05  RPT-ALTERNATE-KEY-TWO.
               10  RPT-RTE-SEQ PIC 9(5).
               10  RPT-DEALER-NO-R2 PIC 9(5).
               10  RPT-TITLE-NO-R2 PIC 9(5).
           05  RPT-QTY PIC 9(5).
           05  RPT-AMT PIC 9(5)V99.
           05  RPT-TITLE PIC X(21).
           05  RPT-INVNO PIC 99999.
           05  RPT-MON  PIC 99999.
           05  RPT-TUE  PIC 99999.
           05  RPT-WED  PIC 99999.
           05  RPT-THU  PIC 99999.
           05  RPT-FRI  PIC 99999.
           05  RPT-SAT  PIC 99999.
           05  RPT-WEEK PIC 9.
       FD  PRLINE LABEL RECORD OMITTED DATA RECORD LINEPR.
       01  LINEPR  PIC X(132).
       FD  WNAFILE LABEL RECORD STANDARD DATA RECORD WNA.
       01  WNA.
           COPY WNA.
       FD  WOJFILE LABEL RECORD STANDARD DATA RECORD WOJ.
       01  WOJ.
	   COPY WOJ.
       WORKING-STORAGE SECTION.
       01  FL-STATUS           PIC XX.
       01  FILE-ID             PIC XXX.
       01  REPLYX              PIC X.
       01  SAVE-DEALER-NO PIC 9(5).
       01  SAVE-TITLE-NO PIC 9(5).
       01  SUB-AMT PIC 9(5)V99 VALUE ZERO.
       01  GRAND-AMT PIC 9(5)V99 VALUE ZERO.
       01  GRAND-QTY PIC 9(5) VALUE ZERO.
       01  SUB-QTY PIC 9(5) VALUE ZERO.
       01  WS-DATE.
           10  WS-YY       PIC XX.
           10  WS-MM       PIC XX.
           10  WS-DD       PIC 99.
       01  EDIT-DATE.
           10  EDIT-MM     PIC XX.
           10  FILLER      PIC X   VALUE "/".
           10  EDIT-DD     PIC XX.
           10  FILLER      PIC X   VALUE "/".
           10  EDIT-YY     PIC XX.
       01  TIME-O-DAY.
        05  TIME-HR  PIC XX.
        05  TIME-MIN PIC XX.
        05  FILLER   PIC X(4).
       01  WS-FILENAME.
        05  FILLER PIC X(22) VALUE "/u/prb/f/trenton/send/". 
      * 05  FILLER PIC X(22) VALUE "/u/prb/f/trenton/test/".
        05  FILLER PIC X(10) VALUE "SHORTAGES-".  
      * 05  FILLER PIC X(14) VALUE "SHORTAGES-NEW-".
        05  WS-FILENAME-DATE-MM PIC 99.
        05  WS-FILENAME-DATE-DD PIC 99.
        05  FILLER PIC XXXX VALUE ".csv".
       01  HEAD-1.
            10  FILLER      PIC X(32) VALUE
                                "NEWSPAPER SHORTAGE RECAP".
            10  FILLER      PIC X(7) VALUE SPACE.
            10  FILLER      PIC X(15) VALUE "RUN DATE:   ".
            10  HDR-DATE    PIC X(8).
            10  FILLER      PIC X(9)  VALUE "   TIME:".
            10  H1-HR       PIC XX.
            10  FILLER      PIC X VALUE ".".
            10  H1-MIN      PIC XX.
            10  FILLER      PIC X(4) VALUE SPACE.
       01  HEAD-2.
        05  FILLER PIC X(8) VALUE "DATE,   ".
        05  FILLER PIC X(7) VALUE "PAP NO,".
        05  FILLER PIC X(25) VALUE "PAPER,".
        05  FILLER PIC X(7)  VALUE "QUANT".
        05  FILLER PIC X(10)  VALUE ",AMOUNT,  ".
        05  FILLER PIC X(15)  VALUE "DEALER NO, NAME".
        05  FILLER PIC X(21)  VALUE ",INV NO, REGION, SEQ,".
        05  FILLER PIC X(04)  VALUE "ZIP,".
       01  DETAIL-LINE.
06      05  DETAIL-DATE PIC 9(6).
08      05  COMMA-1 PIC XX VALUE SPACE.
14      05  DETAIL-TITLE-NO PIC ZZZZ99.
16      05  COMMA-2 PIC XX VALUE SPACE.
36      05  DETAIL-TITLE PIC X(18).
38      05  COMMA-3 PIC XX VALUE SPACE.
43      05  DETAIL-QTY PIC ZZZZ9.
45      05  COMMA-4 PIC XX VALUE SPACE.
53      05  DETAIL-AMT PIC ZZZZZ.99.
55      05  COMMA-5 PIC XX VALUE SPACE.
60      05  DETAIL-DEALER-NO PIC ZZ999.
62      05  COMMA-6 PIC XX VALUE SPACE.
73      05  DETAIL-DEALER PIC X(11).
75      05  COMMA-7 PIC XX VALUE SPACES.
80      05  DETAIL-INVNO PIC 99999.
        05  COMMA-8 PIC X.
        05  DETAIL-REGION PIC 9.
        05  COMMA-9 PIC X.
        05  DETAIL-SEQ-NSP-DAY PIC 99B999.
        05  COMMA-10 PIC X.
        05  DETAIL-ZIP PIC 99999.
 
       PROCEDURE DIVISION.
       START-PROGRAM.
           OPEN I-O RPTFILE INPUT WOJFILE WNAFILE.
       BEGIN-BEGIN.
           MOVE "WNR25" TO WOJ-PRIMARY-KEY.
           READ WOJFILE.
           MOVE WOJ-DATE-MM TO WS-FILENAME-DATE-MM.
           MOVE WOJ-DATE-DD TO WS-FILENAME-DATE-DD.
           OPEN OUTPUT PRLINE.
           MOVE SPACES TO LINEPR.
           WRITE LINEPR FROM HEAD-2 AFTER ADVANCING 1.
           ACCEPT TIME-O-DAY FROM TIME.
           MOVE TIME-HR TO H1-HR.
           MOVE TIME-MIN TO H1-MIN.
           ACCEPT WS-DATE FROM DATE.
           MOVE WS-MM TO EDIT-MM.
           MOVE WS-DD TO EDIT-DD.
           MOVE WS-YY TO EDIT-YY.
           MOVE EDIT-DATE TO HDR-DATE.
           MOVE 0 TO RPT-DEALER-NO-R RPT-TITLE-NO-R.
           START RPTFILE KEY GREATER RPT-ALTERNATE-KEY-ONE.
           READ RPTFILE NEXT AT END GO TO END-JOB.
           PERFORM HEAD-RTN.
           MOVE RPT-DEALER-NO-R TO SAVE-DEALER-NO.
           MOVE RPT-TITLE-NO-R TO SAVE-TITLE-NO.
      * MOD1 ADDED BIPADS 149 AND 549
      * MOD3 ADDED BIPADS 349 AND 449 BULLDOG EDITION ADDED TO PAPER
           IF RPT-TITLE-NO-R = 049 OR
              RPT-TITLE-NO-R = 149 OR
              RPT-TITLE-NO-R = 249 OR
              RPT-TITLE-NO-R = 349 OR
              RPT-TITLE-NO-R = 449 OR
              RPT-TITLE-NO-R = 549
              GO TO DETAIL-RTN.

       READ-RPT.
           READ RPTFILE NEXT AT END GO TO END-JOB.
           IF RPT-TITLE-NO-R NOT = 049 AND
              RPT-TITLE-NO-R NOT = 149 AND
              RPT-TITLE-NO-R NOT = 249 AND
              RPT-TITLE-NO-R NOT = 349 AND
              RPT-TITLE-NO-R NOT = 449 AND
              RPT-TITLE-NO-R NOT = 549
              GO TO READ-RPT.
           IF RPT-DEALER-NO-R NOT = SAVE-DEALER-NO
              MOVE RPT-DEALER-NO-R TO SAVE-DEALER-NO
              MOVE RPT-TITLE-NO-R TO SAVE-TITLE-NO
              PERFORM SUB-RTN
              GO TO DETAIL-RTN.
           IF RPT-TITLE-NO-R NOT = SAVE-TITLE-NO MOVE RPT-TITLE-NO-R
              TO SAVE-TITLE-NO MOVE RPT-DEALER-NO-R TO SAVE-DEALER-NO
              PERFORM SUB-RTN.


       DETAIL-RTN.
           MOVE SPACES TO LINEPR.
           MOVE RPT-DATE TO DETAIL-DATE.
           MOVE RPT-TITLE-NO-R TO DETAIL-TITLE-NO.
           MOVE RPT-TITLE TO DETAIL-TITLE.
           MOVE RPT-QTY TO DETAIL-QTY.
           ADD  RPT-QTY TO SUB-QTY.
           ADD  RPT-QTY TO GRAND-QTY.
           MOVE RPT-AMT TO DETAIL-AMT.
           ADD  RPT-AMT TO SUB-AMT.
           ADD  RPT-AMT TO GRAND-AMT.
           MOVE RPT-INVNO TO DETAIL-INVNO.
           MOVE RPT-DEALER-NO TO DETAIL-DEALER-NO
                            WNA-DEALER-NO.
           READ WNAFILE INVALID KEY MOVE "INVALID DEALER NO" TO
                DETAIL-DEALER GO TO PRT-PRT.
           MOVE WNA-NAME TO DETAIL-DEALER.
           MOVE WNA-SEQ-MISC-D TO DETAIL-REGION.
           MOVE WNA-SEQ-NSP-DAY TO DETAIL-SEQ-NSP-DAY.
           MOVE WNA-ZIP-HI-5 TO DETAIL-ZIP.

       PRT-PRT.
           MOVE "," TO COMMA-1.
           MOVE "," TO COMMA-2.
           MOVE "," TO COMMA-3.
           MOVE "," TO COMMA-4.
           MOVE "," TO COMMA-5.
           MOVE "," TO COMMA-6.
           MOVE "," TO COMMA-7.
           MOVE "," TO COMMA-8.
           MOVE "," TO COMMA-9.
           MOVE "," TO COMMA-10.
           WRITE LINEPR FROM DETAIL-LINE AFTER ADVANCING 1.
           GO TO READ-RPT.

       SUB-RTN.
           MOVE SPACES TO DETAIL-LINE.
           MOVE SUB-QTY TO DETAIL-QTY.
           MOVE SUB-AMT TO DETAIL-AMT.
           MOVE SPACES TO LINEPR.
           MOVE 0 TO SUB-QTY SUB-AMT.

       END-JOB.
           PERFORM SUB-RTN.
           MOVE SPACES TO DETAIL-LINE.
           MOVE GRAND-QTY TO DETAIL-QTY.
           MOVE GRAND-AMT TO DETAIL-AMT.
           MOVE SPACES TO LINEPR. 
           CLOSE PRLINE RPTFILE WNAFILE WOJFILE.
           STOP RUN.

       HEAD-RTN.
           MOVE SPACES TO LINEPR.
       END-HEADRTN. EXIT.
