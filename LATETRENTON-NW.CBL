       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. LATETRENTON.
       AUTHOR. Jeff Black
      ******************************************
      * REMARKS:
      *    TRENTON LATE RETURNS FILE.
      *
      *
      *
      ***************JEC2*************************
      *  MODIFICATION HISTORY:
      *
      *   MODIFIED: 04/04/2018
      *   PROGRAMMER: Jeffrey Clendening
      *   MODIFICATION: 
      *   JEC1. added missing Bipads 149 AND 549,  
      *   JEC2. ADDED MODIFICATION HISTORY TO PROGRA
      *
      *
      *
      *
      ********************************************* 
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. TI-990.                                                 
       OBJECT-COMPUTER. TI-990.                                                 
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
       COPY WOJSEL.
       COPY WNASEL.
           SELECT LATEFILE ASSIGN RANDOM "LATEFL"
           ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD LATE-PRIMARY-KEY OF LATE
           ALTERNATE RECORD LATE-ALTERNATE-KEY-ONE OF LATE DUPLICATES
           STATUS FL-STATUS.

           SELECT PRLINE ASSIGN PRINT WS-FILENAME
           STATUS FL-STATUS.

       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  WOJFILE LABEL RECORD STANDARD DATA RECORD WOJ.
       01  WOJ.
       COPY WOJ.
       FD  WNAFILE LABEL RECORD STANDARD DATA RECORD WNA.
       01  WNA.
       COPY WNA.
       FD  LATEFILE LABEL RECORD STANDARD DATA RECORD LATE.
       01  LATE.
       COPY LATE.
       FD  PRLINE LABEL RECORD OMITTED DATA RECORD LINEPR.
       01  LINEPR.
        05  LP-NAME                 PIC X(30).
        05  LP-A                    PIC X.
        05  LP-DEALER-NO-ONE        PIC 9(5).
        05  LP-1                    PIC X.
        05  LP-TITLE-NO-ONE         PIC 999.
        05  LP-2                    PIC X.
        05  LP-WEEK-ONE             PIC 9.
        05  LP-3                    PIC X.
        05  LP-REGION               PIC 99999.
        05  LP-4                    PIC X.
        05  LP-AFF-DATE.
         10  LP-AFF-MM              PIC 99.
         10  LP-5                   PIC X.
         10  LP-AFF-DD              PIC 99.
         10  LP-6                   PIC X.
        05  LP-ISS-DATE.
         10  LP-ISS-MM              PIC 99.
         10  LP-7                   PIC X.
         10  LP-ISS-DD              PIC 99.
         10  LP-8                   PIC X.
        05  LP-ISSUE.
          10  LP-ISS-COST-DLR       PIC 99.999.
          10  LP-9                  PIC X.
          10  LP-ISS-RET-MON        PIC 9(5).
          10  LP-10                 PIC X.
          10  LP-ISS-RET-TUE        PIC 9(5).
          10  LP-11                 PIC X.
          10  LP-ISS-RET-WED        PIC 9(5).
          10  LP-12                 PIC X.
          10  LP-ISS-RET-THU        PIC 9(5).
          10  LP-13                 PIC X.
          10  LP-ISS-RET-FRI        PIC 9(5).
          10  LP-14                 PIC X.
          10  LP-ISS-RET-SAT        PIC 9(5).
          10  LP-15                 PIC X.
          10  FILLER                PIC X(50).
       WORKING-STORAGE SECTION.                                                 
       01  WS-FILENAME.
        05  FILLER PIC X(17) VALUE "/u/prb/f/trenton/".
      * 05  FILLER PIC X(22) VALUE "/u/prb/f/trenton/test/".
        05  FILLER PIC X(17) VALUE "LATE-LATETRENTON-".
      * 05  FILLER PIC X(21) VALUE "LATE-LATETRENTON-NEW-".
        05  WS-FILENAME-DATE.
           10  WS-YY PIC 99.
           10  WS-MM PIC 99.
           10  WS-DD PIC 99.
        05  FILLER PIC XXXX VALUE ".csv".
       01  FL-STATUS  PIC XX    VALUE SPACE.
       01  FILE-ID    PIC XXX   VALUE SPACE.
       01  REPLYX     PIC X     VALUE SPACE.
       01  WS-DEALER-NO  PIC 99999.
       01  HEAD-ONE.
        05  PIC X(05) VALUE "NAME,".
        05  PIC X(35) VALUE "DEALER,TITLE,WEEK,REGION,AFF DATE,".
        05  PIC X(32) VALUE "ISSUE DATE,COST,SUN/MON,TUE,WED,".
        05  PIC X(12) VALUE "THU,FRI,SAT,".

       PROCEDURE DIVISION.
       OPEN-FILES.
           OPEN INPUT LATEFILE WOJFILE WNAFILE.
       READ-WOJ.
           MOVE "AFFID" TO WOJ-PRIMARY-KEY.
           READ WOJFILE.
           MOVE WOJ-DATE-MM TO WS-MM.
           MOVE WOJ-DATE-DD TO WS-DD.
           MOVE WOJ-DATE-YY TO WS-YY.
           OPEN OUTPUT PRLINE.
           MOVE 0 TO WS-DEALER-NO.
           MOVE SPACE TO LINEPR.
           MOVE HEAD-ONE TO LINEPR.
           WRITE LINEPR AFTER ADVANCING 1.
           INITIALIZE LATE.
           MOVE WS-DEALER-NO TO LATE-DEALER-NO.
           START LATEFILE KEY GREATER LATE-PRIMARY-KEY.
       READ-LATE.
           READ LATEFILE NEXT AT END GO TO END-JOB.
           IF WS-DEALER-NO NOT = LATE-DEALER-NO
           AND WS-DEALER-NO NOT = 0
           GO TO END-JOB.
*******JEC1***************************

           IF LATE-TITLE-NO-ONE NOT = 049 AND
              LATE-TITLE-NO-ONE NOT = 149 AND
              LATE-TITLE-NO-ONE NOT = 249 AND
              LATE-TITLE-NO-ONE NOT = 549
              GO TO READ-LATE.
           MOVE SPACE TO LINEPR.
           MOVE LATE-DEALER-NO-ONE TO  LP-DEALER-NO-ONE
           MOVE LATE-DEALER-NO-ONE TO  WNA-DEALER-NO.
           READ WNAFILE INVALID KEY MOVE SPACE TO WNA-NAME.
           MOVE WNA-NAME           TO   LP-NAME
           MOVE LATE-TITLE-NO-ONE  TO   LP-TITLE-NO-ONE
           MOVE LATE-WEEK-ONE      TO   LP-WEEK-ONE
           MOVE LATE-REGION        TO   LP-REGION
           MOVE LATE-AFF-MM        TO   LP-AFF-MM
           MOVE LATE-AFF-DD        TO   LP-AFF-DD
           MOVE LATE-ISS-MM        TO   LP-ISS-MM
           MOVE LATE-ISS-DD        TO   LP-ISS-DD
           MOVE LATE-ISS-COST-DLR  TO   LP-ISS-COST-DLR
           MOVE LATE-ISS-RET-MON   TO   LP-ISS-RET-MON
           MOVE LATE-ISS-RET-TUE   TO   LP-ISS-RET-TUE
           MOVE LATE-ISS-RET-WED   TO   LP-ISS-RET-WED
           MOVE LATE-ISS-RET-THU   TO   LP-ISS-RET-THU
           MOVE LATE-ISS-RET-FRI   TO   LP-ISS-RET-FRI
           MOVE LATE-ISS-RET-SAT   TO   LP-ISS-RET-SAT
           MOVE "," TO LP-A.
           MOVE "," TO LP-1.
           MOVE "," TO LP-2.
           MOVE "," TO LP-3.
           MOVE "," TO LP-4.
           MOVE "/" TO LP-5.
           MOVE "," TO LP-6.
           MOVE "/" TO LP-7.
           MOVE "," TO LP-8.
           MOVE "," TO LP-9.
           MOVE "," TO LP-10.
           MOVE "," TO LP-11.
           MOVE "," TO LP-12.
           MOVE "," TO LP-13.
           MOVE "," TO LP-14.
           MOVE "," TO LP-15.
           WRITE LINEPR AFTER ADVANCING 1.
           DISPLAY LINEPR.
           GO TO READ-LATE.
       END-JOB.
           CLOSE LATEFILE PRLINE WNAFILE.
           STOP RUN.
