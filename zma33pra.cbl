       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. ZMA33PRA.                                                       
       AUTHOR. JEFFREY CLENDENING                                              
      ******************************************************************
      *REMARKS.                                                        *
      * created to replace demo pay grade with real pay grade          *
      * takes pay grade from zmdm29a and add pay grade to the end of   *
      *  zmd41a1 file used in zma33 process                            *
      ******************************************************************
      * MODIFICATION HISTORY:                                          *
      *                                                                *
      *           MODIFIED: 99/99/99                                   *
      *    EFFECTIVE CYCLE: ME XXX 99  (CHG PKG 9999)                  *
      *         PROGRAMMER:                                            *
      *       MODIFICATION:                                            *
      *                                                                *
      ******************************************************************
      /                                                                         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. HP-9000.                                               
       OBJECT-COMPUTER. HP-9000.                                               
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT STR-FILE ASSIGN TO ZMA41A1I                             
                  FILE STATUS IS STR-STATUS
                  ORGANIZATION IS LINE SEQUENTIAL. 

           SELECT PAY-GRADE-FILE ASSIGN  TO ZMAM29AI                            
                  FILE STATUS IS PAY-STATUS
                  ORGANIZATION IS LINE SEQUENTIAL. 

           SELECT STR-OUTPUT-FILE ASSIGN TO ZMA41A3O                            
                  FILE STATUS IS STR-OUTPUT-STATUS
                  ORGANIZATION IS LINE SEQUENTIAL. 
      *
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  STR-FILE                                                            
           RECORDING MODE IS F                                                  
           BLOCK CONTAINS 0 RECORDS                                             
           LABEL RECORDS ARE STANDARD.                                          
       01  STR-INPUT-REC                  PIC X(300).                          
      *
       FD  PAY-GRADE-FILE                                                      
           RECORDING MODE IS F                                                  
           BLOCK CONTAINS 0 RECORDS                                             
           LABEL RECORDS ARE STANDARD.                                          
       01  PAY-GRADE-INPUT-REC           PIC X(13).
      *
       FD  STR-OUTPUT-FILE
           RECORDING MODE IS F                                                  
           BLOCK CONTAINS 0 RECORDS                                             
           LABEL RECORDS ARE STANDARD.
       01  STR-OUTPUT-RECORD               PIC X(300).                                          
      *                                                                         
       WORKING-STORAGE SECTION.                                                 

       01  SYS-DATE1                  PIC X(021).
 
       01  MODULE-NAME                    PIC X(08) VALUE
                                           "ZMA33PRC".
       01 COUNTERS.
          03 READ-CNT                 PIC S9(6) COMP-3 VALUE ZEROS. 
          03 MATCHED-CNT              PIC S9(6) COMP-3 VALUE ZEROS.
          03 WRITE-CNT                PIC S9(6) COMP-3 VALUE ZEROS.
          03 STATUS-OUT-CNT           PIC S9(6) COMP-3 VALUE ZEROS.

       01 STR-STATUS                  PIC X(02)  VALUE SPACES.
          88 STR-OK                      VALUE   '00'.
          88 STR-EOF                     VALUE   '10'.
       01 PAY-STATUS                  PIC X(02)  VALUE SPACES.
          88 PAY-OK                      VALUE   '00'.
          88 PAY-EOF                     VALUE   '10'.
       01 STR-OUTPUT-STATUS           PIC X(02)  VALUE SPACES.
          88 STR-OUT-OK                  VALUE   '00'.
          88 STR-OUT-EOF                 VALUE   '10'.

       01 STR-PAY-GRD-HOLD.
         03 STR-PAY-GRD-HOLD          PIC X(2).
         03 STR-SSN-HOLD              PIC X(9).
         03 T01-SSN-HOLD              PIC X(9).

       01 STR-EOF-IND.   
          05 EOF-STR                  PIC X(1) VALUE 'N'.
          05 EOF-PAY                  PIC X(1) VALUE 'N'.  

       01 STR-RECORD-IN.
          03 STR-FILLER-1-IN          PIC X(132) VALUE SPACES.
          03 STR-SSN-IN               PIC X(009) VALUE SPACES.
          03 STR-FILLER-2-IN          PIC X(117) VALUE SPACES.
          03 STR-FILLER-3-IN          PIC X(040) VALUE SPACES.
          03 STR-PAY-GRADE-IN         PIC X(002) VALUE SPACES.

       01 STR-RECORD-OUT.
          03 STR-FILLER-1-OUT         PIC X(132) VALUE SPACES.
          03 STR-SSN-OUT              PIC X(009) VALUE SPACES.
          03 STR-FILLER-2-OUT         PIC X(117) VALUE SPACES.
          03 STR-FILLER-3-OUT         PIC X(040) VALUE SPACES.
          03 STR-PAY-GRD              PIC X(002) VALUE SPACES.

       COPY STR0010.
       COPY PAY0010.
      
       PROCEDURE DIVISION.
       BEGIN.
      *     DISPLAY SFNAME UPON ENVIRONMENT-NAME.
      *     ACCEPT  SFNAME FROM ENVIRONMENT-VALUE.

       1000-MAIN-RTN.                                                           
           PERFORM OPEN-ROUTINE THRU OPEN-ROUTINE-EXIT.
           PERFORM READ-ROUTINE-1 THRU READ-ROUTINE-1-EXIT.  
           PERFORM READ-ROUTINE-2 THRU READ-ROUTINE-2-EXIT.  
           PERFORM MATCH-ROUTINE THRU MATCH-ROUTINE-EXIT 
                UNTIL EOF-STR = 'Y'.
           PERFORM CLOSE-ROUTINE THRU CLOSE-ROUTINE-EXIT.                                
       OPEN-ROUTINE.
           OPEN INPUT STR-FILE.
           IF NOT STR-OK
                DISPLAY 'STR-FILE OPEN FAILED:  ' STR-STATUS
                STOP RUN.
        
           OPEN INPUT PAY-GRADE-FILE.
           IF NOT PAY-OK
                DISPLAY 'PAY-GRADE OPEN FAILED: ' PAY-STATUS
                STOP RUN.

           OPEN OUTPUT STR-OUTPUT-FILE.
           IF NOT STR-OUT-OK
                DISPLAY 'STR-OUTPUT FAILED: ' STR-OUTPUT-STATUS 
                STOP RUN.

       OPEN-ROUTINE-EXIT.
                      EXIT.
       
       READ-ROUTINE-1.
           READ PAY-GRADE-FILE INTO AF-TRN01-RECORD
               AT END MOVE '10' TO PAY-STATUS
               DISPLAY 'END OF PAY-FILE: ' PAY-STATUS 
               MOVE '999999999' TO T01-SSN-HOLD
               GO TO  MATCH-ROUTINE.      
           MOVE T01-SSN TO T01-SSN-HOLD.

           IF PAY-OK OR PAY-EOF
              NEXT SENTENCE
           ELSE
             DISPLAY 'PAY READ FAILED: ' PAY-STATUS
             PERFORM CLOSE-ROUTINE
           END-IF.             
       READ-ROUTINE-1-EXIT.
                     EXIT.

       READ-ROUTINE-2.
           READ STR-FILE INTO STR-RECORD 
                  AT END MOVE '10' TO STR-STATUS
                  MOVE 'Y' TO EOF-STR
                  DISPLAY 'END OF FILE STR: ' STR-STATUS 
      *           MOVE '999999999' TO STR-SSN-HOLD
                  PERFORM CLOSE-ROUTINE.       

           IF STR-OK OR STR-EOF
              NEXT SENTENCE
           ELSE
             DISPLAY 'STR READ FAILED: ' STR-STATUS
             PERFORM CLOSE-ROUTINE
           END-IF.
           MOVE STR-RECORD TO STR-RECORD-IN.
           MOVE STR-SSN TO STR-SSN-HOLD.
       READ-ROUTINE-2-EXIT.
                       EXIT.
  

       MATCH-ROUTINE. 
             IF STR-SSN-HOLD = T01-SSN-HOLD
                MOVE T01-PAY-GRADE TO STR-PAY-GRD
                ADD 1 TO MATCHED-CNT
                PERFORM WRITE-RECORDS THRU WRITE-RECORDS-EXIT
                PERFORM READ-ROUTINE-1
                PERFORM READ-ROUTINE-2
             ELSE
               IF STR-SSN-HOLD IS GREATER 
                                THAN T01-SSN-HOLD AND NOT STR-EOF
                                PERFORM READ-ROUTINE-1
               ELSE
                 IF STR-SSN-HOLD IS LESS 
                              THAN T01-SSN-HOLD
                              MOVE STR-PAY-GRD-IN IN
                                     STR-RECORD TO STR-PAY-GRD
                              PERFORM WRITE-RECORDS
                              PERFORM READ-ROUTINE-2.
       MATCH-ROUTINE-EXIT.
                      EXIT.
      * STR-EOF PAY-EOF 
       WRITE-RECORDS.
            MOVE STR-FILLER-1-IN TO STR-FILLER-1-OUT.
            MOVE STR-SSN-IN TO STR-SSN-OUT.
            MOVE STR-FILLER-2-IN TO STR-FILLER-2-OUT. 
            MOVE STR-FILLER-3-IN TO STR-FILLER-3-OUT.
            WRITE STR-OUTPUT-RECORD FROM STR-RECORD-OUT.
            IF NOT STR-OUT-OK 
                 DISPLAY 'OUTPUT WRITE FAILED: ' STR-OUTPUT-STATUS
                 PERFORM CLOSE-ROUTINE
            ELSE
                 ADD 1 TO WRITE-CNT 
            END-IF.
       WRITE-RECORDS-EXIT.
                      EXIT.
      
       CLOSE-ROUTINE.
              DISPLAY 'RECORDS MATCHED:  ' MATCHED-CNT
              DISPLAY 'RECORDS WRITTEN:  ' WRITE-CNT
              CLOSE  STR-FILE 
                     PAY-GRADE-FILE
                     STR-OUTPUT-FILE.
              STOP RUN.
       CLOSE-ROUTINE-EXIT.
                      EXIT.                          
