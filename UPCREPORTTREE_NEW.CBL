       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPCREPORTTREE-NEW.
      *******************************************
	  * REMARKS:
	  * REPORT RETURNS FOR PAPER BY WEEK. 
	  *
	  *
	  *
	  ********************************************
      *  MODIFICATION HISTORY:
      *
      *   MODIFIED: 3/21/2018
      *   PROGRAMMER: Jeffrey Clendening
      *   MODIFICATION: 
	  *   JEC-1: added missing Bipads. 
	  *   JEC-2: Added SAT-439 so that saturday bipads
	  *          get counted. 
	  *   JEC-3  Made changes so that the  
	  *          programs would process FAMILY-Dollar  
	  *  ********************************************                          
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. TI-990.                                                 
       OBJECT-COMPUTER. TI-990.                                                 
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                          
	    SELECT WRKFILE ASSIGN RANDOM "/home/prb/f/WRKFL.KEY"
	    ORGANIZATION INDEXED
	    ACCESS DYNAMIC RECORD WRK-PRIMARY-KEY OF WRK
	    ALTERNATE RECORD WRK-ALTERNATE-KEY-ONE OF WRK DUPLICATES
	    STATUS FL-STATUS.
        COPY WNASEL. 
	    COPY WNRSEL.
        COPY WNKSEL.               
	    COPY WNKTREESEL.
	    COPY TREESEL_NEW.
	    COPY UPCSEL.
	    SELECT PRLINE ASSIGN PRINT "/home/prb/w/UPCREPORTTREE.PRT"
               STATUS FL-STATUS.                                                
           COPY WNXSEL.                                                         
	    SELECT DATESORTFILE ASSIGN RANDOM "/home/prb/f/DATESORT.KEY"
	       ORGANIZATION INDEXED
	       ACCESS DYNAMIC RECORD DATESORT-PRIMARY-KEY OF DATESORT
	       STATUS FL-STATUS.
       DATA DIVISION.                                                           
       FILE SECTION. 
	   
       FD  WNAFILE LABEL RECORD STANDARD DATA RECORD WNA.                       
        01  WNA.                                                                 
           COPY WNA.  
		   
       FD  WNRFILE LABEL RECORD STANDARD DATA RECORD WNR.
        01  WNR.
	    COPY WNR.
	   
       FD  WNKFILE LABEL RECORD STANDARD DATA RECORD WNK.
        01  WNK.
	    COPY WNK.
	   
       FD  WRKFILE LABEL RECORD STANDARD DATA RECORD WRK.
        01  WRK.
	      05  WRK-PRIMARY-KEY.
	          10  WRK-STORE-NO  PIC 99999.
	      05  WRK-ALTERNATE-KEY-ONE.
	          10  WRK-DEALER-NO PIC 99999.
	      05  WRK-FSI           PIC XXXXX.
	      05  WRK-REGION        PIC 99999.
	      05  WRK-ZIP           PIC 99999.
	   
       FD  WNKTREEFILE LABEL RECORD STANDARD DATA RECORD WNKTREE.
         01  WNKTREE.
	     COPY WNKTREE.
	   
       FD  TREEFILE LABEL RECORD STANDARD DATA RECORD TREE.
        01  TREE.
	    COPY TREE.
	   
       FD  UPCFILE LABEL RECORD STANDARD DATA RECORD UPC.
        01  UPC.
	    COPY UPC.
	   
       FD  WNXFILE LABEL RECORD STANDARD DATA RECORD WNX.                       
        01  WNX.                                                                 
        COPY WNX.
		   
       FD  DATESORTFILE LABEL RECORD STANDARD DATA RECORD DATESORT.
        01  DATESORT.
	     05  DATESORT-PRIMARY-KEY.
	       10  DATESORT-YYYY PIC 9999.
	       10  DATESORT-MM-DD.
		     15  DATESORT-MM   PIC 99.
		     15  DATESORT-DD   PIC 99.
		   
       FD  PRLINE LABEL RECORD OMITTED DATA RECORD LINEPR.                      
        01  LINEPR.                                                              
4           05  LP-DEALER-NO   PIC 9(3).
5           05  FILLER         PIC X.                                            
	        05  LP-BIPAD      PIC Z99999.
	        05  FILLER         PIC X.
	        05  LP-TITLE       PIC X(30).
	        05  FILLER         PIC X(5).
	        05  LP-REASON      PIC X(25).
	   
       WORKING-STORAGE SECTION.                                                 
        01  WORK-FIELDS.                                                         
            05  FL-STATUS      PIC XX     VALUE SPACE.                           
	        05  WS-RETURNS     PIC S99999 VALUE 0.
	        05  WHICH-WEEK     PIC 99     VALUE 0.
                05  REPLYX         PIC X      VALUE SPACE.                           
            05  FILE-ID        PIC XXX    VALUE SPACE.                           
            05  RETURN-PERCENT PIC 9V99   VALUE 0.                               
            05  RTN-PCT REDEFINES RETURN-PERCENT PIC 999.                        
            05  PGCT           PIC 999    VALUE 0.                               
            05  LNCT           PIC 999    VALUE 0.                               
	        05  X1             PIC 9999   VALUE 0.
            05  X2             PIC 999    VALUE 0.                               
            05  X3             PIC 999    VALUE 0.                               
            05  X4             PIC 999    VALUE 0.                               
            05  DLR-CT         PIC 999    VALUE 0.                               
	        05  WS-DAY-OF-WEEK OCCURS 7 TIMES PIC 9999.
	   
        01  WHICH-AREA         PIC 9.
	       88  REGION1        VALUE 1.
	       88  REGION2        VALUE 2.
	       88  REGION3        VALUE 3.
	    
        01  HOLD-AREAS.                                                          
           05  HOLD-TITLE-NO       PIC 9(3).                                    
           05  HOLD-WEEKS          PIC 9.                                       
           05  SAVE-NAME           PIC X(5).
		   
        01  CLEAR-DRAWS.                                                         
               10  HOLD-DRAW OCCURS 8 TIMES PIC 9(5).                           
               10  DLR-DRAW  OCCURS 8 TIMES PIC 9(5).                           
               10  HOLD-RETN OCCURS 8 TIMES PIC 9(5).                           
               10  DLR-RETN  OCCURS 8 TIMES PIC 9(5).                           
               10  DRAW-MON  OCCURS 8 TIMES PIC 9(5).                           
               10  RETN-MON  OCCURS 8 TIMES PIC 9(5).                           
               10  DRAW-TUE  OCCURS 8 TIMES PIC 9(5).                           
               10  RETN-TUE  OCCURS 8 TIMES PIC 9(5).                           
               10  DRAW-WED  OCCURS 8 TIMES PIC 9(5).                           
               10  RETN-WED  OCCURS 8 TIMES PIC 9(5).                           
               10  DRAW-THU  OCCURS 8 TIMES PIC 9(5).                           
               10  RETN-THU  OCCURS 8 TIMES PIC 9(5).                           
               10  DRAW-FRI  OCCURS 8 TIMES PIC 9(5).                           
               10  RETN-FRI  OCCURS 8 TIMES PIC 9(5).                           
               10  DRAW-SAT  OCCURS 8 TIMES PIC 9(5).                           
               10  RETN-SAT  OCCURS 8 TIMES PIC 9(5).
			   
        01  WS-DATE.                                                             
           05  WS-YY PIC XX.                                                    
           05  WS-MM PIC XX.                                                    
           05  WS-DD PIC XX. 
		   
        01  F-KEY PIC 9999.
	   
        01  WS-UPC-TABLE.
	      05  WS-UPC-DATA OCCURS 1500 TIMES.
	        10  WS-UPC-TITLE-NO PIC 999.
	        10  WS-UPC-TITLE PIC X(24).
	        10  WS-UPC-DATA-UPC.
		      15  WS-UPC-CHECK PIC 9.
		      15  WS-UPC-MFG   PIC 9(6).
		      15  WS-UPC-BIPAD PIC 9(5).
		   
		   
       PROCEDURE DIVISION.                                                      
       BEGIN-BEGIN.                                                             
	    OPEN INPUT WNAFILE WNKFILE WNXFILE OUTPUT DATESORTFILE PRLINE.
	    OPEN INPUT WNRFILE UPCFILE TREEFILE.
	    OPEN OUTPUT WNKTREEFILE.
	    OPEN OUTPUT WRKFILE.
	    CLOSE WNKTREEFILE.
	    CLOSE WRKFILE.
	    OPEN I-O WNKTREEFILE.
	    OPEN I-O WRKFILE.
            ACCEPT WS-DATE FROM DATE. 
	    INITIALIZE WS-UPC-TABLE.
	    MOVE 0 TO X1.
       LOOP-UPC-TABLE.
	    ADD 1 TO X1.
	     IF X1 GREATER 1500 DISPLAY "UPC TABLE SIZE ERROR"
	       ACCEPT REPLYX
	       STOP RUN.
	    READ UPCFILE AT END GO TO END-LOAD-UPC-TABLE.
	    MOVE UPC-TITLE-NO  TO  WS-UPC-TITLE-NO (X1).
	    MOVE UPC-TITLE     TO  WS-UPC-TITLE    (X1).
	    MOVE UPC-CHECK     TO  WS-UPC-CHECK    (X1).
	    MOVE UPC-MFG       TO  WS-UPC-MFG      (X1).
	    MOVE UPC-BIPAD     TO  WS-UPC-BIPAD    (X1).
	    GO TO LOOP-UPC-TABLE.
        END-LOAD-UPC-TABLE.
	    ADD 1 TO X1.
	    MOVE 999999999999999 TO WS-UPC-DATA (X1).
	    MOVE 0 TO X1.
       FIRST-READ-TREE.
        WNR-LOOK-UP.
	    CLOSE DATESORTFILE TREEFILE.
	    OPEN INPUT DATESORTFILE TREEFILE.
	    INITIALIZE WNR.
*******    INITIALIZE DATESORT.
      *    START DATESORTFILE KEY GREATER DATESORT-PRIMARY-KEY.
	  
	    INITIALIZE WNR.
	    MOVE 1000 TO WNR-DEALER-NO.
	    START WNRFILE KEY GREATER WNR-PRIMARY-KEY.
	    READ WNRFILE NEXT AT END
	        DISPLAY "WNR PROBLEM"
	        ACCEPT REPLYX
	        STOP RUN.
	    MOVE 3 TO WHICH-WEEK.
	    
       END-WNR-LOOK-UP. EXIT.
       START-WNK.
	    INITIALIZE WNK.
	    MOVE "DOLLAR T" TO WNK-NAME.
	    START WNKFILE KEY GREATER WNK-ALTERNATE-KEY-THREE.
        READ-WNK.
	   
***********JEC-1: 

	    READ WNKFILE NEXT AT END GO TO START-TREE.
	    IF WNK-NAME (1:12) NOT = "DOLLAR TREE " GO TO READ-WNK.
	    IF WNK-TITLE-NO NOT = 116 AND
	       WNK-TITLE-NO NOT = 149 AND
	       WNK-TITLE-NO NOT = 316 AND
	       WNK-TITLE-NO NOT = 416 AND
           WNK-TITLE-NO NOT = 439 
	       GO TO READ-WNK.

	    INITIALIZE WNKTREE.
	    MOVE WNK-TITLE-NO      TO WNKTREE-TITLE-NO.
	    MOVE WNK-NAME (14:4)  TO WNKTREE-STORE-NO.
	    MOVE WNK-DEALER-NO-ONE TO WNKTREE-DEALER-NO-ONE.
	    MOVE WNK-TITLE-NO-ONE  TO WNKTREE-TITLE-NO-ONE.
	    MOVE WNK-ISS-DRAW-MON (WHICH-WEEK) TO WNKTREE-ISS-DRAW-MON.
	    MOVE WNK-ISS-DRAW-MON (WHICH-WEEK) TO WNKTREE-ISS-RET-MON.
	    MOVE WNK-ISS-DRAW-TUE (WHICH-WEEK) TO WNKTREE-ISS-DRAW-TUE.
	    MOVE WNK-ISS-DRAW-TUE (WHICH-WEEK) TO WNKTREE-ISS-RET-TUE.
	    MOVE WNK-ISS-DRAW-WED (WHICH-WEEK) TO WNKTREE-ISS-DRAW-WED.
	    MOVE WNK-ISS-DRAW-WED (WHICH-WEEK) TO WNKTREE-ISS-RET-WED.
	    MOVE WNK-ISS-DRAW-THU (WHICH-WEEK) TO WNKTREE-ISS-DRAW-THU.
	    MOVE WNK-ISS-DRAW-THU (WHICH-WEEK) TO WNKTREE-ISS-RET-THU.
	    MOVE WNK-ISS-DRAW-FRI (WHICH-WEEK) TO WNKTREE-ISS-DRAW-FRI.
	    MOVE WNK-ISS-DRAW-FRI (WHICH-WEEK) TO WNKTREE-ISS-RET-FRI.
	    MOVE WNK-ISS-DRAW-SAT (WHICH-WEEK) TO WNKTREE-ISS-DRAW-SAT.
	    MOVE WNK-ISS-DRAW-SAT (WHICH-WEEK) TO WNKTREE-ISS-RET-SAT.
	    MOVE WNK-ISS-COST-DLR (WHICH-WEEK) TO WNKTREE-ISS-COST-DLR.
	    MOVE 0 TO WNKTREE-ISS-COST-TREE.
	    MOVE 0 TO WNKTREE-ISS-NET-MON.
	    MOVE 0 TO WNKTREE-ISS-NET-TUE.
	    MOVE 0 TO WNKTREE-ISS-NET-WED.
	    MOVE 0 TO WNKTREE-ISS-NET-THU.
	    MOVE 0 TO WNKTREE-ISS-NET-FRI.
	    MOVE 0 TO WNKTREE-ISS-NET-SAT.
	    WRITE WNKTREE INVALID KEY
	        DISPLAY WNKTREE
	        DISPLAY "INVALID INITIAL WRITE ON WNKTREE"
	        ACCEPT REPLYX
	        STOP RUN.
	    MOVE WNK-DEALER-NO TO WNA-DEALER-NO.
	    READ WNAFILE INVALID KEY
	        MOVE 99999 TO WNA-DEALER-NO
	        MOVE "XXXXX" TO WNA-FSI
	        MOVE 99999 TO WNA-SEQ-MISC-D.
	    MOVE WNA-DEALER-NO  TO  WRK-DEALER-NO.
	    MOVE WNA-FSI        TO  WRK-FSI.
	    MOVE WNA-SEQ-MISC-D TO  WRK-REGION.
	    MOVE WNA-ZIP-HI-5   TO  WRK-ZIP.
	    MOVE WNKTREE-STORE-NO  TO WRK-STORE-NO.
	    WRITE WRK INVALID KEY REWRITE WRK.
	    GO TO READ-WNK.
       START-TREE.
	    READ TREEFILE AT END GO TO END-JOB.
        
      *     INITIALIZE WNKTREE.
	    PERFORM FIND-TITLE-NO THRU END-FIND-TITLE-NO.
	    MOVE TREE-LOCATION TO WNKTREE-STORE-NO.
	    PERFORM CROSS-CHECK-TITLE-NO THRU END-CROSS-CHECK-TITLE-NO.
        
	    IF WNKTREE-TITLE-NO < 200 GO TO CALC-SUN.
        
	    READ WNKTREEFILE INVALID KEY
	       DISPLAY WNKTREE ACCEPT REPLYX
		   GO TO CALC-SAT.
		   
		
       CALC-MON.
	    SUBTRACT TREE-MON FROM WNKTREE-ISS-RET-MON.
	    ADD TREE-MON TO WNKTREE-ISS-NET-MON.
       CALC-TUE.
	    SUBTRACT TREE-TUE FROM WNKTREE-ISS-RET-TUE.
	    ADD TREE-TUE TO WNKTREE-ISS-NET-TUE.
       CALC-WED.
	    SUBTRACT TREE-WED FROM WNKTREE-ISS-RET-WED.
	    ADD TREE-WED TO WNKTREE-ISS-NET-WED.
       CALC-THU.
	    SUBTRACT TREE-THU FROM WNKTREE-ISS-RET-THU.
	    ADD TREE-THU TO WNKTREE-ISS-NET-THU.
       CALC-FRI.
	    SUBTRACT TREE-FRI FROM WNKTREE-ISS-RET-FRI.
	    ADD TREE-FRI TO WNKTREE-ISS-NET-FRI.
	  * JEC-2 and 3
	   CALC-SAT-439.
	    SUBTRACT TREE-SAT FROM WNKTREE-ISS-RET-SAT.
	    ADD TREE-SAT TO WNKTREE-ISS-NET-SAT.
		REWRITE WNKTREE.
		 
	   CALC-SAT.
	    MOVE 316 TO WNKTREE-TITLE-NO.
	    READ WNKTREEFILE INVALID KEY GO TO START-TREE.
	    SUBTRACT TREE-SAT FROM WNKTREE-ISS-RET-SAT.
	    ADD TREE-SAT TO WNKTREE-ISS-NET-SAT.
		REWRITE WNKTREE.
	    GO TO START-TREE.
				
       CALC-SUN.
	    READ WNKTREEFILE INVALID KEY GO TO START-TREE.
	    SUBTRACT TREE-SUN FROM WNKTREE-ISS-RET-MON.
	    ADD TREE-SUN TO WNKTREE-ISS-NET-MON.
	    REWRITE WNKTREE.
	    GO TO START-TREE.
		
*******JEC-1:   
 	   FIND-TITLE-NO.
	     
	     IF  TREE-SKU = 900239
	         MOVE 116 TO WNKTREE-TITLE-NO
	         DISPLAY WNKTREE
	         GO TO END-FIND-TITLE-NO.
		 IF TREE-SKU = 934554
             MOVE 149 TO WNKTREE-TITLE-NO
      	     DISPLAY WNKTREE
     	     GO TO END-FIND-TITLE-NO.	
		 IF TREE-SKU = 022623
	         MOVE 416 TO WNKTREE-TITLE-NO
	         DISPLAY WNKTREE
	         GO TO END-FIND-TITLE-NO.	 
	     IF TREE-SKU = 273488
	         MOVE 439 TO WNKTREE-TITLE-NO
		     DISPLAY WNKTREE
		     GO TO END-FIND-TITLE-NO.
             MOVE SPACES TO LINEPR
	         MOVE "NOT FOUND " TO LP-REASON
	         MOVE TREE-SKU TO LP-BIPAD
	         MOVE TREE-LOCATION TO LP-DEALER-NO
	         WRITE LINEPR AFTER ADVANCING 1
	         GO TO START-TREE.
       END-FIND-TITLE-NO.
	   
       END-JOB.
	    CLOSE WNAFILE WNKFILE WNXFILE TREEFILE PRLINE WNRFILE      UPCFILE WNKTREEFILE DATESORTFILE WRKFILE.
	    STOP RUN.
		
       CROSS-CHECK-TITLE-NO.
	    INITIALIZE WRK.
	    MOVE TREE-LOCATION TO WRK-STORE-NO.
	    READ WRKFILE INVALID KEY
	        GO TO END-CROSS-CHECK-TITLE-NO.
	    MOVE WRK-REGION TO WHICH-AREA.

	    COPY FIXIT.
	    COPY TURKEY.



       END-CROSS-CHECK-TITLE-NO. EXIT.
