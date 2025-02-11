       IDENTIFICATION DIVISION.
       PROGRAM-ID. STDGRAV.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
      *SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       88 WS-STUDENT-NAME     PIC X(15).
       88 WS-I                PIC 9(01).
       88 WS-RPT              PIC X(01).

       01 WS-LEARNING.
          02 WS-LEARNING      PIC X(10).
          02 WS-MEDIA           PIC 9(02)V99.
          02 WS-RESULT        PIC X(10) VALUE 0.
          02 WS-NOTE-1        PIC 9(02).
          02 WS-NOTE-2        PIC 9(02).
          02 WS-NOTE-3         PIC 9(02).
          02 WS-NOTE-4        PIC 9(02).

       PROCEDURE DIVISION.
       P-START.

            INITIALISE WS-LEARNING
                       WS-STUDENT-NAME
                       WS-I.

            DISPLAY "INSERT STUDENT FIRST NAME: "
            ACCEPT WS-STUDENT-NAME.

            DISPLAY "INSERT GRADE NAME: "
            ACCEPT WS-LEARNING OF WS-LEARNING

            PERFORM P-DEFINE-NOTES    THRU P-DEFINE-NOTES-END
                    4 TIMES.

            PERFORM P-CALCULATE         THRU P-CALCULATE-END.

            PERFORM P-REPEAT          THRU P-REPEAT-END.

       P-START-END.

       P-DEFINE-NOTES.
            ADD 1 TO WS-I.

            DISPLAY "INSERT " WS-I "st GRADE: ".

            EVALUATE TRUE
                WHEN WS-I EQUAL 1
                     ACCEPT WS-NOTE-1 OF WS-LEARNING
                 WHEN WS-I EQUAL 2
                     ACCEPT WS-NOTE-2 OF WS-LEARNING
                WHEN WS-I EQUAL 3
                   ACCEPT WS-NOTE-3 OF WS-LEARNING
                WHEN WS-I EQUAL 4
                     ACCEPT WS-NOTE-4 OF WS-LEARNING
            END-EVALUATE.


       P-DEFINE-NOTES-END.

       P-PARAGRAPH-1.

            IF WS-NOTE-1 IS <= 0
                         OR WS-NOTE-1 IS NOT NUMERIC
                         OR WS-NOTE-1 IS >= 10
                DISPLAY '***************************************'
                DISPLAY '*   GRADE INVALID, PLEASE TRY AGAIN   *'
                DISPLAY '***************************************'
                PERFORM P-START THRU P-START-END
            END-IF.

            IF WS-NOTE-2 IS <= 0
                         OR WS-NOTE-2 IS NOT NUMERIC
                         OR WS-NOTE-2 IS >= 10
                DISPLAY '***************************************'
                DISPLAY '*   GRADE INVALID, PLEASE TRY AGAIN   *'
                DISPLAY '***************************************'
                PERFORM P-START THRU P-START-END
            END-IF.

            IF WS-NOTE-3 IS <= 0
                         OR WS-NOTE-3 IS NOT NUMERIC
                         OR WS-NOTE-3 IS >= 10
                DISPLAY '***************************************'
                DISPLAY '*   GRADE INVALID, PLEASE TRY AGAIN   *'
                DISPLAY '***************************************'
                PERFORM P-START THRU P-START-END
            END-IF.

            IF WS-NOTE-4 IS <= 0
                         OR WS-NOTE-4 IS NOT NUMERIC
                         OR WS-NOTE-4 IS >= 10
                DISPLAY '***************************************'
                DISPLAY '*   GRADE INVALID, PLEASE TRY AGAIN   *'
                DISPLAY '***************************************'
                PERFORM P-START THRU P-START-END
            END-IF.

       P-PARAGRAPH-1-END.

       P-CALCULATE.

            PERFORM P-PARAGRAPH-1    THRU P-PARAGRAPH-1-END.

            COMPUTE WS-MEDIA OF WS-LEARNING =
                   (WS-NOTE-1 OF WS-LEARNING + WS-NOTE-2 OF WS-LEARNING
                   + WS-NOTE-3 OF WS-LEARNING + WS-NOTE-4 OF WS-LEARNING)
                   / 4.

            DISPLAY WS-MEDIA.

            IF WS-MEDIA GREATER THAN OR EQUAL 7 THEN
                MOVE 'APPROVED' TO WS-RESULT
            ELSE
                MOVE 'REPROVED' TO WS-RESULT
            END-IF.

            DISPLAY '*********** PROCESSING RESULT ***********'
            DISPLAY 'STUDENT NAME : ' FUNCTION TRIM(WS-STUDENT-NAME)
            DISPLAY 'GRADE NAME   : ' FUNCTION TRIM(WS-LEARNING)
            DISPLAY 'GRADE AVERAGE: ' WS-MEDIA
            DISPLAY 'RESULT       : ' FUNCTION TRIM(WS-RESULT)
            DISPLAY '*****************************************'.

       P-CALCULATE-END.

       P-REPEAT.
            DISPLAY 'REPEAT? (Y/N)'
            ACCEPT WS-RPT

            IF WS-RPT IS EQUAL 'Y' OR WS-RPT IS EQUAL 'y'
                PERFORM P-START THRU P-START-END.

            IF WS-RPT IS EQUAL 'N' OR WS-RPT IS EQUAL 'n'
                PERFORM P-STOP-PGM THRU P-STOP-PGM-END.

       P-REPEAT-END.

       P-STOP-PGM.
           STOP RUN.
       P-STOP-PGM-END.
       END PROGRAM STDGRAV.
