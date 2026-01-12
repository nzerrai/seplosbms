       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTIMP.
       AUTHOR. TEST-TEAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO 'input.dat'.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  REC-ID          PIC 9(5).
           05  REC-DATA        PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-EOF              PIC X VALUE 'N'.
       01  WS-COUNTER          PIC 9(5) VALUE 0.
       01  WS-TOTAL            PIC 9(7) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           OPEN INPUT INPUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM
           DISPLAY 'TOTAL PROCESSED: ' WS-COUNTER
           CLOSE INPUT-FILE
           STOP RUN.

       PROCESS-RECORD.
           ADD 1 TO WS-COUNTER
           ADD REC-ID TO WS-TOTAL
           DISPLAY 'Record: ' REC-ID.
