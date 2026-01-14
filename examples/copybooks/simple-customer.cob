       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROC.
       AUTHOR. MIGRATION-TEAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO 'customers.dat'.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID         PIC 9(6).
           05  CUST-NAME       PIC X(30).
           05  CUST-AMOUNT     PIC 9(7)V99 COMP-3.
           05  CUST-DATE       PIC 9(8).

       WORKING-STORAGE SECTION.
       01  WS-EOF              PIC X VALUE 'N'.
       01  WS-COUNT            PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT CUSTOMER-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CUSTOMER-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM 1000-PROCESS-RECORD
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE
           DISPLAY 'PROCESSED: ' WS-COUNT
           STOP RUN.

       1000-PROCESS-RECORD.
           ADD 1 TO WS-COUNT
           IF CUST-AMOUNT > 1000
               DISPLAY 'HIGH VALUE: ' CUST-NAME
           END-IF.
