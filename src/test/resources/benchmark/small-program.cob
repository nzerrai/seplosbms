       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMALL-PROGRAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-RECORD.
           05  CUST-ID         PIC 9(6).
           05  CUST-NAME       PIC X(30).
           05  CUST-BALANCE    PIC 9(9)V99.
           05  CUST-STATUS     PIC X.
       
       01  WS-COUNTERS.
           05  RECORD-COUNT    PIC 9(7) VALUE 0.
           05  ERROR-COUNT     PIC 9(7) VALUE 0.
       
       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           PERFORM UNTIL RECORD-COUNT > 100
               ADD 1 TO RECORD-COUNT
               PERFORM 1000-PROCESS-RECORD
           END-PERFORM.
           STOP RUN.
       
       1000-PROCESS-RECORD.
           IF CUST-BALANCE > 1000
               MOVE 'A' TO CUST-STATUS
           ELSE
               MOVE 'I' TO CUST-STATUS
           END-IF.
