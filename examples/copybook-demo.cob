       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPYBOOK-DEMO.
       AUTHOR. COBOL TO JAVA TRANSLATOR TEAM.
      *****************************************************************
      * Demonstrates COPY statement with REPLACING and nested copies
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO INFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS INPUT-FILE-STATUS.
           
           SELECT OUTPUT-FILE
               ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS OUTPUT-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 250 CHARACTERS.
       COPY CUSTOMER-RECORD.
       
       FD  OUTPUT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS.
       COPY TRANSACTION-RECORD.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  INPUT-FILE-STATUS      PIC XX.
               88  INPUT-EOF          VALUE '10'.
           05  OUTPUT-FILE-STATUS     PIC XX.
       
       COPY ERROR-CODES.
       
      * COPY with REPLACING example
       COPY CUSTOMER-RECORD REPLACING
           ==CUSTOMER-RECORD== BY ==WS-CUSTOMER==
           ==CUST-== BY ==WS-CUST-==.
       
       01  WS-COUNTERS.
           05  WS-READ-COUNT          PIC 9(7) VALUE ZERO.
           05  WS-WRITE-COUNT         PIC 9(7) VALUE ZERO.
           05  WS-ERROR-COUNT         PIC 9(7) VALUE ZERO.
       
       01  WS-CURRENT-DATE            PIC X(10).
       01  WS-TOTAL-AMOUNT            PIC S9(11)V99 COMP-3 VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INIT-PROCESS
           PERFORM PROCESS-RECORDS
           PERFORM TERMINATE-PROCESS
           STOP RUN.
       
       INIT-PROCESS.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           
           IF INPUT-FILE-STATUS NOT = '00' OR OUTPUT-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING FILES'
               STOP RUN
           END-IF
           
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           DISPLAY 'PROCESSING STARTED: ' WS-CURRENT-DATE.
       
       PROCESS-RECORDS.
           PERFORM READ-CUSTOMER
           PERFORM UNTIL INPUT-EOF
               PERFORM PROCESS-CUSTOMER
               PERFORM READ-CUSTOMER
           END-PERFORM.
       
       READ-CUSTOMER.
           READ INPUT-FILE
               AT END
                   SET INPUT-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-COUNT
           END-READ.
       
       PROCESS-CUSTOMER.
           DISPLAY 'PROCESSING CUSTOMER: ' CUST-ID
           
           IF CUST-STATUS = 'A'
               PERFORM CREATE-TRANSACTION
               ADD CUST-BALANCE TO WS-TOTAL-AMOUNT
           ELSE
               DISPLAY '  CUSTOMER NOT ACTIVE: ' CUST-STATUS
               ADD 1 TO WS-ERROR-COUNT
           END-IF.
       
       CREATE-TRANSACTION.
           INITIALIZE TRANSACTION-RECORD
           
           MOVE CUST-ID TO TXN-ACCOUNT-ID
           MOVE WS-CURRENT-DATE TO TXN-DATE
           MOVE 'PR' TO TXN-TYPE
           MOVE CUST-BALANCE TO TXN-AMOUNT
           MOVE 'Balance Transfer' TO TXN-DESCRIPTION
           MOVE 'P' TO TXN-STATUS
           
           WRITE TRANSACTION-RECORD
           IF OUTPUT-FILE-STATUS = '00'
               ADD 1 TO WS-WRITE-COUNT
           ELSE
               DISPLAY 'ERROR WRITING TRANSACTION'
               ADD 1 TO WS-ERROR-COUNT
           END-IF.
       
       TERMINATE-PROCESS.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           
           DISPLAY ' '
           DISPLAY '===== PROCESSING SUMMARY ====='
           DISPLAY 'CUSTOMERS READ  : ' WS-READ-COUNT
           DISPLAY 'TRANSACTIONS    : ' WS-WRITE-COUNT
           DISPLAY 'ERRORS          : ' WS-ERROR-COUNT
           DISPLAY 'TOTAL AMOUNT    : ' WS-TOTAL-AMOUNT
           DISPLAY '============================'.
