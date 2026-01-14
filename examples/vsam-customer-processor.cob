       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAM-CUSTOMER-PROCESSOR.
       AUTHOR. COBOL TO JAVA TRANSLATOR TEAM.
      *****************************************************************
      * VSAM Customer File Processing with Indexed Access
      * Demonstrates KSDS (Key-Sequenced Data Set) with primary
      * and alternate keys
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO CUSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               ALTERNATE RECORD KEY IS CUST-EMAIL
                   WITH DUPLICATES
               ALTERNATE RECORD KEY IS CUST-PHONE
               FILE STATUS IS CUST-FILE-STATUS.
           
           SELECT ERROR-LOG-FILE
               ASSIGN TO ERRLOG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS ERR-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 250 CHARACTERS.
       COPY CUSTOMER-RECORD.
       
       FD  ERROR-LOG-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS.
       01  ERROR-LOG-RECORD           PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  CUST-FILE-STATUS       PIC XX.
               88  CUST-FILE-OK       VALUE '00'.
               88  CUST-EOF           VALUE '10'.
               88  CUST-NOT-FOUND     VALUE '23'.
               88  CUST-DUPLICATE     VALUE '22'.
           05  ERR-FILE-STATUS        PIC XX.
       
       COPY ERROR-CODES.
       
       01  WS-COUNTERS.
           05  WS-READ-COUNT          PIC 9(7) VALUE ZERO.
           05  WS-WRITE-COUNT         PIC 9(7) VALUE ZERO.
           05  WS-UPDATE-COUNT        PIC 9(7) VALUE ZERO.
           05  WS-DELETE-COUNT        PIC 9(7) VALUE ZERO.
           05  WS-ERROR-COUNT         PIC 9(7) VALUE ZERO.
       
       01  WS-SEARCH-KEY              PIC 9(10).
       01  WS-SEARCH-EMAIL            PIC X(50).
       01  WS-CURRENT-DATE            PIC X(10).
       01  WS-OPERATION               PIC X(1).
           88  OP-READ                VALUE 'R'.
           88  OP-WRITE               VALUE 'W'.
           88  OP-UPDATE              VALUE 'U'.
           88  OP-DELETE              VALUE 'D'.
       
       01  WS-ERROR-MESSAGE           PIC X(80).
       
      * Level 66 RENAMES example
       01  CUSTOMER-NAME-PARTS.
           COPY CUSTOMER-RECORD REPLACING
               ==CUSTOMER-RECORD== BY ==TEMP-CUST-REC==.
       66  FULL-NAME RENAMES CUST-FIRST-NAME THRU CUST-LAST-NAME.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INIT-PROCESS
           PERFORM PROCESS-CUSTOMERS
           PERFORM TERMINATE-PROCESS
           STOP RUN.
       
       INIT-PROCESS.
           OPEN I-O CUSTOMER-FILE
           OPEN OUTPUT ERROR-LOG-FILE
           
           IF NOT CUST-FILE-OK
               DISPLAY 'ERROR OPENING CUSTOMER FILE: ' CUST-FILE-STATUS
               MOVE ERR-DATABASE TO WS-ERROR-MESSAGE
               PERFORM WRITE-ERROR-LOG
               STOP RUN
           END-IF
           
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           DISPLAY 'CUSTOMER PROCESSING STARTED: ' WS-CURRENT-DATE.
       
       PROCESS-CUSTOMERS.
           PERFORM PROCESS-BY-ID
           PERFORM PROCESS-BY-EMAIL
           PERFORM UPDATE-BALANCE
           PERFORM DISPLAY-STATISTICS.
       
       PROCESS-BY-ID.
           DISPLAY 'PROCESSING CUSTOMERS BY ID...'
           
           MOVE 1000000001 TO WS-SEARCH-KEY
           MOVE WS-SEARCH-KEY TO CUST-ID
           
           READ CUSTOMER-FILE KEY IS CUST-ID
               INVALID KEY
                   DISPLAY 'CUSTOMER NOT FOUND: ' WS-SEARCH-KEY
                   MOVE ERR-NOT-FOUND TO WS-ERROR-MESSAGE
                   PERFORM WRITE-ERROR-LOG
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   DISPLAY 'FOUND CUSTOMER: ' CUST-ID ' - ' CUST-NAME
                   ADD 1 TO WS-READ-COUNT
                   PERFORM VALIDATE-CUSTOMER
           END-READ.
       
       PROCESS-BY-EMAIL.
           DISPLAY 'PROCESSING CUSTOMERS BY EMAIL (ALTERNATE KEY)...'
           
           MOVE 'customer@example.com' TO WS-SEARCH-EMAIL
           MOVE WS-SEARCH-EMAIL TO CUST-EMAIL
           
           READ CUSTOMER-FILE KEY IS CUST-EMAIL
               INVALID KEY
                   DISPLAY 'CUSTOMER EMAIL NOT FOUND: ' WS-SEARCH-EMAIL
                   MOVE ERR-NOT-FOUND TO WS-ERROR-MESSAGE
                   PERFORM WRITE-ERROR-LOG
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   DISPLAY 'FOUND BY EMAIL: ' CUST-EMAIL
                   ADD 1 TO WS-READ-COUNT
           END-READ.
       
       VALIDATE-CUSTOMER.
           EVALUATE CUST-STATUS
               WHEN 'A'
                   DISPLAY '  STATUS: ACTIVE'
               WHEN 'I'
                   DISPLAY '  STATUS: INACTIVE'
               WHEN 'S'
                   DISPLAY '  STATUS: SUSPENDED'
               WHEN OTHER
                   DISPLAY '  STATUS: UNKNOWN'
                   MOVE ERR-INVALID-DATA TO WS-ERROR-MESSAGE
                   PERFORM WRITE-ERROR-LOG
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           IF CUST-BALANCE > CUST-CREDIT-LIMIT
               DISPLAY '  WARNING: BALANCE EXCEEDS CREDIT LIMIT'
               MOVE 'Balance over limit' TO WS-ERROR-MESSAGE
               PERFORM WRITE-ERROR-LOG
           END-IF.
       
       UPDATE-BALANCE.
           DISPLAY 'UPDATING CUSTOMER BALANCE...'
           
           MOVE 1000000001 TO CUST-ID
           
           READ CUSTOMER-FILE KEY IS CUST-ID
               INVALID KEY
                   DISPLAY 'CUSTOMER NOT FOUND FOR UPDATE'
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   ADD 100.00 TO CUST-BALANCE
                   MOVE WS-CURRENT-DATE TO CUST-LAST-UPDATE
                   REWRITE CUSTOMER-RECORD
                       INVALID KEY
                           DISPLAY 'ERROR REWRITING CUSTOMER'
                           MOVE ERR-DATABASE TO WS-ERROR-MESSAGE
                           PERFORM WRITE-ERROR-LOG
                           ADD 1 TO WS-ERROR-COUNT
                       NOT INVALID KEY
                           DISPLAY 'BALANCE UPDATED: ' CUST-BALANCE
                           ADD 1 TO WS-UPDATE-COUNT
                   END-REWRITE
           END-READ.
       
       WRITE-ERROR-LOG.
           MOVE WS-ERROR-MESSAGE TO ERROR-LOG-RECORD
           WRITE ERROR-LOG-RECORD
           IF ERR-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING TO ERROR LOG'
           END-IF.
       
       DISPLAY-STATISTICS.
           DISPLAY ' '
           DISPLAY '===== PROCESSING STATISTICS ====='
           DISPLAY 'RECORDS READ    : ' WS-READ-COUNT
           DISPLAY 'RECORDS WRITTEN : ' WS-WRITE-COUNT
           DISPLAY 'RECORDS UPDATED : ' WS-UPDATE-COUNT
           DISPLAY 'RECORDS DELETED : ' WS-DELETE-COUNT
           DISPLAY 'ERRORS          : ' WS-ERROR-COUNT
           DISPLAY '================================'.
       
       TERMINATE-PROCESS.
           CLOSE CUSTOMER-FILE
           CLOSE ERROR-LOG-FILE
           DISPLAY 'CUSTOMER PROCESSING COMPLETED.'.
