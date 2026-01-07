       IDENTIFICATION DIVISION.
       PROGRAM-ID. LARGE-PROGRAM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE
               ASSIGN TO WS-MASTER-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MASTER-STATUS.
           SELECT TRANSACTION-FILE
               ASSIGN TO WS-TRANS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.
           SELECT OUTPUT-FILE
               ASSIGN TO WS-OUTPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       01  MASTER-RECORD.
           05  M-RECORD-ID     PIC 9(6).
           05  M-CUSTOMER-NAME PIC X(40).
           05  M-ADDRESS       PIC X(50).
           05  M-PHONE         PIC X(15).
           05  M-BALANCE       PIC 9(9)V99.
           05  M-STATUS        PIC X.
       
       FD  TRANSACTION-FILE.
       01  TRANS-RECORD.
           05  T-RECORD-ID     PIC 9(6).
           05  T-TYPE          PIC X(1).
           05  T-AMOUNT        PIC 9(9)V99.
           05  T-DATE          PIC 9(8).
           05  T-DESCRIPTION   PIC X(50).
       
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  O-RECORD-ID     PIC 9(6).
           05  O-NAME          PIC X(40).
           05  O-OLD-BALANCE   PIC 9(9)V99.
           05  O-TRANS-AMOUNT  PIC 9(9)V99.
           05  O-NEW-BALANCE   PIC 9(9)V99.
           05  O-STATUS        PIC X.
           05  O-TRANS-TYPE    PIC X(1).
       
       WORKING-STORAGE SECTION.
       01  WS-FILENAMES.
           05  WS-MASTER-FILENAME     PIC X(50).
           05  WS-TRANS-FILENAME      PIC X(50).
           05  WS-OUTPUT-FILENAME     PIC X(50).
       
       01  WS-FILE-STATUSES.
           05  WS-MASTER-STATUS       PIC 9(2).
           05  WS-TRANS-STATUS        PIC 9(2).
           05  WS-OUTPUT-STATUS       PIC 9(2).
       
       01  WS-CONTROL-FIELDS.
           05  WS-EOF-MASTER          PIC X VALUE 'N'.
           05  WS-EOF-TRANS           PIC X VALUE 'N'.
           05  WS-REC-COUNT           PIC 9(9) VALUE 0.
           05  WS-ERROR-COUNT         PIC 9(9) VALUE 0.
           05  WS-TRANS-COUNT         PIC 9(9) VALUE 0.
       
       01  WS-MASTER-CURRENT.
           05  M-CURR-ID              PIC 9(6).
           05  M-CURR-NAME            PIC X(40).
           05  M-CURR-ADDRESS         PIC X(50).
           05  M-CURR-BALANCE         PIC 9(9)V99.
           05  M-CURR-STATUS          PIC X.
       
       01  WS-TRANS-CURRENT.
           05  T-CURR-ID              PIC 9(6).
           05  T-CURR-TYPE            PIC X.
           05  T-CURR-AMOUNT          PIC 9(9)V99.
       
       01  WS-CALCULATION-FIELDS.
           05  WS-NEW-BALANCE         PIC 9(9)V99.
           05  WS-TOTAL-DEBIT         PIC 9(11)V99 VALUE 0.
           05  WS-TOTAL-CREDIT        PIC 9(11)V99 VALUE 0.
           05  WS-NET-CHANGE          PIC 9(11)V99 VALUE 0.
           05  WS-PERCENTAGE          PIC 9V999 VALUE 0.
       
       01  WS-LOOKUP-TABLES.
           05  WS-STATUS-TABLE.
               10  WS-STATUS-ACTIVE   PIC X VALUE 'A'.
               10  WS-STATUS-INACTIVE PIC X VALUE 'I'.
               10  WS-STATUS-PENDING  PIC X VALUE 'P'.
               10  WS-STATUS-BLOCKED  PIC X VALUE 'B'.
       
       01  WS-TRANSACTION-DETAILS.
           05  WS-TRANS-DESC          PIC X(50).
           05  WS-TRANS-DATE          PIC 9(8).
           05  WS-TRANS-POSTED        PIC X VALUE 'N'.
           05  WS-TRANS-VALIDATED     PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           PERFORM 0100-INITIALIZE.
           PERFORM 0200-READ-AND-PROCESS UNTIL WS-EOF-MASTER = 'Y'.
           PERFORM 0300-FINALIZE.
           STOP RUN.
       
       0100-INITIALIZE.
           OPEN INPUT MASTER-FILE TRANSACTION-FILE.
           OPEN OUTPUT OUTPUT-FILE.
       
       0200-READ-AND-PROCESS.
           READ MASTER-FILE
               AT END MOVE 'Y' TO WS-EOF-MASTER
               NOT AT END
                   PERFORM 1000-VALIDATE-MASTER
                   IF WS-EOF-MASTER NOT = 'Y'
                       PERFORM 2000-FIND-TRANSACTIONS
                       PERFORM 3000-APPLY-TRANSACTIONS
                   END-IF
           END-READ.
       
       0300-FINALIZE.
           CLOSE MASTER-FILE TRANSACTION-FILE OUTPUT-FILE.
           PERFORM 4000-PRINT-SUMMARY.
       
       1000-VALIDATE-MASTER.
           IF M-RECORD-ID > 0 AND M-BALANCE > 0
               MOVE 'Y' TO WS-TRANS-POSTED
           ELSE
               MOVE 'N' TO WS-TRANS-POSTED
               ADD 1 TO WS-ERROR-COUNT
           END-IF.
       
       2000-FIND-TRANSACTIONS.
           MOVE M-RECORD-ID TO M-CURR-ID.
           MOVE M-CUSTOMER-NAME TO M-CURR-NAME.
           MOVE M-ADDRESS TO M-CURR-ADDRESS.
           MOVE M-BALANCE TO M-CURR-BALANCE.
           MOVE M-STATUS TO M-CURR-STATUS.
       
       3000-APPLY-TRANSACTIONS.
           PERFORM VARYING WS-REC-COUNT FROM 1 BY 1
               UNTIL WS-REC-COUNT > 100
               READ TRANSACTION-FILE
                   AT END MOVE 'Y' TO WS-EOF-TRANS
                   NOT AT END
                       IF T-RECORD-ID = M-CURR-ID
                           PERFORM 3100-VALIDATE-TRANSACTION
                           IF WS-TRANS-VALIDATED = 'Y'
                               PERFORM 3200-UPDATE-BALANCE
                               PERFORM 3300-WRITE-OUTPUT
                               ADD 1 TO WS-TRANS-COUNT
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
       
       3100-VALIDATE-TRANSACTION.
           IF T-TYPE = 'D' OR T-TYPE = 'C'
               IF T-AMOUNT > 0
                   MOVE 'Y' TO WS-TRANS-VALIDATED
               ELSE
                   MOVE 'N' TO WS-TRANS-VALIDATED
               END-IF
           ELSE
               MOVE 'N' TO WS-TRANS-VALIDATED
           END-IF.
       
       3200-UPDATE-BALANCE.
           MOVE T-RECORD-ID TO T-CURR-ID.
           MOVE T-TYPE TO T-CURR-TYPE.
           MOVE T-AMOUNT TO T-CURR-AMOUNT.
           
           EVALUATE T-CURR-TYPE
               WHEN 'D'
                   COMPUTE WS-NEW-BALANCE = M-CURR-BALANCE - T-CURR-AMOUNT
                   ADD T-CURR-AMOUNT TO WS-TOTAL-DEBIT
               WHEN 'C'
                   COMPUTE WS-NEW-BALANCE = M-CURR-BALANCE + T-CURR-AMOUNT
                   ADD T-CURR-AMOUNT TO WS-TOTAL-CREDIT
               WHEN OTHER
                   MOVE M-CURR-BALANCE TO WS-NEW-BALANCE
           END-EVALUATE.
           
           IF WS-NEW-BALANCE >= 0
               MOVE WS-NEW-BALANCE TO M-CURR-BALANCE
               MOVE 'A' TO WS-TRANS-POSTED
           ELSE
               MOVE 'B' TO WS-TRANS-POSTED
               ADD 1 TO WS-ERROR-COUNT
           END-IF.
       
       3300-WRITE-OUTPUT.
           MOVE M-CURR-ID TO O-RECORD-ID.
           MOVE M-CURR-NAME TO O-NAME.
           MOVE M-BALANCE TO O-OLD-BALANCE.
           MOVE T-CURR-AMOUNT TO O-TRANS-AMOUNT.
           MOVE WS-NEW-BALANCE TO O-NEW-BALANCE.
           MOVE WS-TRANS-POSTED TO O-STATUS.
           MOVE T-CURR-TYPE TO O-TRANS-TYPE.
           WRITE OUTPUT-RECORD.
           ADD 1 TO WS-REC-COUNT.
       
       4000-PRINT-SUMMARY.
           DISPLAY 'TOTAL RECORDS PROCESSED: ' WS-REC-COUNT.
           DISPLAY 'TOTAL TRANSACTIONS: ' WS-TRANS-COUNT.
           DISPLAY 'TOTAL ERRORS: ' WS-ERROR-COUNT.
           DISPLAY 'TOTAL DEBITS: ' WS-TOTAL-DEBIT.
           DISPLAY 'TOTAL CREDITS: ' WS-TOTAL-CREDIT.
