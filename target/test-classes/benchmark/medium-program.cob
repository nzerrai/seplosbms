       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIUM-PROGRAM.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  IN-RECORD-ID    PIC 9(6).
           05  IN-RECORD-DATA  PIC X(100).
       
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  OUT-RECORD-ID   PIC 9(6).
           05  OUT-RECORD-DATA PIC X(100).
       
       WORKING-STORAGE SECTION.
       01  WS-CONTROL-FIELDS.
           05  WS-EOF          PIC X VALUE 'N'.
           05  WS-REC-COUNT    PIC 9(7) VALUE 0.
           05  WS-ERROR-COUNT  PIC 9(7) VALUE 0.
       
       01  WS-CUSTOMER-GROUP.
           05  CUST-ID         PIC 9(6).
           05  CUST-NAME       PIC X(30).
           05  CUST-ADDRESS    PIC X(50).
           05  CUST-PHONE      PIC X(15).
           05  CUST-BALANCE    PIC 9(9)V99.
           05  CUST-STATUS     PIC X.
           05  CUST-CREATED    PIC 9(8).
       
       01  WS-CALCULATION-FIELDS.
           05  WS-TOTAL        PIC 9(11)V99 VALUE 0.
           05  WS-AVERAGE      PIC 9(9)V99 VALUE 0.
           05  WS-TAX-AMOUNT   PIC 9(9)V99 VALUE 0.
           05  WS-FINAL-TOTAL  PIC 9(11)V99 VALUE 0.
       
       01  WS-SWITCHES.
           05  WS-VALID-RECORD PIC X VALUE 'N'.
           05  WS-PROCESS-FLAG PIC X VALUE 'Y'.
       
       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM 1000-VALIDATE-RECORD
                       IF WS-VALID-RECORD = 'Y'
                           PERFORM 2000-PROCESS-RECORD
                           PERFORM 3000-WRITE-OUTPUT
                           ADD 1 TO WS-REC-COUNT
                       ELSE
                           ADD 1 TO WS-ERROR-COUNT
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.
       
       1000-VALIDATE-RECORD.
           IF IN-RECORD-DATA NOT = SPACES
               MOVE 'Y' TO WS-VALID-RECORD
           ELSE
               MOVE 'N' TO WS-VALID-RECORD
           END-IF.
       
       2000-PROCESS-RECORD.
           MOVE IN-RECORD-ID TO CUST-ID.
           MOVE FUNCTION UPPER-CASE(IN-RECORD-DATA) TO CUST-NAME.
           
           EVALUATE CUST-ID
               WHEN 1
                   MOVE 'ACTIVE' TO CUST-STATUS
               WHEN 2
                   MOVE 'INACTIVE' TO CUST-STATUS
               WHEN 3
                   MOVE 'PENDING' TO CUST-STATUS
               WHEN OTHER
                   MOVE 'UNKNOWN' TO CUST-STATUS
           END-EVALUATE.
       
       3000-WRITE-OUTPUT.
           MOVE CUST-ID TO OUT-RECORD-ID.
           MOVE CUST-NAME TO OUT-RECORD-DATA.
           WRITE OUTPUT-RECORD.
