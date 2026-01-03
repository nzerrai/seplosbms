       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILLERDEMO.
       AUTHOR. COBOL Translator.
      *****************************************************************
      * Example program demonstrating FILLER field usage
      * FILLER fields are used to reserve space without symbolic names
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTOMER.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID              PIC 9(6).
           05  FILLER               PIC X(2).
           05  CUST-NAME            PIC X(30).
           05  FILLER               PIC X(5).
           05  CUST-ADDRESS         PIC X(50).
           05  FILLER               PIC X(3).
           05  CUST-PHONE           PIC 9(10).
           05  FILLER               PIC X(4).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS           PIC XX.

       01  WS-COUNTERS.
           05  WS-RECORD-COUNT      PIC 9(5) VALUE ZERO.
           05  FILLER               PIC X(10) VALUE SPACES.
           05  WS-ERROR-COUNT       PIC 9(3) VALUE ZERO.

       01  WS-HEADER-LINE.
           05  FILLER               PIC X(10) VALUE 'CUSTOMER: '.
           05  WS-HEADER-ID         PIC 9(6).
           05  FILLER               PIC X(3) VALUE ' - '.
           05  WS-HEADER-NAME       PIC X(30).

       01  WS-FORMATTED-DATA.
           05  WS-FD-PREFIX         PIC X(5) VALUE 'DATA:'.
           05  FILLER               PIC X VALUE SPACE.
           05  WS-FD-VALUE          PIC X(20).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-FD-SUFFIX         PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT CUSTOMER-FILE

           PERFORM UNTIL WS-FILE-STATUS NOT = '00'
               READ CUSTOMER-FILE
                   AT END
                       MOVE '10' TO WS-FILE-STATUS
                   NOT AT END
                       ADD 1 TO WS-RECORD-COUNT
                       MOVE CUST-ID TO WS-HEADER-ID
                       MOVE CUST-NAME TO WS-HEADER-NAME
                       DISPLAY WS-HEADER-LINE
               END-READ
           END-PERFORM

           CLOSE CUSTOMER-FILE

           DISPLAY 'Total records processed: ' WS-RECORD-COUNT

           STOP RUN.
