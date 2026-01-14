       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-TRANSFORMER.
       AUTHOR. TEST-SUITE.
      *****************************************************************
      * Programme de test - Transformation de donn\u00e9es                *
      * Couvre: STRING, UNSTRING, INSPECT, SEARCH, CALL              *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO 'rawdata.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-INPUT-STATUS.

           SELECT OUTPUT-FILE
               ASSIGN TO 'transformed.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD             PIC X(100).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD            PIC X(150).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-INPUT-STATUS      PIC XX.
           05  WS-OUTPUT-STATUS     PIC XX.

       01  WS-FLAGS.
           05  WS-EOF-FLAG          PIC X(1) VALUE 'N'.
               88  WS-EOF           VALUE 'Y'.
           05  WS-FOUND-FLAG        PIC X(1) VALUE 'N'.
               88  WS-FOUND         VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-RECORDS-READ      PIC 9(7) VALUE 0.
           05  WS-RECORDS-WRITTEN   PIC 9(7) VALUE 0.
           05  WS-CHAR-COUNT        PIC 9(5) VALUE 0.
           05  WS-SPACE-COUNT       PIC 9(5) VALUE 0.

       01  WS-INPUT-FIELDS.
           05  WS-RAW-DATA          PIC X(100).
           05  WS-FIELD-1           PIC X(20).
           05  WS-FIELD-2           PIC X(20).
           05  WS-FIELD-3           PIC X(20).
           05  WS-FIELD-4           PIC X(20).
           05  WS-FIELD-5           PIC X(20).

       01  WS-OUTPUT-FIELDS.
           05  WS-TRANSFORMED-DATA  PIC X(150).
           05  WS-TEMP-STRING       PIC X(100).
           05  WS-FORMATTED-STRING  PIC X(100).

       01  WS-STRING-OPERATIONS.
           05  WS-DELIMITER         PIC X(1) VALUE '|'.
           05  WS-SEPARATOR         PIC X(3) VALUE ' - '.
           05  WS-PREFIX            PIC X(10) VALUE 'DATA:'.
           05  WS-SUFFIX            PIC X(10) VALUE ':END'.

       01  WS-SEARCH-TABLES.
           05  WS-VALID-CODES-TABLE.
               10  WS-VALID-CODE OCCURS 10 TIMES
                                     INDEXED BY WS-CODE-IDX
                                     PIC X(4).
           05  WS-CODE-DESCRIPTIONS.
               10  WS-CODE-DESC OCCURS 10 TIMES PIC X(30).

       01  WS-SEARCH-KEY            PIC X(4).
       01  WS-SEARCH-RESULT         PIC X(30).

       01  WS-INSPECT-WORK.
           05  WS-INSPECT-STRING    PIC X(100).
           05  WS-LEADING-SPACES    PIC 9(5).
           05  WS-TRAILING-SPACES   PIC 9(5).

       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-RECORDS
               UNTIL WS-EOF.
           PERFORM 3000-FINALIZE.
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.

           IF WS-INPUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING INPUT FILE: ' WS-INPUT-STATUS
               STOP RUN
           END-IF.

           IF WS-OUTPUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING OUTPUT FILE: ' WS-OUTPUT-STATUS
               STOP RUN
           END-IF.

           PERFORM 1100-LOAD-VALID-CODES.
           PERFORM 1200-READ-INPUT.

       1100-LOAD-VALID-CODES.
      *    Initialize valid codes table
           MOVE 'A001' TO WS-VALID-CODE(1).
           MOVE 'B002' TO WS-VALID-CODE(2).
           MOVE 'C003' TO WS-VALID-CODE(3).
           MOVE 'D004' TO WS-VALID-CODE(4).
           MOVE 'E005' TO WS-VALID-CODE(5).
           MOVE 'F006' TO WS-VALID-CODE(6).
           MOVE 'G007' TO WS-VALID-CODE(7).
           MOVE 'H008' TO WS-VALID-CODE(8).
           MOVE 'I009' TO WS-VALID-CODE(9).
           MOVE 'J010' TO WS-VALID-CODE(10).

      *    Initialize descriptions
           MOVE 'Account Type A' TO WS-CODE-DESC(1).
           MOVE 'Account Type B' TO WS-CODE-DESC(2).
           MOVE 'Account Type C' TO WS-CODE-DESC(3).
           MOVE 'Account Type D' TO WS-CODE-DESC(4).
           MOVE 'Account Type E' TO WS-CODE-DESC(5).
           MOVE 'Account Type F' TO WS-CODE-DESC(6).
           MOVE 'Account Type G' TO WS-CODE-DESC(7).
           MOVE 'Account Type H' TO WS-CODE-DESC(8).
           MOVE 'Account Type I' TO WS-CODE-DESC(9).
           MOVE 'Account Type J' TO WS-CODE-DESC(10).

       1200-READ-INPUT.
           READ INPUT-FILE INTO WS-RAW-DATA
               AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ.

       2000-PROCESS-RECORDS.
           IF NOT WS-EOF
               ADD 1 TO WS-RECORDS-READ
               PERFORM 2100-PARSE-INPUT-DATA
               PERFORM 2200-INSPECT-FIELDS
               PERFORM 2300-SEARCH-CODE-TABLE
               PERFORM 2400-BUILD-OUTPUT
               PERFORM 2500-WRITE-OUTPUT
               PERFORM 1200-READ-INPUT
           END-IF.

       2100-PARSE-INPUT-DATA.
      *    Parse delimited input using UNSTRING
           UNSTRING WS-RAW-DATA
               DELIMITED BY WS-DELIMITER
               INTO WS-FIELD-1
                    WS-FIELD-2
                    WS-FIELD-3
                    WS-FIELD-4
                    WS-FIELD-5
           END-UNSTRING.

      *    Trim leading and trailing spaces
           PERFORM 2110-TRIM-FIELD-1.
           PERFORM 2120-TRIM-FIELD-2.

       2110-TRIM-FIELD-1.
      *    Remove leading spaces from field 1
           INSPECT WS-FIELD-1 TALLYING WS-LEADING-SPACES
               FOR LEADING SPACES.

           IF WS-LEADING-SPACES > 0
               MOVE WS-FIELD-1(WS-LEADING-SPACES + 1:) TO WS-TEMP-STRING
               MOVE WS-TEMP-STRING TO WS-FIELD-1
           END-IF.

       2120-TRIM-FIELD-2.
      *    Replace multiple spaces with single space in field 2
           INSPECT WS-FIELD-2 REPLACING ALL '  ' BY ' '.

       2200-INSPECT-FIELDS.
      *    Count specific characters in field 1
           MOVE WS-FIELD-1 TO WS-INSPECT-STRING.
           MOVE 0 TO WS-CHAR-COUNT.

      *    Count 'A' characters
           INSPECT WS-INSPECT-STRING TALLYING WS-CHAR-COUNT
               FOR ALL 'A'.

      *    Count spaces in entire record
           MOVE WS-RAW-DATA TO WS-INSPECT-STRING.
           MOVE 0 TO WS-SPACE-COUNT.
           INSPECT WS-INSPECT-STRING TALLYING WS-SPACE-COUNT
               FOR ALL ' '.

      *    Replace specific characters
           MOVE WS-FIELD-3 TO WS-INSPECT-STRING.
           INSPECT WS-INSPECT-STRING REPLACING
               ALL 'X' BY 'Y'
               ALL '0' BY 'O'.
           MOVE WS-INSPECT-STRING TO WS-FIELD-3.

       2300-SEARCH-CODE-TABLE.
      *    Search for field 4 in valid codes table
           MOVE WS-FIELD-4 TO WS-SEARCH-KEY.
           MOVE 'NOT FOUND' TO WS-SEARCH-RESULT.
           MOVE 'N' TO WS-FOUND-FLAG.

           SET WS-CODE-IDX TO 1.
           SEARCH WS-VALID-CODE
               AT END
                   DISPLAY 'CODE NOT FOUND: ' WS-SEARCH-KEY
               WHEN WS-VALID-CODE(WS-CODE-IDX) = WS-SEARCH-KEY
                   MOVE WS-CODE-DESC(WS-CODE-IDX) TO WS-SEARCH-RESULT
                   MOVE 'Y' TO WS-FOUND-FLAG
                   DISPLAY 'CODE FOUND: ' WS-SEARCH-KEY
                       ' = ' WS-SEARCH-RESULT
           END-SEARCH.

       2400-BUILD-OUTPUT.
      *    Build output string using STRING
           MOVE SPACES TO WS-TRANSFORMED-DATA.

           STRING WS-PREFIX DELIMITED BY SIZE
                  WS-SEPARATOR DELIMITED BY SIZE
                  WS-FIELD-1 DELIMITED BY '  '
                  WS-SEPARATOR DELIMITED BY SIZE
                  WS-FIELD-2 DELIMITED BY '  '
                  WS-SEPARATOR DELIMITED BY SIZE
                  WS-FIELD-3 DELIMITED BY '  '
                  WS-SEPARATOR DELIMITED BY SIZE
                  WS-SEARCH-RESULT DELIMITED BY SIZE
                  WS-SEPARATOR DELIMITED BY SIZE
                  WS-SUFFIX DELIMITED BY SIZE
                  INTO WS-TRANSFORMED-DATA
               ON OVERFLOW
                   DISPLAY 'STRING OVERFLOW IN RECORD: ' WS-RECORDS-READ
           END-STRING.

       2500-WRITE-OUTPUT.
           MOVE WS-TRANSFORMED-DATA TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

           IF WS-OUTPUT-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING OUTPUT: ' WS-OUTPUT-STATUS
           ELSE
               ADD 1 TO WS-RECORDS-WRITTEN
           END-IF.

       3000-FINALIZE.
           PERFORM 3100-DISPLAY-STATISTICS.
           CLOSE INPUT-FILE OUTPUT-FILE.

       3100-DISPLAY-STATISTICS.
           DISPLAY '======================================'.
           DISPLAY 'DATA TRANSFORMATION STATISTICS'.
           DISPLAY '======================================'.
           DISPLAY 'Records Read:      ' WS-RECORDS-READ.
           DISPLAY 'Records Written:   ' WS-RECORDS-WRITTEN.
           DISPLAY 'Last Char Count:   ' WS-CHAR-COUNT.
           DISPLAY 'Last Space Count:  ' WS-SPACE-COUNT.
           DISPLAY '======================================'.
