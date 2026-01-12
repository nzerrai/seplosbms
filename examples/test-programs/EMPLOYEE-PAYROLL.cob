       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-PAYROLL.
       AUTHOR. TEST-SUITE.
      *****************************************************************
      * Programme de test - Calcul de paie des employ\u00e9s              *
      * Couvre: COMPUTE, MULTIPLY, DIVIDE, SUBTRACT, STRING, UNSTRING*
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO 'employees.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-EMP-STATUS.

           SELECT PAYROLL-FILE
               ASSIGN TO 'payroll.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PAY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05  EMP-ID               PIC X(6).
           05  EMP-NAME             PIC X(30).
           05  EMP-DEPARTMENT       PIC X(4).
           05  EMP-LEVEL            PIC 9(2).
           05  HOURS-WORKED         PIC 9(3)V99.
           05  HOURLY-RATE          PIC 9(4)V99.
           05  TAX-CODE             PIC X(1).
               88  TAX-STANDARD     VALUE 'S'.
               88  TAX-REDUCED      VALUE 'R'.
               88  TAX-EXEMPT       VALUE 'E'.
           05  OVERTIME-HOURS       PIC 9(3)V99.
           05  BONUS-AMOUNT         PIC 9(6)V99.

       FD  PAYROLL-FILE.
       01  PAYROLL-RECORD.
           05  PAY-EMP-ID           PIC X(6).
           05  PAY-EMP-NAME         PIC X(30).
           05  PAY-GROSS-SALARY     PIC 9(8)V99.
           05  PAY-TAX-AMOUNT       PIC 9(8)V99.
           05  PAY-NET-SALARY       PIC 9(8)V99.
           05  PAY-DEPARTMENT       PIC X(4).
           05  PAY-PERIOD           PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-EMP-STATUS        PIC XX.
           05  WS-PAY-STATUS        PIC XX.

       01  WS-FLAGS.
           05  WS-EOF-FLAG          PIC X(1) VALUE 'N'.
               88  WS-EOF           VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-EMPLOYEES-PROCESSED PIC 9(7) VALUE 0.
           05  WS-STANDARD-TAX-COUNT  PIC 9(7) VALUE 0.
           05  WS-REDUCED-TAX-COUNT   PIC 9(7) VALUE 0.
           05  WS-EXEMPT-TAX-COUNT    PIC 9(7) VALUE 0.

       01  WS-SALARY-COMPONENTS.
           05  WS-BASE-SALARY       PIC 9(8)V99.
           05  WS-OVERTIME-PAY      PIC 9(8)V99.
           05  WS-BONUS-PAY         PIC 9(8)V99.
           05  WS-GROSS-SALARY      PIC 9(8)V99.
           05  WS-TAX-AMOUNT        PIC 9(8)V99.
           05  WS-SOCIAL-SEC        PIC 9(8)V99.
           05  WS-NET-SALARY        PIC 9(8)V99.

       01  WS-TAX-RATES.
           05  WS-STANDARD-TAX-RATE PIC 99V99 VALUE 25.00.
           05  WS-REDUCED-TAX-RATE  PIC 99V99 VALUE 15.00.
           05  WS-EXEMPT-TAX-RATE   PIC 99V99 VALUE 00.00.
           05  WS-SOCIAL-SEC-RATE   PIC 99V99 VALUE 07.50.

       01  WS-OVERTIME-CONFIG.
           05  WS-REGULAR-HOURS     PIC 9(3) VALUE 160.
           05  WS-OVERTIME-MULT     PIC 9V99 VALUE 1.50.

       01  WS-TOTALS.
           05  WS-TOTAL-GROSS       PIC 9(11)V99 VALUE 0.
           05  WS-TOTAL-TAX         PIC 9(11)V99 VALUE 0.
           05  WS-TOTAL-NET         PIC 9(11)V99 VALUE 0.

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-YEAR          PIC 9(4).
               10  WS-MONTH         PIC 9(2).
               10  WS-DAY           PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-HOURS         PIC 9(2).
               10  WS-MINUTES       PIC 9(2).
               10  WS-SECONDS       PIC 9(2).

       01  WS-PAY-PERIOD-STRING     PIC X(10).

       01  WS-NAME-PARTS.
           05  WS-FIRST-NAME        PIC X(15).
           05  WS-LAST-NAME         PIC X(15).

       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-EMPLOYEES
               UNTIL WS-EOF.
           PERFORM 3000-FINALIZE.
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT EMPLOYEE-FILE
                OUTPUT PAYROLL-FILE.

           IF WS-EMP-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING EMPLOYEE FILE: ' WS-EMP-STATUS
               STOP RUN
           END-IF.

           IF WS-PAY-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING PAYROLL FILE: ' WS-PAY-STATUS
               STOP RUN
           END-IF.

           PERFORM 1100-GET-CURRENT-DATE.
           PERFORM 1200-BUILD-PAY-PERIOD.
           PERFORM 1300-READ-EMPLOYEE.

       1100-GET-CURRENT-DATE.
           ACCEPT WS-CURRENT-DATE-DATA FROM DATE YYYYMMDD.

       1200-BUILD-PAY-PERIOD.
      *    Build pay period string: YYYY-MM-DD
           STRING WS-YEAR DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-MONTH DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-DAY DELIMITED BY SIZE
                  INTO WS-PAY-PERIOD-STRING
           END-STRING.

       1300-READ-EMPLOYEE.
           READ EMPLOYEE-FILE
               AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ.

       2000-PROCESS-EMPLOYEES.
           IF NOT WS-EOF
               ADD 1 TO WS-EMPLOYEES-PROCESSED
               PERFORM 2100-CALCULATE-BASE-SALARY
               PERFORM 2200-CALCULATE-OVERTIME
               PERFORM 2300-CALCULATE-GROSS
               PERFORM 2400-CALCULATE-DEDUCTIONS
               PERFORM 2500-CALCULATE-NET
               PERFORM 2600-UPDATE-TOTALS
               PERFORM 2700-WRITE-PAYROLL
               PERFORM 1300-READ-EMPLOYEE
           END-IF.

       2100-CALCULATE-BASE-SALARY.
      *    Base salary = hours worked * hourly rate
           COMPUTE WS-BASE-SALARY = HOURS-WORKED * HOURLY-RATE
               ON SIZE ERROR
                   DISPLAY 'SIZE ERROR IN BASE SALARY FOR: ' EMP-ID
                   MOVE 0 TO WS-BASE-SALARY
           END-COMPUTE.

       2200-CALCULATE-OVERTIME.
      *    Calculate overtime pay at 1.5x rate
           IF OVERTIME-HOURS > 0
               MULTIPLY OVERTIME-HOURS BY HOURLY-RATE
                   GIVING WS-OVERTIME-PAY
               MULTIPLY WS-OVERTIME-PAY BY WS-OVERTIME-MULT
                   GIVING WS-OVERTIME-PAY
           ELSE
               MOVE 0 TO WS-OVERTIME-PAY
           END-IF.

       2300-CALCULATE-GROSS.
      *    Gross = Base + Overtime + Bonus
           MOVE BONUS-AMOUNT TO WS-BONUS-PAY.
           COMPUTE WS-GROSS-SALARY =
               WS-BASE-SALARY + WS-OVERTIME-PAY + WS-BONUS-PAY.

       2400-CALCULATE-DEDUCTIONS.
      *    Calculate tax based on tax code
           EVALUATE TRUE
               WHEN TAX-STANDARD
                   COMPUTE WS-TAX-AMOUNT =
                       WS-GROSS-SALARY * WS-STANDARD-TAX-RATE / 100
                   ADD 1 TO WS-STANDARD-TAX-COUNT
               WHEN TAX-REDUCED
                   COMPUTE WS-TAX-AMOUNT =
                       WS-GROSS-SALARY * WS-REDUCED-TAX-RATE / 100
                   ADD 1 TO WS-REDUCED-TAX-COUNT
               WHEN TAX-EXEMPT
                   MOVE 0 TO WS-TAX-AMOUNT
                   ADD 1 TO WS-EXEMPT-TAX-COUNT
               WHEN OTHER
                   DISPLAY 'UNKNOWN TAX CODE FOR: ' EMP-ID
                   COMPUTE WS-TAX-AMOUNT =
                       WS-GROSS-SALARY * WS-STANDARD-TAX-RATE / 100
                   ADD 1 TO WS-STANDARD-TAX-COUNT
           END-EVALUATE.

      *    Calculate social security
           COMPUTE WS-SOCIAL-SEC =
               WS-GROSS-SALARY * WS-SOCIAL-SEC-RATE / 100.

       2500-CALCULATE-NET.
      *    Net = Gross - Tax - Social Security
           COMPUTE WS-NET-SALARY =
               WS-GROSS-SALARY - WS-TAX-AMOUNT - WS-SOCIAL-SEC.

      *    Apply level-based adjustment
           EVALUATE EMP-LEVEL
               WHEN 1 THRU 3
                   SUBTRACT 50.00 FROM WS-NET-SALARY
               WHEN 4 THRU 6
                   CONTINUE
               WHEN 7 THRU 10
                   ADD 100.00 TO WS-NET-SALARY
               WHEN OTHER
                   DISPLAY 'INVALID LEVEL FOR: ' EMP-ID
           END-EVALUATE.

       2600-UPDATE-TOTALS.
           ADD WS-GROSS-SALARY TO WS-TOTAL-GROSS.
           ADD WS-TAX-AMOUNT TO WS-TOTAL-TAX.
           ADD WS-NET-SALARY TO WS-TOTAL-NET.

       2700-WRITE-PAYROLL.
           MOVE EMP-ID TO PAY-EMP-ID.
           MOVE EMP-NAME TO PAY-EMP-NAME.
           MOVE WS-GROSS-SALARY TO PAY-GROSS-SALARY.
           MOVE WS-TAX-AMOUNT TO PAY-TAX-AMOUNT.
           MOVE WS-NET-SALARY TO PAY-NET-SALARY.
           MOVE EMP-DEPARTMENT TO PAY-DEPARTMENT.
           MOVE WS-PAY-PERIOD-STRING TO PAY-PERIOD.

           WRITE PAYROLL-RECORD.

           IF WS-PAY-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING PAYROLL RECORD: ' WS-PAY-STATUS
           END-IF.

       3000-FINALIZE.
           PERFORM 3100-DISPLAY-STATISTICS.
           CLOSE EMPLOYEE-FILE PAYROLL-FILE.

       3100-DISPLAY-STATISTICS.
           DISPLAY '======================================'.
           DISPLAY 'PAYROLL PROCESSING STATISTICS'.
           DISPLAY '======================================'.
           DISPLAY 'Employees Processed:   ' WS-EMPLOYEES-PROCESSED.
           DISPLAY 'Standard Tax Count:    ' WS-STANDARD-TAX-COUNT.
           DISPLAY 'Reduced Tax Count:     ' WS-REDUCED-TAX-COUNT.
           DISPLAY 'Exempt Tax Count:      ' WS-EXEMPT-TAX-COUNT.
           DISPLAY '--------------------------------------'.
           DISPLAY 'Total Gross Payroll:   ' WS-TOTAL-GROSS.
           DISPLAY 'Total Tax Withheld:    ' WS-TOTAL-TAX.
           DISPLAY 'Total Net Payroll:     ' WS-TOTAL-NET.
           DISPLAY '======================================'.
