       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-PROCESSOR.
       AUTHOR. TEST-SUITE.
      *****************************************************************
      * Programme de test complet - Traitement de commandes          *
      * Couvre: PERFORM, IF, EVALUATE, COMPUTE, ADD, MULTIPLY        *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORDER-FILE
               ASSIGN TO 'orders.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ORDER-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO 'order-report.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ORDER-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ORDER-RECORD.
           05  ORDER-ID             PIC X(10).
           05  CUSTOMER-ID          PIC X(8).
           05  ORDER-DATE           PIC X(10).
           05  PRODUCT-CODE         PIC X(6).
           05  QUANTITY             PIC 9(5).
           05  UNIT-PRICE           PIC 9(7)V99.
           05  DISCOUNT-RATE        PIC 99V99.
           05  ORDER-STATUS         PIC X(1).
               88  STATUS-PENDING   VALUE 'P'.
               88  STATUS-APPROVED  VALUE 'A'.
               88  STATUS-REJECTED  VALUE 'R'.
           05  PRIORITY-CODE        PIC X(1).
               88  PRIORITY-LOW     VALUE 'L'.
               88  PRIORITY-MEDIUM  VALUE 'M'.
               88  PRIORITY-HIGH    VALUE 'H'.

       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-LINE              PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ORDER-STATUS      PIC XX.
           05  WS-REPORT-STATUS     PIC XX.

       01  WS-FLAGS.
           05  WS-EOF-FLAG          PIC X(1) VALUE 'N'.
               88  WS-EOF           VALUE 'Y'.
           05  WS-VALID-FLAG        PIC X(1) VALUE 'N'.
               88  WS-VALID-ORDER   VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-TOTAL-ORDERS      PIC 9(7) VALUE 0.
           05  WS-APPROVED-ORDERS   PIC 9(7) VALUE 0.
           05  WS-REJECTED-ORDERS   PIC 9(7) VALUE 0.
           05  WS-PENDING-ORDERS    PIC 9(7) VALUE 0.

       01  WS-AMOUNTS.
           05  WS-ORDER-AMOUNT      PIC 9(9)V99.
           05  WS-DISCOUNT-AMOUNT   PIC 9(9)V99.
           05  WS-NET-AMOUNT        PIC 9(9)V99.
           05  WS-TOTAL-REVENUE     PIC 9(11)V99 VALUE 0.
           05  WS-TOTAL-DISCOUNT    PIC 9(11)V99 VALUE 0.

       01  WS-PRODUCT-LIMITS.
           05  WS-MIN-QUANTITY      PIC 9(5) VALUE 1.
           05  WS-MAX-QUANTITY      PIC 9(5) VALUE 10000.
           05  WS-MIN-PRICE         PIC 9(7)V99 VALUE 0.01.
           05  WS-MAX-PRICE         PIC 9(7)V99 VALUE 9999999.99.

       01  WS-REPORT-HEADER.
           05  FILLER               PIC X(20) VALUE 'ORDER PROCESSING'.
           05  FILLER               PIC X(10) VALUE ' REPORT'.
           05  FILLER               PIC X(102) VALUE SPACES.

       01  WS-REPORT-DETAIL.
           05  WS-RPT-ORDER-ID      PIC X(10).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-RPT-CUSTOMER      PIC X(8).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-RPT-PRODUCT       PIC X(6).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-RPT-QUANTITY      PIC ZZZ,ZZ9.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-RPT-AMOUNT        PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-RPT-NET           PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-RPT-STATUS        PIC X(10).

       01  WS-REPORT-SUMMARY.
           05  FILLER               PIC X(20) VALUE 'SUMMARY STATISTICS'.
           05  FILLER               PIC X(112) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-ORDERS
               UNTIL WS-EOF.
           PERFORM 3000-FINALIZE.
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT ORDER-FILE
                OUTPUT REPORT-FILE.

           IF WS-ORDER-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING ORDER FILE: ' WS-ORDER-STATUS
               STOP RUN
           END-IF.

           IF WS-REPORT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING REPORT FILE: ' WS-REPORT-STATUS
               STOP RUN
           END-IF.

           PERFORM 1100-WRITE-REPORT-HEADER.
           PERFORM 1200-READ-ORDER.

       1100-WRITE-REPORT-HEADER.
           WRITE REPORT-LINE FROM WS-REPORT-HEADER.
           MOVE SPACES TO REPORT-LINE.
           WRITE REPORT-LINE.

       1200-READ-ORDER.
           READ ORDER-FILE
               AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ.

       2000-PROCESS-ORDERS.
           IF NOT WS-EOF
               ADD 1 TO WS-TOTAL-ORDERS
               PERFORM 2100-VALIDATE-ORDER
               IF WS-VALID-ORDER
                   PERFORM 2200-CALCULATE-AMOUNTS
                   PERFORM 2300-UPDATE-STATUS-COUNTERS
                   PERFORM 2400-WRITE-DETAIL-LINE
               ELSE
                   PERFORM 2500-HANDLE-INVALID-ORDER
               END-IF
               PERFORM 1200-READ-ORDER
           END-IF.

       2100-VALIDATE-ORDER.
           MOVE 'Y' TO WS-VALID-FLAG.

      *    Validation 1: Quantity in range
           IF QUANTITY < WS-MIN-QUANTITY OR
              QUANTITY > WS-MAX-QUANTITY
               MOVE 'N' TO WS-VALID-FLAG
               DISPLAY 'INVALID QUANTITY FOR ORDER: ' ORDER-ID
           END-IF.

      *    Validation 2: Unit price in range
           IF UNIT-PRICE < WS-MIN-PRICE OR
              UNIT-PRICE > WS-MAX-PRICE
               MOVE 'N' TO WS-VALID-FLAG
               DISPLAY 'INVALID PRICE FOR ORDER: ' ORDER-ID
           END-IF.

      *    Validation 3: Discount rate validation
           IF DISCOUNT-RATE > 50.00
               MOVE 'N' TO WS-VALID-FLAG
               DISPLAY 'EXCESSIVE DISCOUNT FOR ORDER: ' ORDER-ID
           END-IF.

      *    Validation 4: Product code not empty
           IF PRODUCT-CODE = SPACES
               MOVE 'N' TO WS-VALID-FLAG
               DISPLAY 'MISSING PRODUCT CODE FOR ORDER: ' ORDER-ID
           END-IF.

       2200-CALCULATE-AMOUNTS.
      *    Calculate order amount = quantity * unit price
           COMPUTE WS-ORDER-AMOUNT = QUANTITY * UNIT-PRICE.

      *    Calculate discount amount
           COMPUTE WS-DISCOUNT-AMOUNT =
               WS-ORDER-AMOUNT * DISCOUNT-RATE / 100.

      *    Calculate net amount after discount
           COMPUTE WS-NET-AMOUNT =
               WS-ORDER-AMOUNT - WS-DISCOUNT-AMOUNT.

      *    Apply priority discount
           EVALUATE TRUE
               WHEN PRIORITY-HIGH
                   COMPUTE WS-NET-AMOUNT =
                       WS-NET-AMOUNT * 0.95
               WHEN PRIORITY-MEDIUM
                   COMPUTE WS-NET-AMOUNT =
                       WS-NET-AMOUNT * 0.98
               WHEN PRIORITY-LOW
                   CONTINUE
           END-EVALUATE.

      *    Update running totals
           ADD WS-NET-AMOUNT TO WS-TOTAL-REVENUE.
           ADD WS-DISCOUNT-AMOUNT TO WS-TOTAL-DISCOUNT.

       2300-UPDATE-STATUS-COUNTERS.
           EVALUATE TRUE
               WHEN STATUS-PENDING
                   ADD 1 TO WS-PENDING-ORDERS
               WHEN STATUS-APPROVED
                   ADD 1 TO WS-APPROVED-ORDERS
               WHEN STATUS-REJECTED
                   ADD 1 TO WS-REJECTED-ORDERS
               WHEN OTHER
                   DISPLAY 'UNKNOWN STATUS FOR ORDER: ' ORDER-ID
           END-EVALUATE.

       2400-WRITE-DETAIL-LINE.
           MOVE ORDER-ID TO WS-RPT-ORDER-ID.
           MOVE CUSTOMER-ID TO WS-RPT-CUSTOMER.
           MOVE PRODUCT-CODE TO WS-RPT-PRODUCT.
           MOVE QUANTITY TO WS-RPT-QUANTITY.
           MOVE WS-ORDER-AMOUNT TO WS-RPT-AMOUNT.
           MOVE WS-NET-AMOUNT TO WS-RPT-NET.

           EVALUATE TRUE
               WHEN STATUS-PENDING
                   MOVE 'PENDING   ' TO WS-RPT-STATUS
               WHEN STATUS-APPROVED
                   MOVE 'APPROVED  ' TO WS-RPT-STATUS
               WHEN STATUS-REJECTED
                   MOVE 'REJECTED  ' TO WS-RPT-STATUS
           END-EVALUATE.

           WRITE REPORT-LINE FROM WS-REPORT-DETAIL.

       2500-HANDLE-INVALID-ORDER.
           DISPLAY 'INVALID ORDER SKIPPED: ' ORDER-ID.

       3000-FINALIZE.
           PERFORM 3100-WRITE-SUMMARY.
           CLOSE ORDER-FILE REPORT-FILE.

           DISPLAY 'PROCESSING COMPLETE'.
           DISPLAY 'TOTAL ORDERS PROCESSED: ' WS-TOTAL-ORDERS.
           DISPLAY 'TOTAL REVENUE: ' WS-TOTAL-REVENUE.

       3100-WRITE-SUMMARY.
           MOVE SPACES TO REPORT-LINE.
           WRITE REPORT-LINE.
           WRITE REPORT-LINE FROM WS-REPORT-SUMMARY.
           MOVE SPACES TO REPORT-LINE.
           WRITE REPORT-LINE.

           MOVE 'Total Orders:     ' TO REPORT-LINE.
           MOVE WS-TOTAL-ORDERS TO REPORT-LINE(20:7).
           WRITE REPORT-LINE.

           MOVE 'Approved Orders:  ' TO REPORT-LINE.
           MOVE WS-APPROVED-ORDERS TO REPORT-LINE(20:7).
           WRITE REPORT-LINE.

           MOVE 'Rejected Orders:  ' TO REPORT-LINE.
           MOVE WS-REJECTED-ORDERS TO REPORT-LINE(20:7).
           WRITE REPORT-LINE.

           MOVE 'Pending Orders:   ' TO REPORT-LINE.
           MOVE WS-PENDING-ORDERS TO REPORT-LINE(20:7).
           WRITE REPORT-LINE.

           MOVE 'Total Revenue:    ' TO REPORT-LINE.
           MOVE WS-TOTAL-REVENUE TO REPORT-LINE(20:14).
           WRITE REPORT-LINE.

           MOVE 'Total Discounts:  ' TO REPORT-LINE.
           MOVE WS-TOTAL-DISCOUNT TO REPORT-LINE(20:14).
           WRITE REPORT-LINE.
