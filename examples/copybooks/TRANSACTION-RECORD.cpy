      * TRANSACTION-RECORD Copybook
      * Standard transaction structure
       01  TRANSACTION-RECORD.
           05  TXN-ID                 PIC 9(15).
           05  TXN-DATE               PIC X(10).
           05  TXN-TIME               PIC X(8).
           05  TXN-TYPE               PIC X(2).
           05  TXN-AMOUNT             PIC S9(9)V99 COMP-3.
           05  TXN-DESCRIPTION        PIC X(50).
           05  TXN-STATUS             PIC X(1).
           05  TXN-ACCOUNT-ID         PIC 9(10).
