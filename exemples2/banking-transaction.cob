       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKTRAN.
       AUTHOR. BANKING SYSTEM TEAM.
      ******************************************************************
      * PROGRAMME DE TRAITEMENT DES TRANSACTIONS BANCAIRES            *
      * - Lecture des transactions journalières                       *
      * - Validation des transactions                                 *
      * - Mise à jour des soldes de comptes                           *
      * - Génération de rapports d'erreurs et d'audit                 *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE
               ASSIGN TO TRANIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.

           SELECT MASTER-ACCOUNT-FILE
               ASSIGN TO ACCTIN
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MA-ACCOUNT-NUMBER
               FILE STATUS IS WS-ACCT-STATUS.

           SELECT UPDATED-ACCOUNT-FILE
               ASSIGN TO ACCTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-UPD-STATUS.

           SELECT ERROR-REPORT-FILE
               ASSIGN TO ERROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERR-STATUS.

           SELECT AUDIT-TRAIL-FILE
               ASSIGN TO AUDITOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUD-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TRANSACTION-RECORD.
           05  TR-TRANSACTION-ID       PIC X(16).
           05  TR-ACCOUNT-NUMBER       PIC 9(12).
           05  TR-TRANSACTION-TYPE     PIC X(02).
               88  TR-DEBIT            VALUE 'DB'.
               88  TR-CREDIT           VALUE 'CR'.
               88  TR-TRANSFER         VALUE 'TF'.
           05  TR-AMOUNT               PIC 9(13)V99 COMP-3.
           05  TR-DEST-ACCOUNT         PIC 9(12).
           05  TR-TRANSACTION-DATE     PIC 9(8).
           05  TR-TRANSACTION-TIME     PIC 9(6).
           05  TR-BRANCH-CODE          PIC X(6).
           05  TR-TELLER-ID            PIC X(8).
           05  TR-REFERENCE            PIC X(20).
           05  FILLER                  PIC X(17).

       FD  MASTER-ACCOUNT-FILE
           RECORDING MODE IS F.
       01  MASTER-ACCOUNT-RECORD.
           05  MA-ACCOUNT-NUMBER       PIC 9(12).
           05  MA-CUSTOMER-NAME        PIC X(50).
           05  MA-ACCOUNT-TYPE         PIC X(02).
               88  MA-CHECKING         VALUE 'CK'.
               88  MA-SAVINGS          VALUE 'SV'.
               88  MA-INVESTMENT       VALUE 'IN'.
           05  MA-CURRENT-BALANCE      PIC S9(13)V99 COMP-3.
           05  MA-AVAILABLE-BALANCE    PIC S9(13)V99 COMP-3.
           05  MA-OVERDRAFT-LIMIT      PIC 9(11)V99 COMP-3.
           05  MA-LAST-TRANSACTION     PIC 9(8).
           05  MA-OPEN-DATE            PIC 9(8).
           05  MA-STATUS-CODE          PIC X(01).
               88  MA-ACTIVE           VALUE 'A'.
               88  MA-FROZEN           VALUE 'F'.
               88  MA-CLOSED           VALUE 'C'.
           05  MA-BRANCH-CODE          PIC X(6).
           05  FILLER                  PIC X(40).

       FD  UPDATED-ACCOUNT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  UPDATED-ACCOUNT-RECORD.
           05  UA-ACCOUNT-NUMBER       PIC 9(12).
           05  UA-CUSTOMER-NAME        PIC X(50).
           05  UA-ACCOUNT-TYPE         PIC X(02).
           05  UA-OLD-BALANCE          PIC S9(13)V99 COMP-3.
           05  UA-NEW-BALANCE          PIC S9(13)V99 COMP-3.
           05  UA-TRANSACTION-COUNT    PIC 9(5) COMP-3.
           05  UA-LAST-UPDATE          PIC 9(8).
           05  UA-STATUS-CODE          PIC X(01).
           05  FILLER                  PIC X(50).

       FD  ERROR-REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ERROR-REPORT-RECORD         PIC X(132).

       FD  AUDIT-TRAIL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  AUDIT-TRAIL-RECORD          PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS-CODES.
           05  WS-TRAN-STATUS          PIC X(02) VALUE SPACES.
           05  WS-ACCT-STATUS          PIC X(02) VALUE SPACES.
           05  WS-UPD-STATUS           PIC X(02) VALUE SPACES.
           05  WS-ERR-STATUS           PIC X(02) VALUE SPACES.
           05  WS-AUD-STATUS           PIC X(02) VALUE SPACES.

       01  WS-COUNTERS.
           05  WS-TRANS-READ           PIC 9(7) COMP-3 VALUE ZERO.
           05  WS-TRANS-PROCESSED      PIC 9(7) COMP-3 VALUE ZERO.
           05  WS-TRANS-ERROR          PIC 9(7) COMP-3 VALUE ZERO.
           05  WS-ACCTS-UPDATED        PIC 9(7) COMP-3 VALUE ZERO.
           05  WS-TOTAL-DEBITS         PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOTAL-CREDITS        PIC S9(15)V99 COMP-3 VALUE ZERO.

       01  WS-CURRENT-ACCOUNT.
           05  WS-CURR-ACCT-NUM        PIC 9(12).
           05  WS-CURR-OLD-BALANCE     PIC S9(13)V99 COMP-3.
           05  WS-CURR-NEW-BALANCE     PIC S9(13)V99 COMP-3.
           05  WS-CURR-TRANS-COUNT     PIC 9(5) COMP-3.

       01  WS-VALIDATION-FLAGS.
           05  WS-VALID-TRANSACTION    PIC X(01) VALUE 'Y'.
               88  VALID-TRANS         VALUE 'Y'.
               88  INVALID-TRANS       VALUE 'N'.
           05  WS-ACCOUNT-FOUND        PIC X(01) VALUE 'N'.
               88  ACCOUNT-EXISTS      VALUE 'Y'.
               88  ACCOUNT-NOT-FOUND   VALUE 'N'.

       01  WS-ERROR-MESSAGE.
           05  WS-ERR-TRAN-ID          PIC X(16).
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-ERR-ACCOUNT          PIC 9(12).
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-ERR-CODE             PIC X(04).
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-ERR-DESCRIPTION      PIC X(80).

       01  WS-AUDIT-RECORD.
           05  WS-AUD-TIMESTAMP        PIC X(26).
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-AUD-TRAN-ID          PIC X(16).
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-AUD-ACCOUNT          PIC 9(12).
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-AUD-TYPE             PIC X(02).
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-AUD-AMOUNT           PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-AUD-OLD-BAL          PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03) VALUE ' | '.
           05  WS-AUD-NEW-BAL          PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(50) VALUE SPACES.

       01  WS-SUMMARY-REPORT.
           05  FILLER                  PIC X(50) VALUE
               '================================================'.
           05  FILLER                  PIC X(50) VALUE
               '  BANKING TRANSACTION SUMMARY REPORT          '.
           05  WS-SUMM-TRANS-READ      PIC ZZZ,ZZ9.
           05  WS-SUMM-TRANS-PROC      PIC ZZZ,ZZ9.
           05  WS-SUMM-TRANS-ERR       PIC ZZZ,ZZ9.
           05  WS-SUMM-ACCTS-UPD       PIC ZZZ,ZZ9.
           05  WS-SUMM-TOT-DEBITS      PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  WS-SUMM-TOT-CREDITS     PIC Z,ZZZ,ZZZ,ZZ9.99.

       01  WS-DATE-TIME-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YEAR        PIC 9(4).
               10  WS-CURR-MONTH       PIC 9(2).
               10  WS-CURR-DAY         PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-CURR-HOUR        PIC 9(2).
               10  WS-CURR-MINUTE      PIC 9(2).
               10  WS-CURR-SECOND      PIC 9(2).

       01  WS-SWITCHES.
           05  WS-END-OF-TRANS         PIC X(01) VALUE 'N'.
               88  END-OF-TRANSACTIONS VALUE 'Y'.
           05  WS-PROCESS-OK           PIC X(01) VALUE 'Y'.
               88  PROCESSING-OK       VALUE 'Y'.
               88  PROCESSING-ERROR    VALUE 'N'.

       PROCEDURE DIVISION.
       000-MAIN-CONTROL.
           PERFORM 100-INITIALIZE
           PERFORM 200-PROCESS-TRANSACTIONS
               UNTIL END-OF-TRANSACTIONS
           PERFORM 300-FINALIZE
           STOP RUN.

       100-INITIALIZE.
           DISPLAY '*** DEBUT TRAITEMENT TRANSACTIONS BANCAIRES ***'
           OPEN INPUT  TRANSACTION-FILE
                I-O    MASTER-ACCOUNT-FILE
                OUTPUT UPDATED-ACCOUNT-FILE
                       ERROR-REPORT-FILE
                       AUDIT-TRAIL-FILE

           IF WS-TRAN-STATUS NOT = '00'
               DISPLAY 'ERREUR OUVERTURE FICHIER TRANSACTIONS: '
                       WS-TRAN-STATUS
               MOVE 'N' TO WS-PROCESS-OK
           END-IF

           IF WS-ACCT-STATUS NOT = '00'
               DISPLAY 'ERREUR OUVERTURE FICHIER COMPTES: '
                       WS-ACCT-STATUS
               MOVE 'N' TO WS-PROCESS-OK
           END-IF

           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME

           PERFORM 110-READ-TRANSACTION.

       110-READ-TRANSACTION.
           READ TRANSACTION-FILE
               AT END
                   MOVE 'Y' TO WS-END-OF-TRANS
               NOT AT END
                   ADD 1 TO WS-TRANS-READ
           END-READ.

       200-PROCESS-TRANSACTIONS.
           IF NOT END-OF-TRANSACTIONS
               PERFORM 210-VALIDATE-TRANSACTION
               IF VALID-TRANS
                   PERFORM 220-PROCESS-VALID-TRANSACTION
               ELSE
                   PERFORM 230-LOG-ERROR
               END-IF
               PERFORM 110-READ-TRANSACTION
           END-IF.

       210-VALIDATE-TRANSACTION.
           MOVE 'Y' TO WS-VALID-TRANSACTION

      *    Validation du numéro de compte
           IF TR-ACCOUNT-NUMBER = ZERO
               MOVE 'N' TO WS-VALID-TRANSACTION
               MOVE 'E001' TO WS-ERR-CODE
               MOVE 'NUMERO DE COMPTE INVALIDE'
                   TO WS-ERR-DESCRIPTION
           END-IF

      *    Validation du type de transaction
           IF NOT (TR-DEBIT OR TR-CREDIT OR TR-TRANSFER)
               MOVE 'N' TO WS-VALID-TRANSACTION
               MOVE 'E002' TO WS-ERR-CODE
               MOVE 'TYPE DE TRANSACTION INVALIDE'
                   TO WS-ERR-DESCRIPTION
           END-IF

      *    Validation du montant
           IF TR-AMOUNT <= ZERO
               MOVE 'N' TO WS-VALID-TRANSACTION
               MOVE 'E003' TO WS-ERR-CODE
               MOVE 'MONTANT INVALIDE'
                   TO WS-ERR-DESCRIPTION
           END-IF

      *    Validation de la date
           IF TR-TRANSACTION-DATE NOT NUMERIC
               MOVE 'N' TO WS-VALID-TRANSACTION
               MOVE 'E004' TO WS-ERR-CODE
               MOVE 'DATE INVALIDE'
                   TO WS-ERR-DESCRIPTION
           END-IF.

       220-PROCESS-VALID-TRANSACTION.
           PERFORM 221-READ-ACCOUNT

           IF ACCOUNT-EXISTS
               PERFORM 222-CHECK-ACCOUNT-STATUS
               IF PROCESSING-OK
                   PERFORM 223-UPDATE-ACCOUNT-BALANCE
                   PERFORM 224-WRITE-AUDIT-TRAIL
                   PERFORM 225-WRITE-UPDATED-ACCOUNT
                   ADD 1 TO WS-TRANS-PROCESSED
               END-IF
           ELSE
               PERFORM 230-LOG-ERROR
           END-IF.

       221-READ-ACCOUNT.
           MOVE TR-ACCOUNT-NUMBER TO MA-ACCOUNT-NUMBER
           READ MASTER-ACCOUNT-FILE
               KEY IS MA-ACCOUNT-NUMBER
               INVALID KEY
                   MOVE 'N' TO WS-ACCOUNT-FOUND
                   MOVE 'E005' TO WS-ERR-CODE
                   MOVE 'COMPTE NON TROUVE' TO WS-ERR-DESCRIPTION
               NOT INVALID KEY
                   MOVE 'Y' TO WS-ACCOUNT-FOUND
           END-READ.

       222-CHECK-ACCOUNT-STATUS.
           IF MA-CLOSED
               MOVE 'N' TO WS-PROCESS-OK
               MOVE 'E006' TO WS-ERR-CODE
               MOVE 'COMPTE FERME' TO WS-ERR-DESCRIPTION
               PERFORM 230-LOG-ERROR
           END-IF

           IF MA-FROZEN
               MOVE 'N' TO WS-PROCESS-OK
               MOVE 'E007' TO WS-ERR-CODE
               MOVE 'COMPTE GELE' TO WS-ERR-DESCRIPTION
               PERFORM 230-LOG-ERROR
           END-IF.

       223-UPDATE-ACCOUNT-BALANCE.
           MOVE MA-CURRENT-BALANCE TO WS-CURR-OLD-BALANCE

           EVALUATE TRUE
               WHEN TR-DEBIT
                   SUBTRACT TR-AMOUNT FROM MA-CURRENT-BALANCE
                   ADD TR-AMOUNT TO WS-TOTAL-DEBITS
               WHEN TR-CREDIT
                   ADD TR-AMOUNT TO MA-CURRENT-BALANCE
                   ADD TR-AMOUNT TO WS-TOTAL-CREDITS
               WHEN TR-TRANSFER
                   SUBTRACT TR-AMOUNT FROM MA-CURRENT-BALANCE
                   PERFORM 227-PROCESS-TRANSFER
           END-EVALUATE

           MOVE MA-CURRENT-BALANCE TO WS-CURR-NEW-BALANCE
           MOVE TR-TRANSACTION-DATE TO MA-LAST-TRANSACTION

      *    Vérification découvert
           IF MA-CURRENT-BALANCE < (MA-OVERDRAFT-LIMIT * -1)
               MOVE 'E008' TO WS-ERR-CODE
               MOVE 'DEPASSEMENT DECOUVERT AUTORISE'
                   TO WS-ERR-DESCRIPTION
               PERFORM 230-LOG-ERROR
               MOVE WS-CURR-OLD-BALANCE TO MA-CURRENT-BALANCE
           ELSE
               REWRITE MASTER-ACCOUNT-RECORD
                   INVALID KEY
                       DISPLAY 'ERREUR REWRITE COMPTE: '
                               MA-ACCOUNT-NUMBER
               END-REWRITE
               ADD 1 TO WS-ACCTS-UPDATED
           END-IF.

       224-WRITE-AUDIT-TRAIL.
           MOVE FUNCTION CURRENT-DATE TO WS-AUD-TIMESTAMP
           MOVE TR-TRANSACTION-ID TO WS-AUD-TRAN-ID
           MOVE TR-ACCOUNT-NUMBER TO WS-AUD-ACCOUNT
           MOVE TR-TRANSACTION-TYPE TO WS-AUD-TYPE
           MOVE TR-AMOUNT TO WS-AUD-AMOUNT
           MOVE WS-CURR-OLD-BALANCE TO WS-AUD-OLD-BAL
           MOVE WS-CURR-NEW-BALANCE TO WS-AUD-NEW-BAL

           WRITE AUDIT-TRAIL-RECORD FROM WS-AUDIT-RECORD.

       225-WRITE-UPDATED-ACCOUNT.
           MOVE MA-ACCOUNT-NUMBER TO UA-ACCOUNT-NUMBER
           MOVE MA-CUSTOMER-NAME TO UA-CUSTOMER-NAME
           MOVE MA-ACCOUNT-TYPE TO UA-ACCOUNT-TYPE
           MOVE WS-CURR-OLD-BALANCE TO UA-OLD-BALANCE
           MOVE WS-CURR-NEW-BALANCE TO UA-NEW-BALANCE
           MOVE WS-CURR-TRANS-COUNT TO UA-TRANSACTION-COUNT
           MOVE TR-TRANSACTION-DATE TO UA-LAST-UPDATE
           MOVE MA-STATUS-CODE TO UA-STATUS-CODE

           WRITE UPDATED-ACCOUNT-RECORD.

       227-PROCESS-TRANSFER.
      *    Logique simplifiée pour le transfert
      *    Dans un système réel, il faudrait créditer le compte destination
           ADD TR-AMOUNT TO WS-TOTAL-CREDITS
           DISPLAY 'TRANSFERT VERS COMPTE: ' TR-DEST-ACCOUNT.

       230-LOG-ERROR.
           MOVE TR-TRANSACTION-ID TO WS-ERR-TRAN-ID
           MOVE TR-ACCOUNT-NUMBER TO WS-ERR-ACCOUNT
           ADD 1 TO WS-TRANS-ERROR

           WRITE ERROR-REPORT-RECORD FROM WS-ERROR-MESSAGE.

       300-FINALIZE.
           DISPLAY '*** FINALISATION TRAITEMENT ***'

           PERFORM 310-PRINT-SUMMARY

           CLOSE TRANSACTION-FILE
                 MASTER-ACCOUNT-FILE
                 UPDATED-ACCOUNT-FILE
                 ERROR-REPORT-FILE
                 AUDIT-TRAIL-FILE

           DISPLAY '*** FIN TRAITEMENT TRANSACTIONS BANCAIRES ***'.

       310-PRINT-SUMMARY.
           DISPLAY ' '
           DISPLAY '================================================'
           DISPLAY '    RAPPORT SOMMAIRE DE TRAITEMENT             '
           DISPLAY '================================================'
           MOVE WS-TRANS-READ TO WS-SUMM-TRANS-READ
           DISPLAY 'TRANSACTIONS LUES      : ' WS-SUMM-TRANS-READ
           MOVE WS-TRANS-PROCESSED TO WS-SUMM-TRANS-PROC
           DISPLAY 'TRANSACTIONS TRAITEES  : ' WS-SUMM-TRANS-PROC
           MOVE WS-TRANS-ERROR TO WS-SUMM-TRANS-ERR
           DISPLAY 'TRANSACTIONS EN ERREUR : ' WS-SUMM-TRANS-ERR
           MOVE WS-ACCTS-UPDATED TO WS-SUMM-ACCTS-UPD
           DISPLAY 'COMPTES MIS A JOUR     : ' WS-SUMM-ACCTS-UPD
           MOVE WS-TOTAL-DEBITS TO WS-SUMM-TOT-DEBITS
           DISPLAY 'TOTAL DEBITS           : ' WS-SUMM-TOT-DEBITS
           MOVE WS-TOTAL-CREDITS TO WS-SUMM-TOT-CREDITS
           DISPLAY 'TOTAL CREDITS          : ' WS-SUMM-TOT-CREDITS
           DISPLAY '================================================'
           DISPLAY ' '.
