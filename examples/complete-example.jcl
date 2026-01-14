//COMPLETE JOB 'ADVANCED FEATURES DEMO',CLASS=A,MSGCLASS=X
//*
//* Example demonstrating all 4 advanced features:
//* 1. Conditional execution (IF/THEN/ELSE)
//* 2. PROC with symbolic parameters
//* 3. Temporary datasets (&&TEMP)
//* 4. Business logic translation (from COBOL)
//*

//*────────────────────────────────────────────────────────
//* PROC Definition: Backup utility with parameters
//*────────────────────────────────────────────────────────
//BACKUP   PROC HLQ=PROD,REGION=4M
//BKUP01   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&HLQ..INPUT.DATA,DISP=SHR
//SYSUT2   DD DSN=&HLQ..BACKUP.DATA,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//         PEND

//*────────────────────────────────────────────────────────
//* STEP 1: Process customer data (COBOL program)
//*────────────────────────────────────────────────────────
//STEP01   EXEC PGM=CUSTPROC
//CUSIN    DD DSN=CUSTOMER.INPUT.FILE,DISP=SHR
//CUSOUT   DD DSN=&&TEMP01,
//            DISP=(NEW,PASS),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSOUT   DD SYSOUT=*

//*────────────────────────────────────────────────────────
//* STEP 2: Conditional validation
//* Execute only if STEP01 succeeded (RC = 0)
//*────────────────────────────────────────────────────────
//STEP02   IF (STEP01.RC = 0) THEN
//         EXEC PGM=VALIDATE
//TEMPIN   DD DSN=&&TEMP01,DISP=(OLD,PASS)
//VALOUT   DD DSN=&&TEMP02,
//            DISP=(NEW,PASS),
//            UNIT=SYSDA,
//            SPACE=(TRK,(3,1))
//SYSOUT   DD SYSOUT=*
//         ENDIF

//*────────────────────────────────────────────────────────
//* STEP 3: Conditional branching
//* If validation succeeded, process normally
//* If validation failed, run error handler
//*────────────────────────────────────────────────────────
//STEP03   IF (STEP02.RC = 0) THEN
//NORMAL   EXEC PGM=PROCESS
//INPUT    DD DSN=&&TEMP02,DISP=(OLD,PASS)
//OUTPUT   DD DSN=CUSTOMER.OUTPUT.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,5))
//         ELSE
//ERROR    EXEC PGM=ERRORHAND
//INPUT    DD DSN=&&TEMP01,DISP=(OLD,DELETE)
//ERRLOG   DD DSN=ERROR.LOG.FILE,
//            DISP=(MOD,KEEP),
//            UNIT=SYSDA
//         ENDIF

//*────────────────────────────────────────────────────────
//* STEP 4: Invoke PROC with override parameters
//* Backup the output using TEST environment
//*────────────────────────────────────────────────────────
//STEP04   EXEC BACKUP,HLQ=TEST,REGION=8M

//*────────────────────────────────────────────────────────
//* STEP 5: Cleanup if everything succeeded
//*────────────────────────────────────────────────────────
//STEP05   IF (STEP03.RC = 0) AND (STEP04.RC = 0) THEN
//         EXEC PGM=CLEANUP
//WORKFILE DD DSN=&&TEMP02,DISP=(OLD,DELETE)
//SYSOUT   DD SYSOUT=*
//         ENDIF

//*────────────────────────────────────────────────────────
//* STEP 6: Final notification
//*────────────────────────────────────────────────────────
//STEP06   IF (STEP05.RC = 0) THEN
//         EXEC PGM=NOTIFY
//MESSAGE  DD *
JOB COMPLETED SUCCESSFULLY
ALL STEPS EXECUTED WITHOUT ERRORS
TEMPORARY DATASETS CLEANED UP
/*
//         ELSE
//         EXEC PGM=ALERT
//MESSAGE  DD *
JOB COMPLETED WITH WARNINGS OR ERRORS
CHECK JOB LOG FOR DETAILS
/*
//         ENDIF
