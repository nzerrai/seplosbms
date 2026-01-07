//COPYDEMO JOB (ACCT),'COPYBOOK DEMO JOB',CLASS=A,MSGCLASS=X
//*****************************************************************
//* JCL for Copybook Demonstration Program
//* Tests COPY and COPY REPLACING functionality
//*****************************************************************
//STEP01  EXEC PGM=COPYBOOK-DEMO
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//INFILE   DD DSN=PROD.CUSTOMER.DATA,
//            DISP=SHR
//OUTFILE  DD DSN=PROD.TRANSACTION.OUTPUT,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=15000)
//SYSOUT   DD SYSOUT=*
