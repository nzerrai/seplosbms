//VSAMCUST JOB (ACCT),'VSAM CUSTOMER JOB',CLASS=A,MSGCLASS=X
//*****************************************************************
//* JCL for VSAM Customer Processing with KSDS file
//* Demonstrates indexed file with primary and alternate keys
//*****************************************************************
//STEP01  EXEC PGM=VSAM-CUSTOMER-PROCESSOR
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//CUSTFILE DD DSN=PROD.CUSTOMER.VSAM.KSDS,
//            DISP=SHR
//ERRLOG   DD DSN=PROD.CUSTOMER.ERROR.LOG,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
