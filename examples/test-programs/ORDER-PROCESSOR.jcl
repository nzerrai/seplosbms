//ORDPROC  JOB (ACCT),'ORDER PROCESSING',
//         CLASS=A,
//         MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID
//*****************************************************************
//* JCL POUR ORDER PROCESSOR - TRAITEMENT DES COMMANDES          *
//*****************************************************************
//STEP010  EXEC PGM=ORDER-PROCESSOR
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//* INPUT FILE - ORDER DATA
//ORDERS   DD DSN=PROD.ORDERS.INPUT,
//            DISP=SHR,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
//*
//* OUTPUT FILE - PROCESSED REPORT
//REPORT   DD DSN=PROD.ORDERS.REPORT,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//*
//* ERROR LOG
//ERRLOG   DD DSN=PROD.ORDERS.ERRORS,
//            DISP=(MOD,KEEP,KEEP),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
