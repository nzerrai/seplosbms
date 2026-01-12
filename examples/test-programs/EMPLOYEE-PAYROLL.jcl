//PAYROLL  JOB (ACCT),'PAYROLL PROCESSING',
//         CLASS=A,
//         MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID
//*****************************************************************
//* JCL POUR EMPLOYEE PAYROLL - CALCUL DE PAIE                   *
//*****************************************************************
//STEP010  EXEC PGM=EMPLOYEE-PAYROLL
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//* INPUT FILE - EMPLOYEE DATA
//EMPLOYEE DD DSN=PROD.EMPLOYEE.MASTER,
//            DISP=SHR,
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=27900)
//*
//* OUTPUT FILE - PAYROLL DATA
//PAYROLL  DD DSN=PROD.PAYROLL.OUTPUT(&PERIOD),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
//*
//* BACKUP FILE
//BACKUP   DD DSN=PROD.PAYROLL.BACKUP(&PERIOD),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=TAPE,
//            VOL=SER=TAPE01,
//            LABEL=(,SL),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
