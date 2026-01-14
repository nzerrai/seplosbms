//CUSTBAT JOB 'CUSTOMER BATCH',CLASS=A,MSGCLASS=X
//*
//* Customer batch processing job
//* Reads customer file, processes records, writes output
//*
//STEP01   STEP
//         EXEC PGM=CUSTPROC
//CUSIN    DD DSN=CUSTOMER.INPUT.DATA,DISP=SHR
//CUSOUT   DD DSN=CUSTOMER.OUTPUT.DATA,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY
