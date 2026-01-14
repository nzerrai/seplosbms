//BANKBAT JOB 'BANKING TRANSACTIONS',
//             CLASS=A,
//             MSGCLASS=X,
//             NOTIFY=SYSUID,
//             REGION=4096K
//*
//*******************************************************************
//* JOB DE TRAITEMENT DES TRANSACTIONS BANCAIRES JOURNALIERES      *
//*                                                                 *
//* ETAPES:                                                         *
//* 1. BACKUP  - Sauvegarde des fichiers maîtres                   *
//* 2. SORT    - Tri des transactions par compte                   *
//* 3. BANKTRAN- Traitement principal des transactions             *
//* 4. REPORT  - Génération des rapports de synthèse               *
//* 5. ARCHIVE - Archivage des fichiers traités                    *
//*******************************************************************
//*
//*==================================================================
//* STEP 1: BACKUP DES FICHIERS MAITRES
//*==================================================================
//BACKUP   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=BANK.MASTER.ACCOUNTS,
//            DISP=SHR
//SYSUT2   DD DSN=BANK.BACKUP.ACCOUNTS.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=TAPE,
//            VOL=SER=BKUP01,
//            LABEL=(1,SL),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//SYSIN    DD DUMMY
//*
//*==================================================================
//* STEP 2: TRI DES TRANSACTIONS PAR NUMERO DE COMPTE
//*==================================================================
//SORT     EXEC PGM=SORT
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=BANK.DAILY.TRANSACTIONS,
//            DISP=SHR,
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=15000)
//SORTOUT  DD DSN=&&SORTED.TRANSACTIONS,
//            DISP=(NEW,PASS,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(100,20),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=15000)
//SYSIN    DD *
  SORT FIELDS=(17,12,CH,A)
  SUM FIELDS=NONE
/*
//*
//*==================================================================
//* STEP 3: TRAITEMENT PRINCIPAL DES TRANSACTIONS
//*==================================================================
//BANKTRAN EXEC PGM=BANKTRAN,
//             PARM='MODE=PROD,DEBUG=NO',
//             COND=(0,NE,SORT)
//STEPLIB  DD DSN=BANK.PROD.LOADLIB,
//            DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//* Fichier des transactions triées en entrée
//TRANIN   DD DSN=&&SORTED.TRANSACTIONS,
//            DISP=(OLD,DELETE,DELETE)
//*
//* Fichier maître des comptes (mise à jour)
//ACCTIN   DD DSN=BANK.MASTER.ACCOUNTS,
//            DISP=SHR,
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//*
//* Fichier des comptes mis à jour
//ACCTOUT  DD DSN=BANK.UPDATED.ACCOUNTS.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(200,50),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//*
//* Rapport des erreurs
//ERROUT   DD DSN=BANK.ERROR.REPORT.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//*
//* Piste d'audit
//AUDITOUT DD DSN=BANK.AUDIT.TRAIL.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(500,100),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//*
//* Tables de paramètres
//PARMLIB  DD DSN=BANK.PARM.LIBRARY,
//            DISP=SHR
//*
//* Tables de référence
//REFDATA  DD DSN=BANK.REFERENCE.DATA,
//            DISP=SHR
//*
//*==================================================================
//* STEP 4: GENERATION DES RAPPORTS DE SYNTHESE
//*==================================================================
//REPORT   EXEC PGM=RPTGEN,
//             COND=((0,NE,SORT),(0,NE,BANKTRAN))
//STEPLIB  DD DSN=BANK.PROD.LOADLIB,
//            DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//* Fichier des comptes mis à jour (entrée)
//ACCTIN   DD DSN=BANK.UPDATED.ACCOUNTS.D&LYYMMDD,
//            DISP=SHR
//*
//* Fichier d'audit (entrée)
//AUDITIN  DD DSN=BANK.AUDIT.TRAIL.D&LYYMMDD,
//            DISP=SHR
//*
//* Fichier des erreurs (entrée)
//ERRIN    DD DSN=BANK.ERROR.REPORT.D&LYYMMDD,
//            DISP=SHR
//*
//* Rapport journalier de synthèse
//SUMRPT   DD DSN=BANK.DAILY.SUMMARY.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,2),RLSE),
//            DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//*
//* Rapport détaillé par compte
//DETRPT   DD DSN=BANK.DETAIL.REPORT.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(50,10),RLSE),
//            DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//*
//* Rapport des exceptions
//EXCRPT   DD DSN=BANK.EXCEPTION.REPORT.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//*
//* Paramètres du rapport
//SYSIN    DD *
TITLE='RAPPORT JOURNALIER DES TRANSACTIONS BANCAIRES'
DATE=&LYYMMDD
DETAIL_LEVEL=FULL
INCLUDE_SUMMARY=YES
INCLUDE_EXCEPTIONS=YES
/*
//*
//*==================================================================
//* STEP 5: ARCHIVAGE DES FICHIERS TRAITES
//*==================================================================
//ARCHIVE  EXEC PGM=IEBGENER,
//             COND=((0,NE,SORT),(0,NE,BANKTRAN),(0,NE,REPORT))
//SYSPRINT DD SYSOUT=*
//*
//* Archivage de la piste d'audit
//SYSUT1   DD DSN=BANK.AUDIT.TRAIL.D&LYYMMDD,
//            DISP=SHR
//SYSUT2   DD DSN=BANK.ARCHIVE.AUDIT.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=TAPE,
//            VOL=SER=ARCH01,
//            LABEL=(,SL),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//SYSIN    DD DUMMY
//*
//*==================================================================
//* STEP 6: NOTIFICATION ET STATISTIQUES
//*==================================================================
//NOTIFY   EXEC PGM=IEFBR14,
//             COND=((0,EQ,BANKTRAN))
//SYSPRINT DD SYSOUT=*
//STATS    DD DSN=BANK.JOB.STATISTICS.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)
//*
//*==================================================================
//* FIN DU JOB
//*==================================================================
