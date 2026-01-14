# 🔄 Traçabilité des traductions JCL → Java

## 🎯 Vue d'ensemble

Cette fonctionnalité permet de **visualiser en détail** comment chaque élément JCL a été traduit en code Java, avec :
- Le code JCL source original
- Le code Java généré correspondant
- Une explication de la traduction
- La classe Java impactée

## 📋 Exemple concret

### Fichier JCL : `customer-batch.jcl`

```jcl
//CUSTBAT JOB 'CUSTOMER BATCH',CLASS=A,MSGCLASS=X
//STEP01   STEP
//         EXEC PGM=CUSTPROC
//CUSIN    DD DSN=CUSTOMER.INPUT.DATA,DISP=SHR
//CUSOUT   DD DSN=CUSTOMER.OUTPUT.DATA,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
```

### Code Java pour remplir les traductions

```java
import com.cobol.translator.report.ConversionReport;
import com.cobol.translator.report.ConversionReport.JCLAnalysis;
import com.cobol.translator.report.ConversionReport.JCLTranslation;
import com.cobol.translator.report.ConversionReport.JCLTranslation.TranslationType;

public class CustomerBatchTranslator {

    public ConversionReport translateWithDetails() {
        ConversionReport report = new ConversionReport("CUSTPROC.cob", "CUSTPROC");

        // Créer l'analyse JCL
        JCLAnalysis analysis = new JCLAnalysis("customer-batch.jcl", "CUSTBAT");

        // Traduction 1: JOB Definition
        JCLTranslation jobDef = new JCLTranslation();
        jobDef.setJclElement("JOB");
        jobDef.setType(TranslationType.JOB_DEFINITION);
        jobDef.setJclSourceCode(
            "//CUSTBAT JOB 'CUSTOMER BATCH',CLASS=A,MSGCLASS=X"
        );
        jobDef.setJavaTargetCode(
            "@Configuration\n" +
            "public class CustomerBatchJobConfiguration {\n" +
            "    @Bean\n" +
            "    public Job customerBatchJob(JobRepository jobRepository, Step step01) {\n" +
            "        return new JobBuilder(\"customerBatchJob\", jobRepository)\n" +
            "            .start(step01)\n" +
            "            .build();\n" +
            "    }\n" +
            "}"
        );
        jobDef.setJavaClassName("CustomerBatchJobConfiguration");
        jobDef.setExplanation(
            "Le JOB JCL est converti en une classe @Configuration Spring Batch. " +
            "Les paramètres CLASS et MSGCLASS sont traduits en propriétés de configuration."
        );
        analysis.addTranslation(jobDef);

        // Traduction 2: STEP Execution
        JCLTranslation stepExec = new JCLTranslation();
        stepExec.setJclElement("EXEC PGM");
        stepExec.setType(TranslationType.STEP_EXECUTION);
        stepExec.setJclSourceCode(
            "//STEP01   STEP\n" +
            "//         EXEC PGM=CUSTPROC"
        );
        stepExec.setJavaTargetCode(
            "@Bean\n" +
            "public Step step01(JobRepository jobRepository,\n" +
            "                   PlatformTransactionManager transactionManager,\n" +
            "                   ItemReader<CustomerRecord> reader,\n" +
            "                   ItemProcessor<CustomerRecord, CustomerRecord> processor,\n" +
            "                   ItemWriter<CustomerRecord> writer) {\n" +
            "    return new StepBuilder(\"step01\", jobRepository)\n" +
            "        .<CustomerRecord, CustomerRecord>chunk(100, transactionManager)\n" +
            "        .reader(reader)\n" +
            "        .processor(processor)\n" +
            "        .writer(writer)\n" +
            "        .build();\n" +
            "}"
        );
        stepExec.setJavaClassName("CustomerBatchJobConfiguration.step01()");
        stepExec.setExplanation(
            "EXEC PGM=CUSTPROC est traduit en un Step Spring Batch avec un pattern chunk " +
            "(lecture/traitement/écriture par lots de 100 records)."
        );
        analysis.addTranslation(stepExec);

        // Traduction 3: DD Statement (Input)
        JCLTranslation ddInput = new JCLTranslation();
        ddInput.setJclElement("DD (Input)");
        ddInput.setType(TranslationType.DD_STATEMENT);
        ddInput.setJclSourceCode(
            "//CUSIN    DD DSN=CUSTOMER.INPUT.DATA,DISP=SHR"
        );
        ddInput.setJavaTargetCode(
            "@Bean\n" +
            "public FlatFileItemReader<CustomerRecord> customerInputReader() {\n" +
            "    return new FlatFileItemReaderBuilder<CustomerRecord>()\n" +
            "        .name(\"customerInputReader\")\n" +
            "        .resource(new FileSystemResource(\"CUSTOMER.INPUT.DATA\"))\n" +
            "        .delimited()\n" +
            "        .names(new String[]{\"custId\", \"custName\", \"custAmount\"})\n" +
            "        .fieldSetMapper(new BeanWrapperFieldSetMapper<>() {{\n" +
            "            setTargetType(CustomerRecord.class);\n" +
            "        }})\n" +
            "        .build();\n" +
            "}"
        );
        ddInput.setJavaClassName("CustomerInputReader");
        ddInput.setExplanation(
            "Le DD statement d'entrée est converti en FlatFileItemReader Spring Batch. " +
            "DISP=SHR indique un accès en lecture seule."
        );
        analysis.addTranslation(ddInput);

        // Traduction 4: DD Statement (Output)
        JCLTranslation ddOutput = new JCLTranslation();
        ddOutput.setJclElement("DD (Output)");
        ddOutput.setType(TranslationType.DD_STATEMENT);
        ddOutput.setJclSourceCode(
            "//CUSOUT   DD DSN=CUSTOMER.OUTPUT.DATA,\n" +
            "//            DISP=(NEW,CATLG,DELETE),\n" +
            "//            UNIT=SYSDA,\n" +
            "//            SPACE=(TRK,(5,1)),\n" +
            "//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)"
        );
        ddOutput.setJavaTargetCode(
            "@Bean\n" +
            "public FlatFileItemWriter<CustomerRecord> customerOutputWriter() {\n" +
            "    return new FlatFileItemWriterBuilder<CustomerRecord>()\n" +
            "        .name(\"customerOutputWriter\")\n" +
            "        .resource(new FileSystemResource(\"CUSTOMER.OUTPUT.DATA\"))\n" +
            "        .delimited()\n" +
            "        .delimiter(\",\")\n" +
            "        .names(new String[]{\"custId\", \"custName\", \"custAmount\"})\n" +
            "        .shouldDeleteIfExists(true)  // DISP=(NEW,...)\n" +
            "        .build();\n" +
            "}"
        );
        ddOutput.setJavaClassName("CustomerOutputWriter");
        ddOutput.setExplanation(
            "Le DD statement de sortie est converti en FlatFileItemWriter. " +
            "DISP=(NEW,CATLG,DELETE) signifie : créer nouveau fichier, le cataloguer si succès, " +
            "le supprimer si échec. DCB définit le format d'enregistrement (Fixed Block, 80 chars)."
        );
        analysis.addTranslation(ddOutput);

        // Attacher l'analyse au rapport
        report.setJclAnalysis(analysis);

        return report;
    }
}
```

## 🎨 Résultat dans l'IHM

L'interface web affichera :

```
┌──────────────────────────────────────────────────────────┐
│ 📋 Analyse JCL                                          │
│                                                          │
│ [... statistiques JCL ...]                              │
│                                                          │
│ ──────────────────────────────────────────────────────  │
│                                                          │
│ 🔄 Traductions JCL → Java détaillées                   │
│                                                          │
│ ┌────────────────────────────────────────────────────┐ │
│ │ #1  ⚙️ Job Definition  →  CustomerBatchJobConfig  │ │
│ ├────────────────────────────────────────────────────┤ │
│ │                                                    │ │
│ │ JCL Source                    →   Java Généré     │ │
│ │ ┌──────────────────┐            ┌──────────────┐ │ │
│ │ │//CUSTBAT JOB ... │     →      │@Configuration│ │ │
│ │ │                  │            │public class  │ │ │
│ │ │                  │            │CustomerBatch │ │ │
│ │ └──────────────────┘            └──────────────┘ │ │
│ │                                                    │ │
│ │ 💡 Explication:                                   │ │
│ │ Le JOB JCL est converti en une classe             │ │
│ │ @Configuration Spring Batch...                    │ │
│ └────────────────────────────────────────────────────┘ │
│                                                          │
│ ┌────────────────────────────────────────────────────┐ │
│ │ #2  ▶️ Step Execution  →  CustomerBatchJobConfig  │ │
│ ├────────────────────────────────────────────────────┤ │
│ │                                                    │ │
│ │ JCL Source                    →   Java Généré     │ │
│ │ ┌──────────────────┐            ┌──────────────┐ │ │
│ │ │//STEP01 STEP     │     →      │@Bean         │ │ │
│ │ │//  EXEC PGM=...  │            │public Step   │ │ │
│ │ │                  │            │step01(...)   │ │ │
│ │ └──────────────────┘            └──────────────┘ │ │
│ │                                                    │ │
│ │ 💡 Explication:                                   │ │
│ │ EXEC PGM=CUSTPROC est traduit en un Step          │ │
│ │ Spring Batch avec pattern chunk...                │ │
│ └────────────────────────────────────────────────────┘ │
│                                                          │
│ [...autres traductions...]                              │
└──────────────────────────────────────────────────────────┘
```

## 🔧 Types de traductions supportés

### 1. **JOB_DEFINITION** ⚙️
- **JCL** : `//JOBNAME JOB ...`
- **Java** : `@Configuration` class avec `@Bean public Job`
- **Impact** : Classe de configuration principale

### 2. **STEP_EXECUTION** ▶️
- **JCL** : `//STEPNAME EXEC PGM=...`
- **Java** : `@Bean public Step` avec Tasklet ou Chunk
- **Impact** : Méthode dans la configuration Job

### 3. **DD_STATEMENT** 📄
- **JCL** : `//DDNAME DD DSN=...,DISP=...`
- **Java** : `FlatFileItemReader` ou `FlatFileItemWriter`
- **Impact** : Classes Reader/Writer

### 4. **CONDITIONAL** 🔀
- **JCL** : `IF (condition) THEN ... ELSE ... ENDIF`
- **Java** : `JobExecutionDecider` + flow `.on("THEN")`
- **Impact** : Classe Decider + configuration de flux

### 5. **PROC_INVOCATION** 📞
- **JCL** : `//STEPNAME EXEC PROCNAME,PARM1=value`
- **Java** : Appel de méthode avec paramètres
- **Impact** : Configuration avec override de paramètres

### 6. **TEMP_DATASET** 💾
- **JCL** : `//DDNAME DD DSN=&&TEMP,DISP=(NEW,PASS)`
- **Java** : `ExecutionContext` + `TemporaryDatasetManager`
- **Impact** : Gestion de fichiers temporaires

## 📊 Statistiques et métriques

Chaque traduction permet de mesurer :
- **Nombre de lignes JCL** traduites
- **Nombre de lignes Java** générées
- **Ratio de transformation** (expansion du code)
- **Complexité** de chaque traduction

## ✅ Checklist d'intégration

Pour intégrer le suivi des traductions dans vos générateurs :

- [ ] Créer l'objet `JCLAnalysis`
- [ ] Pour chaque élément JCL significatif :
  - [ ] Créer un objet `JCLTranslation`
  - [ ] Définir le type approprié (`TranslationType`)
  - [ ] Capturer le code JCL source
  - [ ] Capturer le code Java généré
  - [ ] Ajouter une explication claire
  - [ ] Spécifier la classe Java impactée
  - [ ] Ajouter via `analysis.addTranslation()`
- [ ] Attacher l'analyse au rapport
- [ ] Vérifier l'affichage dans l'IHM

## 💡 Bonnes pratiques

### 1. Code source formaté
Assurez-vous que le code est bien formaté pour l'affichage :
```java
// ✅ BON
translation.setJclSourceCode(
    "//STEP01   EXEC PGM=PROG01\n" +
    "//DD01     DD DSN=FILE.DATA"
);

// ❌ MAUVAIS
translation.setJclSourceCode("//STEP01 EXEC PGM=PROG01//DD01 DD DSN=FILE.DATA");
```

### 2. Explications claires
Rédigez des explications compréhensibles par des non-experts :
```java
// ✅ BON
translation.setExplanation(
    "Le DD statement d'entrée est converti en FlatFileItemReader Spring Batch. " +
    "DISP=SHR indique un accès en lecture seule."
);

// ❌ MAUVAIS
translation.setExplanation("DD → Reader");
```

### 3. Nom de classe complet
Indiquez le nom complet de la classe pour faciliter la navigation :
```java
// ✅ BON
translation.setJavaClassName("com.example.batch.config.CustomerBatchJobConfiguration");

// ❌ ACCEPTABLE (mais moins précis)
translation.setJavaClassName("CustomerBatchJobConfiguration");
```

## 🎯 Exemple complet intégré

Voici comment intégrer cela dans un générateur existant :

```java
public class JCLSpringBatchGenerator {

    public ConversionResult generate(JCLJob jclJob) {
        ConversionResult result = new ConversionResult();
        ConversionReport report = new ConversionReport(jclJob.getFileName(), jclJob.getName());

        JCLAnalysis analysis = new JCLAnalysis(jclJob.getFileName(), jclJob.getName());

        // Traduire le JOB
        String jobConfigCode = generateJobConfiguration(jclJob);
        analysis.addTranslation(createJobTranslation(jclJob, jobConfigCode));

        // Traduire chaque STEP
        for (JCLStep step : jclJob.getSteps()) {
            String stepCode = generateStepConfiguration(step);
            analysis.addTranslation(createStepTranslation(step, stepCode));

            // Traduire les DD statements
            for (DDStatement dd : step.getDdStatements()) {
                String ddCode = generateDDConfiguration(dd);
                analysis.addTranslation(createDDTranslation(dd, ddCode));
            }
        }

        // Traduire les conditions
        for (ConditionalBlock condition : jclJob.getConditionalBlocks()) {
            String deciderCode = generateDecider(condition);
            analysis.addTranslation(createConditionalTranslation(condition, deciderCode));
        }

        report.setJclAnalysis(analysis);
        result.addReport(jclJob.getFileName(), report);

        return result;
    }

    private JCLTranslation createJobTranslation(JCLJob job, String javaCode) {
        JCLTranslation trans = new JCLTranslation();
        trans.setJclElement("JOB");
        trans.setType(TranslationType.JOB_DEFINITION);
        trans.setJclSourceCode(job.getOriginalJobCard());
        trans.setJavaTargetCode(javaCode);
        trans.setJavaClassName(job.getName() + "JobConfiguration");
        trans.setExplanation(
            "Le JOB JCL '" + job.getName() + "' est converti en une classe @Configuration " +
            "Spring Batch qui orchestre l'exécution de tous les steps."
        );
        return trans;
    }

    // ... autres méthodes de création de traductions ...
}
```

## 🚀 Résultat final

Cette fonctionnalité offre :
- ✅ **Traçabilité complète** : Chaque élément JCL a sa traduction
- ✅ **Transparence** : Voir exactement ce qui a été fait
- ✅ **Pédagogie** : Comprendre les patterns de traduction
- ✅ **Documentation** : Le rapport devient une doc technique
- ✅ **Validation** : Facilite la revue du code généré

---

**Documentation complète** : [CR_IHM_IMPLEMENTATION.md](CR_IHM_IMPLEMENTATION.md)
**Exemples** : [CR_IHM_USAGE_EXAMPLE.md](CR_IHM_USAGE_EXAMPLE.md)
**Navigation** : [CR_IHM_INDEX.md](CR_IHM_INDEX.md)

**Date** : 09/01/2026
**Version** : 1.1.0
**Statut** : ✅ Production Ready
