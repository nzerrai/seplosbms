# Exemple d'utilisation - Analyse JCL et Classes Java dans l'IHM

## 🎯 Cas d'usage complet

Ce document montre comment remplir les données d'analyse JCL et de classes générées pour qu'elles s'affichent dans l'interface web.

## 📝 Exemple concret : Job de traitement client

### Contexte
Fichier JCL : `CUSTOMER-JOB.jcl`
```jcl
//CUSTJOB  JOB 'CUSTOMER PROCESSING',CLASS=A
//STEP01   EXEC PGM=CUSTPROC
//CUSIN    DD DSN=CUSTOMER.INPUT.FILE,DISP=SHR
//CUSOUT   DD DSN=&&TEMP01,DISP=(NEW,PASS)
//STEP02   IF (STEP01.RC = 0) THEN
//         EXEC PGM=VALIDATE
//TEMPIN   DD DSN=&&TEMP01,DISP=(OLD,DELETE)
//         ENDIF
```

### Code Java pour remplir l'analyse

```java
import com.cobol.translator.report.ConversionReport;
import com.cobol.translator.report.ConversionReport.JCLAnalysis;
import com.cobol.translator.report.ConversionReport.GeneratedJavaClass;
import com.cobol.translator.report.ConversionReport.GeneratedJavaClass.ClassType;

public class CustomerJobTranslator {

    public ConversionReport translate() {
        // 1. Créer le rapport de base
        ConversionReport report = new ConversionReport("CUSTPROC.cob", "CUSTPROC");

        // Statistiques de conversion COBOL (exemple)
        report.setTotalStatements(25);
        report.setConvertedStatements(25);
        report.setPartiallyConvertedStatements(0);
        report.setUnconvertedStatements(0);
        report.setOverallConfidence(ConversionReport.ConfidenceLevel.TRES_HAUTE);

        // 2. Remplir l'analyse JCL
        JCLAnalysis jclAnalysis = createJclAnalysis();
        report.setJclAnalysis(jclAnalysis);

        // 3. Ajouter les classes Java générées
        addGeneratedClasses(report);

        return report;
    }

    private JCLAnalysis createJclAnalysis() {
        JCLAnalysis analysis = new JCLAnalysis();

        // Informations de base
        analysis.setJclFileName("CUSTOMER-JOB.jcl");
        analysis.setJobName("CUSTJOB");

        // Statistiques
        analysis.setTotalSteps(2);
        analysis.setConditionalSteps(1);  // STEP02 est conditionnel
        analysis.setProcInvocations(0);
        analysis.setTemporaryDatasets(1); // &&TEMP01

        // Steps détectés
        analysis.addStep("STEP01 - EXEC PGM=CUSTPROC");
        analysis.addStep("STEP02 - EXEC PGM=VALIDATE (conditionnel)");

        // Conditions trouvées
        analysis.addCondition("IF STEP01.RC = 0 THEN");

        // Datasets temporaires utilisés
        analysis.addTempDataset("&&TEMP01");

        // DD Statements importants
        analysis.addDdStatement("CUSIN", "CUSTOMER.INPUT.FILE");
        analysis.addDdStatement("CUSOUT", "&&TEMP01 (temporary)");
        analysis.addDdStatement("TEMPIN", "&&TEMP01 (temporary)");

        return analysis;
    }

    private void addGeneratedClasses(ConversionReport report) {
        // 1. Configuration Spring Batch
        GeneratedJavaClass jobConfig = new GeneratedJavaClass(
            "CustomerJobConfiguration",
            "com.nz.batch.processor.config",
            ClassType.CONFIGURATION
        );
        jobConfig.setPurpose("Configuration principale du job Spring Batch CUSTJOB");
        jobConfig.setLinesOfCode(180);
        jobConfig.setFilePath("src/main/java/com/nz/batch/processor/config/CustomerJobConfiguration.java");
        jobConfig.setNew(true);

        jobConfig.addMethod("customerJob(JobRepository, Step, Step, Decider)");
        jobConfig.addMethod("step01Step(JobRepository, TransactionManager, ItemReader, ItemProcessor, ItemWriter)");
        jobConfig.addMethod("step02Step(JobRepository, TransactionManager)");

        report.addGeneratedClass(jobConfig);

        // 2. Decider pour la condition JCL
        GeneratedJavaClass decider = new GeneratedJavaClass(
            "Step01RcEq0Decider",
            "com.nz.batch.processor.decider",
            ClassType.DECIDER
        );
        decider.setPurpose("Décideur pour la condition IF STEP01.RC = 0 THEN");
        decider.setLinesOfCode(45);
        decider.setFilePath("src/main/java/com/nz/batch/processor/decider/Step01RcEq0Decider.java");
        decider.setNew(true);

        decider.addMethod("decide(JobExecution, StepExecution)");

        report.addGeneratedClass(decider);

        // 3. Entité JPA
        GeneratedJavaClass entity = new GeneratedJavaClass(
            "CustomerRecord",
            "com.nz.batch.processor.model",
            ClassType.ENTITY
        );
        entity.setPurpose("Entité JPA représentant un enregistrement client");
        entity.setLinesOfCode(120);
        entity.setFilePath("src/main/java/com/nz/batch/processor/model/CustomerRecord.java");
        entity.setNew(true);

        entity.addMethod("getId()");
        entity.addMethod("getCustName()");
        entity.addMethod("getCustAmount()");
        entity.addMethod("getCustDate()");

        report.addGeneratedClass(entity);

        // 4. ItemProcessor
        GeneratedJavaClass processor = new GeneratedJavaClass(
            "CustomerProcessor",
            "com.nz.batch.processor.processor",
            ClassType.PROCESSOR
        );
        processor.setPurpose("Traitement métier des enregistrements clients (logique COBOL traduite)");
        processor.setLinesOfCode(95);
        processor.setFilePath("src/main/java/com/nz/batch/processor/processor/CustomerProcessor.java");
        processor.setNew(true);

        processor.addMethod("process(CustomerRecord)");
        processor.addMethod("validateAmount(BigDecimal)");
        processor.addMethod("checkHighValue(CustomerRecord)");

        report.addGeneratedClass(processor);

        // 5. ItemReader
        GeneratedJavaClass reader = new GeneratedJavaClass(
            "CustomerItemReader",
            "com.nz.batch.processor.reader",
            ClassType.READER
        );
        reader.setPurpose("Lecture des enregistrements depuis CUSTOMER.INPUT.FILE");
        reader.setLinesOfCode(60);
        reader.setFilePath("src/main/java/com/nz/batch/processor/reader/CustomerItemReader.java");
        reader.setNew(true);

        reader.addMethod("read()");
        reader.addMethod("open(ExecutionContext)");
        reader.addMethod("close()");

        report.addGeneratedClass(reader);

        // 6. ItemWriter
        GeneratedJavaClass writer = new GeneratedJavaClass(
            "CustomerItemWriter",
            "com.nz.batch.processor.writer",
            ClassType.WRITER
        );
        writer.setPurpose("Écriture des enregistrements vers le dataset temporaire");
        writer.setLinesOfCode(55);
        writer.setFilePath("src/main/java/com/nz/batch/processor/writer/CustomerItemWriter.java");
        writer.setNew(true);

        writer.addMethod("write(Chunk<CustomerRecord>)");
        writer.addMethod("beforeWrite(List<CustomerRecord>)");
        writer.addMethod("afterWrite(List<CustomerRecord>)");

        report.addGeneratedClass(writer);

        // 7. Repository JPA
        GeneratedJavaClass repository = new GeneratedJavaClass(
            "CustomerRepository",
            "com.nz.batch.processor.repository",
            ClassType.REPOSITORY
        );
        repository.setPurpose("Repository JPA pour l'accès aux données clients");
        repository.setLinesOfCode(25);
        repository.setFilePath("src/main/java/com/nz/batch/processor/repository/CustomerRepository.java");
        repository.setNew(true);

        repository.addMethod("findAll()");
        repository.addMethod("findByAmount(BigDecimal)");
        repository.addMethod("save(CustomerRecord)");

        report.addGeneratedClass(repository);

        // 8. Validator
        GeneratedJavaClass validator = new GeneratedJavaClass(
            "CustomerValidator",
            "com.nz.batch.processor.validator",
            ClassType.VALIDATOR
        );
        validator.setPurpose("Validation des données clients (règles métier)");
        validator.setLinesOfCode(70);
        validator.setFilePath("src/main/java/com/nz/batch/processor/validator/CustomerValidator.java");
        validator.setNew(true);

        validator.addMethod("validate(CustomerRecord)");
        validator.addMethod("validateAmount(BigDecimal)");
        validator.addMethod("validateDate(LocalDate)");

        report.addGeneratedClass(validator);

        // 9. Listener de nettoyage (datasets temporaires)
        GeneratedJavaClass listener = new GeneratedJavaClass(
            "TemporaryDatasetCleanupListener",
            "com.nz.batch.processor.listener",
            ClassType.LISTENER
        );
        listener.setPurpose("Nettoyage automatique des datasets temporaires (&&TEMP01)");
        listener.setLinesOfCode(40);
        listener.setFilePath("src/main/java/com/nz/batch/processor/listener/TemporaryDatasetCleanupListener.java");
        listener.setNew(true);

        listener.addMethod("beforeJob(JobExecution)");
        listener.addMethod("afterJob(JobExecution)");

        report.addGeneratedClass(listener);

        // 10. Classe utilitaire
        GeneratedJavaClass utility = new GeneratedJavaClass(
            "CobolDataConverter",
            "com.nz.batch.processor.util",
            ClassType.UTILITY
        );
        utility.setPurpose("Utilitaires de conversion de formats COBOL vers Java");
        utility.setLinesOfCode(85);
        utility.setFilePath("src/main/java/com/nz/batch/processor/util/CobolDataConverter.java");
        utility.setNew(true);

        utility.addMethod("parsePicX(String)");
        utility.addMethod("parsePic9(String)");
        utility.addMethod("formatAmount(BigDecimal)");

        report.addGeneratedClass(utility);
    }
}
```

## 📊 Résultat dans l'IHM

Après exécution, l'interface web affichera :

### Section 1 : Analyse JCL
```
📋 Analyse JCL

┌──────────────────────────────────────────────────┐
│ Fichier JCL:           CUSTOMER-JOB.jcl         │
│ Job:                   CUSTJOB                   │
│ Steps totaux:          2                         │
│ Steps conditionnels:   1                         │
│ Invocations PROC:      0                         │
│ Datasets temporaires:  1                         │
└──────────────────────────────────────────────────┘

▶ Steps détectés (2)
  • STEP01 - EXEC PGM=CUSTPROC
  • STEP02 - EXEC PGM=VALIDATE (conditionnel)

▶ Conditions trouvées (1)
  • IF STEP01.RC = 0 THEN

▶ Datasets temporaires (1)
  • &&TEMP01

▶ DD Statements (3)
  • CUSIN: CUSTOMER.INPUT.FILE
  • CUSOUT: &&TEMP01 (temporary)
  • TEMPIN: &&TEMP01 (temporary)
```

### Section 2 : Classes Java générées (10 classes)
```
☕ Classes Java générées

┌──────────────────────────────────────────────────┐
│ ⚙️ CustomerJobConfiguration              [NEW]  │
│    Configuration                                 │
│    com.nz.batch.processor.config                │
│    Configuration principale du job...            │
│    180 lignes                                   │
│    ▶ Méthodes (3)                               │
└──────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────┐
│ 🔀 Step01RcEq0Decider                    [NEW]  │
│    Decider                                       │
│    com.nz.batch.processor.decider               │
│    Décideur pour la condition IF...              │
│    45 lignes                                    │
│    ▶ Méthodes (1)                               │
└──────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────┐
│ 📊 CustomerRecord                        [NEW]  │
│    Entity                                        │
│    com.nz.batch.processor.model                 │
│    Entité JPA représentant...                    │
│    120 lignes                                   │
│    ▶ Méthodes (4)                               │
└──────────────────────────────────────────────────┘

[... 7 autres classes ...]
```

## 🔧 Intégration dans le workflow de génération

### Dans JCLSpringBatchGenerator.java

```java
public ConversionResult generateSpringBatchProject(JCLJob jclJob, String outputDir) {
    ConversionResult result = new ConversionResult();
    ConversionReport report = new ConversionReport(jclJob.getJclFileName(), jclJob.getName());

    // 1. Analyser le JCL
    JCLAnalysis analysis = analyzeJCL(jclJob);
    report.setJclAnalysis(analysis);

    // 2. Générer les classes
    List<GeneratedJavaClass> classes = new ArrayList<>();

    // Générer la configuration
    String configClass = generateJobConfiguration(jclJob);
    classes.add(trackGeneratedClass("JobConfiguration", "config",
                                   ClassType.CONFIGURATION, configClass));

    // Générer les deciders pour les conditions
    for (ConditionalStep condition : jclJob.getConditionalSteps()) {
        String deciderClass = generateDecider(condition);
        classes.add(trackGeneratedClass(
            condition.getDeciderName(),
            "decider",
            ClassType.DECIDER,
            deciderClass
        ));
    }

    // Etc. pour chaque classe générée...

    report.setGeneratedClasses(classes);
    result.addReport(jclJob.getJclFileName(), report);

    return result;
}

private JCLAnalysis analyzeJCL(JCLJob jclJob) {
    JCLAnalysis analysis = new JCLAnalysis(jclJob.getJclFileName(), jclJob.getName());

    analysis.setTotalSteps(jclJob.getSteps().size());
    analysis.setConditionalSteps(countConditionalSteps(jclJob));
    analysis.setProcInvocations(countProcInvocations(jclJob));
    analysis.setTemporaryDatasets(countTempDatasets(jclJob));

    for (JCLStep step : jclJob.getSteps()) {
        analysis.addStep(step.getName() + " - " + step.getDescription());
    }

    // Etc.

    return analysis;
}

private GeneratedJavaClass trackGeneratedClass(
        String name, String packageSuffix, ClassType type, String sourceCode) {

    GeneratedJavaClass cls = new GeneratedJavaClass(
        name,
        basePackage + "." + packageSuffix,
        type
    );

    cls.setLinesOfCode(countLines(sourceCode));
    cls.setFilePath(getFilePath(name, packageSuffix));
    cls.setNew(true);

    // Extraire les méthodes du code source
    extractMethods(sourceCode).forEach(cls::addMethod);

    return cls;
}
```

## ✅ Checklist d'intégration

Pour intégrer cette fonctionnalité dans vos générateurs :

- [ ] Créer l'objet `JCLAnalysis` au début de la génération
- [ ] Remplir les statistiques (steps, conditions, PROCs, datasets)
- [ ] Ajouter les listes détaillées (steps, conditions, etc.)
- [ ] Pour chaque classe Java générée :
  - [ ] Créer un objet `GeneratedJavaClass`
  - [ ] Définir le type approprié (CONFIGURATION, PROCESSOR, etc.)
  - [ ] Ajouter la description/rôle
  - [ ] Compter les lignes de code
  - [ ] Extraire les noms des méthodes
  - [ ] Ajouter au rapport via `report.addGeneratedClass()`
- [ ] Attacher le rapport au `ConversionResult`

## 📚 Références

- Documentation complète : [CR_IHM_IMPLEMENTATION.md](CR_IHM_IMPLEMENTATION.md)
- Code backend : [ConversionReport.java](../src/main/java/com/cobol/translator/report/ConversionReport.java)
- Code frontend : [conversion.js](../src/main/resources/static/js/conversion.js)
- Styles : [conversion.css](../src/main/resources/static/css/conversion.css)

---

**Note** : Cet exemple est complet et fonctionnel. Vous pouvez l'adapter à vos besoins spécifiques en ajoutant ou supprimant des informations selon le contexte de votre projet.
