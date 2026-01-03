# Quick Start Guide

## Installation

```bash
# Compiler le projet
cd cobol-to-java-translator
mvn clean package

# Le JAR exécutable est créé dans target/cobol-translator.jar
```

## Utilisation

### 1. Traduire un fichier COBOL unique

```bash
java -jar target/cobol-translator.jar translate \
  examples/simple-customer.cob \
  --package com.mycompany.batch \
  --output generated/src/main/java
```

**Résultat attendu:**
```
╔════════════════════════════════════════════════════════╗
║   COBOL to Java Spring Batch Translator v1.0.0        ║
╚════════════════════════════════════════════════════════╝

📄 Source file: examples/simple-customer.cob
📦 Package: com.mycompany.batch
📂 Output directory: generated/src/main/java

🔄 Starting translation...

✅ Translation completed successfully!

📊 Metrics:
   Metrics: COBOL lines=42, Java lines=156, Data items=6, Statements=8, Files=3

📝 Generated files:
   ✓ CustomerFileRecord.java
   ✓ CustprocProcessor.java
   ✓ CustprocJobConfiguration.java
```

### 2. Traduire un répertoire complet

```bash
java -jar target/cobol-translator.jar translate-all \
  /path/to/cobol/programs \
  --package com.mycompany.batch \
  --output generated/src/main/java
```

### 3. Utiliser l'API en Java

```java
import com.cobol.translator.CobolTranslator;
import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.result.TranslationResult;

public class TranslatorExample {
    public static void main(String[] args) {
        // Configuration
        TranslationConfig config = TranslationConfig.builder()
            .sourceFile("CUSTPROC.cob")
            .outputPackage("com.mycompany.batch")
            .targetDirectory("src/main/java")
            .generateTests(true)
            .build();

        // Traduction
        CobolTranslator translator = new CobolTranslator();
        TranslationResult result = translator.translate(config);

        // Résultats
        if (result.isSuccess()) {
            System.out.println("Success! Generated files:");
            result.getGeneratedFiles().forEach(f ->
                System.out.println("  - " + f.getName()));

            System.out.println("\nMetrics: " + result.getMetrics());
        } else {
            System.err.println("Error: " + result.getErrorMessage());
        }
    }
}
```

## Fichiers générés

Pour le programme COBOL `CUSTPROC.cob`, les fichiers suivants sont générés:

### 1. CustomerFileRecord.java

```java
package com.mycompany.batch;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Entity for COBOL file: CUSTOMER-FILE
 * Generated from program: CUSTPROC
 */
public class CustomerFileRecord {

    private Integer custId;
    // COBOL: PIC 9(6)

    private String custName;
    // COBOL: PIC X(30)

    private BigDecimal custAmount;
    // COBOL: PIC 9(7)V99 COMP-3

    private LocalDate custDate;
    // COBOL: PIC 9(8)

    // Getters and Setters
    public Integer getCustId() {
        return custId;
    }

    public void setCustId(Integer custId) {
        this.custId = custId;
    }

    // ... (autres getters/setters)
}
```

### 2. CustprocProcessor.java

```java
package com.mycompany.batch;

import org.springframework.batch.item.ItemProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.math.BigDecimal;

/**
 * Processor for COBOL program: CUSTPROC
 * Auto-generated from PROCEDURE DIVISION logic
 */
public class CustprocProcessor implements ItemProcessor<Record, Record> {

    private static final Logger logger = LoggerFactory.getLogger(CustprocProcessor.class);

    @Override
    public Record process(Record record) throws Exception {
        logger.debug("Processing record: {}", record);

        // TODO: Implement COBOL procedure logic
        // Original COBOL statements: 8

        return record;
    }
}
```

### 3. CustprocJobConfiguration.java

```java
package com.mycompany.batch;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
// ... imports

/**
 * Spring Batch job configuration for COBOL program: CUSTPROC
 */
@Configuration
public class CustprocJobConfiguration {

    @Bean
    public Job custprocJob(JobRepository jobRepository, Step custprocJobStep) {
        return new JobBuilder("custprocJob", jobRepository)
                .start(custprocJobStep)
                .build();
    }

    @Bean
    public Step custprocJobStep(
            JobRepository jobRepository,
            PlatformTransactionManager transactionManager) {
        return new StepBuilder("custprocJobStep", jobRepository)
                .<Record, Record>chunk(100, transactionManager)
                // TODO: Add reader, processor, writer
                .build();
    }
}
```

## Prochaines étapes

1. **Compléter le code généré**: Les fichiers générés contiennent des `TODO` pour la logique métier
2. **Ajouter les readers/writers**: Configurer les ItemReader et ItemWriter appropriés
3. **Tester**: Exécuter les tests générés et ajouter des tests supplémentaires
4. **Intégrer**: Intégrer dans votre application Spring Boot existante

## Options de configuration

### Suffixes de nommage

Par défaut:
- Entités: `*Record` (ex: `CustomerFileRecord`)
- Processors: `*Processor` (ex: `CustprocProcessor`)
- Jobs: `*Job` (ex: `custprocJob`)

Pour personnaliser:

```java
TranslationConfig config = TranslationConfig.builder()
    .namingEntitySuffix("Entity")
    .namingProcessorSuffix("Handler")
    .namingJobSuffix("BatchJob")
    .build();
```

### Style de code

```java
TranslationConfig config = TranslationConfig.builder()
    .codeStyle(TranslationConfig.CodeStyle.GOOGLE)  // ou ORACLE, SPRING
    .build();
```

## Dépannage

### Erreur: "Could not parse COBOL file"

- Vérifier que le fichier COBOL est bien formaté (colonnes 7-72)
- Vérifier que l'encodage est correct (UTF-8 recommandé)

### Fichiers non générés

- Vérifier les permissions d'écriture dans le répertoire de sortie
- Vérifier les logs pour des erreurs détaillées

### Types Java incorrects

- Le traducteur fait de son mieux pour déduire les types
- Revue manuelle recommandée pour les champs critiques

## Support

Pour plus d'informations, consultez:
- [README.md](README.md) - Documentation complète
- [MIGRATION_GUIDE.md](docs/MIGRATION_GUIDE.md) - Guide de migration
- [API Documentation](docs/API.md) - Référence API
