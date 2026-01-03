# Support JCL dans le Traducteur COBOL vers Java

## Vue d'ensemble

Le traducteur COBOL vers Java supporte désormais les fichiers **JCL (Job Control Language)** pour générer automatiquement une configuration complète de Spring Batch. Lorsqu'un fichier JCL est fourni avec vos fichiers COBOL, le système :

- 🔍 Analyse la structure du job JCL (JOB, STEP, DD statements)
- 📊 Extrait les métadonnées des fichiers d'entrée/sortie
- ⚙️ Génère automatiquement les ItemReader et ItemWriter Spring Batch
- 🔗 Configure les steps avec les dépendances appropriées
- 📝 Crée les processeurs avec des TODOs pour la logique métier COBOL

## Fonctionnalités JCL supportées

### Statements JOB

```jcl
//JOBNAME JOB 'ACCOUNTING INFO',CLASS=A,MSGCLASS=X
```

**Supporté** :
- Nom du job
- Informations de comptabilité
- Paramètres CLASS, MSGCLASS, NOTIFY, REGION, etc.

### Statements EXEC

```jcl
//STEPNAME EXEC PGM=PROGNAME
//STEPNAME EXEC PROC=PROCNAME
```

**Supporté** :
- Nom du step
- Programme à exécuter (PGM)
- Procédure à exécuter (PROC)
- Paramètres PARM, COND, TIME, REGION

### Statements DD (Data Definition)

#### Dataset Name (DSN)

```jcl
//DDNAME DD DSN=DATASET.NAME,DISP=SHR
```

#### Disposition (DISP)

```jcl
DISP=(NEW,CATLG,DELETE)
DISP=(OLD,KEEP)
DISP=SHR
```

**Supporté** :
- Status : NEW, OLD, SHR, MOD
- Disposition normale : CATLG, KEEP, DELETE, PASS
- Disposition anormale : CATLG, KEEP, DELETE

#### DCB (Data Control Block)

```jcl
DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)
```

**Supporté** :
- RECFM : FB (Fixed Block), VB (Variable Block), F, V, etc.
- LRECL : Longueur d'enregistrement
- BLKSIZE : Taille de bloc
- DSORG : Organisation du dataset

#### Space Allocation

```jcl
SPACE=(TRK,(5,1))
SPACE=(CYL,(10,5))
```

**Supporté** :
- Unités : TRK (tracks), CYL (cylinders)
- Allocation primaire et secondaire

#### Autres paramètres DD

```jcl
UNIT=SYSDA
VOL=SER=WORK01
SYSOUT=*
DUMMY
```

**Supporté** :
- UNIT : Unité de stockage
- VOL : Information de volume
- SYSOUT : Sortie système
- DUMMY : Dataset fictif

## Exemple complet

### Fichier JCL d'entrée

```jcl
//CUSTBAT JOB 'CUSTOMER BATCH',CLASS=A,MSGCLASS=X
//*
//* Customer batch processing job
//*
//STEP01   EXEC PGM=CUSTPROC
//CUSIN    DD DSN=CUSTOMER.INPUT.DATA,DISP=SHR
//CUSOUT   DD DSN=CUSTOMER.OUTPUT.DATA,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY
```

### Configuration Spring Batch générée

#### CustbatJobConfiguration.java

```java
@Configuration
@EnableBatchProcessing
public class CustbatJobConfiguration {

    @Autowired
    private JobBuilderFactory jobBuilderFactory;

    @Autowired
    private StepBuilderFactory stepBuilderFactory;

    @Bean
    public Job custbatJob() {
        return jobBuilderFactory.get("CUSTBAT")
                .start(step01())
                .build();
    }

    @Bean
    public Step step01() {
        return stepBuilderFactory.get("STEP01")
                .<CustomerRecord, CustomerRecord>chunk(100)
                .reader(step01Reader())
                .processor(step01Processor())
                .writer(step01Writer())
                .build();
    }

    @Bean
    public ItemReader<CustomerRecord> step01Reader() {
        FlatFileItemReader<CustomerRecord> reader = new FlatFileItemReader<>();
        reader.setResource(new FileSystemResource("CUSTOMER.INPUT.DATA"));
        reader.setLineMapper(new DefaultLineMapper<CustomerRecord>() {{
            setLineTokenizer(new FixedLengthTokenizer() {{
                setNames("data");
                setColumns(new Range(1, 80));
            }});
            setFieldSetMapper(fieldSet -> {
                CustomerRecord record = new CustomerRecord();
                record.setData(fieldSet.readString("data"));
                return record;
            });
        }});
        return reader;
    }

    @Bean
    public ItemProcessor<CustomerRecord, CustomerRecord> step01Processor() {
        return new Step01Processor();
    }

    @Bean
    public ItemWriter<CustomerRecord> step01Writer() {
        FlatFileItemWriter<CustomerRecord> writer = new FlatFileItemWriter<>();
        writer.setResource(new FileSystemResource("CUSTOMER.OUTPUT.DATA"));
        writer.setLineAggregator(new PassThroughLineAggregator<>());
        return writer;
    }
}
```

#### Step01Processor.java

```java
public class Step01Processor implements ItemProcessor<CustomerRecord, CustomerRecord> {

    @Override
    public CustomerRecord process(CustomerRecord item) throws Exception {
        // TODO: Implement CUSTPROC business logic from COBOL
        return item;
    }
}
```

## Utilisation

### Via l'interface web

1. Accédez à l'interface web du traducteur
2. Sélectionnez vos fichiers COBOL (.cob, .cbl)
3. **Ajoutez votre fichier JCL (.jcl)** - optionnel mais recommandé
4. Remplissez le nom du projet et le package de base
5. Cliquez sur "Convertir"

Le système détectera automatiquement le fichier JCL et générera une configuration Spring Batch complète.

### Via l'API REST

```bash
curl -X POST http://localhost:8080/api/convert/upload \
  -F "files=@customer.cob" \
  -F "files=@customer-batch.jcl" \
  -F "projectName=customer-batch" \
  -F "basePackage=com.example.customer"
```

### Programmatiquement

```java
@Autowired
private CobolConversionService conversionService;

public void convertWithJCL() {
    List<Path> cobolFiles = Arrays.asList(
        Paths.get("src/customer.cob")
    );
    Path jclFile = Paths.get("src/customer-batch.jcl");

    Path outputDir = conversionService.convertWithJCL(
        cobolFiles,
        jclFile,
        "customer-batch",
        "com.example.customer"
    );
}
```

## Mapping JCL → Spring Batch

| Élément JCL | Équivalent Spring Batch |
|-------------|-------------------------|
| JOB | `Job` avec `JobBuilderFactory` |
| STEP | `Step` avec `StepBuilderFactory` |
| DD avec DISP=SHR/OLD | `FlatFileItemReader` |
| DD avec DISP=NEW/MOD | `FlatFileItemWriter` |
| DSN | `FileSystemResource` avec le nom du dataset |
| LRECL | `FixedLengthTokenizer` avec Range |
| RECFM=FB | `FixedLengthTokenizer` |
| RECFM=VB | `DelimitedLineTokenizer` |
| PGM | `ItemProcessor` à implémenter |
| PARM | Paramètres du job Spring Batch |

## Détection automatique des fichiers

Le générateur analyse les DD statements pour identifier :

### Fichiers d'entrée
- DISP=OLD
- DISP=SHR
→ Génère un `ItemReader`

### Fichiers de sortie
- DISP=NEW
- DISP=MOD
→ Génère un `ItemWriter`

### Fichiers spéciaux
- SYSOUT → Log (commentaire généré)
- DUMMY → Ignoré
- SYSIN → Paramètres d'entrée (commentaire généré)

## Limitations actuelles

1. **Procédures (PROC)** : Les procédures JCL ne sont pas encore totalement supportées. Le système reconnaît `EXEC PROC=` mais ne résout pas les bibliothèques de procédures.

2. **Conditional Execution** : Les conditions COND et IF/THEN/ELSE ne sont pas encore traduites en logique Spring Batch.

3. **Génération de Sets (GDG)** : Les Generation Data Groups ne sont pas supportés.

4. **Concatenation DD** : Les DD concaténés ne sont pas encore gérés.

5. **Logique métier** : Le système génère des stubs `ItemProcessor` avec des TODOs. La logique COBOL doit être implémentée manuellement.

6. **Formats de données complexes** : Seuls RECFM=FB et VB sont supportés. Les formats plus complexes nécessitent une configuration manuelle.

## Architecture technique

### Composants

```
JCL.g4 (Grammaire ANTLR4)
    ↓
JCLParser (Analyse syntaxique)
    ↓
JCLASTBuilder (Visitor ANTLR4)
    ↓
JCLJob/JCLStep/DDStatement (Modèle)
    ↓
JCLSpringBatchGenerator (Génération de code)
    ↓
Configuration Spring Batch Java
```

### Classes principales

- **JCLParser** : Point d'entrée pour l'analyse JCL
- **JCLJob** : Représentation d'un job JCL
- **JCLStep** : Représentation d'un step avec DD statements
- **DDStatement** : Statement DD avec disposition, DCB, etc.
- **JCLSpringBatchGenerator** : Générateur de configuration Spring Batch
- **JCLASTBuilder** : Visitor ANTLR4 pour construire le modèle

### Fichiers générés

Pour chaque JCL, le système génère :
- `{JobName}JobConfiguration.java` : Configuration principale du job
- `{StepName}Reader.java` : ItemReader pour chaque step avec fichier d'entrée
- `{StepName}Writer.java` : ItemWriter pour chaque step avec fichier de sortie
- `{StepName}Processor.java` : ItemProcessor avec TODOs pour la logique métier

## Bonnes pratiques

1. **Nommage cohérent** : Utilisez des noms de DD explicites (CUSIN, CUSOUT) qui seront reflétés dans le code généré.

2. **DCB complet** : Spécifiez toujours RECFM, LRECL et BLKSIZE pour une génération optimale.

3. **Commentaires JCL** : Les commentaires JCL (lignes //*) ne sont pas perdus, ils peuvent être conservés dans la documentation.

4. **Fichiers de test** : Préparez des datasets d'exemple avec les formats spécifiés dans le DCB.

5. **Revue du code généré** : Vérifiez toujours le code généré et implémentez les TODOs avant l'exécution.

## Dépannage

### Le fichier JCL n'est pas reconnu

Vérifiez que :
- Le fichier a l'extension `.jcl`
- La première ligne commence par `//`
- Le mot-clé `JOB` est présent
- La syntaxe JCL est correcte

### Les fichiers d'entrée/sortie ne sont pas détectés

Assurez-vous que :
- Le paramètre DISP est spécifié
- Le DSN est défini
- Le format est DISP=status ou DISP=(status,normal,abnormal)

### Erreurs de compilation du code généré

- Vérifiez que Spring Batch est dans les dépendances
- Assurez-vous que le package de base est correct
- Vérifiez que les imports sont présents

## Exemples supplémentaires

### JCL avec plusieurs steps

```jcl
//MULTISTEP JOB 'MULTI STEP JOB',CLASS=A
//STEP01   EXEC PGM=PROG1
//INPUT1   DD DSN=FILE.INPUT1,DISP=SHR
//OUTPUT1  DD DSN=FILE.OUTPUT1,DISP=(NEW,CATLG,DELETE),
//            DCB=(RECFM=FB,LRECL=100)
//STEP02   EXEC PGM=PROG2
//INPUT2   DD DSN=FILE.OUTPUT1,DISP=SHR
//OUTPUT2  DD DSN=FILE.FINAL,DISP=(NEW,CATLG,DELETE),
//            DCB=(RECFM=FB,LRECL=100)
```

Génère un job Spring Batch avec 2 steps chaînés.

### JCL avec SYSOUT

```jcl
//REPORT   EXEC PGM=REPRPT
//DATAIN   DD DSN=DATA.INPUT,DISP=SHR
//REPORT   DD SYSOUT=*
//SYSIN    DD *
PARM1=VALUE1
PARM2=VALUE2
/*
```

Le SYSOUT est commenté dans le code généré avec une suggestion d'utiliser un logger.

## Support et contribution

Pour signaler des bugs ou demander des fonctionnalités :
- GitHub Issues : [https://github.com/anthropics/claude-code/issues](https://github.com/anthropics/claude-code/issues)

Pour contribuer :
- Ajoutez des tests unitaires pour les nouvelles fonctionnalités JCL
- Documentez les limitations connues
- Créez des exemples JCL réalistes

## Références

- [IBM JCL Reference](https://www.ibm.com/docs/en/zos)
- [Spring Batch Documentation](https://docs.spring.io/spring-batch/docs/current/reference/html/)
- [ANTLR4 Documentation](https://github.com/antlr/antlr4/blob/master/doc/index.md)
