# 📊 DIAGRAMMES - Architecture d'Inférence

## 1️⃣ Flux COBOL/JCL → Détection & Inférence

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                         PHASE 1: ANALYSE COBOL/JCL                              │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                   │
│   Fichier COBOL/JCL                                                             │
│   ├── IDENTIFICATION DIVISION                                                   │
│   ├── ENVIRONMENT DIVISION                                                      │
│   ├── DATA DIVISION                                                             │
│   │   ├── FILE SECTION                                                          │
│   │   │   └── 01 CUSTOMER-RECORD                                               │
│   │   │       ├── 05 CUST-ID PIC 9(8)          ← DÉTECTÉE                      │
│   │   │       ├── 05 CUST-NAME PIC X(30)       ← DÉTECTÉE                      │
│   │   │       └── 05 FILLER PIC X(100)                                         │
│   │   │                                                                          │
│   │   ├── WORKING-STORAGE SECTION                                               │
│   │   │   ├── 01 WS-CUSTOMER-BALANCE PIC 9(10)V99  ← DÉTECTÉE                 │
│   │   │   ├── 01 WS-TODAY PIC 9(8)              ← DÉTECTÉE (YYYYMMDD)         │
│   │   │   └── [autres variables locales...]                                    │
│   │   │                                                                          │
│   │   └── LINKAGE SECTION                                                       │
│   │       └── 01 LS-PARAMETERS                                                 │
│   │           └── 05 LS-STATUS PIC X(2)        ← DÉTECTÉE                     │
│   │                                                                              │
│   └── PROCEDURE DIVISION                                                        │
│       ├── MAIN PROGRAM                                                          │
│       │   ├── PERFORM READ-CUSTOMER-DATA        ← Référence CUST-ID           │
│       │   ├── PERFORM VALIDATE-BALANCE          ← Référence CUST-BALANCE      │
│       │   └── PERFORM WRITE-RESULTS                                            │
│       │                                                                          │
│       └── 050-READ-CUSTOMER.                                                   │
│           ├── READ FILE ...                                                     │
│           └── MOVE CUST-ID TO WS-CUST-ID       ← Déplacement de champ        │
│                                                                                   │
└─────────────────────────────────────────────────────────────────────────────────┘
                                    ↓
        ┌─────────────────────────────────────────────────────────┐
        │   PHASE 2: EXTRACTION DES RÉFÉRENCES (Regex Pattern)    │
        └─────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────────┐
│              FieldReferenceAnalyzer: Détecte les patterns                       │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                   │
│  Pattern Matching sur code COBOL:                                               │
│  ┌─────────────────────────────────────┐                                        │
│  │ record.getCustId()      → ID_NAMES  │  ← Accès par ID                       │
│  │ record.setCustName()    → STRING_OPS│  ← Opération chaîne                   │
│  │ record.getCustBalance()→ ARITHMETIC │  ← Opération arithmétique             │
│  │ record.getTxnDate()     → DATE_OPS  │  ← Opération date                     │
│  │ record.getStatus()      → STATUS    │  ← Vérification status                │
│  │ record.setTotal()       → TOTALS    │  ← Calcul de total                    │
│  │ record.getAmount()      → MONETARY  │  ← Montant monétaire                 │
│  │ record.getRowCount()    → COUNTERS  │  ← Compteur/index                    │
│  └─────────────────────────────────────┘                                        │
│                                                                                   │
│  Sortie: Map<String, List<UsageContext>> referenceMap                          │
│  ───────────────────────────────────────────────────────────                   │
│  {                                                                               │
│    "custId":       [ID_NAMES],                                                  │
│    "custBalance":  [ARITHMETIC, MONETARY],                                      │
│    "custName":     [STRING_OPS],                                                │
│    "txnDate":      [DATE_OPS],                                                  │
│    "status":       [STATUS, COMPARISONS],                                       │
│    "total":        [TOTALS, ARITHMETIC],                                        │
│    "amount":       [MONETARY, ARITHMETIC]                                       │
│  }                                                                               │
│                                                                                   │
└─────────────────────────────────────────────────────────────────────────────────┘
                                    ↓
        ┌─────────────────────────────────────────────────────────┐
        │   PHASE 3: INFÉRENCE DE TYPES (Règles Prioritaires)     │
        └─────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────────┐
│            TypeInferenceEngine: 11 Règles d'Inférence Prioritaires              │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                   │
│  Règle 1: ID_NAMES + numérique         → Long                  (Confiance 98%) │
│  Règle 2: MONETARY + arithmétique      → BigDecimal             (Confiance 100%)│
│  Règle 3: DATE_OPS + yyyymmdd pattern  → LocalDate              (Confiance 95%) │
│  Règle 4: COUNTERS + index operations  → Integer                (Confiance 90%) │
│  Règle 5: STATUS + string ops          → String/Enum            (Confiance 75%) │
│  Règle 6: TOTALS + sum operations      → BigDecimal/Long        (Confiance 85%) │
│  Règle 7: String ops sans type         → String                 (Confiance 80%) │
│  Règle 8: Alphabetic only (COBOL)      → String                 (Confiance 85%) │
│  Règle 9: Comparaison avec constantes  → Inférer du type        (Confiance 70%) │
│  Règle 10: Multiple contextes = boost  → +15% confiance         (Multiplicatif) │
│  Règle 11: COBOL layout disponible     → Valider inférence      (Règle override)│
│                                                                                   │
│  Processus:                                                                      │
│  ──────────                                                                      │
│  Pour chaque champ inféré:                                                      │
│    1. Appliquer Règle 1 → Score 0.98 si correspondance                         │
│    2. Sinon Règle 2   → Score 1.0  si correspondance                          │
│    3. Sinon Règle 3   → Score 0.95 si correspondance                          │
│    ... (jusqu'à Règle 11)                                                      │
│                                                                                   │
│    Contextes multiples: score_final = score × 1.15 (max 1.0)                  │
│                                                                                   │
│  Résultat par champ: (javaType, confidenceScore, contexts, reasoning)          │
│                                                                                   │
└─────────────────────────────────────────────────────────────────────────────────┘
                                    ↓
        ┌─────────────────────────────────────────────────────────┐
        │   PHASE 4: GÉNÉRATION ENTITÉ ENRICHIE                   │
        └─────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────────┐
│   EntityGenerator: Crée/Enrichit les classes Java avec champs inférés           │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                   │
│  Entrée: (Entity de base, Map<String, InferredField> additionalFields)         │
│                                                                                   │
│  @Data                                                                           │
│  @Entity                                                                         │
│  @Table(name = "CUSTOMER")                                                      │
│  public class Customer {                                                        │
│      @Id                                                                         │
│      private Long custId;                 ← Du layout COBOL                     │
│                                                                                   │
│      private String custName;             ← Du layout COBOL                     │
│                                                                                   │
│      @Digits(integer=10, fraction=2)      ← Annotation inférée                 │
│      private BigDecimal custBalance;      ← INFÉRÉ (confiance 100%)            │
│                                                                                   │
│      @Convert(converter = LocalDateConverter.class)                             │
│      private LocalDate txnDate;           ← INFÉRÉ (confiance 95%)             │
│                                                                                   │
│      private String status;               ← INFÉRÉ (confiance 75%) + suggestion│
│                                                                                   │
│      private Integer rowCount;            ← INFÉRÉ (confiance 90%)             │
│                                                                                   │
│      @Column(precision=19, scale=2)                                             │
│      private BigDecimal totalAmount;      ← INFÉRÉ (confiance 85%)             │
│                                                                                   │
│      // getters/setters auto-générés                                           │
│  }                                                                               │
│                                                                                   │
│  Déduplication:                                                                  │
│  ────────────                                                                    │
│  Si layout contient custId ET inférence propose custId:                        │
│    → Fusionner avec annotations inférées (non dupliquer)                       │
│                                                                                   │
└─────────────────────────────────────────────────────────────────────────────────┘
                                    ↓
        ┌─────────────────────────────────────────────────────────┐
        │   PHASE 5: RAPPORT D'INFÉRENCE POUR IHM                 │
        └─────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────────┐
│              InferenceReportData: Statistiques & Recommandations                │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                   │
│  InferenceReportData {                                                          │
│    totalFieldsInferred: 7                                                       │
│    totalFieldsIncludingLayout: 9                                                │
│                                                                                   │
│    typeDistribution: {                                                          │
│      "String": 2,         (custName, status)                                    │
│      "Long": 1,           (custId)                                              │
│      "BigDecimal": 3,     (custBalance, totalAmount, amount)                   │
│      "LocalDate": 1       (txnDate)                                             │
│    }                                                                              │
│                                                                                   │
│    contextStats: {                                                              │
│      "ARITHMETIC": 4,     (balance, total, amount, count)                       │
│      "MONETARY": 3,       (balance, total, amount)                              │
│      "ID_NAMES": 1,       (custId)                                              │
│      "DATE_OPS": 1,       (txnDate)                                             │
│      "STATUS": 1          (status)                                              │
│    }                                                                              │
│                                                                                   │
│    overallQualityScore: 92      ← Moyenne confiance: 92%                       │
│    qualityLevel: "EXCELLENT"    ← ⭐⭐⭐⭐⭐                                    │
│                                                                                   │
│    layerBreakdown: {                                                            │
│      fromLayout: 2,        (custId, custName)                                   │
│      fromInference: 7,     (custBalance, txnDate, status, rowCount, etc)       │
│      deduplicatedCount: 0                                                       │
│    }                                                                              │
│                                                                                   │
│    recommendations: [                                                           │
│      "Status champ: considérer enum plutôt que String",                        │
│      "BigDecimal champs: ajouter @Digits(19,2) pour persistance",              │
│      "LocalDate: vérifier pattern entrée vs YYYYMMDD attendu",                 │
│      "Générer tests JPA pour nouvelles entités"                                │
│    ]                                                                              │
│  }                                                                               │
│                                                                                   │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## 2️⃣ Architecture Spring Batch Générée avec Entités Enrichies

```
┌────────────────────────────────────────────────────────────────────────────────────────┐
│                    PROJET SPRING BATCH GÉNÉRÉ (Maven Structure)                        │
├────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                        │
│   generated-project-customer/                                                         │
│   │                                                                                    │
│   ├── pom.xml                                                                         │
│   │   ├── spring-boot-starter-web                                                    │
│   │   ├── spring-boot-starter-batch                                                  │
│   │   ├── spring-boot-starter-data-jpa                                               │
│   │   ├── h2 (ou mysql)                                                              │
│   │   └── lombok                                                                     │
│   │                                                                                    │
│   ├── src/main/java/com/generated/                                                   │
│   │                                                                                    │
│   ├── 1️⃣  DOMAIN LAYER (Entités)                                                      │
│   │   └── entity/                                                                     │
│   │       ├── Customer.java              ← ENRICHIE avec champs inférés             │
│   │       │   ├── @Entity @Table("CUSTOMER")                                        │
│   │       │   ├── @Id Long custId       ← Du layout                               │
│   │       │   ├── String custName       ← Du layout                               │
│   │       │   ├── @Digits BigDecimal custBalance  ← INFÉRÉ                        │
│   │       │   ├── @Convert LocalDate txnDate      ← INFÉRÉ                        │
│   │       │   ├── String status         ← INFÉRÉ + enum suggestion                │
│   │       │   ├── Integer rowCount      ← INFÉRÉ                                  │
│   │       │   └── getters/setters                                                 │
│   │       │                                                                         │
│   │       ├── Transaction.java          ← ENRICHIE                                 │
│   │       │   ├── @Id Long txnId        ← INFÉRÉ                                  │
│   │       │   ├── @ManyToOne Customer   ← Relation détectée                       │
│   │       │   ├── @Digits BigDecimal amount     ← INFÉRÉ                          │
│   │       │   ├── LocalDate txnDate     ← INFÉRÉ                                  │
│   │       │   └── String type           ← INFÉRÉ                                  │
│   │       │                                                                         │
│   │       └── Audit.java               ← Générée automatiquement                  │
│   │           ├── @CreationTimestamp LocalDateTime createdAt                      │
│   │           ├── @UpdateTimestamp LocalDateTime updatedAt                        │
│   │           └── String createdBy                                                │
│   │                                                                                │
│   ├── 2️⃣  REPOSITORY LAYER                                                          │
│   │   └── repository/                                                              │
│   │       ├── CustomerRepository.java                                              │
│   │       │   extends JpaRepository<Customer, Long> {                              │
│   │       │     List<Customer> findByStatus(String status);                       │
│   │       │     List<Customer> findByCustBalance(                                 │
│   │       │        GreaterThan(BigDecimal));  ← Requête générée                   │
│   │       │   }                                                                    │
│   │       │                                                                        │
│   │       └── TransactionRepository.java                                           │
│   │           extends JpaRepository<Transaction, Long> {                           │
│   │             List<Transaction> findByTxnDate(LocalDate);  ← INFÉRÉ            │
│   │           }                                                                    │
│   │                                                                                │
│   ├── 3️⃣  SERVICE LAYER                                                             │
│   │   └── service/                                                                 │
│   │       ├── CustomerProcessor.java    ← Du code COBOL original                  │
│   │       │   implements ItemProcessor<CustomerRecord, Customer> {                │
│   │       │     @Override                                                         │
│   │       │     public Customer process(CustomerRecord item) {                    │
│   │       │         // Logique COBOL convertie                                    │
│   │       │         Customer customer = new Customer();                           │
│   │       │         customer.setCustId(item.getCustId());      ← Layout         │
│   │       │         customer.setCustBalance(item.getCustBalance()); ← INFÉRÉ    │
│   │       │         customer.setTxnDate(                       ← INFÉRÉ         │
│   │       │           LocalDate.parse(item.getTxnDate(), fmt));                 │
│   │       │         return customer;                                             │
│   │       │     }                                                                │
│   │       │   }                                                                   │
│   │       │                                                                       │
│   │       └── CustomerService.java                                               │
│   │           ├── @Autowired CustomerRepository repo                            │
│   │           ├── void importCustomers(List<Customer>)                          │
│   │           └── Page<Customer> findByStatus(String)  ← Requête inférée       │
│   │                                                                              │
│   ├── 4️⃣  BATCH CONFIGURATION                                                     │
│   │   └── batch/                                                                  │
│   │       ├── BatchConfiguration.java                                             │
│   │       │   ├── @Bean Job importCustomersJob() {                               │
│   │       │   │     return jobBuilderFactory                                     │
│   │       │   │       .get("importCustomersJob")                                 │
│   │       │   │       .start(importCustomersStep())  ← Étape générée            │
│   │       │   │       .build();                                                  │
│   │       │   │   }                                                              │
│   │       │   │                                                                  │
│   │       │   ├── @Bean Step importCustomersStep() {                             │
│   │       │   │     return stepBuilderFactory                                    │
│   │       │   │       .get("importCustomersStep")                                │
│   │       │   │       .<CustomerRecord, Customer>chunk(100)                      │
│   │       │   │       .reader(customerReader())                                  │
│   │       │   │       .processor(customerProcessor())  ← Processor enrichi      │
│   │       │   │       .writer(customerWriter())                                  │
│   │       │   │       .build();                                                  │
│   │       │   │   }                                                              │
│   │       │   │                                                                  │
│   │       │   ├── @Bean FlatFileItemReader<CustomerRecord> reader() {           │
│   │       │   │     // Lit fichier source COBOL/CSV                             │
│   │       │   │   }                                                              │
│   │       │   │                                                                  │
│   │       │   └── @Bean JpaItemWriter<Customer> writer() {                       │
│   │       │         // Écrit en base de données persistée                       │
│   │       │       }                                                              │
│   │       │                                                                      │
│   │       └── JobLauncher configuration                                          │
│   │           └── REST endpoint: POST /api/batch/import                          │
│   │                                                                              │
│   ├── 5️⃣  CONTROLLER LAYER                                                        │
│   │   └── web/                                                                    │
│   │       ├── BatchController.java                                                │
│   │       │   ├── POST /api/batch/import        → Lance job batch                │
│   │       │   ├── GET /api/batch/status/{id}    → Status du job                 │
│   │       │   └── GET /api/customers            → Liste entities                │
│   │       │                                                                      │
│   │       └── ReportController.java                                              │
│   │           └── GET /api/inference-report     → Rapport IHM                   │
│   │                                                                              │
│   ├── 6️⃣  APPLICATION PROPERTIES                                                 │
│   │   └── application.yml                                                        │
│   │       ├── spring.batch.*                   ← Configuré                      │
│   │       ├── spring.jpa.*                     ← Configuré                      │
│   │       ├── spring.datasource.*              ← Base H2/MySQL                  │
│   │       └── server.port=8080                                                  │
│   │                                                                              │
│   └── src/main/resources/                                                        │
│       └── batch-schema.sql          ← Tables Spring Batch                       │
│                                                                                  │
└────────────────────────────────────────────────────────────────────────────────────────┘
                                     ↓
    ┌──────────────────────────────────────────────────────────────┐
    │   BUILD & PACKAGE (Maven)                                    │
    └──────────────────────────────────────────────────────────────┘
                                     ↓
┌────────────────────────────────────────────────────────────────────────────────────────┐
│                    SORTIE: JAR EXÉCUTABLE                                              │
├────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                        │
│   customer-import-app-1.0.0-SNAPSHOT.jar                                             │
│   ├── Manifest: Main-Class=com.generated.Application                                 │
│   ├── Ressources: application.yml                                                    │
│   ├── Classes compilées (Entities, Repositories, Processors enrichis)               │
│   └── Dépendances: Spring Batch, JPA, H2/MySQL, Lombok                             │
│                                                                                        │
│   Exécution:                                                                          │
│   ──────────                                                                          │
│   $ java -jar customer-import-app-1.0.0-SNAPSHOT.jar                                │
│     ├── Démarre Spring Boot context                                                  │
│     ├── Initialise bases de données                                                  │
│     ├── Enregistre jobs Batch                                                        │
│     ├── Démarre Tomcat sur port 8080                                                │
│     └── Prêt pour requêtes HTTP                                                     │
│                                                                                        │
└────────────────────────────────────────────────────────────────────────────────────────┘
```

---

## 3️⃣ Flux d'Intégration: COBOL → Inférence → Spring Batch → IHM

```
┌─────────────────────┐
│  FICHIER COBOL/JCL  │
│  (courant de test)  │
└──────────┬──────────┘
           │
           ▼
┌──────────────────────────────────────────────────┐
│  CobolTranslator.translate()                     │
│  ├─ Parser COBOL (Antlr)                        │
│  ├─ FieldReferenceAnalyzer (PHASE 2)            │
│  ├─ TypeInferenceEngine (PHASE 3)               │
│  └─ EntityGenerator + enrichissement (PHASE 4)  │
└──────────┬───────────────────────────────────────┘
           │
           ├──────────────────────────────────┐
           │                                  │
           ▼                                  ▼
   ┌──────────────────┐         ┌──────────────────────────┐
   │   Entity.java    │         │ InferenceReportData      │
   │   (enrichie)     │         │ + InferredField[]        │
   └────────┬─────────┘         └────────────┬─────────────┘
            │                                │
            │                                ▼
            │                   ┌─────────────────────────┐
            │                   │  JSON Response          │
            │                   │  ConversionResponse {   │
            │                   │    success: true,       │
            │                   │    zipFileBase64: "...",│
            │                   │    inferenceReport: {   │
            │                   │      totalFields: 7,    │
            │                   │      typeDistribution..│
            │                   │    }                    │
            │                   │  }                      │
            │                   └─────────────┬───────────┘
            │                                │
            │                                ▼
            │                   ┌──────────────────────┐
            │                   │  IHM / Frontend      │
            │                   │  ├─ Tableau champs  │
            │                   │  ├─ Distribution    │
            │                   │  ├─ Heatmap         │
            │                   │  └─ Recommandations │
            │                   └────────────┬────────┘
            │                                │
            ▼                                │
   ┌──────────────────────────────────┐    │
   │  Maven Package (Spring Boot JAR) │    │
   └──────────────────────────────────┘    │
                                           │
                                 ┌─────────┴─────────────┐
                                 │                       │
                                 ▼                       ▼
                    ┌──────────────────────┐  ┌──────────────────┐
                    │   Batch Job Import   │  │  User Feedback   │
                    │   Process Entity +   │  │  dans l'IHM      │
                    │   Write to Database  │  │                  │
                    └──────────────────────┘  └──────────────────┘
```

---

## 4️⃣ Matrice de Transformation Type

```
┌──────────────────────┬─────────────────┬─────────────────┬──────────────────┐
│  Contexte COBOL      │  Pattern Java   │  Type Inféré    │  Confiance       │
├──────────────────────┼─────────────────┼─────────────────┼──────────────────┤
│                      │                 │                 │                  │
│ PIC 9(8)             │ getId()         │ Long            │ 98% (ID_NAMES)   │
│ 05 CUSTOMER-ID       │ setId()         │                 │                  │
│                      │                 │                 │                  │
├──────────────────────┼─────────────────┼─────────────────┼──────────────────┤
│                      │                 │                 │                  │
│ PIC 9(10)V99         │ getBalance()    │ BigDecimal      │ 100% (MONETARY + │
│ 05 BALANCE           │ setBalance(bd)  │                 │  ARITHMETIC)     │
│                      │                 │                 │                  │
├──────────────────────┼─────────────────┼─────────────────┼──────────────────┤
│                      │                 │                 │                  │
│ PIC 9(8) (YYYYMMDD)  │ getDate()       │ LocalDate       │ 95% (DATE_OPS +  │
│ 05 TRANSACTION-DATE  │ setDate()       │                 │  pattern match)  │
│                      │                 │                 │                  │
├──────────────────────┼─────────────────┼─────────────────┼──────────────────┤
│                      │                 │                 │                  │
│ PIC X(30)            │ getName()       │ String          │ 85% (STRING_OPS) │
│ 05 CUSTOMER-NAME     │ setName()       │                 │                  │
│                      │                 │                 │                  │
├──────────────────────┼─────────────────┼─────────────────┼──────────────────┤
│                      │                 │                 │                  │
│ PIC 9(5)             │ getRowCount()   │ Integer         │ 90% (COUNTERS)   │
│ 05 ROW-COUNTER       │ setRowCount()   │                 │                  │
│                      │                 │                 │                  │
├──────────────────────┼─────────────────┼─────────────────┼──────────────────┤
│                      │                 │                 │                  │
│ PIC X(2)             │ getStatus()     │ String/Enum*    │ 75% (STATUS)     │
│ 05 STATUS-CODE       │ setStatus()     │                 │ *Recommandé      │
│                      │                 │                 │                  │
└──────────────────────┴─────────────────┴─────────────────┴──────────────────┘
```

---

## 📌 Annotations Générées Automatiquement

```
@Entity
@Table(name = "CUSTOMER")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Customer {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    // Du layout COBOL:
    @Column(length = 30, nullable = false)
    private String name;
    
    // Inférés avec annotations:
    @Digits(integer = 10, fraction = 2)
    @Column(precision = 12, scale = 2)
    private BigDecimal balance;           // INFÉRÉ: 100% confiance
    
    @Convert(converter = LocalDateConverter.class)
    @Column(columnDefinition = "DATE")
    private LocalDate transactionDate;    // INFÉRÉ: 95% confiance
    
    @Enumerated(EnumType.STRING)          // Suggestion d'annotation
    @Column(length = 2)
    private String status;                // INFÉRÉ: 75% confiance → ENUM
    
    @Min(0)
    @Max(999999)
    private Integer rowCount;             // INFÉRÉ: 90% confiance
    
    @CreationTimestamp
    private LocalDateTime createdAt;
    
    @UpdateTimestamp
    private LocalDateTime updatedAt;
}
```

---

**Ces diagrammes illustrent:**
1. ✅ Flux complet: COBOL → Détection → Inférence → Spring Batch
2. ✅ Architecture projet généré avec entités enrichies
3. ✅ Transformation types COBOL → Java avec confiance
4. ✅ Annotations Java auto-générées basées sur l'inférence

