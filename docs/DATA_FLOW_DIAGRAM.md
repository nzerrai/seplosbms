# Diagrammes de Flux de Données - ORDER-PROCESSOR

Visualisation du flux de données entre COBOL et Java Spring Batch

---

## 📊 Architecture Données - Vue d'Ensemble

### COBOL: Architecture en Couches

```mermaid
graph TB
    subgraph "FILE SECTION"
        IF[ORDER-FILE<br/>INPUT]
        OF[REPORT-FILE<br/>OUTPUT]
    end

    subgraph "WORKING-STORAGE SECTION"
        WS1[File Status<br/>WS-ORDER-STATUS<br/>WS-REPORT-STATUS]
        WS2[Flags<br/>WS-EOF-FLAG<br/>WS-VALID-FLAG]
        WS3[Counters<br/>WS-TOTAL-ORDERS<br/>WS-APPROVED-ORDERS<br/>WS-REJECTED-ORDERS<br/>WS-PENDING-ORDERS]
        WS4[Amounts<br/>WS-ORDER-AMOUNT<br/>WS-DISCOUNT-AMOUNT<br/>WS-NET-AMOUNT<br/>WS-TOTAL-REVENUE<br/>WS-TOTAL-DISCOUNT]
        WS5[Limits<br/>WS-MIN-QUANTITY<br/>WS-MAX-QUANTITY<br/>WS-MIN-PRICE<br/>WS-MAX-PRICE]
        WS6[Report Fields<br/>WS-REPORT-HEADER<br/>WS-REPORT-DETAIL<br/>WS-REPORT-SUMMARY]
    end

    subgraph "PROCEDURE DIVISION"
        MAIN[0000-MAIN-PROCESS]
        INIT[1000-INITIALIZE]
        PROC[2000-PROCESS-ORDERS]
        FIN[3000-FINALIZE]
    end

    IF -->|READ| PROC
    PROC -->|Validate| WS2
    PROC -->|Calculate| WS4
    PROC -->|Count| WS3
    PROC -->|Format| WS6
    WS6 -->|WRITE| OF

    style IF fill:#e3f2fd
    style OF fill:#c8e6c9
    style WS1 fill:#fff9c4
    style WS2 fill:#fff9c4
    style WS3 fill:#fff9c4
    style WS4 fill:#fff9c4
    style WS5 fill:#fff9c4
    style WS6 fill:#fff9c4
```

### Java: Architecture Spring Batch

```mermaid
graph TB
    subgraph "Model Layer"
        IM[OrderFileRecord<br/>@Entity<br/>- orderId<br/>- customerId<br/>- quantity<br/>- unitPrice<br/>- etc.]
        OM[ReportFileRecord<br/>@Entity<br/>- reportLine<br/>or detailed fields]
    end

    subgraph "Processor Layer"
        P[OrderProcessor<br/>@Component<br/>implements ItemProcessor]
        WS[Working Storage Fields<br/>- wsValidFlag<br/>- wsTotalOrders<br/>- wsOrderAmount<br/>- etc.]
        V[OrderValidator<br/>@Component<br/>Business Rules]
    end

    subgraph "Configuration Layer"
        JC[OrderJobConfiguration<br/>@Configuration]
        R[FlatFileItemReader<br/>@Bean @StepScope]
        W[FlatFileItemWriter<br/>@Bean @StepScope]
    end

    subgraph "Spring Batch Framework"
        J[Job]
        S[Step]
        SE[StepExecution]
        JR[JobRepository]
    end

    R -->|Read| IM
    IM -->|Process| P
    P <-->|Uses| WS
    P -->|Validate| V
    P -->|Transform| OM
    OM -->|Write| W

    JC -->|Configure| R
    JC -->|Configure| P
    JC -->|Configure| W
    JC -->|Define| S
    S -->|Contains| R
    S -->|Contains| P
    S -->|Contains| W
    J -->|Execute| S
    S -->|Update| SE
    SE -->|Persist| JR

    style IM fill:#e3f2fd
    style OM fill:#c8e6c9
    style P fill:#fff3e0
    style V fill:#ffccbc
    style JC fill:#f3e5f5
    style J fill:#e1bee7
```

---

## 🔄 Flux de Transformation des Données

### Transformation Record: Input → Output

```mermaid
flowchart LR
    subgraph Input["INPUT: ORDER-FILE Record"]
        I1[ORDER-ID: X'10']
        I2[CUSTOMER-ID: X'8']
        I3[ORDER-DATE: X'10']
        I4[PRODUCT-CODE: X'6']
        I5[QUANTITY: 9'5']
        I6[UNIT-PRICE: 9'7'V99]
        I7[DISCOUNT-RATE: 99V99]
        I8[ORDER-STATUS: X'1']
        I9[PRIORITY-CODE: X'1']
    end

    subgraph Process["PROCESSING: Working Storage"]
        P1[WS-ORDER-AMOUNT<br/>= QUANTITY * UNIT-PRICE]
        P2[WS-DISCOUNT-AMOUNT<br/>= AMOUNT * RATE / 100]
        P3[WS-NET-AMOUNT<br/>= AMOUNT - DISCOUNT]
        P4[Apply Priority Discount<br/>HIGH: *0.95<br/>MEDIUM: *0.98]
        P5[Update Counters<br/>by Status]
    end

    subgraph Output["OUTPUT: REPORT-FILE Record"]
        O1[RPT-ORDER-ID]
        O2[RPT-CUSTOMER]
        O3[RPT-PRODUCT]
        O4[RPT-QUANTITY]
        O5[RPT-AMOUNT]
        O6[RPT-NET]
        O7[RPT-STATUS]
    end

    I1 --> P1
    I5 --> P1
    I6 --> P1
    P1 --> P2
    I7 --> P2
    P2 --> P3
    P3 --> P4
    I9 --> P4
    I8 --> P5

    I1 --> O1
    I2 --> O2
    I4 --> O3
    I5 --> O4
    P1 --> O5
    P4 --> O6
    P5 --> O7

    style Input fill:#e3f2fd
    style Process fill:#fff3e0
    style Output fill:#c8e6c9
```

### Exemple Concret de Transformation

```mermaid
flowchart TB
    subgraph "Enregistrement Input"
        IN["ORDER-ID: 'ORD0001'<br/>CUSTOMER-ID: 'CUST0001'<br/>PRODUCT-CODE: 'PROD01'<br/>QUANTITY: 1000<br/>UNIT-PRICE: 50.00<br/>DISCOUNT-RATE: 10.00<br/>STATUS: 'A' Approved<br/>PRIORITY: 'H' High"]
    end

    subgraph "Calculs Intermédiaires"
        C1["WS-ORDER-AMOUNT<br/>= 1000 * 50.00<br/>= 50,000.00"]
        C2["WS-DISCOUNT-AMOUNT<br/>= 50,000 * 10 / 100<br/>= 5,000.00"]
        C3["WS-NET-AMOUNT<br/>= 50,000 - 5,000<br/>= 45,000.00"]
        C4["Priority Discount HIGH<br/>= 45,000 * 0.95<br/>= 42,750.00"]
        C5["WS-APPROVED-ORDERS++<br/>= 1"]
        C6["WS-TOTAL-REVENUE<br/>+= 42,750.00"]
    end

    subgraph "Enregistrement Output"
        OUT["ORDER-ID: 'ORD0001'<br/>CUSTOMER: 'CUST0001'<br/>PRODUCT: 'PROD01'<br/>QUANTITY: 1,000<br/>AMOUNT: 50,000.00<br/>NET: 42,750.00<br/>STATUS: 'APPROVED'"]
    end

    IN --> C1
    C1 --> C2
    C2 --> C3
    C3 --> C4
    C4 --> C5
    C4 --> C6
    C5 --> OUT
    C6 --> OUT

    style IN fill:#e3f2fd
    style C1 fill:#fff9c4
    style C2 fill:#fff9c4
    style C3 fill:#fff9c4
    style C4 fill:#c8e6c9
    style C5 fill:#ffccbc
    style C6 fill:#ffccbc
    style OUT fill:#c8e6c9
```

---

## 🗂️ Structure des Données

### COBOL: Hiérarchie des Données

```mermaid
graph TD
    FD[FD ORDER-FILE] --> OR[01 ORDER-RECORD]
    OR --> F1[05 ORDER-ID PIC X'10']
    OR --> F2[05 CUSTOMER-ID PIC X'8']
    OR --> F3[05 ORDER-DATE PIC X'10']
    OR --> F4[05 PRODUCT-CODE PIC X'6']
    OR --> F5[05 QUANTITY PIC 9'5']
    OR --> F6[05 UNIT-PRICE PIC 9'7'V99]
    OR --> F7[05 DISCOUNT-RATE PIC 99V99]
    OR --> F8[05 ORDER-STATUS PIC X'1']
    F8 --> C1[88 STATUS-PENDING VALUE 'P']
    F8 --> C2[88 STATUS-APPROVED VALUE 'A']
    F8 --> C3[88 STATUS-REJECTED VALUE 'R']
    OR --> F9[05 PRIORITY-CODE PIC X'1']
    F9 --> C4[88 PRIORITY-LOW VALUE 'L']
    F9 --> C5[88 PRIORITY-MEDIUM VALUE 'M']
    F9 --> C6[88 PRIORITY-HIGH VALUE 'H']

    style FD fill:#e1f5ff
    style OR fill:#fff9e1
    style F8 fill:#ffe0b2
    style F9 fill:#ffe0b2
    style C1 fill:#ffccbc
    style C2 fill:#c8e6c9
    style C3 fill:#ef9a9a
```

### Java: Modèle Objet

```mermaid
classDiagram
    class OrderFileRecord {
        -String orderId
        -String customerId
        -String orderDate
        -String productCode
        -Integer quantity
        -BigDecimal unitPrice
        -BigDecimal discountRate
        -String orderStatus
        -String priorityCode
        +getOrderId() String
        +setOrderId(String)
        +getQuantity() Integer
        +getUnitPrice() BigDecimal
        +isStatusPending() boolean
        +isStatusApproved() boolean
        +isPriorityHigh() boolean
    }

    class ReportFileRecord {
        -String orderId
        -String customerId
        -String productCode
        -Integer quantity
        -BigDecimal amount
        -BigDecimal netAmount
        -String status
        +getOrderId() String
        +setOrderId(String)
        +getNetAmount() BigDecimal
    }

    class OrderProcessor {
        -String wsValidFlag
        -Integer wsTotalOrders
        -BigDecimal wsOrderAmount
        -BigDecimal wsNetAmount
        -BigDecimal wsTotalRevenue
        +process(OrderFileRecord) ReportFileRecord
        -validate2100Order(OrderFileRecord)
        -calculate2200Amounts(OrderFileRecord)
        -updateStatus2300Counters(OrderFileRecord)
        -buildDetail2400Line(OrderFileRecord) ReportFileRecord
    }

    OrderProcessor ..> OrderFileRecord : reads
    OrderProcessor ..> ReportFileRecord : produces
```

---

## 💾 Mappage Champs: COBOL ↔ Java

### Fichier Input

| COBOL Picture | Type COBOL | Champ Java | Type Java | Conversion |
|---------------|------------|------------|-----------|------------|
| `PIC X(10)` | Alphanum | `orderId` | `String` | Direct |
| `PIC X(8)` | Alphanum | `customerId` | `String` | Direct |
| `PIC X(10)` | Alphanum | `orderDate` | `String` | Direct (ou LocalDate) |
| `PIC X(6)` | Alphanum | `productCode` | `String` | Direct |
| `PIC 9(5)` | Numérique | `quantity` | `Integer` | parseInt() |
| `PIC 9(7)V99` | Décimal | `unitPrice` | `BigDecimal` | new BigDecimal() |
| `PIC 99V99` | Décimal | `discountRate` | `BigDecimal` | new BigDecimal() |
| `PIC X(1)` | Alphanum | `orderStatus` | `String` | Direct |
| `PIC X(1)` | Alphanum | `priorityCode` | `String` | Direct |

### Working Storage

| COBOL | Java | Utilisation |
|-------|------|-------------|
| `WS-EOF-FLAG PIC X` | *(géré par reader)* | End-of-file flag |
| `WS-VALID-FLAG PIC X` | `String wsValidFlag` | Validation result |
| `WS-TOTAL-ORDERS PIC 9(7)` | `Integer wsTotalOrders` | Counter |
| `WS-ORDER-AMOUNT PIC 9(9)V99` | `BigDecimal wsOrderAmount` | Calculated amount |
| `WS-NET-AMOUNT PIC 9(9)V99` | `BigDecimal wsNetAmount` | Net after discounts |
| `WS-TOTAL-REVENUE PIC 9(11)V99` | `BigDecimal wsTotalRevenue` | Accumulated total |

### 88-Levels (Conditions)

| COBOL | Java Équivalent |
|-------|-----------------|
| `88 STATUS-PENDING VALUE 'P'` | `record.getOrderStatus().equals("P")` |
| `88 STATUS-APPROVED VALUE 'A'` | `record.getOrderStatus().equals("A")` |
| `88 STATUS-REJECTED VALUE 'R'` | `record.getOrderStatus().equals("R")` |
| `88 PRIORITY-HIGH VALUE 'H'` | `record.getPriorityCode().equals("H")` |
| `88 PRIORITY-MEDIUM VALUE 'M'` | `record.getPriorityCode().equals("M")` |
| `88 PRIORITY-LOW VALUE 'L'` | `record.getPriorityCode().equals("L")` |

---

## 📊 Flux d'États

### Cycle de Vie d'un Enregistrement

```mermaid
stateDiagram-v2
    [*] --> FileRead: READ ORDER-FILE
    FileRead --> Validate: PERFORM 2100-VALIDATE

    Validate --> ValidCheck: Check validations
    ValidCheck --> Valid: All checks pass
    ValidCheck --> Invalid: Any check fails

    Valid --> Calculate: PERFORM 2200-CALCULATE
    Calculate --> CounterUpdate: PERFORM 2300-UPDATE-COUNTERS
    CounterUpdate --> FormatOutput: PERFORM 2400-WRITE-DETAIL
    FormatOutput --> WriteReport: WRITE REPORT-LINE
    WriteReport --> NextRecord: Read next

    Invalid --> LogError: DISPLAY error
    LogError --> NextRecord

    NextRecord --> FileRead: More records
    NextRecord --> [*]: EOF reached
```

### États des Compteurs

```mermaid
stateDiagram-v2
    [*] --> Initialize: WS-TOTAL-ORDERS = 0
    Initialize --> ProcessRecord: Read record

    ProcessRecord --> IncrementTotal: ADD 1 TO WS-TOTAL-ORDERS
    IncrementTotal --> CheckStatus: EVALUATE ORDER-STATUS

    CheckStatus --> Pending: STATUS-PENDING
    CheckStatus --> Approved: STATUS-APPROVED
    CheckStatus --> Rejected: STATUS-REJECTED

    Pending --> IncrementPending: ADD 1 TO WS-PENDING-ORDERS
    Approved --> IncrementApproved: ADD 1 TO WS-APPROVED-ORDERS
    Rejected --> IncrementRejected: ADD 1 TO WS-REJECTED-ORDERS

    IncrementPending --> NextRecord
    IncrementApproved --> NextRecord
    IncrementRejected --> NextRecord

    NextRecord --> ProcessRecord: More records
    NextRecord --> Finalize: EOF

    Finalize --> [*]: Display totals
```

---

## 🔄 Transformation des Montants

```mermaid
flowchart TB
    subgraph "Input Data"
        Q[QUANTITY<br/>1000 units]
        UP[UNIT-PRICE<br/>$50.00]
        DR[DISCOUNT-RATE<br/>10%]
        PC[PRIORITY-CODE<br/>'H' High]
    end

    subgraph "Step 1: Order Amount"
        S1["WS-ORDER-AMOUNT<br/>= QUANTITY * UNIT-PRICE<br/>= 1000 * 50.00<br/>= $50,000.00"]
    end

    subgraph "Step 2: Discount Amount"
        S2["WS-DISCOUNT-AMOUNT<br/>= ORDER-AMOUNT * RATE / 100<br/>= 50,000 * 10 / 100<br/>= $5,000.00"]
    end

    subgraph "Step 3: Net Before Priority"
        S3["WS-NET-AMOUNT<br/>= ORDER-AMOUNT - DISCOUNT<br/>= 50,000 - 5,000<br/>= $45,000.00"]
    end

    subgraph "Step 4: Priority Discount"
        S4["WS-NET-AMOUNT<br/>= NET * PRIORITY-MULTIPLIER<br/>High: 45,000 * 0.95<br/>= $42,750.00"]
    end

    subgraph "Step 5: Update Totals"
        S5A["WS-TOTAL-REVENUE<br/>+= 42,750.00"]
        S5B["WS-TOTAL-DISCOUNT<br/>+= 5,000.00"]
    end

    subgraph "Output"
        OUT["REPORT-LINE<br/>Amount: $50,000.00<br/>Net: $42,750.00<br/>Status: APPROVED"]
    end

    Q --> S1
    UP --> S1
    S1 --> S2
    DR --> S2
    S2 --> S3
    S3 --> S4
    PC --> S4
    S4 --> S5A
    S2 --> S5B
    S5A --> OUT
    S5B --> OUT

    style Q fill:#e3f2fd
    style UP fill:#e3f2fd
    style DR fill:#e3f2fd
    style PC fill:#e3f2fd
    style S1 fill:#fff9c4
    style S2 fill:#fff9c4
    style S3 fill:#fff9c4
    style S4 fill:#c8e6c9
    style S5A fill:#ffccbc
    style S5B fill:#ffccbc
    style OUT fill:#c8e6c9
```

---

## 📈 Agrégation des Données

### Vue Globale des Totaux

```mermaid
graph TD
    subgraph "Records Processed"
        R1[Order 1<br/>Net: $42,750]
        R2[Order 2<br/>Net: $14,250]
        R3[Order 3<br/>Net: $35,625]
        R4[Order 4<br/>Net: $142,500]
        R5[Order 5<br/>Net: $19,845]
    end

    subgraph "Status Counters"
        P[WS-PENDING-ORDERS<br/>= 2]
        A[WS-APPROVED-ORDERS<br/>= 2]
        R[WS-REJECTED-ORDERS<br/>= 1]
    end

    subgraph "Financial Totals"
        TR[WS-TOTAL-REVENUE<br/>= $254,970.00]
        TD[WS-TOTAL-DISCOUNT<br/>= $28,330.00]
    end

    subgraph "Summary Report"
        SR["SUMMARY STATISTICS<br/>─────────────────<br/>Total Orders: 5<br/>Approved: 2<br/>Rejected: 1<br/>Pending: 2<br/>─────────────────<br/>Total Revenue: $254,970.00<br/>Total Discounts: $28,330.00"]
    end

    R1 --> P
    R2 --> A
    R3 --> R
    R4 --> P
    R5 --> A

    R1 --> TR
    R2 --> TR
    R3 --> TR
    R4 --> TR
    R5 --> TR

    R1 --> TD
    R2 --> TD
    R3 --> TD
    R4 --> TD
    R5 --> TD

    P --> SR
    A --> SR
    R --> SR
    TR --> SR
    TD --> SR

    style R1 fill:#fff9c4
    style R2 fill:#c8e6c9
    style R3 fill:#ef9a9a
    style R4 fill:#fff9c4
    style R5 fill:#c8e6c9
    style SR fill:#e1f5ff
```

---

## 🎯 Conclusion

Ces diagrammes illustrent:

1. **Transformation complète** des données du format COBOL fixe vers objets Java
2. **Conservation de la logique** métier à travers tous les calculs
3. **Mappage 1:1** des structures de données (avec enrichissement)
4. **Flux identique** malgré des architectures différentes
5. **Traçabilité parfaite** de chaque transformation

Le convertisseur génère du code Java qui:
- ✅ Respecte la sémantique COBOL
- ✅ Utilise les types appropriés (BigDecimal pour l'argent)
- ✅ Maintient la précision des calculs
- ✅ Préserve l'ordre des opérations
- ✅ Génère le même résultat final
