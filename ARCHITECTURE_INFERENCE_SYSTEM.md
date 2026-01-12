# Architecture Globale: Système d'Inférence Algorithmique IHM

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         UTILISATEUR (Browser)                           │
│                                                                         │
│  Upload COBOL files + JCL → conversion.html → JavaScript affichage    │
└──────────────────────────────┬──────────────────────────────────────────┘
                               │
                    HTTP POST /conversion/upload
                               │
┌──────────────────────────────▼──────────────────────────────────────────┐
│                      SPRING BOOT CONTROLLER                             │
│         ConversionController.uploadAndConvert()                         │
│                                                                         │
│  • Parse multipart files (COBOL + JCL)                                 │
│  • Créer temp directory                                                │
│  • Appeler ConversionService                                           │
└──────────────────────────────┬──────────────────────────────────────────┘
                               │
                    invoke conversionService
                               │
┌──────────────────────────────▼──────────────────────────────────────────┐
│                    COBOL CONVERSION SERVICE                             │
│    CobolConversionService.convertToSpringBatchProject()                │
│                                                                         │
│  • Parse COBOL files (ANTLR4)                                          │
│  • Generate AST (Abstract Syntax Tree)                                 │
│  • Création du projet Spring Batch                                     │
│  • Retour ConversionResult                                             │
└──────────────────────────────┬──────────────────────────────────────────┘
                               │
                    return ConversionResult
                               │
┌──────────────────────────────▼──────────────────────────────────────────┐
│                  PROCESSOR GENERATION (NEW)                             │
│         ProcessorGenerator.generate()                                   │
│                                                                         │
│  ╔══════════════════════════════════════════════════════════════╗      │
│  ║ ALGORITHMIC INFERENCE SYSTEM (buildInferenceReport)        ║      │
│  ║                                                             ║      │
│  ║ 1. FieldReferenceAnalyzer                                 ║      │
│  ║    ├─ Analyze processor code                              ║      │
│  ║    ├─ Extract field references                            ║      │
│  ║    └─ Filter entity fields                                ║      │
│  ║                                                             ║      │
│  ║ 2. TypeInferenceEngine                                    ║      │
│  ║    ├─ Infer optimal Java types                            ║      │
│  ║    ├─ Map COBOL types to Java                             ║      │
│  ║    └─ Return Map<fieldName, javaType>                     ║      │
│  ║                                                             ║      │
│  ║ 3. Confidence Scoring (Multi-Factor Algorithm)            ║      │
│  ║    ├─ Base score: 0.70                                    ║      │
│  ║    ├─ + Reference count bonus (0-0.15)                    ║      │
│  ║    ├─ + Context diversity bonus (0-0.10)                  ║      │
│  ║    ├─ + Type consistency bonus (0-0.05)                   ║      │
│  ║    └─ Final: min(1.0, total)                              ║      │
│  ║                                                             ║      │
│  ║ 4. InferredField Population                               ║      │
│  ║    ├─ fieldName, javaType                                 ║      │
│  ║    ├─ confidenceScore, confidenceLevel                    ║      │
│  ║    ├─ usageContexts, referenceCount                       ║      │
│  ║    ├─ reasoning, suggestedAnnotations                     ║      │
│  ║    └─ Store in Map<fieldName, InferredField>              ║      │
│  ║                                                             ║      │
│  ║ 5. Quality Metrics Calculation                            ║      │
│  ║    ├─ Type distribution analysis                          ║      │
│  ║    ├─ Context statistics                                  ║      │
│  ║    ├─ Overall quality score (avg confidence * coverage)   ║      │
│  ║    └─ Quality level (EXCELLENT/GOOD/FAIR/POOR)            ║      │
│  ║                                                             ║      │
│  ║ 6. Smart Recommendations Engine                           ║      │
│  ║    ├─ Rule 1: Status/Code < 80% → suggest Enum           ║      │
│  ║    ├─ Rule 2: BigDecimal → @Digits annotation             ║      │
│  ║    ├─ Rule 3: >30% low confidence → manual review         ║      │
│  ║    ├─ Rule 4: Date fields → LocalDate conversion          ║      │
│  ║    ├─ Rule 5: >70% high confidence → positive feedback    ║      │
│  ║    └─ Rule 6: Complex types → custom converters           ║      │
│  ║                                                             ║      │
│  ║ 7. Conversion Metrics Assembly                            ║      │
│  ║    ├─ automatedTypeCount                                  ║      │
│  ║    ├─ manualTypeCount                                     ║      │
│  ║    ├─ automationPercentage                                ║      │
│  ║    └─ averageConfidence                                   ║      │
│  ║                                                             ║      │
│  ║ 8. Layer Breakdown Analysis                               ║      │
│  ║    ├─ fromLayout (count)                                  ║      │
│  ║    ├─ fromInference (count)                               ║      │
│  ║    └─ deduplicatedCount                                   ║      │
│  ║                                                             ║      │
│  ║ OUTPUT: InferenceReportData                               ║      │
│  ║  ├─ totalFieldsInferred: 42                               ║      │
│  ║  ├─ typeDistribution: {String: 15, BigDecimal: 12, ...}   ║      │
│  ║  ├─ inferredFieldsMap: {field: InferredField, ...}        ║      │
│  ║  ├─ overallQualityScore: 87                               ║      │
│  ║  ├─ recommendations: [recommendation1, ...]               ║      │
│  ║  ├─ conversionMetrics: ConversionMetrics                  ║      │
│  ║  └─ layerBreakdown: LayerBreakdown                        ║      │
│  ║                                                             ║      │
│  ╚══════════════════════════════════════════════════════════════╝      │
│                                                                         │
│  return ProcessorGenerationResult(file, fields, type, inferenceData)  │
└──────────────────────────────┬──────────────────────────────────────────┘
                               │
            return ProcessorGenerationResult
                               │
┌──────────────────────────────▼──────────────────────────────────────────┐
│                    RESPONSE ASSEMBLY                                    │
│         ConversionController.uploadAndConvert()                         │
│                                                                         │
│  ConversionResponse.success(message, projectName, result,              │
│                            processorGenerationResult)                   │
│                                                                         │
│  Sets: inferenceReport = processorResult.getInferenceReportData()      │
│                                                                         │
│  Encodes: zipFileBase64 = Base64.encode(zip)                           │
└──────────────────────────────┬──────────────────────────────────────────┘
                               │
                    HTTP 200 OK (JSON)
                               │
┌──────────────────────────────▼──────────────────────────────────────────┐
│                  FRONTEND RESPONSE PROCESSING                           │
│         conversion.js - showSuccessWithReport()                        │
│                                                                         │
│  if (responseData.inferenceReport) {                                   │
│      displayInferenceReport(responseData.inferenceReport)               │
│  }                                                                      │
│                                                                         │
│  Calls specialized rendering functions:                                │
│  • displayInferredFields(fieldsMap)      ─→ Render table               │
│  • displayTypeDistribution(distribution) ─→ Render chart               │
│  • displayRecommendations(list)          ─→ Render cards               │
└──────────────────────────────┬──────────────────────────────────────────┘
                               │
                    Update DOM + apply CSS
                               │
┌──────────────────────────────▼──────────────────────────────────────────┐
│                    USER SEES RENDERED REPORT                            │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐  │
│  │ 🤖 Rapport d'inférence de champs                               │  │
│  │                                                                 │  │
│  │ Champs inférés: 42  |  Qualité: 87%  |  Confiance moy: 82%   │  │
│  │                                                                 │  │
│  │ ┌────────────────────────────────────────────────────────────┐│  │
│  │ │ 📋 Champs inférés                                         ││  │
│  │ ├────────────────────────────────────────────────────────────┤│  │
│  │ │ Champ | Type Java | Confiance | Contextes | Suggestions   ││  │
│  │ ├────────────────────────────────────────────────────────────┤│  │
│  │ │ accountBalance | BigDecimal | VERY_HIGH | NUMERIC | @Digits││  │
│  │ │ transactionDate | LocalDate | HIGH | TEMPORAL | @PastOrPres││  │
│  │ │ statusCode | String | MEDIUM | STATUS | Suggest Enum      ││  │
│  │ └────────────────────────────────────────────────────────────┘│  │
│  │                                                                 │  │
│  │ ┌────────────────────────────────────────────────────────────┐│  │
│  │ │ 📊 Distribution des types Java                            ││  │
│  │ ├────────────────────────────────────────────────────────────┤│  │
│  │ │ String [15]  | BigDecimal [12]  | Integer [10] | ...      ││  │
│  │ └────────────────────────────────────────────────────────────┘│  │
│  │                                                                 │  │
│  │ ┌────────────────────────────────────────────────────────────┐│  │
│  │ │ 💡 Recommandations intelligentes                           ││  │
│  │ ├────────────────────────────────────────────────────────────┤│  │
│  │ │ ✅ Qualité d'inférence excellente...                      ││  │
│  │ │ 💡 Convertir 3 champs status en Enum...                  ││  │
│  │ │ 💡 Ajouter @Digits à 12 champs BigDecimal...            ││  │
│  │ └────────────────────────────────────────────────────────────┘│  │
│  └─────────────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────────────────┘
```

---

## Data Flow: Détail du Rapport d'Inférence

```
ConversionResult
    ├─ projectPath
    ├─ fileReports: List<FileConversionReport>
    │   ├─ fileName
    │   └─ report: ConversionReport
    │       ├─ totalStatements: 150
    │       ├─ convertedStatements: 145
    │       └─ ... (other conversion metrics)
    └─ generatedFiles

                          ↓ (+ ProcessorGenerationResult)

ProcessorGenerationResult
    ├─ processorFile
    ├─ inferredFields: Map<String, String>
    │   ├─ "accountBalance": "BigDecimal"
    │   ├─ "transactionDate": "LocalDate"
    │   └─ ...
    └─ inferenceReportData: InferenceReportData
        ├─ totalFieldsInferred: 42
        ├─ typeDistribution: Map<String, Integer>
        │   ├─ "BigDecimal": 12
        │   ├─ "String": 15
        │   ├─ "LocalDate": 8
        │   └─ ...
        ├─ contextStats: Map<String, Integer>
        ├─ overallQualityScore: 87
        ├─ qualityLevel: QualityLevel.GOOD
        ├─ recommendations: List<String>
        │   ├─ "✅ Qualité d'inférence excellente..."
        │   ├─ "💡 Convertir 3 status en Enum..."
        │   └─ ...
        ├─ inferredFieldsMap: Map<String, InferredField>
        │   └─ "accountBalance": InferredField
        │       ├─ fieldName: "accountBalance"
        │       ├─ javaType: "BigDecimal"
        │       ├─ confidenceScore: 0.94
        │       ├─ confidenceLevel: ConfidenceLevel.VERY_HIGH
        │       ├─ confidenceIcon: "VERY_HIGH"
        │       ├─ usageContexts: ["Usage:NUMERIC_CALCULATION", "FieldPattern:MONETARY"]
        │       ├─ referenceCount: 7
        │       ├─ isFromLayout: false
        │       ├─ reasoning: "Field 'accountBalance' inferred as 'BigDecimal' with very high confidence..."
        │       └─ suggestedAnnotations: ["@Digits(integer=19, fraction=2)", "@DecimalMin(\"0\")"]
        ├─ conversionMetrics: ConversionMetrics
        │   ├─ automatedTypeCount: 42
        │   ├─ manualTypeCount: 0
        │   ├─ automationPercentage: 100.0
        │   └─ averageConfidence: 0.82
        ├─ layerBreakdown: LayerBreakdown
        │   ├─ fromLayout: 15
        │   ├─ fromInference: 27
        │   └─ deduplicatedCount: 0
        └─ generatedAt: 2026-01-12T11:45:50+01:00

                          ↓ (+ ConversionResult)

ConversionResponse
    ├─ success: true
    ├─ message: "Conversion completed successfully"
    ├─ projectName: "my-project"
    ├─ zipFileBase64: "UEsDBAoA..."
    ├─ reports: List<ConversionReportSummary>
    │   └─ ... (file-level conversion reports)
    └─ inferenceReport: InferenceReportData (complete object above)

                          ↓ (JSON serialization)

HTTP Response Body (JSON)
{
  "success": true,
  "message": "Conversion completed successfully",
  "projectName": "my-project",
  "zipFileBase64": "UEsDBAoA...",
  "reports": [...],
  "inferenceReport": {
    "totalFieldsInferred": 42,
    "typeDistribution": {...},
    "inferredFieldsMap": {
      "accountBalance": {
        "fieldName": "accountBalance",
        "javaType": "BigDecimal",
        "confidenceScore": 0.94,
        "confidenceLevel": "VERY_HIGH",
        ...
      },
      ...
    },
    "overallQualityScore": 87,
    "qualityLevel": "GOOD",
    "recommendations": [...],
    "conversionMetrics": {...},
    "layerBreakdown": {...},
    "generatedAt": "2026-01-12T11:45:50+01:00"
  }
}

                          ↓ (JavaScript async/await)

displayInferenceReport(inferenceData) {
  // Update summary metrics
  // Render fields table
  // Render type distribution
  // Render recommendations
  // Show inference-report-section
}
```

---

## Classe de données: InferredField

```java
public class InferredField {
    // Identification
    private String fieldName;
    private String javaType;
    
    // Confidence (0.0-1.0)
    private double confidenceScore;
    private ConfidenceLevel confidenceLevel;
    private String confidenceIcon;
    
    // Analysis
    private List<String> usageContexts;
    private Integer referenceCount;
    private Boolean isFromLayout;
    
    // Generation
    private String reasoning;
    private List<String> suggestedAnnotations;
    
    // Enum
    public enum ConfidenceLevel {
        VERY_HIGH(0.9),
        HIGH(0.7),
        MEDIUM(0.5),
        LOW(0.3);
        
        public static ConfidenceLevel fromScore(double score) { ... }
    }
}
```

---

## Algorithme de Notation de Confiance (Pseudo-code)

```python
def calculateConfidenceScore(reference, fieldName, program):
    baseScore = 0.70  # Start at 70% (algorithmic inference)
    
    if reference is None:
        return baseScore
    
    # Factor 1: Reference Frequency (0-0.15 bonus)
    count = reference.getterCount + reference.setterCount
    countBonus = min(0.15, count * 0.02)  # Each ref adds 2%
    
    # Factor 2: Context Diversity (0-0.10 bonus)
    contextCount = len(reference.contexts)
    contextBonus = min(0.10, contextCount * 0.03)  # Each type adds 3%
    
    # Factor 3: Type Consistency (0-0.05 bonus)
    consistencyBonus = 0.05  # Base bonus
    
    # Pattern-based adjustments
    if "amount" in fieldName or "price" in fieldName:
        consistencyBonus = 0.05  # Numeric patterns
    elif "date" in fieldName or "time" in fieldName:
        consistencyBonus = 0.04  # Temporal patterns
    elif "code" in fieldName or "status" in fieldName:
        consistencyBonus = 0.03  # Enumeration patterns
    elif "flag" in fieldName or "indicator" in fieldName:
        consistencyBonus = 0.02  # Boolean patterns
    
    totalScore = min(1.0, baseScore + countBonus + contextBonus + consistencyBonus)
    return totalScore

# Example:
score = calculateConfidenceScore(ref_accountBalance, "accountBalance", program)
# baseScore=0.70 + refBonus=0.10 + contextBonus=0.09 + typeBonus=0.05 = 0.94
```

---

## Performance Characteristics

| Operation | Time | Space |
|-----------|------|-------|
| Analyze field references | O(n) | O(n) |
| Infer types | O(n) | O(n) |
| Calculate confidence scores | O(n) | O(1) |
| Build distribution | O(n) | O(m) where m = distinct types |
| Generate recommendations | O(n) | O(k) where k = rules |
| Total buildInferenceReport | **O(n)** | **O(n+m+k)** |

Where:
- n = number of inferred fields
- m = number of distinct Java types
- k = number of recommendation rules

---

## Integration Checklist

- [x] Phase 2.1: ConversionResponse + inferenceReport field
- [x] Phase 2.2: Enhanced factory method
- [x] Phase 2.3: buildInferenceReport() algorithm
- [x] Phase 2.4: ConversionController integration
- [x] Phase 3.1: HTML section + structure
- [x] Phase 3.2: CSS styling + dark mode
- [x] Phase 3.3: JavaScript display functions
- [x] Phase 4.1: Smart recommendations engine
- [x] Phase 4.2: Annotation suggestions
- [ ] Phase 5.1: Performance optimization (caching)
- [ ] Phase 5.2: End-to-end testing (banktran project)
- [ ] Phase 5.3: Benchmarking and metrics

---

*Architecture diagram created 2026-01-12*
