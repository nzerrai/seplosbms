# 🚀 Intégration du Système d'Inférence Algorithmique à l'IHM

**Date**: 2026-01-12  
**Auteur**: GitHub Copilot  
**Version**: 1.0  
**Status**: 🟢 PROPOSITION DE DESIGN

---

## 📋 Vue d'ensemble

L'IHM actuelle affiche un rapport de conversion avec statistiques. Cette proposition intègre le **système d'inférence algorithmique** pour enrichir la restitution avec:

1. **Analyse des champs inférés** - Quels champs ont été détectés automatiquement
2. **Visualisation des types** - Tableau des types inférés (BigDecimal, Long, LocalDate, etc.)
3. **Statistiques d'inférence** - Champs détectés vs layout COBOL
4. **Confiance de l'inférence** - Score basé sur les contextes détectés
5. **Recommandations** - Actions suggérées pour valider/corriger

---

## 🎨 Mockup - Rapport de Conversion Enrichi

```
┌─────────────────────────────────────────────────────────────────┐
│  📊 RAPPORT DE CONVERSION - COBOL TO JAVA                       │
└─────────────────────────────────────────────────────────────────┘

┌─ Métriques Générales ────────────────────────────────────────┐
│  Taux de conversion: ████████░░ 85%                          │
│  Confiance globale:  🟢 TRÈS HAUTE (92%)                    │
│  Temps: 2.3s                                                │
└──────────────────────────────────────────────────────────────┘

┌─ Inférence Algorithmique ────────────────────────────────────┐
│                                                              │
│  📈 Résumé Inférence:                                       │
│     ✅ 7 champs inférés                                     │
│     ✅ 6 champs du layout COBOL                             │
│     ✅ 0 duplicates évités                                  │
│     ⚠️  3 champs non détectés (type String par défaut)      │
│                                                              │
│  🔍 Champs Inférés Détectés:                               │
│  ┌─────────────────┬──────────┬─────────────┬──────────┐   │
│  │ Nom du Champ    │ Type Java│ Contexte    │ Confiance│   │
│  ├─────────────────┼──────────┼─────────────┼──────────┤   │
│  │ CustId          │ Long     │ ID_NAMES    │ 🟢 100%  │   │
│  │ CustName        │ String   │ STRING_OPS  │ 🟢 95%   │   │
│  │ CustBalance     │ BigDecimal│ARITHMETIC  │ 🟢 100%  │   │
│  │ TxnAmount       │ BigDecimal│MONETARY    │ 🟢 98%   │   │
│  │ TxnDate         │ LocalDate│ DATE_OPS   │ 🟢 100%  │   │
│  │ TxnStatus       │ String   │ STATUS_OPS │ 🟡 75%   │   │
│  │ TxnDescription  │ String   │ STRING_OPS │ 🟢 90%   │   │
│  └─────────────────┴──────────┴─────────────┴──────────┘   │
│                                                              │
│  💡 Cas Intéressants:                                       │
│     • CustBalance inféré comme BigDecimal (opérations     │
│       arithmétiques détectées + pattern "balance")          │
│     • TxnStatus: Confiance moyenne → vérifier si           │
│       devrait être ENUM plutôt que String                   │
│                                                              │
└──────────────────────────────────────────────────────────────┘

┌─ Analyse des Types Inférés ──────────────────────────────────┐
│                                                              │
│  Distribution par Type:                                     │
│  ┌────────────┬───────┬─────────────────────────────────┐  │
│  │ Type       │ Count │ Visualisation                   │  │
│  ├────────────┼───────┼─────────────────────────────────┤  │
│  │ String     │   4   │ ████░░░░░░░░░░░░░░░░░░░░░░░░  │  │
│  │ BigDecimal │   2   │ ██░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │
│  │ Long       │   1   │ █░░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │
│  │ LocalDate  │   1   │ █░░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │
│  └────────────┴───────┴─────────────────────────────────┘  │
│                                                              │
│  Tendances:                                                 │
│  • Majoritairement String (40%) - champs texte du COBOL    │
│  • BigDecimal pour les montants (20%) - bon détection      │
│  • Types spécialisés (30%) - Long/LocalDate bien inférés   │
│                                                              │
└──────────────────────────────────────────────────────────────┘

┌─ Contextes Détectés ─────────────────────────────────────────┐
│                                                              │
│  Heatmap des Contextes d'Utilisation:                       │
│                                                              │
│  ARITHMETIC_CONTEXT     ███░░░░░░  8 occurrences          │
│  STRING_OPS             ██░░░░░░░  5 occurrences          │
│  MONETARY_CONTEXT       ███░░░░░░  7 occurrences          │
│  DATE_OPS              █░░░░░░░░  2 occurrences          │
│  COMPARISON_CONTEXT     ██░░░░░░░  3 occurrences          │
│  BOOLEAN_CONTEXT        █░░░░░░░░  1 occurrence           │
│                                                              │
└──────────────────────────────────────────────────────────────┘

┌─ Évaluation de la Qualité ───────────────────────────────────┐
│                                                              │
│  ✅ Très Bon (90-100%):        5 champs  ████░░░░░░      │
│  🟡 Bon (75-89%):              1 champ   █░░░░░░░░░      │
│  🟠 À Vérifier (50-74%):       1 champ   █░░░░░░░░░      │
│  ❌ Faible (<50%):             0 champ                     │
│                                                              │
│  Score Global: 92/100 🏆                                   │
│                                                              │
└──────────────────────────────────────────────────────────────┘

┌─ Recommandations ────────────────────────────────────────────┐
│                                                              │
│  1. ✅ TxnStatus pourrait être un ENUM (values: P, E, C)  │
│     Suggestion: Remplacer String par enum TxnStatus        │
│     Action: Générer classe enum + mettre à jour entity     │
│                                                              │
│  2. ⚠️  Vérifier que 3 champs non détectés sont corrects  │
│     • WsFileStatus (working storage - bon)                 │
│     • WsErrorCount (working storage - bon)                 │
│     • WsCurrentDate (working storage - bon)                │
│                                                              │
│  3. 💡 Optimisation: BigDecimal monétaires avec scale=2   │
│     • CustBalance, TxnAmount → utiliser @Digits(19,2)     │
│     • Validation JPA recommandée                           │
│                                                              │
│  4. 📚 Générer documentation OpenAPI avec types inférés   │
│     Cette info sera disponible pour /swagger-ui            │
│                                                              │
└──────────────────────────────────────────────────────────────┘

┌─ Fichiers Générés ───────────────────────────────────────────┐
│                                                              │
│  ✓ CustomerRecord.java          (entity enrichie)          │
│  ✓ TransactionRecord.java        (entity enrichie)         │
│  ✓ CustomerProcessor.java        (processor)               │
│  ✓ TransactionProcessor.java     (processor)               │
│  ✓ TxnStatus.java               ⭐ ENUM GÉNÉRÉ             │
│  ✓ BatchConfiguration.java       (spring batch config)     │
│  ✓ Inference-Report.json         ⭐ RAPPORT INFÉRENCE      │
│                                                              │
└──────────────────────────────────────────────────────────────┘

┌─ Actions ────────────────────────────────────────────────────┐
│                                                              │
│  [⬇️  Télécharger le projet]  [📊 Afficher rapport JSON]  │
│  [🔍 Afficher détails]        [🔄 Nouvelle conversion]    │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

---

## 🔧 Architecture Technique

### 1. **Backend - Nouvelle Classe: `InferenceReportData`**

```java
@Data
@AllArgsConstructor
public class InferenceReportData {
    
    // Résumé général
    private int totalFieldsInferred;        // 7
    private int fieldsFromLayout;           // 6
    private int uniqueFieldsAdded;          // 1
    private int duplicatesAvoided;          // 0
    private int fieldsNotDetected;          // 3
    
    // Détail par champ
    private List<InferredField> inferredFields;  // [{name, type, contexts, confidence}, ...]
    
    // Statistiques par type
    private Map<String, Integer> typeDistribution;  // {String: 4, BigDecimal: 2, ...}
    
    // Contextes détectés
    private Map<String, Integer> contextStats;  // {ARITHMETIC: 8, STRING_OPS: 5, ...}
    
    // Score global
    private double overallQualityScore;     // 92.0
    
    // Recommandations
    private List<String> recommendations;
    
    // Timing
    private long inferenceTimeMs;
}

@Data
public class InferredField {
    private String fieldName;                       // "CustId"
    private String javaType;                        // "Long"
    private Set<UsageContext> contexts;            // [ID_NAMES, NUMERIC_COMPARISON]
    private double confidenceScore;                 // 98.5
    private String confidenceIcon;                  // "🟢"
    private String confidenceLevel;                 // "VERY_HIGH"
    private String reasoning;                       // "Pattern 'id' detected + numeric comparison usage"
    private boolean isFromLayout;                   // false (= inferred)
}
```

### 2. **ProcessorGenerator - Modification**

```java
public ProcessorGenerationResult generate(...) {
    // ... code existant ...
    
    // Nouveau: créer InferenceReportData
    InferenceReportData inferenceData = buildInferenceReport(
        allReferences, 
        entityReferences, 
        inferredTypes,
        existingFields
    );
    
    result.setInferenceReport(inferenceData);
    return result;
}

private InferenceReportData buildInferenceReport(...) {
    InferenceReportData report = new InferenceReportData();
    
    // Remplir rapport avec les stats
    report.setTotalFieldsInferred(inferredTypes.size());
    report.setFieldsFromLayout(existingFields.size());
    report.setInferredFields(convertToInferredFields(inferredTypes));
    // ... etc
    
    return report;
}
```

### 3. **ConversionResult/ConversionResponse - Modification**

```java
@Data
public class ConversionResult {
    // ... existing fields ...
    private InferenceReportData inferenceReport;  // ← NEW
}

@Data
public class ConversionResponse {
    // ... existing fields ...
    private InferenceReportData inferenceReport;  // ← NEW
}
```

### 4. **CobolConversionService - Modification**

```java
public ConversionResult convert(...) {
    // ... existing code ...
    
    ProcessorGenerationResult processorResult = 
        processorGenerator.generate(...);
    
    // Récupérer le rapport d'inférence
    InferenceReportData inferenceData = 
        processorResult.getInferenceReport();
    
    result.setInferenceReport(inferenceData);
    return result;
}
```

---

## 🎨 Frontend - IHM

### 1. **HTML - Nouvelle Section dans `conversion.html`**

```html
<!-- Inférence Algorithmique Section -->
<div id="inferenceSection" class="inference-section hidden">
    
    <!-- Résumé rapide -->
    <div class="inference-summary">
        <h3>📈 Système d'Inférence Algorithmique</h3>
        <div class="inference-quick-stats">
            <div class="quick-stat">
                <span class="stat-value" id="inferredCount">7</span>
                <span class="stat-label">Champs Inférés</span>
            </div>
            <div class="quick-stat">
                <span class="stat-value" id="duplicatesAvoided">0</span>
                <span class="stat-label">Duplicates Évités</span>
            </div>
            <div class="quick-stat">
                <span class="stat-value" id="inferenceScore">92%</span>
                <span class="stat-label">Score de Qualité</span>
            </div>
            <div class="quick-stat">
                <span class="stat-value" id="inferenceTime">142ms</span>
                <span class="stat-label">Temps d'Inférence</span>
            </div>
        </div>
    </div>
    
    <!-- Tableau détaillé -->
    <div class="inference-details">
        <h4>🔍 Détail des Champs Inférés</h4>
        <table class="inference-table">
            <thead>
                <tr>
                    <th>Nom du Champ</th>
                    <th>Type Java</th>
                    <th>Contextes Détectés</th>
                    <th>Confiance</th>
                    <th>Statut</th>
                </tr>
            </thead>
            <tbody id="inferenceTableBody">
                <!-- Dynamique -->
            </tbody>
        </table>
    </div>
    
    <!-- Distribution des types -->
    <div class="type-distribution">
        <h4>📊 Distribution par Type</h4>
        <div class="distribution-chart" id="typeDistributionChart">
            <!-- Chart.js ou simple SVG -->
        </div>
    </div>
    
    <!-- Heatmap des contextes -->
    <div class="context-heatmap">
        <h4>🔥 Contextes Détectés</h4>
        <div id="contextHeatmap"></div>
    </div>
    
    <!-- Recommandations -->
    <div class="inference-recommendations">
        <h4>💡 Recommandations</h4>
        <ul id="recommendationsList"></ul>
    </div>
    
</div>
```

### 2. **CSS - Styling**

```css
.inference-section {
    background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
    border-radius: 12px;
    padding: 24px;
    margin: 24px 0;
    border-left: 5px solid #5B9BD5;
}

.inference-summary {
    background: white;
    border-radius: 8px;
    padding: 16px;
    margin-bottom: 24px;
}

.inference-quick-stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
    gap: 12px;
    margin-top: 12px;
}

.quick-stat {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 12px;
    border-radius: 8px;
    text-align: center;
}

.stat-value {
    display: block;
    font-size: 24px;
    font-weight: bold;
}

.stat-label {
    display: block;
    font-size: 11px;
    margin-top: 4px;
    opacity: 0.9;
}

.inference-table {
    width: 100%;
    border-collapse: collapse;
    background: white;
    border-radius: 8px;
    overflow: hidden;
}

.inference-table th {
    background: #2c3e50;
    color: white;
    padding: 12px;
    text-align: left;
    font-weight: 600;
}

.inference-table td {
    padding: 12px;
    border-bottom: 1px solid #ecf0f1;
}

.inference-table tr:hover {
    background: #f8f9fa;
}

/* Indicateurs de confiance */
.confidence-badge {
    display: inline-block;
    padding: 4px 8px;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 600;
}

.confidence-badge.very-high {
    background: #d4edda;
    color: #155724;
}

.confidence-badge.high {
    background: #d1ecf1;
    color: #0c5460;
}

.confidence-badge.medium {
    background: #fff3cd;
    color: #856404;
}

/* Contextes */
.context-tag {
    display: inline-block;
    background: #e7f3ff;
    color: #0066cc;
    padding: 4px 8px;
    border-radius: 4px;
    font-size: 11px;
    margin-right: 4px;
}

/* Distribution chart */
.distribution-chart {
    height: 300px;
    background: white;
    border-radius: 8px;
    padding: 16px;
}

/* Heatmap */
.context-heatmap {
    background: white;
    border-radius: 8px;
    padding: 16px;
}

.heatmap-row {
    display: flex;
    gap: 12px;
    margin-bottom: 12px;
    align-items: center;
}

.heatmap-label {
    width: 150px;
    font-size: 12px;
    font-weight: 600;
}

.heatmap-bar {
    flex: 1;
    height: 24px;
    background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
    border-radius: 4px;
    display: flex;
    align-items: center;
    padding: 0 8px;
    color: white;
    font-size: 11px;
    font-weight: 600;
}

/* Recommandations */
.inference-recommendations {
    background: white;
    border-radius: 8px;
    padding: 16px;
    margin-top: 24px;
}

.inference-recommendations ul {
    list-style: none;
    padding: 0;
}

.inference-recommendations li {
    padding: 8px;
    margin-bottom: 8px;
    border-left: 3px solid #667eea;
    padding-left: 12px;
    background: #f8f9fa;
}

.inference-recommendations li:before {
    content: attr(data-icon);
    margin-right: 8px;
}
```

### 3. **JavaScript - Affichage Dynamique**

```javascript
function displayInferenceReport(inferenceData) {
    if (!inferenceData) return;
    
    // Afficher la section
    document.getElementById('inferenceSection').classList.remove('hidden');
    
    // Remplir les stats rapides
    document.getElementById('inferredCount').textContent = 
        inferenceData.totalFieldsInferred;
    document.getElementById('duplicatesAvoided').textContent = 
        inferenceData.duplicatesAvoided;
    document.getElementById('inferenceScore').textContent = 
        Math.round(inferenceData.overallQualityScore) + '%';
    document.getElementById('inferenceTime').textContent = 
        inferenceData.inferenceTimeMs + 'ms';
    
    // Tableau des champs
    renderInferenceTable(inferenceData.inferredFields);
    
    // Distribution des types
    renderTypeDistribution(inferenceData.typeDistribution);
    
    // Heatmap des contextes
    renderContextHeatmap(inferenceData.contextStats);
    
    // Recommandations
    renderRecommendations(inferenceData.recommendations);
}

function renderInferenceTable(fields) {
    const tbody = document.getElementById('inferenceTableBody');
    tbody.innerHTML = '';
    
    fields.forEach(field => {
        const row = document.createElement('tr');
        row.innerHTML = `
            <td><code>${field.fieldName}</code></td>
            <td><span class="type-badge">${field.javaType}</span></td>
            <td>
                ${field.contexts.map(ctx => 
                    `<span class="context-tag">${ctx}</span>`
                ).join('')}
            </td>
            <td>
                ${field.confidenceIcon} 
                <span class="confidence-badge ${field.confidenceLevel.toLowerCase()}">
                    ${Math.round(field.confidenceScore)}%
                </span>
            </td>
            <td>
                ${field.isFromLayout ? 
                    '<span class="status-badge layout">📋 Layout</span>' :
                    '<span class="status-badge inferred">⭐ Inféré</span>'}
            </td>
        `;
        tbody.appendChild(row);
    });
}

function renderTypeDistribution(distribution) {
    const colors = {
        'String': '#667eea',
        'BigDecimal': '#764ba2',
        'Long': '#f093fb',
        'Integer': '#4facfe',
        'LocalDate': '#43e97b',
        'Boolean': '#fa709a'
    };
    
    const chartContainer = document.getElementById('typeDistributionChart');
    const total = Object.values(distribution).reduce((a, b) => a + b, 0);
    
    let html = '';
    Object.entries(distribution).forEach(([type, count]) => {
        const percentage = (count / total * 100).toFixed(1);
        html += `
            <div class="heatmap-row">
                <div class="heatmap-label">${type}</div>
                <div class="heatmap-bar" style="width: ${percentage}%; background: ${colors[type] || '#999'};">
                    ${count}
                </div>
            </div>
        `;
    });
    
    chartContainer.innerHTML = html;
}

function renderContextHeatmap(contextStats) {
    const contextNames = {
        'ARITHMETIC_CONTEXT': 'Opérations Arithmétiques',
        'STRING_OPS': 'Opérations Texte',
        'MONETARY_CONTEXT': 'Contexte Monétaire',
        'DATE_OPS': 'Opérations Dates',
        'COMPARISON_CONTEXT': 'Comparaisons',
        'BOOLEAN_CONTEXT': 'Contexte Booléen'
    };
    
    const heatmap = document.getElementById('contextHeatmap');
    const maxCount = Math.max(...Object.values(contextStats));
    
    let html = '';
    Object.entries(contextStats).forEach(([context, count]) => {
        const percentage = (count / maxCount * 100).toFixed(1);
        html += `
            <div class="heatmap-row">
                <div class="heatmap-label">${contextNames[context] || context}</div>
                <div class="heatmap-bar" style="width: ${percentage}%;">
                    ${count}
                </div>
            </div>
        `;
    });
    
    heatmap.innerHTML = html;
}

function renderRecommendations(recommendations) {
    const list = document.getElementById('recommendationsList');
    list.innerHTML = '';
    
    recommendations.forEach((rec, idx) => {
        const li = document.createElement('li');
        const icon = rec.includes('ENUM') ? '🏷️' :
                    rec.includes('vérifier') ? '⚠️' :
                    rec.includes('Optimisation') ? '⚡' : '💡';
        li.setAttribute('data-icon', icon);
        li.textContent = rec;
        list.appendChild(li);
    });
}
```

---

## 📊 Format JSON Étendu

```json
{
  "success": true,
  "message": "Conversion completed successfully",
  "projectName": "CustomerBatch",
  "zipFileBase64": "UEsDBBQACAgIAA...",
  
  "report": {
    "programName": "CUSTOMER-PROCESSOR",
    "conversionPercentage": 85.0,
    "confidenceLevel": "TRÈS HAUTE",
    "confidenceIcon": "🟢"
  },
  
  "inferenceReport": {
    "totalFieldsInferred": 7,
    "fieldsFromLayout": 6,
    "uniqueFieldsAdded": 1,
    "duplicatesAvoided": 0,
    "fieldsNotDetected": 3,
    "overallQualityScore": 92.0,
    "inferenceTimeMs": 142,
    
    "inferredFields": [
      {
        "fieldName": "CustId",
        "javaType": "Long",
        "contexts": ["ID_NAMES", "NUMERIC_COMPARISON"],
        "confidenceScore": 98.5,
        "confidenceIcon": "🟢",
        "confidenceLevel": "VERY_HIGH",
        "reasoning": "Pattern 'id' detected + numeric comparison usage",
        "isFromLayout": false
      },
      {
        "fieldName": "CustBalance",
        "javaType": "BigDecimal",
        "contexts": ["ARITHMETIC_CONTEXT", "MONETARY_CONTEXT"],
        "confidenceScore": 100.0,
        "confidenceIcon": "🟢",
        "confidenceLevel": "VERY_HIGH",
        "reasoning": "Arithmetic operations + monetary field pattern",
        "isFromLayout": true
      }
    ],
    
    "typeDistribution": {
      "String": 4,
      "BigDecimal": 2,
      "Long": 1,
      "LocalDate": 1
    },
    
    "contextStats": {
      "ARITHMETIC_CONTEXT": 8,
      "STRING_OPS": 5,
      "MONETARY_CONTEXT": 7,
      "DATE_OPS": 2,
      "COMPARISON_CONTEXT": 3,
      "BOOLEAN_CONTEXT": 1
    },
    
    "recommendations": [
      "TxnStatus pourrait être un ENUM avec values: P, E, C",
      "BigDecimal fields devraient avoir @Digits(19,2) validation",
      "3 champs non détectés sont probablement correct (working storage)",
      "Générer documentation OpenAPI avec types inférés"
    ]
  }
}
```

---

## 🎯 Cas d'Usage - Affichage en Action

### Scénario 1: Conversion Excellente (92%)

```
┌─ Rapport Affiché ───────────────────────────────────┐
│ ✅ Conversion 85% | 🟢 Confiance TRÈS HAUTE        │
│                                                     │
│ 📈 Inférence Algorithmique                         │
│    ✓ 7 champs inférés avec 92% de qualité         │
│    ✓ Types: 4×String, 2×BigDecimal, 1×Long        │
│    ✓ 0 duplicates, génération propre               │
│                                                     │
│ 💡 Recommandations:                                │
│    • TxnStatus → enum                              │
│    • Validation @Digits pour montants              │
│                                                     │
│ [⬇️ Télécharger] [🔍 Détails] [🔄 Nouveau]        │
└─────────────────────────────────────────────────────┘
```

### Scénario 2: Conversion Partielle (50%)

```
┌─ Rapport Affiché ───────────────────────────────────┐
│ ⚠️  Conversion 50% | 🟡 Confiance MOYENNE          │
│                                                     │
│ 📈 Inférence Algorithmique                         │
│    ⚠️  3 champs inférés (confiance 62%)            │
│    ❌ 5 champs non détectés                        │
│    ⚠️  Possibles duplicates                        │
│                                                     │
│ ⚠️  Avertissements:                                │
│    • Vérifier types générés (nombreux defaults)   │
│    • Copybooks non résolus → utiliser String      │
│    • Recommandé: éditer entity manuellement        │
│                                                     │
│ [⬇️ Télécharger] [📋 Voir détails] [🔄 Nouveau]   │
└─────────────────────────────────────────────────────┘
```

---

## 🚀 Plan d'Implémentation

### Phase 1: Backend (2 jours)
- [ ] Créer `InferenceReportData` et `InferredField`
- [ ] Modifier `ProcessorGenerationResult` pour inclure rapport
- [ ] Modifier `ProcessorGenerator.buildInferenceReport()`
- [ ] Tester sérialisation JSON

### Phase 2: Frontend Basique (1 jour)
- [ ] Ajouter HTML section (tableau + stats)
- [ ] Ajouter CSS styling
- [ ] Intégrer JavaScript pour affichage simple

### Phase 3: Visualisations (2 jours)
- [ ] Implémenter Chart.js pour distribution types
- [ ] Implémenter heatmap contextes
- [ ] Ajouter animations CSS

### Phase 4: Recommandations (1 jour)
- [ ] Engine de recommandations basé sur règles
- [ ] Affichage intelligent des suggestions
- [ ] Tests avec différents scénarios

### Phase 5: Documentation (1 jour)
- [ ] Guide utilisateur
- [ ] Documentation technique
- [ ] Exemples d'interprétation

---

## ✨ Avantages de cette Approche

1. **Transparence** - Utilisateur voit exactement quoi a été inféré
2. **Confiance** - Scores de confiance montrent fiabilité
3. **Éducation** - Utilisateur apprend comment l'inférence marche
4. **Debuggable** - Contextes détectés aident à debugger erreurs
5. **Actionnable** - Recommandations guident corrections

---

## 📚 Intégration avec Autres Features

- **API OpenAPI**: Utiliser `inferenceReport` pour documenter types
- **Code Review UI**: Afficher champs inférés vs layout COBOL côte à côte
- **Export PDF**: Inclure rapport d'inférence dans documentation générée
- **Webhooks**: Notifier si qualité < threshold

---

**Prochaine Étape**: Valider design avec UX team, puis implémenter Phase 1
