# 🚀 PHASE 2, 3, 4, 5 - FEUILLE DE ROUTE

**Status**: Phase 1 ✅ COMPLÉTÉE | Phase 2-5 À faire  
**Timeline estimée**: 6 jours (1 jour par phase sauf Phase 1 déjà fait)

---

## PHASE 2️⃣  - Intégration API & Response (1-2 jours)

### 2.1 Modifier ConversionResponse.java

**Fichier**: `src/main/java/com/cobol/translator/web/ConversionResponse.java`

```java
// Avant:
public class ConversionResponse {
    private boolean success;
    private String message;
    private String projectName;
    private String zipFileBase64;
    private ConversionReport report;  // ← Existant
}

// Après:
public class ConversionResponse {
    private boolean success;
    private String message;
    private String projectName;
    private String zipFileBase64;
    private ConversionReport report;              // ← Existant
    private InferenceReportData inferenceReport;  // ← NEW
    
    public InferenceReportData getInferenceReport() { ... }
    public void setInferenceReport(InferenceReportData inferenceReport) { ... }
}
```

### 2.2 Modifier ConversionController.java

**Fichier**: `src/main/java/com/cobol/translator/web/ConversionController.java`

Où se fait l'appel à ProcessorGenerator et où on construit la response:

```java
@PostMapping("/conversion/upload")
public ResponseEntity<ConversionResponse> upload(...) {
    // ... conversion logic ...
    
    // Récupérer le rapport d'inférence si présent
    if (processorResult.hasInferenceReport()) {
        response.setInferenceReport(processorResult.getInferenceReportData());
    }
    
    return ResponseEntity.ok(response);
}
```

### 2.3 Tester JSON Response

```bash
curl -X POST http://localhost:8080/api/conversion/upload \
  -F "file=@test.cobol" | jq '.inferenceReport'

# Output attendu:
{
  "totalFieldsInferred": 7,
  "overallQualityScore": 92,
  "qualityLevel": "EXCELLENT",
  "typeDistribution": {
    "String": 2,
    "Long": 1,
    "BigDecimal": 3,
    "LocalDate": 1
  },
  "contextStats": { ... },
  "recommendations": [ ... ],
  "inferredFieldsMap": { ... }
}
```

---

## PHASE 3️⃣  - Frontend HTML/CSS/JS (2 jours)

### 3.1 Ajouter Section HTML dans conversion.html

**Fichier**: `src/main/resources/templates/conversion.html`

**Insertion point**: Après `<div id="reportCardsContainer">` (ligne ~200)

```html
<!-- Section Inférence Algorithmique -->
<div id="inferenceSection" class="inference-section" style="display:none;">
    
    <h3 class="inference-title">📊 Rapport d'Inférence Algorithmique</h3>
    
    <!-- Quick Stats -->
    <div class="inference-quick-stats">
        <div class="quick-stat">
            <div class="stat-value" id="totalFieldsInferred">0</div>
            <div class="stat-label">Champs Inférés</div>
        </div>
        <div class="quick-stat">
            <div class="stat-value" id="qualityScore">0%</div>
            <div class="stat-label">Qualité</div>
        </div>
        <div class="quick-stat">
            <div class="stat-value" id="qualityLevel">-</div>
            <div class="stat-label">Niveau</div>
        </div>
    </div>
    
    <!-- Inference Table -->
    <table class="inference-table" id="inferenceTable">
        <thead>
            <tr>
                <th>Champ</th>
                <th>Type Java</th>
                <th>Confiance</th>
                <th>Contextes</th>
                <th>Annotations</th>
            </tr>
        </thead>
        <tbody id="inferenceTableBody">
            <!-- Généré par JavaScript -->
        </tbody>
    </table>
    
    <!-- Type Distribution -->
    <div class="inference-section-title">Distribution des Types</div>
    <div class="type-distribution" id="typeDistribution">
        <!-- Chart généré par JavaScript -->
    </div>
    
    <!-- Context Heatmap -->
    <div class="inference-section-title">Contextes d'Utilisation</div>
    <div class="context-heatmap" id="contextHeatmap">
        <!-- Heatmap générée par JavaScript -->
    </div>
    
    <!-- Recommendations -->
    <div class="inference-section-title">💡 Recommandations</div>
    <ul class="recommendations-list" id="recommendationsList">
        <!-- Généré par JavaScript -->
    </ul>
    
</div>
```

### 3.2 Ajouter CSS dans conversion.css

**Fichier**: `src/main/resources/static/css/conversion.css`

```css
/* Inference Section */
.inference-section {
    background: linear-gradient(135deg, #f5f7fa 0%, #f8f9fb 100%);
    border: 1px solid #e8ecf1;
    border-radius: 8px;
    padding: 20px;
    margin-top: 20px;
}

.inference-title {
    font-size: 18px;
    font-weight: 600;
    color: #2c3e50;
    margin-bottom: 20px;
    display: flex;
    align-items: center;
    gap: 10px;
}

/* Quick Stats */
.inference-quick-stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 15px;
    margin-bottom: 25px;
}

.quick-stat {
    background: white;
    border: 1px solid #e0e6ed;
    border-radius: 6px;
    padding: 15px;
    text-align: center;
}

.stat-value {
    font-size: 24px;
    font-weight: 700;
    color: #3498db;
    margin-bottom: 5px;
}

.stat-label {
    font-size: 12px;
    color: #7f8c8d;
    text-transform: uppercase;
    letter-spacing: 0.5px;
}

/* Inference Table */
.inference-table {
    width: 100%;
    border-collapse: collapse;
    margin: 15px 0;
    background: white;
    border: 1px solid #e0e6ed;
    border-radius: 6px;
    overflow: hidden;
}

.inference-table thead {
    background: #f8f9fa;
    font-weight: 600;
}

.inference-table th,
.inference-table td {
    padding: 12px 15px;
    text-align: left;
    border-bottom: 1px solid #e0e6ed;
}

/* Confidence Badge */
.confidence-badge {
    display: inline-block;
    padding: 4px 8px;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 600;
}

.confidence-very-high {
    background: #d4edda;
    color: #155724;
}

.confidence-high {
    background: #d1ecf1;
    color: #0c5460;
}

.confidence-medium {
    background: #fff3cd;
    color: #856404;
}

.confidence-low {
    background: #f8d7da;
    color: #721c24;
}

/* Type Distribution */
.type-distribution {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
    gap: 10px;
    margin-bottom: 20px;
}

.type-dist-item {
    background: white;
    border: 1px solid #e0e6ed;
    border-radius: 6px;
    padding: 12px;
    text-align: center;
}

.type-name {
    font-weight: 600;
    color: #2c3e50;
    font-size: 13px;
}

.type-count {
    font-size: 20px;
    color: #3498db;
    margin-top: 5px;
}

/* Context Heatmap */
.context-heatmap {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(100px, 1fr));
    gap: 10px;
    margin-bottom: 20px;
}

.context-tag {
    background: white;
    border: 1px solid #e0e6ed;
    border-radius: 4px;
    padding: 10px;
    text-align: center;
    font-size: 12px;
}

.context-tag.hot {
    background: #ffe6e6;
    border-color: #ff6b6b;
    color: #c92a2a;
    font-weight: 600;
}

.context-tag.warm {
    background: #fff3bf;
    border-color: #fcc419;
    color: #f08c00;
}

.context-tag.cool {
    background: #d0ebff;
    border-color: #1971c2;
    color: #1971c2;
}

/* Recommendations */
.recommendations-list {
    list-style: none;
    padding: 0;
}

.recommendations-list li {
    background: white;
    border-left: 4px solid #3498db;
    padding: 12px 15px;
    margin-bottom: 8px;
    border-radius: 4px;
    color: #2c3e50;
}

.inference-section-title {
    font-size: 14px;
    font-weight: 600;
    color: #2c3e50;
    margin: 20px 0 10px 0;
    text-transform: uppercase;
    letter-spacing: 0.5px;
}

/* Responsive */
@media (max-width: 768px) {
    .inference-quick-stats {
        grid-template-columns: 1fr;
    }
    
    .type-distribution {
        grid-template-columns: repeat(2, 1fr);
    }
    
    .inference-table {
        font-size: 12px;
    }
}
```

### 3.3 Ajouter JavaScript dans conversion.js

**Fichier**: `src/main/resources/static/js/conversion.js`

```javascript
/**
 * Affiche le rapport d'inférence dans le DOM
 */
function displayInferenceReport(inferenceData) {
    if (!inferenceData) {
        console.warn('No inference data to display');
        return;
    }
    
    const inferenceSection = document.getElementById('inferenceSection');
    if (!inferenceSection) {
        console.error('Inference section not found in DOM');
        return;
    }
    
    // Afficher les stats rapides
    document.getElementById('totalFieldsInferred').textContent = 
        inferenceData.totalFieldsInferred || 0;
    document.getElementById('qualityScore').textContent = 
        (inferenceData.overallQualityScore || 0) + '%';
    document.getElementById('qualityLevel').textContent = 
        inferenceData.qualityLevel || '-';
    
    // Afficher le tableau des champs
    renderInferenceTable(inferenceData.inferredFieldsMap);
    
    // Afficher la distribution des types
    renderTypeDistribution(inferenceData.typeDistribution);
    
    // Afficher la heatmap des contextes
    renderContextHeatmap(inferenceData.contextStats);
    
    // Afficher les recommandations
    renderRecommendations(inferenceData.recommendations);
    
    // Montrer la section
    inferenceSection.style.display = 'block';
}

/**
 * Render tableau des champs inférés
 */
function renderInferenceTable(fieldsMap) {
    const tbody = document.getElementById('inferenceTableBody');
    tbody.innerHTML = '';
    
    if (!fieldsMap) return;
    
    Object.values(fieldsMap).forEach(field => {
        const row = document.createElement('tr');
        
        const contextStr = field.usageContexts 
            ? field.usageContexts.join(', ') 
            : '-';
        
        const annotationStr = field.suggestedAnnotations
            ? field.suggestedAnnotations.join(', ')
            : '-';
        
        const confidenceClass = `confidence-${field.confidenceLevel?.toLowerCase()
            .replace('_', '-')}`;
        
        row.innerHTML = `
            <td>${field.fieldName}</td>
            <td><code>${field.javaType}</code></td>
            <td>
                <span class="confidence-badge ${confidenceClass}">
                    ${field.confidenceIcon} ${(field.confidenceScore * 100).toFixed(0)}%
                </span>
            </td>
            <td>${contextStr}</td>
            <td><small>${annotationStr}</small></td>
        `;
        
        tbody.appendChild(row);
    });
}

/**
 * Render distribution des types
 */
function renderTypeDistribution(distribution) {
    const container = document.getElementById('typeDistribution');
    container.innerHTML = '';
    
    if (!distribution) return;
    
    Object.entries(distribution).forEach(([type, count]) => {
        const item = document.createElement('div');
        item.className = 'type-dist-item';
        item.innerHTML = `
            <div class="type-name">${type}</div>
            <div class="type-count">${count}</div>
        `;
        container.appendChild(item);
    });
}

/**
 * Render heatmap des contextes
 */
function renderContextHeatmap(contextStats) {
    const container = document.getElementById('contextHeatmap');
    container.innerHTML = '';
    
    if (!contextStats) return;
    
    const entries = Object.entries(contextStats);
    const maxCount = Math.max(...entries.map(e => e[1]));
    
    entries.forEach(([context, count]) => {
        const intensity = count / maxCount;
        let tagClass = 'cool';
        if (intensity > 0.6) tagClass = 'hot';
        else if (intensity > 0.3) tagClass = 'warm';
        
        const tag = document.createElement('div');
        tag.className = `context-tag ${tagClass}`;
        tag.innerHTML = `<strong>${context}</strong><br/>${count}×`;
        container.appendChild(tag);
    });
}

/**
 * Render recommandations
 */
function renderRecommendations(recommendations) {
    const list = document.getElementById('recommendationsList');
    list.innerHTML = '';
    
    if (!recommendations || recommendations.length === 0) {
        const li = document.createElement('li');
        li.textContent = 'Aucune recommandation - Conversion de haute qualité ✓';
        list.appendChild(li);
        return;
    }
    
    recommendations.forEach(rec => {
        const li = document.createElement('li');
        li.textContent = rec;
        list.appendChild(li);
    });
}

// Intégration dans le handler de réponse
// (Modifie la fonction existante qui traite la réponse JSON)

// Chercher la fonction handleConversionResponse ou similaire et ajouter:
const originalResponse = ... // votre réponse JSON

if (originalResponse.inferenceReport) {
    displayInferenceReport(originalResponse.inferenceReport);
}
```

### 3.4 Vérification

```bash
# Tester l'upload + affichage
1. Ouvrir http://localhost:8080
2. Upload un fichier COBOL
3. Vérifier que section d'inférence apparaît
4. Vérifier JSON contient inferenceReport
```

---

## PHASE 4️⃣  - Recommandations Intelligentes (1 jour)

### 4.1 Implémenter buildRecommendations() dans ProcessorGenerator

```java
private List<String> buildRecommendations(InferenceReportData report) {
    List<String> recs = new ArrayList<>();
    
    // Rec 1: Si status string avec peu de valeurs → enum
    if (report.getInferredFieldsMap().values().stream()
        .anyMatch(f -> "String".equals(f.getJavaType()) 
                    && f.getConfidenceScore() < 0.8)) {
        recs.add("Considérez utiliser Enum pour les champs String de confiance < 80%");
    }
    
    // Rec 2: BigDecimal sans @Digits
    if (report.getTypeDistribution().getOrDefault("BigDecimal", 0) > 0) {
        recs.add("Ajouter @Digits(integer=19, fraction=2) pour les champs BigDecimal");
    }
    
    // Rec 3: Trop de champs inférés de confiance basse
    long lowConfidence = report.getInferredFieldsMap().values().stream()
        .filter(f -> f.getConfidenceScore() < 0.5).count();
    if (lowConfidence > 0) {
        recs.add("Vérifier manuellement les " + lowConfidence + " champs de faible confiance");
    }
    
    // Rec 4: Générer tests JPA
    if (report.getTotalFieldsInferred() > 0) {
        recs.add("Générer tests unitaires JPA pour les nouvelles entités");
    }
    
    return recs;
}
```

### 4.2 Enrichissement du rapport dans buildInferenceReport()

```java
public InferenceReportData buildInferenceReport(TypeInferenceEngine engine) {
    InferenceReportData report = new InferenceReportData();
    
    // ... populate fields ...
    
    // Ajouter recommandations
    List<String> recs = buildRecommendations(report);
    report.setRecommendations(recs);
    
    return report;
}
```

---

## PHASE 5️⃣  - Documentation & Tests End-to-End (1 jour)

### 5.1 Tests End-to-End

```bash
# 1. Build complet
mvn clean package -q

# 2. Démarrer app
java -jar target/cobol-to-java-translator-1.0.0-SNAPSHOT.jar &

# 3. Upload COBOL test
curl -X POST http://localhost:8080/api/conversion/upload \
  -F "file=@src/test/resources/customer.cobol"

# 4. Vérifier response
# - success: true
# - inferenceReport.totalFieldsInferred > 0
# - inferenceReport.overallQualityScore > 0
# - inferenceReport.recommendations.length > 0

# 5. Vérifier IHM
# - Section inférence visible
# - Tableau champs affiché
# - Heatmap contextes colorée
# - Recommandations listées
```

### 5.2 Documentation Utilisateur

Créer `USER_GUIDE_INFERENCE_REPORT.md`:
- Explication de chaque métrique
- Interprétation des niveaux de confiance
- Conseils sur les recommandations
- Exemples concrets

### 5.3 Validation Performance

```bash
# Mesurer temps response
time curl -X POST http://localhost:8080/api/conversion/upload \
  -F "file=@large-cobol.cobol"

# Target: < 500ms
```

---

## 📅 Timeline Consolidée

| Phase | Tâche | Durée | Total |
|-------|-------|-------|-------|
| 1 | Backend classes | ✅ 15min | 15min |
| 2 | API integration | 1-2j | 1-2j |
| 3 | Frontend (HTML/CSS/JS) | 2j | 3-4j |
| 4 | Smart recommendations | 1j | 4-5j |
| 5 | Tests & documentation | 1j | 5-6j |

---

## ✅ Checklist

Phase 2:
- [ ] ConversionResponse.java modifié
- [ ] ConversionController.java utilise inferenceReport
- [ ] JSON response contient les champs
- [ ] Tests manuels réussis

Phase 3:
- [ ] HTML section ajoutée
- [ ] CSS styles appliqués
- [ ] JavaScript functions implémentées
- [ ] Frontend affiche les données
- [ ] Responsive design testé

Phase 4:
- [ ] buildRecommendations() implémentée
- [ ] Recommandations apparaissent dans IHM
- [ ] Recommandations contextuellement pertinentes

Phase 5:
- [ ] Tests end-to-end passent
- [ ] Documentation utilisateur complète
- [ ] Performance validée (< 500ms)
- [ ] Tous les projets test convertissent

---

**Status**: 🟢 Phase 1 complétée | 🟡 Phase 2-5 en attente

