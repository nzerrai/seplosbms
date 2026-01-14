// ===================================
// Configuration Mermaid
// ===================================
mermaid.initialize({
    startOnLoad: false,
    theme: 'default',
    securityLevel: 'loose',
    flowchart: {
        useMaxWidth: true,
        htmlLabels: true,
        curve: 'basis'
    }
});

// ===================================
// État global
// ===================================
const state = {
    selectedProgram: null,
    selectedTab: 'architecture',
    zoomLevel: 1,
    theme: localStorage.getItem('theme') || 'light'
};

// ===================================
// Données des diagrammes
// ===================================
const diagramsData = {
    'order-processor': {
        architecture: {
            title: 'Architecture Globale - ORDER-PROCESSOR',
            complexity: 'Moyenne',
            nodes: '15',
            diagram: `flowchart TD
    Start([START]) --> Main[0000-MAIN-PROCESS]
    Main --> Init[1000-INITIALIZE]
    Init --> OpenFiles[OPEN INPUT/OUTPUT]
    OpenFiles --> CheckStatus{Status = '00'?}
    CheckStatus -->|Yes| WriteHeader[1100-WRITE-REPORT-HEADER]
    CheckStatus -->|No| Error[DISPLAY ERROR]
    WriteHeader --> FirstRead[1200-READ-ORDER]
    FirstRead --> Loop{WS-EOF?}
    Loop -->|No| Process[2000-PROCESS-ORDERS]
    Process --> Validate[2100-VALIDATE-ORDER]
    Validate --> ValidCheck{Valid?}
    ValidCheck -->|Yes| Calculate[2200-CALCULATE-AMOUNTS]
    ValidCheck -->|No| HandleInvalid[2500-HANDLE-INVALID]
    Calculate --> UpdateCounters[2300-UPDATE-STATUS-COUNTERS]
    UpdateCounters --> WriteDetail[2400-WRITE-DETAIL-LINE]
    WriteDetail --> ReadNext[1200-READ-ORDER]
    HandleInvalid --> ReadNext
    ReadNext --> Loop
    Loop -->|Yes| Finalize[3000-FINALIZE]
    Finalize --> WriteSummary[3100-WRITE-SUMMARY]
    WriteSummary --> CloseFiles[CLOSE FILES]
    CloseFiles --> End([END])

    style Main fill:#e1f5ff
    style Init fill:#fff9e1
    style Process fill:#e8f5e9
    style Finalize fill:#fce4ec`
        },
        flowchart: {
            title: 'Validation des Commandes (2100-VALIDATE-ORDER)',
            complexity: 'Élevée',
            nodes: '8',
            diagram: `flowchart TD
    Start([2100-VALIDATE-ORDER]) --> SetValid[MOVE 'Y' TO WS-VALID-FLAG]
    SetValid --> Check1{QUANTITY in range?}
    Check1 -->|No| Invalid1[MOVE 'N' TO WS-VALID-FLAG<br/>DISPLAY ERROR]
    Check1 -->|Yes| Check2{UNIT-PRICE in range?}
    Invalid1 --> Check2
    Check2 -->|No| Invalid2[MOVE 'N' TO WS-VALID-FLAG<br/>DISPLAY ERROR]
    Check2 -->|Yes| Check3{DISCOUNT-RATE <= 50?}
    Invalid2 --> Check3
    Check3 -->|No| Invalid3[MOVE 'N' TO WS-VALID-FLAG<br/>DISPLAY ERROR]
    Check3 -->|Yes| Check4{PRODUCT-CODE not empty?}
    Invalid3 --> Check4
    Check4 -->|No| Invalid4[MOVE 'N' TO WS-VALID-FLAG<br/>DISPLAY ERROR]
    Check4 -->|Yes| Return([Return])
    Invalid4 --> Return

    style Check1 fill:#ffebee
    style Check2 fill:#ffebee
    style Check3 fill:#ffebee
    style Check4 fill:#ffebee
    style Invalid1 fill:#ef5350
    style Invalid2 fill:#ef5350
    style Invalid3 fill:#ef5350
    style Invalid4 fill:#ef5350`
        },
        dataflow: {
            title: 'Flux de Transformation des Données',
            complexity: 'Moyenne',
            nodes: '12',
            diagram: `flowchart LR
    subgraph Input["INPUT ORDER-FILE"]
        I1[ORDER-ID]
        I2[QUANTITY]
        I3[UNIT-PRICE]
        I4[DISCOUNT-RATE]
        I5[PRIORITY-CODE]
    end

    subgraph Process["PROCESSING"]
        P1[ORDER-AMOUNT<br/>qty × price]
        P2[DISCOUNT-AMOUNT<br/>amount × rate / 100]
        P3[NET-AMOUNT<br/>amount - discount]
        P4[Apply Priority<br/>HIGH: ×0.95<br/>MEDIUM: ×0.98]
    end

    subgraph Output["OUTPUT REPORT"]
        O1[RPT-AMOUNT]
        O2[RPT-NET]
        O3[RPT-STATUS]
    end

    I2 --> P1
    I3 --> P1
    P1 --> P2
    I4 --> P2
    P2 --> P3
    P3 --> P4
    I5 --> P4
    P1 --> O1
    P4 --> O2
    O3

    style Input fill:#e3f2fd
    style Process fill:#fff3e0
    style Output fill:#c8e6c9`
        },
        sequence: {
            title: 'Séquence de Traitement COBOL',
            complexity: 'Moyenne',
            nodes: '10',
            diagram: `sequenceDiagram
    participant M as Main
    participant I as Initialize
    participant P as Process
    participant V as Validate
    participant C as Calculate
    participant W as Write

    M->>I: PERFORM 1000-INITIALIZE
    I->>I: OPEN FILES
    I->>I: Write Header
    I-->>M: Return

    M->>P: PERFORM 2000-PROCESS-ORDERS

    loop For Each Record
        P->>V: PERFORM 2100-VALIDATE
        V-->>P: Return (flag set)

        alt Valid Order
            P->>C: PERFORM 2200-CALCULATE
            C-->>P: Return
            P->>W: PERFORM 2400-WRITE
            W-->>P: Return
        else Invalid
            P->>P: PERFORM 2500-HANDLE-INVALID
        end
    end

    P-->>M: Return (EOF)`
        },
        state: {
            title: 'Cycle de Vie d\'un Enregistrement',
            complexity: 'Faible',
            nodes: '7',
            diagram: `stateDiagram-v2
    [*] --> FileRead: READ ORDER-FILE
    FileRead --> Validate: PERFORM 2100-VALIDATE

    Validate --> ValidCheck
    ValidCheck --> Valid: All checks pass
    ValidCheck --> Invalid: Any check fails

    Valid --> Calculate: PERFORM 2200-CALCULATE
    Calculate --> Write: PERFORM 2400-WRITE
    Write --> NextRecord

    Invalid --> LogError: DISPLAY error
    LogError --> NextRecord

    NextRecord --> FileRead: More records
    NextRecord --> [*]: EOF reached`
        }
    },
    'employee-payroll': {
        architecture: {
            title: 'Architecture Globale - EMPLOYEE-PAYROLL',
            complexity: 'Élevée',
            nodes: '18',
            diagram: `flowchart TD
    Start([START]) --> Main[0000-MAIN-PROCESS]
    Main --> Init[1000-INITIALIZE]
    Init --> GetDate[1100-GET-CURRENT-DATE]
    GetDate --> BuildPeriod[1200-BUILD-PAY-PERIOD]
    BuildPeriod --> ReadEmp[1300-READ-EMPLOYEE]
    ReadEmp --> Loop{WS-EOF?}
    Loop -->|No| Process[2000-PROCESS-EMPLOYEES]
    Process --> CalcBase[2100-CALCULATE-BASE-SALARY]
    CalcBase --> CalcOT[2200-CALCULATE-OVERTIME]
    CalcOT --> CalcGross[2300-CALCULATE-GROSS]
    CalcGross --> CalcDed[2400-CALCULATE-DEDUCTIONS]
    CalcDed --> CalcNet[2500-CALCULATE-NET]
    CalcNet --> UpdateTotals[2600-UPDATE-TOTALS]
    UpdateTotals --> WritePayroll[2700-WRITE-PAYROLL]
    WritePayroll --> ReadEmp
    Loop -->|Yes| Finalize[3000-FINALIZE]
    Finalize --> DisplayStats[3100-DISPLAY-STATISTICS]
    DisplayStats --> End([END])

    style Main fill:#e1f5ff
    style Init fill:#fff9e1
    style Process fill:#e8f5e9
    style Finalize fill:#fce4ec`
        },
        flowchart: {
            title: 'Calcul des Déductions (2400-CALCULATE-DEDUCTIONS)',
            complexity: 'Élevée',
            nodes: '10',
            diagram: `flowchart TD
    Start([2400-CALCULATE-DEDUCTIONS]) --> Evaluate{TAX-CODE}

    Evaluate -->|STANDARD| CalcStd[COMPUTE TAX<br/>= GROSS × 25%]
    Evaluate -->|REDUCED| CalcRed[COMPUTE TAX<br/>= GROSS × 15%]
    Evaluate -->|EXEMPT| CalcExe[TAX = 0]
    Evaluate -->|OTHER| CalcOther[COMPUTE TAX<br/>= GROSS × 25%<br/>DISPLAY WARNING]

    CalcStd --> CalcSoc[COMPUTE SOCIAL-SEC<br/>= GROSS × 7.5%]
    CalcRed --> CalcSoc
    CalcExe --> CalcSoc
    CalcOther --> CalcSoc

    CalcSoc --> Return([Return])

    style Evaluate fill:#fff3e0
    style CalcStd fill:#c8e6c9
    style CalcRed fill:#c8e6c9
    style CalcExe fill:#c8e6c9`
        },
        dataflow: {
            title: 'Transformation Paie - Exemple Concret',
            complexity: 'Élevée',
            nodes: '15',
            diagram: `flowchart TB
    subgraph Input["Employee Input"]
        H[HOURS-WORKED<br/>160h]
        R[HOURLY-RATE<br/>$25/h]
        O[OVERTIME-HOURS<br/>15h]
        B[BONUS<br/>$500]
        T[TAX-CODE<br/>'S' Standard]
    end

    subgraph Calc["Calculations"]
        C1["BASE-SALARY<br/>160 × $25 = $4,000"]
        C2["OVERTIME-PAY<br/>15 × $25 × 1.5 = $563"]
        C3["GROSS-SALARY<br/>$4,000 + $563 + $500<br/>= $5,063"]
        C4["TAX (25%)<br/>$5,063 × 0.25 = $1,266"]
        C5["SOCIAL-SEC (7.5%)<br/>$5,063 × 0.075 = $380"]
        C6["NET-SALARY<br/>$5,063 - $1,266 - $380<br/>= $3,417"]
    end

    subgraph Output["Payroll Output"]
        OUT["GROSS: $5,063<br/>TAX: $1,266<br/>NET: $3,417"]
    end

    H --> C1
    R --> C1
    C1 --> C3
    O --> C2
    R --> C2
    C2 --> C3
    B --> C3
    C3 --> C4
    T --> C4
    C4 --> C6
    C3 --> C5
    C5 --> C6
    C6 --> OUT

    style Input fill:#e3f2fd
    style Calc fill:#fff9c4
    style Output fill:#c8e6c9`
        },
        sequence: {
            title: 'Séquence de Calcul de Paie',
            complexity: 'Élevée',
            nodes: '12',
            diagram: `sequenceDiagram
    participant M as Main
    participant I as Initialize
    participant P as Process
    participant C as Calculator

    M->>I: PERFORM 1000-INITIALIZE
    I->>I: Get Current Date
    I->>I: Build Pay Period
    I-->>M: Return

    M->>P: PERFORM 2000-PROCESS-EMPLOYEES

    loop For Each Employee
        P->>C: Calculate Base Salary
        C-->>P: Base Amount

        P->>C: Calculate Overtime
        C-->>P: OT Amount

        P->>C: Calculate Gross
        C-->>P: Gross Amount

        P->>C: Calculate Deductions
        C-->>P: Tax + Social Sec

        P->>C: Calculate Net
        C-->>P: Net Amount

        P->>P: Write Payroll Record
    end`
        },
        state: {
            title: 'États des Compteurs Fiscaux',
            complexity: 'Faible',
            nodes: '8',
            diagram: `stateDiagram-v2
    [*] --> Initialize: Counters = 0
    Initialize --> ProcessEmployee

    ProcessEmployee --> CheckTaxCode

    CheckTaxCode --> Standard: TAX-CODE = 'S'
    CheckTaxCode --> Reduced: TAX-CODE = 'R'
    CheckTaxCode --> Exempt: TAX-CODE = 'E'

    Standard --> IncrementStd: ++STANDARD-TAX-COUNT
    Reduced --> IncrementRed: ++REDUCED-TAX-COUNT
    Exempt --> IncrementExe: ++EXEMPT-TAX-COUNT

    IncrementStd --> NextEmployee
    IncrementRed --> NextEmployee
    IncrementExe --> NextEmployee

    NextEmployee --> ProcessEmployee: More employees
    NextEmployee --> [*]: EOF`
        }
    },
    'data-transformer': {
        architecture: {
            title: 'Architecture Globale - DATA-TRANSFORMER',
            complexity: 'Moyenne',
            nodes: '12',
            diagram: `flowchart TD
    Start([START]) --> Main[0000-MAIN-PROCESS]
    Main --> Init[1000-INITIALIZE]
    Init --> LoadCodes[1100-LOAD-VALID-CODES]
    LoadCodes --> ReadInput[1200-READ-INPUT]
    ReadInput --> Loop{WS-EOF?}
    Loop -->|No| Process[2000-PROCESS-RECORDS]
    Process --> Parse[2100-PARSE-INPUT-DATA<br/>UNSTRING]
    Parse --> Inspect[2200-INSPECT-FIELDS<br/>TALLYING/REPLACING]
    Inspect --> Search[2300-SEARCH-CODE-TABLE]
    Search --> Build[2400-BUILD-OUTPUT<br/>STRING]
    Build --> Write[2500-WRITE-OUTPUT]
    Write --> ReadInput
    Loop -->|Yes| Finalize[3000-FINALIZE]
    Finalize --> End([END])

    style Main fill:#e1f5ff
    style Init fill:#fff9e1
    style Process fill:#e8f5e9
    style Finalize fill:#fce4ec`
        },
        flowchart: {
            title: 'Parsing de Données (2100-PARSE-INPUT-DATA)',
            complexity: 'Moyenne',
            nodes: '6',
            diagram: `flowchart TD
    Start([2100-PARSE-INPUT-DATA]) --> Unstring[UNSTRING WS-RAW-DATA<br/>DELIMITED BY '|']
    Unstring --> Split[INTO WS-FIELD-1<br/>WS-FIELD-2<br/>WS-FIELD-3<br/>WS-FIELD-4<br/>WS-FIELD-5]
    Split --> Trim1[2110-TRIM-FIELD-1<br/>Remove leading spaces]
    Trim1 --> Trim2[2120-TRIM-FIELD-2<br/>Replace multiple spaces]
    Trim2 --> Return([Return])

    style Unstring fill:#e3f2fd
    style Split fill:#fff3e0
    style Trim1 fill:#c8e6c9
    style Trim2 fill:#c8e6c9`
        },
        dataflow: {
            title: 'Transformation Données - Exemple Concret',
            complexity: 'Moyenne',
            nodes: '10',
            diagram: `flowchart TB
    subgraph Input["Raw Input"]
        RAW["  CUSTOMER1  |  John  Doe  |X12X45|A001|PREMIUM"]
    end

    subgraph Parse["Parse UNSTRING"]
        F1["FIELD-1:<br/>'CUSTOMER1'"]
        F2["FIELD-2:<br/>'John  Doe'"]
        F3["FIELD-3:<br/>'X12X45'"]
        F4["FIELD-4:<br/>'A001'"]
        F5["FIELD-5:<br/>'PREMIUM'"]
    end

    subgraph Transform["Transform"]
        T1["INSPECT REPLACING<br/>X → Y<br/>F3 = 'Y12Y45'"]
        T2["SEARCH TABLE<br/>A001 → 'Account Type A'"]
    end

    subgraph Build["Build OUTPUT"]
        OUT["DATA: - CUSTOMER1 - John Doe - Y12Y45 - Account Type A - :END"]
    end

    RAW --> F1
    RAW --> F2
    RAW --> F3
    RAW --> F4
    RAW --> F5
    F3 --> T1
    F4 --> T2
    T1 --> OUT
    T2 --> OUT

    style Input fill:#e3f2fd
    style Parse fill:#fff3e0
    style Transform fill:#fff9c4
    style Build fill:#c8e6c9`
        },
        sequence: {
            title: 'Séquence de Transformation',
            complexity: 'Moyenne',
            nodes: '8',
            diagram: `sequenceDiagram
    participant M as Main
    participant P as Process
    participant U as UNSTRING
    participant I as INSPECT
    participant S as SEARCH
    participant B as STRING

    M->>P: PERFORM 2000-PROCESS-RECORDS

    loop For Each Record
        P->>U: Parse delimited data
        U-->>P: Fields extracted

        P->>I: Inspect fields
        I-->>P: Tallying/Replacing done

        P->>S: Search code table
        S-->>P: Description found

        P->>B: Build output string
        B-->>P: Formatted output

        P->>P: Write output record
    end`
        },
        state: {
            title: 'Recherche dans Table',
            complexity: 'Faible',
            nodes: '5',
            diagram: `stateDiagram-v2
    [*] --> Initialize: Load Valid Codes
    Initialize --> Search

    Search --> LinearSearch: SET INDEX
    LinearSearch --> CheckMatch: Compare Code

    CheckMatch --> Found: Match
    CheckMatch --> NotFound: No Match
    CheckMatch --> Next: Continue

    Next --> LinearSearch

    Found --> ReturnDesc: Return Description
    NotFound --> ReturnNotFound: Return 'NOT FOUND'

    ReturnDesc --> [*]
    ReturnNotFound --> [*]`
        }
    }
};

// ===================================
// Comparaisons COBOL vs Java
// ===================================
const comparisons = {
    'order-processor': {
        cobol: `<strong>Structure COBOL:</strong><br/>
• PERFORM UNTIL WS-EOF<br/>
• IF/EVALUATE avec 88-levels<br/>
• COMPUTE pour calculs<br/>
• MOVE pour transferts<br/>
• Variables WORKING-STORAGE<br/>
<br/>
<strong>Paragraphes:</strong><br/>
• 2100-VALIDATE-ORDER<br/>
• 2200-CALCULATE-AMOUNTS<br/>
• 2300-UPDATE-STATUS-COUNTERS<br/>
• 2400-WRITE-DETAIL-LINE`,
        java: `<strong>Architecture Spring Batch:</strong><br/>
• ItemReader/ItemProcessor/ItemWriter<br/>
• if/switch avec equals()<br/>
• BigDecimal pour calculs<br/>
• Setters pour transferts<br/>
• Champs private de classe<br/>
<br/>
<strong>Méthodes:</strong><br/>
• validate2100Order()<br/>
• calculate2200Amounts()<br/>
• updateStatus2300Counters()<br/>
• buildDetail2400Line()`
    },
    'employee-payroll': {
        cobol: `<strong>Structure COBOL:</strong><br/>
• Calculs complexes COMPUTE<br/>
• MULTIPLY/DIVIDE<br/>
• EVALUATE pour taxes<br/>
• STRING pour dates<br/>
• ACCEPT FROM DATE<br/>
<br/>
<strong>Taux variables:</strong><br/>
• WS-STANDARD-TAX-RATE (25%)<br/>
• WS-REDUCED-TAX-RATE (15%)<br/>
• WS-SOCIAL-SEC-RATE (7.5%)`,
        java: `<strong>Architecture Spring Batch:</strong><br/>
• BigDecimal.multiply/divide<br/>
• RoundingMode.HALF_UP<br/>
• switch sur taxCode<br/>
• LocalDate/DateTimeFormatter<br/>
• System.currentTimeMillis()<br/>
<br/>
<strong>Constantes:</strong><br/>
• standardTaxRate = 0.25<br/>
• reducedTaxRate = 0.15<br/>
• socialSecRate = 0.075`
    },
    'data-transformer': {
        cobol: `<strong>Structure COBOL:</strong><br/>
• UNSTRING DELIMITED BY<br/>
• STRING DELIMITED BY SIZE<br/>
• INSPECT TALLYING<br/>
• INSPECT REPLACING<br/>
• SEARCH avec INDEXED BY<br/>
<br/>
<strong>Tables:</strong><br/>
• OCCURS 10 TIMES<br/>
• SET INDEX TO<br/>
• WHEN condition`,
        java: `<strong>Architecture Spring Batch:</strong><br/>
• String.split("\\\\|")<br/>
• StringBuilder.append()<br/>
• for loop counting<br/>
• String.replace()<br/>
• Linear search dans List<br/>
<br/>
<strong>Collections:</strong><br/>
• List<String> validCodes<br/>
• for (String code : codes)<br/>
• if (code.equals(key))`
    }
};

// ===================================
// Initialisation
// ===================================
document.addEventListener('DOMContentLoaded', () => {
    initializeTheme();
    initializeEventListeners();
    console.log('Diagrams page initialized');
});

function initializeTheme() {
    document.documentElement.setAttribute('data-theme', state.theme);
    updateThemeButton();
}

function updateThemeButton() {
    const btn = document.getElementById('themeBtn');
    btn.textContent = state.theme === 'dark' ? '☀️' : '🌙';
}

// ===================================
// Event Listeners
// ===================================
function initializeEventListeners() {
    // Bouton de thème
    document.getElementById('themeBtn').addEventListener('click', toggleTheme);

    // Sélection de programme
    document.querySelectorAll('.program-card').forEach(card => {
        card.addEventListener('click', () => selectProgram(card.dataset.program));
    });

    // Onglets de diagramme
    document.querySelectorAll('.tab-btn').forEach(btn => {
        btn.addEventListener('click', () => selectTab(btn.dataset.tab));
    });

    // Actions de zoom
    document.getElementById('zoomInBtn')?.addEventListener('click', zoomIn);
    document.getElementById('zoomOutBtn')?.addEventListener('click', zoomOut);
    document.getElementById('resetZoomBtn')?.addEventListener('click', resetZoom);
    document.getElementById('downloadBtn')?.addEventListener('click', downloadDiagram);
}

function toggleTheme() {
    state.theme = state.theme === 'light' ? 'dark' : 'light';
    document.documentElement.setAttribute('data-theme', state.theme);
    localStorage.setItem('theme', state.theme);
    updateThemeButton();

    // Réinitialiser Mermaid avec le nouveau thème
    mermaid.initialize({
        startOnLoad: false,
        theme: state.theme === 'dark' ? 'dark' : 'default'
    });

    // Recharger le diagramme actuel
    if (state.selectedProgram && state.selectedTab) {
        renderDiagram();
    }
}

// ===================================
// Sélection de programme
// ===================================
function selectProgram(programId) {
    state.selectedProgram = programId;

    // Mettre à jour l'UI
    document.querySelectorAll('.program-card').forEach(card => {
        card.classList.toggle('selected', card.dataset.program === programId);
    });

    // Afficher la navigation des diagrammes
    document.getElementById('diagramNav').style.display = 'block';
    document.getElementById('diagramNav').classList.add('fade-in');

    // Charger le diagramme par défaut
    state.selectedTab = 'architecture';
    renderDiagram();

    // Afficher les sections
    document.getElementById('diagramContainer').style.display = 'block';
    document.getElementById('legendContainer').style.display = 'block';
    document.getElementById('comparisonSection').style.display = 'block';

    // Afficher les comparaisons
    displayComparison();

    // Scroll vers le diagramme
    document.getElementById('diagramContainer').scrollIntoView({ behavior: 'smooth' });
}

// ===================================
// Sélection d'onglet
// ===================================
function selectTab(tabId) {
    state.selectedTab = tabId;

    // Mettre à jour l'UI
    document.querySelectorAll('.tab-btn').forEach(btn => {
        btn.classList.toggle('active', btn.dataset.tab === tabId);
    });

    // Rendre le nouveau diagramme
    renderDiagram();
}

// ===================================
// Rendu du diagramme
// ===================================
async function renderDiagram() {
    if (!state.selectedProgram || !state.selectedTab) return;

    const data = diagramsData[state.selectedProgram][state.selectedTab];
    if (!data) {
        console.error('Diagram data not found');
        return;
    }

    // Mettre à jour le titre
    document.getElementById('diagramTitle').textContent = data.title;

    // Mettre à jour les informations
    document.getElementById('infoProgramme').textContent = state.selectedProgram.toUpperCase();
    document.getElementById('infoType').textContent = state.selectedTab;
    document.getElementById('infoNodes').textContent = data.nodes;
    document.getElementById('infoComplexity').textContent = data.complexity;

    // Afficher un loader
    const canvas = document.getElementById('diagramCanvas');
    canvas.innerHTML = '<div class="loading" style="padding: 60px; text-align: center; font-size: 1.2rem;">Génération du diagramme...</div>';

    try {
        // Générer un ID unique pour ce diagramme
        const diagramId = `mermaid-${Date.now()}`;

        // Rendre le diagramme avec Mermaid
        const { svg } = await mermaid.render(diagramId, data.diagram);

        // Insérer le SVG
        canvas.innerHTML = svg;

        // Réinitialiser le zoom
        state.zoomLevel = 1;
        applyZoom();

        console.log('Diagram rendered successfully');
    } catch (error) {
        console.error('Error rendering diagram:', error);
        canvas.innerHTML = `<div style="padding: 60px; text-align: center; color: var(--sg-danger);">
            <p style="font-size: 1.2rem; margin-bottom: 10px;">❌ Erreur de rendu</p>
            <p>Impossible de générer le diagramme. Vérifiez la console pour plus de détails.</p>
        </div>`;
    }
}

// ===================================
// Gestion du zoom
// ===================================
function zoomIn() {
    state.zoomLevel = Math.min(state.zoomLevel + 0.1, 3);
    applyZoom();
}

function zoomOut() {
    state.zoomLevel = Math.max(state.zoomLevel - 0.1, 0.5);
    applyZoom();
}

function resetZoom() {
    state.zoomLevel = 1;
    applyZoom();
}

function applyZoom() {
    const canvas = document.getElementById('diagramCanvas');
    canvas.style.transform = `scale(${state.zoomLevel})`;
}

// ===================================
// Téléchargement
// ===================================
function downloadDiagram() {
    const svg = document.querySelector('#diagramCanvas svg');
    if (!svg) {
        alert('Aucun diagramme à télécharger');
        return;
    }

    // Créer un canvas
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');

    // Obtenir les dimensions du SVG
    const bbox = svg.getBoundingClientRect();
    canvas.width = bbox.width * 2; // 2x pour meilleure qualité
    canvas.height = bbox.height * 2;

    // Convertir SVG en image
    const data = new XMLSerializer().serializeToString(svg);
    const img = new Image();
    const blob = new Blob([data], { type: 'image/svg+xml' });
    const url = URL.createObjectURL(blob);

    img.onload = function() {
        ctx.fillStyle = state.theme === 'dark' ? '#1e1e1e' : '#ffffff';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(img, 0, 0, canvas.width, canvas.height);

        // Télécharger
        canvas.toBlob(function(blob) {
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = `diagram-${state.selectedProgram}-${state.selectedTab}.png`;
            a.click();
            URL.revokeObjectURL(url);
        });

        URL.revokeObjectURL(url);
    };

    img.src = url;
}

// ===================================
// Affichage des comparaisons
// ===================================
function displayComparison() {
    const comparison = comparisons[state.selectedProgram];
    if (!comparison) return;

    document.getElementById('cobolComparison').innerHTML = comparison.cobol;
    document.getElementById('javaComparison').innerHTML = comparison.java;
}
