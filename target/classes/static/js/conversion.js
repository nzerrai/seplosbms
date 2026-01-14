// ===== DOM Elements =====
const conversionForm = document.getElementById('conversionForm');
const cobolFileInput = document.getElementById('cobolFiles');
const jclFileInput = document.getElementById('jclFiles');
const cobolUploadArea = document.getElementById('cobolUploadArea');
const jclUploadArea = document.getElementById('jclUploadArea');
const cobolFileList = document.getElementById('cobolFileList');
const jclFileList = document.getElementById('jclFileList');
const convertBtn = document.getElementById('convertBtn');
const progressSection = document.getElementById('progressSection');
const progressFill = document.getElementById('progressFill');
const progressPercent = document.getElementById('progressPercent');
const resultSection = document.getElementById('resultSection');
const errorSection = document.getElementById('errorSection');
const errorMessage = document.getElementById('errorMessage');
const downloadMessage = document.getElementById('downloadMessage');
const spinner = document.getElementById('spinner');
const themeBtn = document.getElementById('themeBtn');
const advancedToggle = document.getElementById('advancedToggle');
const advancedOptions = document.getElementById('advancedOptions');

// Selected files arrays
let selectedCobolFiles = [];
let selectedJclFiles = [];
let conversionStartTime = null;

// ===== Theme Toggle =====
themeBtn?.addEventListener('click', () => {
    document.body.classList.toggle('dark-mode');
    localStorage.setItem('theme', document.body.classList.contains('dark-mode') ? 'dark' : 'light');
    updateThemeIcon();
});

function updateThemeIcon() {
    themeBtn.textContent = document.body.classList.contains('dark-mode') ? '☀️' : '🌙';
}

// Initialize theme from localStorage
if (localStorage.getItem('theme') === 'dark') {
    document.body.classList.add('dark-mode');
    updateThemeIcon();
}

// ===== Advanced Options Toggle =====
advancedToggle?.addEventListener('click', () => {
    advancedToggle.classList.toggle('expanded');
    advancedOptions.classList.toggle('show');
});

// ===== File Upload - Drag and Drop for COBOL =====
cobolUploadArea.addEventListener('dragover', (e) => {
    e.preventDefault();
    e.stopPropagation();
    cobolUploadArea.classList.add('dragover');
});

cobolUploadArea.addEventListener('dragleave', (e) => {
    e.preventDefault();
    e.stopPropagation();
    cobolUploadArea.classList.remove('dragover');
});

cobolUploadArea.addEventListener('drop', (e) => {
    e.preventDefault();
    e.stopPropagation();
    cobolUploadArea.classList.remove('dragover');

    const files = Array.from(e.dataTransfer.files);
    const validFiles = files.filter(file =>
        file.name.endsWith('.cob') || file.name.endsWith('.cbl')
    );

    if (validFiles.length > 0) {
        addCobolFiles(validFiles);
    } else {
        showError('Veuillez déposer des fichiers COBOL (.cob, .cbl)');
    }
});

// ===== File Upload - Drag and Drop for JCL =====
jclUploadArea.addEventListener('dragover', (e) => {
    e.preventDefault();
    e.stopPropagation();
    jclUploadArea.classList.add('dragover');
});

jclUploadArea.addEventListener('dragleave', (e) => {
    e.preventDefault();
    e.stopPropagation();
    jclUploadArea.classList.remove('dragover');
});

jclUploadArea.addEventListener('drop', (e) => {
    e.preventDefault();
    e.stopPropagation();
    jclUploadArea.classList.remove('dragover');

    const files = Array.from(e.dataTransfer.files);
    const validFiles = files.filter(file => file.name.endsWith('.jcl'));

    if (validFiles.length > 0) {
        addJclFiles(validFiles);
    } else {
        showError('Veuillez déposer des fichiers JCL (.jcl)');
    }
});

// ===== File Input Change =====
cobolFileInput.addEventListener('change', (e) => {
    const files = Array.from(e.target.files);
    addCobolFiles(files);
});

jclFileInput.addEventListener('change', (e) => {
    const files = Array.from(e.target.files);
    addJclFiles(files);
});

// ===== File Upload Area Click =====
cobolUploadArea.addEventListener('click', () => {
    cobolFileInput.click();
});

jclUploadArea.addEventListener('click', () => {
    jclFileInput.click();
});

// ===== Add COBOL Files =====
function addCobolFiles(files) {
    files.forEach(file => {
        if (!selectedCobolFiles.find(f => f.name === file.name)) {
            selectedCobolFiles.push(file);
        }
    });
    renderCobolFileList();
}

// ===== Add JCL Files =====
function addJclFiles(files) {
    files.forEach(file => {
        if (!selectedJclFiles.find(f => f.name === file.name)) {
            selectedJclFiles.push(file);
        }
    });
    renderJclFileList();
}

// ===== Remove COBOL File =====
function removeCobolFile(index) {
    selectedCobolFiles.splice(index, 1);
    renderCobolFileList();
}

// ===== Remove JCL File =====
function removeJclFile(index) {
    selectedJclFiles.splice(index, 1);
    renderJclFileList();
}

// ===== Render COBOL File List =====
function renderCobolFileList() {
    if (selectedCobolFiles.length === 0) {
        cobolFileList.innerHTML = '';
        cobolFileList.classList.add('hidden');
        return;
    }

    cobolFileList.classList.remove('hidden');
    cobolFileList.innerHTML = selectedCobolFiles.map((file, index) => `
        <div class="file-item">
            <div>
                <span class="file-item-name">📄 ${file.name}</span>
                <span class="file-item-size">(${formatFileSize(file.size)})</span>
            </div>
            <button type="button" class="file-remove-btn" onclick="removeCobolFile(${index})" title="Retirer">✕</button>
        </div>
    `).join('');
}

// ===== Render JCL File List =====
function renderJclFileList() {
    if (selectedJclFiles.length === 0) {
        jclFileList.innerHTML = '';
        jclFileList.classList.add('hidden');
        return;
    }

    jclFileList.classList.remove('hidden');
    jclFileList.innerHTML = selectedJclFiles.map((file, index) => `
        <div class="file-item">
            <div>
                <span class="file-item-name">📋 ${file.name}</span>
                <span class="file-item-size">(${formatFileSize(file.size)})</span>
            </div>
            <button type="button" class="file-remove-btn" onclick="removeJclFile(${index})" title="Retirer">✕</button>
        </div>
    `).join('');
}

// ===== Format File Size =====
function formatFileSize(bytes) {
    if (bytes < 1024) return bytes + ' B';
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
    return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
}

// ===== Form Submission =====
conversionForm.addEventListener('submit', async (e) => {
    e.preventDefault();

    // Validate form
    const projectName = document.getElementById('projectName').value.trim();
    const basePackage = document.getElementById('basePackage').value.trim();

    if (!projectName) {
        showError('Veuillez entrer un nom de projet');
        return;
    }

    if (selectedCobolFiles.length === 0) {
        showError('Veuillez sélectionner au moins un fichier COBOL');
        return;
    }

    // Validate package name
    const packageRegex = /^([a-z]+\.)*[a-z]+$/;
    if (basePackage && !packageRegex.test(basePackage)) {
        showError('Le nom du package n\'est pas valide (ex: com.example.batch)');
        return;
    }

    // Hide previous messages
    hideAllMessages();

    // Track time
    conversionStartTime = Date.now();

    // Disable button and show spinner
    convertBtn.disabled = true;
    spinner.classList.add('show');

    // Prepare form data
    const formData = new FormData();
    formData.append('projectName', projectName);
    formData.append('basePackage', basePackage || 'com.example.batch');
    
    // Add options
    const generateTests = document.getElementById('generateTests').checked;
    const generateDocs = document.getElementById('generateDocs').checked;
    
    formData.append('generateTests', generateTests);
    formData.append('generateDocs', generateDocs);

    // Add COBOL files
    selectedCobolFiles.forEach(file => {
        formData.append('files', file);
    });
    
    // Add JCL files
    selectedJclFiles.forEach(file => {
        formData.append('files', file);
    });

    try {
        // Show progress
        showProgress();
        updateProgressStep('parsing', 'active');

        // Send request
        const response = await fetch('/conversion/upload', {
            method: 'POST',
            body: formData
        });

        updateProgressStep('parsing', 'done');
        updateProgressStep('ast', 'active');
        updateProgress(25);

        if (!response.ok) {
            let errorData;
            try {
                errorData = await response.json();
            } catch {
                throw new Error(`Erreur serveur (${response.status})`);
            }
            throw new Error(errorData.error || 'Erreur lors de la conversion');
        }

        updateProgressStep('ast', 'done');
        updateProgressStep('generation', 'active');
        updateProgress(50);

        // Parse JSON response
        const data = await response.json();
        
        if (!data.success) {
            throw new Error(data.message || 'Erreur lors de la conversion');
        }

        updateProgressStep('generation', 'done');
        updateProgressStep('maven', 'active');
        updateProgress(75);

        updateProgressStep('maven', 'done');
        updateProgress(100);

        // Show success with conversion report
        setTimeout(() => {
            hideProgress();
            showSuccessWithReport(projectName, data);
        }, 500);

    } catch (error) {
        console.error('Conversion error:', error);
        hideProgress();
        showError(error.message || 'Une erreur est survenue lors de la conversion');
    } finally {
        // Re-enable button
        convertBtn.disabled = false;
        spinner.classList.remove('show');
    }
});

// ===== Progress Management =====
function showProgress() {
    progressSection.classList.remove('hidden');
    progressFill.style.width = '0%';
    progressPercent.textContent = '0%';
}

function updateProgress(percent) {
    progressFill.style.width = percent + '%';
    progressPercent.textContent = percent + '%';
}

function updateProgressStep(stepId, status) {
    const step = document.getElementById(`step-${stepId}`);
    if (!step) return;
    
    step.classList.remove('active', 'done');
    if (status === 'active') {
        step.classList.add('active');
        step.innerHTML = '<span class="step-icon">⏳</span> ' + step.textContent.trim().split(' ').slice(1).join(' ');
    } else if (status === 'done') {
        step.classList.add('done');
        const label = step.textContent.trim().split(' ').slice(1).join(' ');
        step.innerHTML = '<span class="step-icon">✅</span> ' + label;
    }
}

function hideProgress() {
    progressSection.classList.add('hidden');
}

// ===== Success Management =====
function showSuccess(projectName) {
    const elapsed = Math.round((Date.now() - conversionStartTime) / 1000);
    resultSection.classList.remove('hidden');
    
    const elapsedText = elapsed < 60 ? `${elapsed}s` : `${Math.round(elapsed/60)}m ${elapsed % 60}s`;
    document.getElementById('timeElapsed').textContent = `(${elapsedText})`;
    
    downloadMessage.textContent = `Le fichier ${projectName}.zip a été généré avec succès. ${selectedFiles.length} fichier(s) COBOL converti(s).`;

    // Set up action buttons
    document.getElementById('downloadBtn').onclick = () => {
        // The download happens automatically, show message
        alert('Le fichier a été téléchargé. Voir le dossier Téléchargements.');
    };

    document.getElementById('resetBtn').onclick = () => {
        resetForm();
    };
}

// New function to show success with conversion report
function showSuccessWithReport(projectName, responseData) {
    const elapsed = Math.round((Date.now() - conversionStartTime) / 1000);
    resultSection.classList.remove('hidden');
    
    const elapsedText = elapsed < 60 ? `${elapsed}s` : `${Math.round(elapsed/60)}m ${elapsed % 60}s`;
    document.getElementById('timeElapsed').textContent = `(${elapsedText})`;
    
    // Display conversion reports if available
    const reportContainer = document.getElementById('reportCardsContainer');
    if (responseData.reports && responseData.reports.length > 0) {
        // Clear any existing reports
        reportContainer.innerHTML = '';
        
        // Create a report card for each file
        responseData.reports.forEach(report => {
            const reportCard = createReportCard(report);
            reportContainer.appendChild(reportCard);
        });
        
        const totalFiles = selectedCobolFiles.length + selectedJclFiles.length;
        downloadMessage.textContent = `${totalFiles} fichier(s) converti(s).`;
    } else {
        const totalFiles = selectedCobolFiles.length + selectedJclFiles.length;
        downloadMessage.textContent = `Le fichier ${projectName}.zip a été généré avec succès. ${totalFiles} fichier(s) converti(s).`;
    }
    
    // Display inference report if available
    if (responseData.inferenceReport) {
        displayInferenceReport(responseData.inferenceReport);
    }

    // Set up download button with base64 ZIP
    const downloadBtn = document.getElementById('downloadBtn');
    downloadBtn.onclick = () => {
        if (responseData.zipFileBase64) {
            // Decode base64 and create blob
            const byteCharacters = atob(responseData.zipFileBase64);
            const byteNumbers = new Array(byteCharacters.length);
            for (let i = 0; i < byteCharacters.length; i++) {
                byteNumbers[i] = byteCharacters.charCodeAt(i);
            }
            const byteArray = new Uint8Array(byteNumbers);
            const blob = new Blob([byteArray], { type: 'application/zip' });
            
            // Create download link
            const url = window.URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = `${projectName}.zip`;
            document.body.appendChild(a);
            a.click();
            window.URL.revokeObjectURL(url);
            document.body.removeChild(a);
        } else {
            alert('Fichier ZIP non disponible.');
        }
    };

    document.getElementById('resetBtn').onclick = () => {
        resetForm();
    };
}

// Create a report card for a single file
function createReportCard(report) {
    const conversionPercent = report.conversionPercentage || 0;
    
    // Set color based on percentage
    let progressColor = '#28a745';
    if (conversionPercent < 80) {
        progressColor = '#ffc107';
    }
    if (conversionPercent < 50) {
        progressColor = '#dc3545';
    }
    
    const card = document.createElement('div');
    card.className = 'report-card';
    card.innerHTML = `
        <div class="report-card-header">
            <h4>📄 ${escapeHtml(report.fileName)}</h4>
        </div>
        <div class="report-card-body">
            <div class="report-metric">
                <div class="report-label">Taux de complétion <small>(traitement terminé)</small></div>
                <div class="progress-bar-container">
                    <div class="progress-bar-fill" style="width: ${conversionPercent}%; background-color: ${progressColor}"></div>
                </div>
                <div class="report-value">${conversionPercent.toFixed(1)}%</div>
            </div>
            
            <div class="report-metric">
                <div class="report-label">
                    <span class="confidence-icon">${report.confidenceIcon || '❓'}</span>
                    Niveau de confiance
                </div>
                <div class="report-value">${report.confidenceLevel || 'Unknown'}</div>
                <div class="report-description">${report.confidenceDescription || ''}</div>
            </div>
            
            <div class="report-stats">
                <div class="stat-item">
                    <div class="stat-label">Instructions totales</div>
                    <div class="stat-value">${report.totalStatements || 0}</div>
                </div>
                <div class="stat-item">
                    <div class="stat-label">Converties</div>
                    <div class="stat-value stat-success">${report.convertedStatements || 0}</div>
                </div>
                <div class="stat-item">
                    <div class="stat-label">Partielles</div>
                    <div class="stat-value stat-warning">${report.partiallyConvertedStatements || 0}</div>
                </div>
                <div class="stat-item">
                    <div class="stat-label">Non converties</div>
                    <div class="stat-value stat-error">${report.unconvertedStatements || 0}</div>
                </div>
            </div>
            
            <div class="report-stats">
                <div class="stat-item">
                    <div class="stat-label">Données totales</div>
                    <div class="stat-value">${report.totalDataItems || 0}</div>
                </div>
                <div class="stat-item">
                    <div class="stat-label">Données converties</div>
                    <div class="stat-value stat-success">${report.convertedDataItems || 0}</div>
                </div>
            </div>

            ${createJclAnalysisSection(report.jclAnalysis)}
            ${createGeneratedClassesSection(report.generatedClasses)}
            ${createWarningDetailsSection(report.warningDetails)}
        </div>
    `;

    return card;
}

function createJclAnalysisSection(jclAnalysis) {
    if (!jclAnalysis) return '';

    return `
        <div class="report-section">
            <h5>📋 Analyse JCL</h5>
            <div class="jcl-info">
                <div class="info-row">
                    <span class="info-label">Fichier JCL:</span>
                    <span class="info-value">${escapeHtml(jclAnalysis.jclFileName || 'N/A')}</span>
                </div>
                <div class="info-row">
                    <span class="info-label">Job:</span>
                    <span class="info-value">${escapeHtml(jclAnalysis.jobName || 'N/A')}</span>
                </div>
                <div class="info-row">
                    <span class="info-label">Steps totaux:</span>
                    <span class="info-value">${jclAnalysis.totalSteps || 0}</span>
                </div>
                <div class="info-row">
                    <span class="info-label">Steps conditionnels:</span>
                    <span class="info-value">${jclAnalysis.conditionalSteps || 0}</span>
                </div>
                <div class="info-row">
                    <span class="info-label">Invocations PROC:</span>
                    <span class="info-value">${jclAnalysis.procInvocations || 0}</span>
                </div>
                <div class="info-row">
                    <span class="info-label">Datasets temporaires:</span>
                    <span class="info-value">${jclAnalysis.temporaryDatasets || 0}</span>
                </div>
            </div>
            ${createArrayList('Steps détectés', jclAnalysis.stepsDetected)}
            ${createArrayList('Conditions trouvées', jclAnalysis.conditionsFound)}
            ${createArrayList('PROCs utilisées', jclAnalysis.procsUsed)}
            ${createArrayList('Datasets temporaires', jclAnalysis.tempDatasetsUsed)}
            ${createTranslationsSection(jclAnalysis.translations)}
        </div>
    `;
}

function createTranslationsSection(translations) {
    if (!translations || translations.length === 0) return '';

    return `
        <div class="translations-section">
            <h6>🔄 Traductions JCL → Java détaillées</h6>
            <div class="translations-list">
                ${translations.map((trans, index) => `
                    <div class="translation-item">
                        <div class="translation-header">
                            <span class="translation-number">#${index + 1}</span>
                            <span class="translation-type">${getTranslationIcon(trans.type)} ${trans.type?.label || trans.jclElement}</span>
                            ${trans.javaClassName ? `<span class="translation-target">→ ${escapeHtml(trans.javaClassName)}</span>` : ''}
                        </div>
                        <div class="translation-body">
                            <div class="code-comparison">
                                <div class="code-block jcl-code">
                                    <div class="code-header">
                                        <span class="code-lang">JCL Source</span>
                                    </div>
                                    <pre><code>${escapeHtml(trans.jclSourceCode || 'N/A')}</code></pre>
                                </div>
                                <div class="code-arrow">→</div>
                                <div class="code-block java-code">
                                    <div class="code-header">
                                        <span class="code-lang">Java Généré</span>
                                    </div>
                                    <pre><code>${escapeHtml(trans.javaTargetCode || 'N/A')}</code></pre>
                                </div>
                            </div>
                            ${trans.explanation ? `
                                <div class="translation-explanation">
                                    <strong>💡 Explication:</strong> ${escapeHtml(trans.explanation)}
                                </div>
                            ` : ''}
                        </div>
                    </div>
                `).join('')}
            </div>
        </div>
    `;
}

function getTranslationIcon(type) {
    if (!type || !type.label) return '🔄';

    const icons = {
        'Step Execution': '▶️',
        'DD Statement': '📄',
        'Conditional': '🔀',
        'PROC Invocation': '📞',
        'Temporary Dataset': '💾',
        'Job Definition': '⚙️',
        'Other': '🔄'
    };

    return icons[type.label] || '🔄';
}

function createGeneratedClassesSection(generatedClasses) {
    if (!generatedClasses || generatedClasses.length === 0) return '';

    return `
        <div class="report-section">
            <h5>☕ Classes Java générées</h5>
            <div class="generated-classes-list">
                ${generatedClasses.map(cls => `
                    <div class="class-item ${cls.new ? 'class-new' : 'class-modified'}">
                        <div class="class-header">
                            <span class="class-icon">${getClassIcon(cls.type)}</span>
                            <span class="class-name">${escapeHtml(cls.className)}</span>
                            <span class="class-badge ${cls.new ? 'badge-new' : 'badge-modified'}">
                                ${cls.new ? 'NEW' : 'MODIFIED'}
                            </span>
                        </div>
                        <div class="class-details">
                            <div class="class-type">${cls.type?.label || 'Other'}</div>
                            <div class="class-package">${escapeHtml(cls.packageName || '')}</div>
                            ${cls.purpose ? `<div class="class-purpose">${escapeHtml(cls.purpose)}</div>` : ''}
                            ${cls.linesOfCode ? `<div class="class-loc">${cls.linesOfCode} lignes</div>` : ''}
                        </div>
                        ${cls.methods && cls.methods.length > 0 ? `
                            <div class="class-methods">
                                <details>
                                    <summary>Méthodes (${cls.methods.length})</summary>
                                    <ul>
                                        ${cls.methods.map(method => `<li>${escapeHtml(method)}</li>`).join('')}
                                    </ul>
                                </details>
                            </div>
                        ` : ''}
                    </div>
                `).join('')}
            </div>
        </div>
    `;
}

function createArrayList(title, items) {
    if (!items || items.length === 0) return '';

    return `
        <div class="array-list">
            <details>
                <summary>${title} (${items.length})</summary>
                <ul>
                    ${items.map(item => `<li>${escapeHtml(item)}</li>`).join('')}
                </ul>
            </details>
        </div>
    `;
}

function getClassIcon(type) {
    if (!type || !type.label) return '📦';

    const icons = {
        'Configuration': '⚙️',
        'Processor': '🔄',
        'Reader': '📖',
        'Writer': '✍️',
        'Entity': '📊',
        'Repository': '🗄️',
        'Validator': '✅',
        'Listener': '👂',
        'Decider': '🔀',
        'Mapper': '🗺️',
        'Utility': '🔧',
        'Other': '📦'
    };

    return icons[type.label] || '📦';
}

function createWarningDetailsSection(warningDetails) {
    if (!warningDetails || warningDetails.length === 0) return '';

    return `
        <div class="report-section">
            <h5>⚠️ Avertissements détaillés</h5>
            <div class="warning-details-list">
                ${warningDetails.map((warning, index) => `
                    <div class="warning-detail-item">
                        <div class="warning-header">
                            <span class="warning-icon warning-clickable"
                                  onclick='showCodeModal(${JSON.stringify(warning).replace(/'/g, "&apos;")})'
                                  title="Cliquez pour voir le code Java">
                                🔍
                            </span>
                            <span class="warning-message">${escapeHtml(warning.message)}</span>
                        </div>
                        <div class="warning-meta">
                            <span class="warning-file">📄 ${escapeHtml(warning.javaFile)}</span>
                            <span class="warning-line">📍 Ligne ${warning.javaLine}</span>
                        </div>
                    </div>
                `).join('')}
            </div>
        </div>
    `;
}

function resetForm() {
    selectedCobolFiles = [];
    selectedJclFiles = [];
    renderCobolFileList();
    renderJclFileList();
    conversionForm.reset();
    resultSection.classList.add('hidden');
}

// ===== Error Management =====
function showError(message) {
    errorSection.classList.remove('hidden');
    errorMessage.innerHTML = `<p>${escapeHtml(message)}</p>`;
    
    document.getElementById('closeErrorBtn').onclick = () => {
        errorSection.classList.add('hidden');
    };
}

// ===== Utility Functions =====
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

function hideAllMessages() {
    progressSection.classList.add('hidden');
    resultSection.classList.add('hidden');
    errorSection.classList.add('hidden');
}

// ===== Tab Navigation =====
document.querySelectorAll('.tab-btn').forEach(btn => {
    btn.addEventListener('click', () => {
        const tabName = btn.dataset.tab;
        
        // Update buttons
        document.querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');
        
        // Update panes
        document.querySelectorAll('.tab-pane').forEach(pane => pane.classList.remove('active'));
        document.getElementById(`tab-${tabName}`).classList.add('active');
    });
});
// ===== INFERENCE REPORT DISPLAY FUNCTIONS =====

/**
 * Display inference report with field analysis, type distribution, and recommendations
 */
function displayInferenceReport(inferenceData) {
    if (!inferenceData) {
        console.log('No inference data available');
        return;
    }
    
    const reportSection = document.getElementById('inferenceReportSection');
    
    // Update summary metrics
    document.getElementById('inferenceFieldsCount').textContent = inferenceData.totalFieldsInferred || 0;
    document.getElementById('inferenceQualityScore').textContent = (inferenceData.overallQualityScore || 0) + '%';
    
    // Calculate average confidence
    const avgConfidence = inferenceData.conversionMetrics?.averageConfidence || 0;
    document.getElementById('inferenceAvgConfidence').textContent = (avgConfidence * 100).toFixed(1) + '%';
    
    // Display inferred fields
    if (inferenceData.inferredFieldsMap && Object.keys(inferenceData.inferredFieldsMap).length > 0) {
        displayInferredFields(inferenceData.inferredFieldsMap);
    }
    
    // Display type distribution
    if (inferenceData.typeDistribution && Object.keys(inferenceData.typeDistribution).length > 0) {
        displayTypeDistribution(inferenceData.typeDistribution);
    }
    
    // Display recommendations
    if (inferenceData.recommendations && inferenceData.recommendations.length > 0) {
        displayRecommendations(inferenceData.recommendations);
    }
    
    // Show the inference report section
    reportSection.classList.remove('hidden');
}

/**
 * Display inferred fields in table
 */
function displayInferredFields(fieldsMap) {
    const tbody = document.getElementById('inferenceFieldsTbody');
    tbody.innerHTML = '';
    
    Object.entries(fieldsMap).forEach(([fieldName, field]) => {
        const row = document.createElement('tr');
        
        // Determine confidence color class
        const confidenceLevel = field.confidenceLevel || 'MEDIUM';
        const confidenceClass = `confidence-${confidenceLevel.toLowerCase()}`;
        const confidenceText = getConfidenceText(field.confidenceScore);
        
        // Prepare contexts
        const contextsHtml = (field.usageContexts || [])
            .slice(0, 2) // Show first 2 contexts
            .map(c => `<small>${c}</small>`)
            .join('<br/>');
        
        // Prepare suggestions
        const suggestionsHtml = (field.suggestedAnnotations || [])
            .slice(0, 2) // Show first 2 suggestions
            .map(s => `<small style="background: #f0f0f0; padding: 2px 6px; border-radius: 3px; display: inline-block; margin: 2px;">${s}</small>`)
            .join(' ');
        
        row.innerHTML = `
            <td><strong>${fieldName}</strong></td>
            <td><code>${field.javaType}</code></td>
            <td><span class="confidence-badge ${confidenceClass}">${confidenceText}</span></td>
            <td>${contextsHtml || '-'}</td>
            <td>${suggestionsHtml || '-'}</td>
        `;
        
        tbody.appendChild(row);
    });
}

/**
 * Display type distribution
 */
function displayTypeDistribution(typeDistribution) {
    const chartContainer = document.getElementById('typeDistributionChart');
    chartContainer.innerHTML = '';
    
    // Sort by count descending
    const sorted = Object.entries(typeDistribution)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 10); // Top 10 types
    
    sorted.forEach(([typeName, count]) => {
        const item = document.createElement('div');
        item.className = 'type-item';
        item.innerHTML = `
            <span class="type-name">${typeName}</span>
            <span class="type-count">${count}</span>
        `;
        chartContainer.appendChild(item);
    });
}

/**
 * Display smart recommendations
 */
function displayRecommendations(recommendations) {
    const container = document.getElementById('recommendationsContainer');
    container.innerHTML = '';
    
    recommendations.forEach(recommendation => {
        const card = document.createElement('div');
        card.className = 'recommendation-card';
        
        // Determine card type based on recommendation text
        if (recommendation.includes('✅')) {
            card.classList.add('positive');
        } else if (recommendation.includes('⚠️')) {
            card.classList.add('warning');
        } else if (recommendation.includes('❌')) {
            card.classList.add('alert');
        } else {
            card.classList.add('warning');
        }
        
        card.innerHTML = `<div class="recommendation-text">${recommendation}</div>`;
        container.appendChild(card);
    });
}

/**
 * Get human-readable confidence text
 */
function getConfidenceText(score) {
    if (score >= 0.9) return 'Très élevée';
    if (score >= 0.7) return 'Élevée';
    if (score >= 0.5) return 'Moyenne';
    return 'Basse';
}

// ===== CODE MODAL FUNCTIONS =====

/**
 * Show code modal with Java snippet
 */
function showCodeModal(warningDetail) {
    const modal = document.getElementById('codeModal');
    const modalTitle = document.getElementById('codeModalTitle');
    const modalFile = document.getElementById('codeModalFile');
    const modalLine = document.getElementById('codeModalLine');
    const modalCobol = document.getElementById('codeModalCobol');
    const modalCobolContainer = document.getElementById('codeModalCobolContainer');
    const modalSnippet = document.getElementById('codeModalSnippet');

    // Set modal content
    modalTitle.textContent = warningDetail.message || 'Code Java Généré';
    modalFile.textContent = warningDetail.javaFile || 'N/A';
    modalLine.textContent = `Ligne ${warningDetail.javaLine || '?'}`;

    // Show COBOL line if available
    if (warningDetail.cobolLine) {
        modalCobol.textContent = warningDetail.cobolLine;
        modalCobolContainer.style.display = 'flex';
    } else {
        modalCobolContainer.style.display = 'none';
    }

    // Set code snippet
    modalSnippet.querySelector('code').textContent = warningDetail.javaCodeSnippet || '// Code non disponible';

    // Show modal
    modal.style.display = 'flex';
    document.body.style.overflow = 'hidden';
}

/**
 * Close code modal
 */
function closeCodeModal() {
    const modal = document.getElementById('codeModal');
    modal.style.display = 'none';
    document.body.style.overflow = 'auto';
}

/**
 * Copy code to clipboard
 */
function copyCodeToClipboard() {
    const codeSnippet = document.getElementById('codeModalSnippet').querySelector('code').textContent;

    navigator.clipboard.writeText(codeSnippet).then(() => {
        const btn = document.querySelector('.copy-code-btn');
        const originalText = btn.textContent;
        btn.textContent = '✓ Copié!';
        btn.style.background = '#00A650';

        setTimeout(() => {
            btn.textContent = originalText;
            btn.style.background = '#007acc';
        }, 2000);
    }).catch(err => {
        console.error('Failed to copy:', err);
        alert('Impossible de copier le code');
    });
}

// Close modal when pressing Escape key
document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
        closeCodeModal();
    }
});