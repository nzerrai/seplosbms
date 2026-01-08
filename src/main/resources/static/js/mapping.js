/**
 * Mapping Viewer - JavaScript
 * Visualisation détaillée du mapping COBOL to Java
 */

// État global
let currentTheme = 'light';
let currentCobolFile = null;
let currentJavaFiles = [];
let mappingData = null;
let selectedLine = null;

// Initialisation
document.addEventListener('DOMContentLoaded', function() {
    initTheme();
    initFileUpload();
    initEventListeners();
});

/**
 * Initialisation du thème
 */
function initTheme() {
    const savedTheme = localStorage.getItem('theme') || 'light';
    setTheme(savedTheme);
    
    document.getElementById('themeBtn').addEventListener('click', toggleTheme);
}

function toggleTheme() {
    const newTheme = currentTheme === 'light' ? 'dark' : 'light';
    setTheme(newTheme);
}

function setTheme(theme) {
    currentTheme = theme;
    document.documentElement.setAttribute('data-theme', theme);
    localStorage.setItem('theme', theme);
    
    const themeBtn = document.getElementById('themeBtn');
    themeBtn.textContent = theme === 'light' ? '🌙' : '☀️';
}

/**
 * Initialisation de l'upload de fichier
 */
function initFileUpload() {
    const fileInput = document.getElementById('cobolFile');
    const fileUploadArea = document.getElementById('fileUploadArea');
    const fileInfo = document.getElementById('fileInfo');
    
    // Événements de drag & drop
    fileUploadArea.addEventListener('dragover', function(e) {
        e.preventDefault();
        fileUploadArea.classList.add('drag-over');
    });
    
    fileUploadArea.addEventListener('dragleave', function() {
        fileUploadArea.classList.remove('drag-over');
    });
    
    fileUploadArea.addEventListener('drop', function(e) {
        e.preventDefault();
        fileUploadArea.classList.remove('drag-over');
        
        const files = e.dataTransfer.files;
        if (files.length > 0) {
            fileInput.files = files;
            displayFileInfo(files[0]);
        }
    });
    
    // Click sur la zone
    fileUploadArea.addEventListener('click', function() {
        fileInput.click();
    });
    
    // Changement de fichier
    fileInput.addEventListener('change', function() {
        if (this.files.length > 0) {
            displayFileInfo(this.files[0]);
        }
    });
}

function displayFileInfo(file) {
    const fileInfo = document.getElementById('fileInfo');
    const size = (file.size / 1024).toFixed(2);
    
    fileInfo.innerHTML = `
        <strong>📄 ${file.name}</strong><br>
        <small>Taille: ${size} KB | Type: ${file.type || 'application/octet-stream'}</small>
    `;
    fileInfo.classList.remove('hidden');
}

/**
 * Initialisation des écouteurs d'événements
 */
function initEventListeners() {
    // Formulaire d'upload
    document.getElementById('uploadForm').addEventListener('submit', handleFileAnalysis);
    
    // Boutons d'action
    document.getElementById('exportBtn')?.addEventListener('click', exportMapping);
    document.getElementById('backBtn')?.addEventListener('click', showUploadSection);
    document.getElementById('closeDetailBtn')?.addEventListener('click', hideDetailPanel);
    
    // Recherche
    document.getElementById('searchInput')?.addEventListener('input', handleSearch);
    
    // Filtres
    document.querySelectorAll('.filter-btn').forEach(btn => {
        btn.addEventListener('click', handleFilter);
    });
}

/**
 * Gestion de l'analyse du fichier
 */
async function handleFileAnalysis(e) {
    e.preventDefault();
    
    const fileInput = document.getElementById('cobolFile');
    const file = fileInput.files[0];
    
    if (!file) {
        showError('Veuillez sélectionner un fichier COBOL');
        return;
    }
    
    const analyzeBtn = document.getElementById('analyzeBtn');
    const originalText = analyzeBtn.innerHTML;
    analyzeBtn.disabled = true;
    analyzeBtn.innerHTML = '<span class="loading"></span> Analyse en cours...';
    
    try {
        // Simulation de l'analyse (à remplacer par un vrai appel API)
        await simulateAnalysis(file);
        
        showMappingViewer();
    } catch (error) {
        console.error('Erreur lors de l\'analyse:', error);
        showError('Erreur lors de l\'analyse du fichier: ' + error.message);
    } finally {
        analyzeBtn.disabled = false;
        analyzeBtn.innerHTML = originalText;
    }
}

/**
 * Simulation de l'analyse (temporaire)
 * TODO: Remplacer par un vrai appel API au backend
 */
async function simulateAnalysis(file) {
    return new Promise((resolve, reject) => {
        const reader = new FileReader();
        
        reader.onload = function(e) {
            const content = e.target.result;
            currentCobolFile = {
                name: file.name,
                content: content,
                lines: content.split('\n')
            };
            
            // Génération de données de mapping simulées
            mappingData = generateMockMappingData(currentCobolFile);
            
            setTimeout(resolve, 1500); // Simulation d'un délai
        };
        
        reader.onerror = function() {
            reject(new Error('Impossible de lire le fichier'));
        };
        
        reader.readAsText(file);
    });
}

/**
 * Génération de données de mapping simulées
 * TODO: Remplacer par les vraies données du backend
 */
function generateMockMappingData(cobolFile) {
    const lines = cobolFile.lines;
    const mappings = [];
    
    // Détection des divisions et sections
    let currentDivision = null;
    let currentSection = null;
    
    lines.forEach((line, index) => {
        const trimmed = line.trim();
        
        // Division
        if (trimmed.includes('DIVISION')) {
            currentDivision = trimmed;
            mappings.push({
                lineNumber: index + 1,
                cobolCode: trimmed,
                type: 'division',
                javaFiles: [`${extractDivisionName(trimmed)}.java`],
                conversionRate: 100,
                status: 'converted',
                confidence: 'high'
            });
        }
        // Section
        else if (trimmed.includes('SECTION')) {
            currentSection = trimmed;
            mappings.push({
                lineNumber: index + 1,
                cobolCode: trimmed,
                type: 'section',
                javaFiles: currentDivision ? [`${extractDivisionName(currentDivision)}.java`] : [],
                conversionRate: 95,
                status: 'converted',
                confidence: 'high'
            });
        }
        // Instructions importantes
        else if (isImportantInstruction(trimmed)) {
            const rate = Math.floor(Math.random() * 30) + 70;
            mappings.push({
                lineNumber: index + 1,
                cobolCode: trimmed,
                type: 'instruction',
                javaFiles: getJavaFilesForInstruction(trimmed, currentDivision),
                conversionRate: rate,
                status: rate === 100 ? 'converted' : rate >= 70 ? 'partial' : 'failed',
                confidence: rate >= 90 ? 'high' : rate >= 70 ? 'medium' : 'low'
            });
        }
    });
    
    return {
        totalLines: lines.length,
        mappedLines: mappings.length,
        averageConversionRate: calculateAverageRate(mappings),
        mappings: mappings,
        javaFiles: extractJavaFiles(mappings)
    };
}

function extractDivisionName(division) {
    const match = division.match(/(\w+)\s+DIVISION/);
    return match ? match[1].charAt(0) + match[1].slice(1).toLowerCase() : 'Unknown';
}

function isImportantInstruction(line) {
    const keywords = [
        'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE',
        'IF', 'ELSE', 'PERFORM', 'CALL', 'ACCEPT', 'DISPLAY',
        'OPEN', 'CLOSE', 'READ', 'WRITE', 'REWRITE',
        'START', 'STOP', 'GO TO', 'EVALUATE'
    ];
    return keywords.some(kw => line.toUpperCase().includes(kw));
}

function getJavaFilesForInstruction(instruction, division) {
    const files = [];
    const divName = division ? extractDivisionName(division) : 'Main';
    
    if (instruction.includes('MOVE') || instruction.includes('COMPUTE')) {
        files.push(`${divName}.java`);
    }
    if (instruction.includes('DISPLAY') || instruction.includes('ACCEPT')) {
        files.push(`${divName}.java`, 'IOHandler.java');
    }
    if (instruction.includes('PERFORM') || instruction.includes('CALL')) {
        files.push(`${divName}.java`, 'Procedure.java');
    }
    
    return files.length > 0 ? files : [`${divName}.java`];
}

function calculateAverageRate(mappings) {
    if (mappings.length === 0) return 0;
    const sum = mappings.reduce((acc, m) => acc + m.conversionRate, 0);
    return Math.round(sum / mappings.length);
}

function extractJavaFiles(mappings) {
    const filesSet = new Set();
    mappings.forEach(m => {
        m.javaFiles.forEach(f => filesSet.add(f));
    });
    return Array.from(filesSet);
}

/**
 * Affichage de la vue de mapping
 */
function showMappingViewer() {
    document.getElementById('uploadSection').style.display = 'none';
    document.getElementById('mappingViewer').classList.remove('hidden');
    
    displayGlobalStats();
    displayCobolCode();
    displayJavaFiles();
    displayMappingLinks();
}

function showUploadSection() {
    document.getElementById('mappingViewer').classList.add('hidden');
    document.getElementById('uploadSection').style.display = 'block';
    
    // Reset
    document.getElementById('uploadForm').reset();
    document.getElementById('fileInfo').classList.add('hidden');
}

/**
 * Affichage des statistiques globales
 */
function displayGlobalStats() {
    document.getElementById('totalLines').textContent = mappingData.totalLines;
    document.getElementById('totalJavaFiles').textContent = mappingData.javaFiles.length;
    document.getElementById('conversionRate').textContent = mappingData.averageConversionRate + '%';
    
    const confidence = mappingData.averageConversionRate >= 90 ? 'Élevé' :
                      mappingData.averageConversionRate >= 70 ? 'Moyen' : 'Faible';
    document.getElementById('confidenceScore').textContent = confidence;
}

/**
 * Affichage du code COBOL
 */
function displayCobolCode() {
    const container = document.getElementById('cobolContent');
    const fileNameSpan = document.getElementById('cobolFileName');
    
    fileNameSpan.textContent = currentCobolFile.name;
    
    const html = currentCobolFile.lines.map((line, index) => {
        const lineNum = index + 1;
        const mapping = mappingData.mappings.find(m => m.lineNumber === lineNum);
        const statusClass = mapping ? mapping.status : '';
        const indicator = mapping ? getIndicator(mapping.conversionRate) : '';
        
        return `
            <div class="code-line ${statusClass}" data-line="${lineNum}">
                <span class="line-number">${lineNum}</span>
                <span class="line-content">${escapeHtml(line)}</span>
                ${indicator ? `<span class="line-indicator">${indicator}</span>` : ''}
            </div>
        `;
    }).join('');
    
    container.innerHTML = html;
    
    // Événements de clic sur les lignes
    container.querySelectorAll('.code-line').forEach(line => {
        line.addEventListener('click', handleLineClick);
    });
}

function getIndicator(rate) {
    if (rate === 100) return '✅';
    if (rate >= 70) return '⚠️';
    return '❌';
}

/**
 * Affichage des fichiers Java
 */
function displayJavaFiles() {
    const selector = document.getElementById('javaFileSelector');
    const content = document.getElementById('javaContent');
    
    if (mappingData.javaFiles.length === 0) {
        content.innerHTML = '<p style="text-align: center; padding: 50px; color: var(--sg-grey);">Aucun fichier Java généré</p>';
        return;
    }
    
    // Sélecteur de fichiers
    selector.innerHTML = mappingData.javaFiles.map((file, index) => `
        <button class="java-file-tab ${index === 0 ? 'active' : ''}" data-file="${file}">
            ${file}
        </button>
    `).join('');
    
    // Afficher le premier fichier
    displayJavaFile(mappingData.javaFiles[0]);
    
    // Événements
    selector.querySelectorAll('.java-file-tab').forEach(tab => {
        tab.addEventListener('click', function() {
            selector.querySelectorAll('.java-file-tab').forEach(t => t.classList.remove('active'));
            this.classList.add('active');
            displayJavaFile(this.dataset.file);
        });
    });
}

function displayJavaFile(fileName) {
    const content = document.getElementById('javaContent');
    
    // Génération de code Java simulé
    const javaCode = generateMockJavaCode(fileName);
    
    const html = javaCode.split('\n').map((line, index) => `
        <div class="code-line">
            <span class="line-number">${index + 1}</span>
            <span class="line-content">${escapeHtml(line)}</span>
        </div>
    `).join('');
    
    content.innerHTML = html;
}

function generateMockJavaCode(fileName) {
    const className = fileName.replace('.java', '');
    return `package com.cobol.translated;

import java.math.BigDecimal;
import java.util.*;

/**
 * Généré automatiquement depuis COBOL
 * Fichier: ${currentCobolFile.name}
 */
public class ${className} {
    
    private Map<String, Object> variables = new HashMap<>();
    
    public ${className}() {
        initializeVariables();
    }
    
    private void initializeVariables() {
        // Variables COBOL converties
    }
    
    public void execute() {
        // Logique principale
    }
    
    // Méthodes générées depuis les SECTIONS COBOL
}`;
}

/**
 * Affichage des liens de mapping
 */
function displayMappingLinks() {
    const container = document.getElementById('mappingLinks');
    
    if (mappingData.mappings.length === 0) {
        container.innerHTML = '<p style="text-align: center; color: var(--sg-grey);">Aucun mapping</p>';
        return;
    }
    
    const significantMappings = mappingData.mappings.filter(m => 
        m.type === 'division' || m.type === 'section' || m.conversionRate < 100
    ).slice(0, 10);
    
    const html = significantMappings.map(mapping => {
        const statusClass = mapping.status === 'converted' ? 'full' :
                          mapping.status === 'partial' ? 'partial' : 'failed';
        return `
            <div class="mapping-link ${statusClass}" 
                 data-line="${mapping.lineNumber}"
                 title="Ligne ${mapping.lineNumber}: ${mapping.conversionRate}%">
                <div class="percentage">${mapping.conversionRate}%</div>
                <div class="label">L${mapping.lineNumber}</div>
            </div>
        `;
    }).join('');
    
    container.innerHTML = html;
    
    // Événements
    container.querySelectorAll('.mapping-link').forEach(link => {
        link.addEventListener('click', function() {
            const lineNum = parseInt(this.dataset.line);
            highlightLine(lineNum);
            showLineDetails(lineNum);
        });
    });
}

/**
 * Gestion du clic sur une ligne COBOL
 */
function handleLineClick(e) {
    const line = e.currentTarget;
    const lineNum = parseInt(line.dataset.line);
    
    highlightLine(lineNum);
    showLineDetails(lineNum);
}

function highlightLine(lineNum) {
    // Retirer les sélections précédentes
    document.querySelectorAll('.code-line.selected').forEach(l => {
        l.classList.remove('selected');
    });
    
    // Sélectionner la ligne
    const line = document.querySelector(`.code-line[data-line="${lineNum}"]`);
    if (line) {
        line.classList.add('selected');
        line.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
    
    selectedLine = lineNum;
}

function showLineDetails(lineNum) {
    const mapping = mappingData.mappings.find(m => m.lineNumber === lineNum);
    
    if (!mapping) {
        hideDetailPanel();
        return;
    }
    
    const panel = document.getElementById('detailPanel');
    const content = document.getElementById('detailContent');
    
    content.innerHTML = `
        <div class="detail-section">
            <h5>📄 Code COBOL (Ligne ${mapping.lineNumber})</h5>
            <div class="detail-code">${escapeHtml(mapping.cobolCode)}</div>
        </div>
        
        <div class="detail-section">
            <h5>☕ Fichiers Java générés</h5>
            ${mapping.javaFiles.map(file => `<div class="detail-code">➜ ${file}</div>`).join('')}
        </div>
        
        <div class="detail-section">
            <h5>📊 Informations de conversion</h5>
            <div class="mapping-info">
                <div class="info-item">
                    <div class="info-label">Taux de conversion</div>
                    <div class="info-value">${mapping.conversionRate}%</div>
                </div>
                <div class="info-item">
                    <div class="info-label">Statut</div>
                    <div class="info-value">${getStatusLabel(mapping.status)}</div>
                </div>
                <div class="info-item">
                    <div class="info-label">Confiance</div>
                    <div class="info-value">${getConfidenceLabel(mapping.confidence)}</div>
                </div>
                <div class="info-item">
                    <div class="info-label">Type</div>
                    <div class="info-value">${mapping.type}</div>
                </div>
            </div>
        </div>
    `;
    
    panel.classList.remove('hidden');
}

function hideDetailPanel() {
    document.getElementById('detailPanel').classList.add('hidden');
}

function getStatusLabel(status) {
    const labels = {
        'converted': '✅ Converti',
        'partial': '⚠️ Partiel',
        'failed': '❌ Échec'
    };
    return labels[status] || status;
}

function getConfidenceLabel(confidence) {
    const labels = {
        'high': '🟢 Élevé',
        'medium': '🟡 Moyen',
        'low': '🔴 Faible'
    };
    return labels[confidence] || confidence;
}

/**
 * Gestion de la recherche
 */
function handleSearch(e) {
    const query = e.target.value.toLowerCase();
    const lines = document.querySelectorAll('#cobolContent .code-line');
    
    lines.forEach(line => {
        const content = line.querySelector('.line-content').textContent.toLowerCase();
        if (content.includes(query) || query === '') {
            line.style.display = 'flex';
        } else {
            line.style.display = 'none';
        }
    });
}

/**
 * Gestion des filtres
 */
function handleFilter(e) {
    const filter = e.target.dataset.filter;
    
    // Mise à jour des boutons
    document.querySelectorAll('.filter-btn').forEach(btn => {
        btn.classList.remove('active');
    });
    e.target.classList.add('active');
    
    // Application du filtre
    const lines = document.querySelectorAll('#cobolContent .code-line');
    lines.forEach(line => {
        if (filter === 'all') {
            line.style.display = 'flex';
        } else {
            if (line.classList.contains(filter)) {
                line.style.display = 'flex';
            } else {
                line.style.display = 'none';
            }
        }
    });
}

/**
 * Export du mapping
 */
function exportMapping() {
    const data = {
        cobolFile: currentCobolFile.name,
        analysisDate: new Date().toISOString(),
        statistics: {
            totalLines: mappingData.totalLines,
            mappedLines: mappingData.mappedLines,
            averageConversionRate: mappingData.averageConversionRate,
            javaFilesCount: mappingData.javaFiles.length
        },
        mappings: mappingData.mappings,
        javaFiles: mappingData.javaFiles
    };
    
    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `mapping-${currentCobolFile.name}-${Date.now()}.json`;
    a.click();
    URL.revokeObjectURL(url);
}

/**
 * Utilitaires
 */
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

function showError(message) {
    alert('❌ ' + message);
}
