// ===== DOM Elements =====
const conversionForm = document.getElementById('conversionForm');
const fileInput = document.getElementById('cobolFiles');
const fileUploadArea = document.getElementById('fileUploadArea');
const fileList = document.getElementById('fileList');
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

// Selected files array
let selectedFiles = [];
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

// ===== File Upload - Drag and Drop =====
fileUploadArea.addEventListener('dragover', (e) => {
    console.log('dragover event');
    e.preventDefault();
    e.stopPropagation();
    fileUploadArea.classList.add('dragover');
});

fileUploadArea.addEventListener('dragleave', (e) => {
    console.log('dragleave event');
    e.preventDefault();
    e.stopPropagation();
    fileUploadArea.classList.remove('dragover');
});

fileUploadArea.addEventListener('drop', (e) => {
    console.log('drop event - files count:', e.dataTransfer.files.length);
    e.preventDefault();
    e.stopPropagation();
    fileUploadArea.classList.remove('dragover');

    const files = Array.from(e.dataTransfer.files);
    console.log('Files received:', files.map(f => f.name));
    
    const validFiles = files.filter(file =>
        file.name.endsWith('.cob') || file.name.endsWith('.cbl') || file.name.endsWith('.jcl')
    );

    console.log('Valid files:', validFiles.map(f => f.name));

    if (validFiles.length > 0) {
        addFiles(validFiles);
    } else {
        showError('Veuillez déposer des fichiers COBOL (.cob, .cbl) ou JCL (.jcl)');
    }
});

// ===== File Input Change =====
fileInput.addEventListener('change', (e) => {
    const files = Array.from(e.target.files);
    addFiles(files);
});

// ===== File Upload Area Click =====
fileUploadArea.addEventListener('click', () => {
    fileInput.click();
});

// ===== Add Files =====
function addFiles(files) {
    console.log('addFiles called with:', files.length, 'files');
    files.forEach(file => {
        if (!selectedFiles.find(f => f.name === file.name)) {
            selectedFiles.push(file);
            console.log('Added file:', file.name, 'Total now:', selectedFiles.length);
        }
    });
    console.log('Final selectedFiles length:', selectedFiles.length);
    renderFileList();
}

// ===== Remove File =====
function removeFile(index) {
    selectedFiles.splice(index, 1);
    renderFileList();
}

// ===== Render File List =====
function renderFileList() {
    if (selectedFiles.length === 0) {
        fileList.innerHTML = '';
        fileList.classList.add('hidden');
        return;
    }

    fileList.classList.remove('hidden');
    fileList.innerHTML = selectedFiles.map((file, index) => `
        <div class="file-item">
            <div>
                <span class="file-item-name">📄 ${file.name}</span>
                <span class="file-item-size">(${formatFileSize(file.size)})</span>
            </div>
            <button type="button" class="file-remove-btn" onclick="removeFile(${index})" title="Retirer">✕</button>
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

    console.log('Form submit event - selectedFiles.length:', selectedFiles.length);

    // Validate form
    const projectName = document.getElementById('projectName').value.trim();
    const basePackage = document.getElementById('basePackage').value.trim();

    if (!projectName) {
        showError('Veuillez entrer un nom de projet');
        return;
    }

    if (selectedFiles.length === 0) {
        console.error('No files selected! selectedFiles:', selectedFiles);
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

    selectedFiles.forEach(file => {
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

        // Get the ZIP file
        const blob = await response.blob();

        updateProgressStep('generation', 'done');
        updateProgressStep('maven', 'active');
        updateProgress(75);

        // Download the file
        const url = window.URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `${projectName}.zip`;
        document.body.appendChild(a);
        a.click();
        window.URL.revokeObjectURL(url);
        document.body.removeChild(a);

        updateProgressStep('maven', 'done');
        updateProgress(100);

        // Show success
        setTimeout(() => {
            hideProgress();
            showSuccess(projectName);
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

    // Reset form after a delay
    setTimeout(() => {
        resetForm();
    }, 2000);
}

function resetForm() {
    selectedFiles = [];
    renderFileList();
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
