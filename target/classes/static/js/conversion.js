// DOM elements
const conversionForm = document.getElementById('conversionForm');
const fileInput = document.getElementById('cobolFiles');
const fileUploadArea = document.getElementById('fileUploadArea');
const fileList = document.getElementById('fileList');
const convertBtn = document.getElementById('convertBtn');
const progressSection = document.getElementById('progressSection');
const progressFill = document.getElementById('progressFill');
const progressText = document.getElementById('progressText');
const resultSection = document.getElementById('resultSection');
const errorSection = document.getElementById('errorSection');
const errorMessage = document.getElementById('errorMessage');
const downloadMessage = document.getElementById('downloadMessage');

// Selected files array
let selectedFiles = [];

// File upload drag and drop
fileUploadArea.addEventListener('dragover', (e) => {
    e.preventDefault();
    fileUploadArea.classList.add('dragover');
});

fileUploadArea.addEventListener('dragleave', () => {
    fileUploadArea.classList.remove('dragover');
});

fileUploadArea.addEventListener('drop', (e) => {
    e.preventDefault();
    fileUploadArea.classList.remove('dragover');

    const files = Array.from(e.dataTransfer.files);
    const cobolFiles = files.filter(file =>
        file.name.endsWith('.cob') || file.name.endsWith('.cbl')
    );

    if (cobolFiles.length > 0) {
        addFiles(cobolFiles);
    } else {
        showError('Veuillez déposer des fichiers COBOL (.cob ou .cbl)');
    }
});

// File input change
fileInput.addEventListener('change', (e) => {
    const files = Array.from(e.target.files);
    addFiles(files);
});

// Add files to the list
function addFiles(files) {
    files.forEach(file => {
        if (!selectedFiles.find(f => f.name === file.name)) {
            selectedFiles.push(file);
        }
    });
    renderFileList();
}

// Remove file from list
function removeFile(index) {
    selectedFiles.splice(index, 1);
    renderFileList();
}

// Render file list
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
            <button type="button" class="file-item-remove" onclick="removeFile(${index})">
                ✕ Retirer
            </button>
        </div>
    `).join('');
}

// Format file size
function formatFileSize(bytes) {
    if (bytes < 1024) return bytes + ' B';
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
    return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
}

// Form submission
conversionForm.addEventListener('submit', async (e) => {
    e.preventDefault();

    // Validate form
    const projectName = document.getElementById('projectName').value.trim();
    const basePackage = document.getElementById('basePackage').value.trim();

    if (!projectName) {
        showError('Veuillez entrer un nom de projet');
        return;
    }

    if (selectedFiles.length === 0) {
        showError('Veuillez sélectionner au moins un fichier COBOL');
        return;
    }

    // Validate package name
    const packageRegex = /^[a-z][a-z0-9_]*(\.[a-z][a-z0-9_]*)*$/;
    if (basePackage && !packageRegex.test(basePackage)) {
        showError('Le nom du package n\'est pas valide (ex: com.example.batch)');
        return;
    }

    // Hide previous results/errors
    hideAllMessages();

    // Show progress
    showProgress('Préparation de la conversion...');

    // Prepare form data
    const formData = new FormData();
    formData.append('projectName', projectName);
    formData.append('basePackage', basePackage || 'com.example.batch');

    selectedFiles.forEach(file => {
        formData.append('files', file);
    });

    // Disable button
    convertBtn.disabled = true;
    convertBtn.classList.add('loading');

    try {
        // Update progress
        updateProgress(20, 'Upload des fichiers COBOL...');

        // Send request
        const response = await fetch('/conversion/upload', {
            method: 'POST',
            body: formData
        });

        updateProgress(50, 'Parsing des fichiers COBOL...');

        if (!response.ok) {
            const errorData = await response.json();

            // Check if it's a detailed error response
            if (errorData.errorType) {
                throw { detailed: true, data: errorData };
            }

            throw new Error(errorData.error || 'Erreur lors de la conversion');
        }

        updateProgress(75, 'Génération du projet Spring Batch...');

        // Get the ZIP file
        const blob = await response.blob();

        updateProgress(100, 'Téléchargement du projet...');

        // Download the file
        const url = window.URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `${projectName}.zip`;
        document.body.appendChild(a);
        a.click();
        window.URL.revokeObjectURL(url);
        document.body.removeChild(a);

        // Show success
        setTimeout(() => {
            hideProgress();
            showSuccess(projectName);
        }, 500);

    } catch (error) {
        console.error('Conversion error:', error);
        hideProgress();

        // Handle detailed error response
        if (error.detailed && error.data) {
            showDetailedError(error.data);
        } else {
            showError(error.message || 'Une erreur est survenue lors de la conversion');
        }
    } finally {
        // Re-enable button
        convertBtn.disabled = false;
        convertBtn.classList.remove('loading');
    }
});

// Show progress
function showProgress(message) {
    progressSection.classList.remove('hidden');
    progressText.textContent = message;
    progressFill.style.width = '0%';
}

// Update progress
function updateProgress(percent, message) {
    progressFill.style.width = percent + '%';
    progressText.textContent = message;
}

// Hide progress
function hideProgress() {
    progressSection.classList.add('hidden');
}

// Show success
function showSuccess(projectName) {
    resultSection.classList.remove('hidden');
    downloadMessage.innerHTML = `
        Le fichier <strong>${projectName}.zip</strong> a été téléchargé avec succès.<br>
        Décompressez-le et exécutez <code>mvn clean package</code> pour compiler le projet.
    `;

    // Reset form after a delay
    setTimeout(() => {
        selectedFiles = [];
        renderFileList();
        conversionForm.reset();
    }, 1000);
}

// Show error
function showError(message) {
    errorSection.classList.remove('hidden');
    errorMessage.innerHTML = `<p>${escapeHtml(message)}</p>`;

    // Auto-hide after 8 seconds
    setTimeout(() => {
        errorSection.classList.add('hidden');
    }, 8000);
}

// Show detailed error with file, line, and suggestions
function showDetailedError(errorData) {
    errorSection.classList.remove('hidden');

    let errorHTML = '<div class="detailed-error">';

    // Error type header
    const errorTypeLabel = {
        'JCL_PARSE_ERROR': '❌ Erreur de syntaxe JCL',
        'COBOL_PARSE_ERROR': '❌ Erreur de syntaxe COBOL',
        'IO_ERROR': '❌ Erreur d\'entrée/sortie',
        'CONVERSION_ERROR': '❌ Erreur de conversion'
    };

    errorHTML += `<h4>${errorTypeLabel[errorData.errorType] || '❌ Erreur'}</h4>`;

    // File name
    if (errorData.fileName) {
        errorHTML += `<p><strong>Fichier:</strong> ${escapeHtml(errorData.fileName)}</p>`;
    }

    // Line and column
    if (errorData.lineNumber) {
        errorHTML += `<p><strong>Ligne:</strong> ${errorData.lineNumber}`;
        if (errorData.columnNumber !== null && errorData.columnNumber !== undefined) {
            errorHTML += `, <strong>Colonne:</strong> ${errorData.columnNumber}`;
        }
        errorHTML += '</p>';
    }

    // Error message
    if (errorData.errorMessage) {
        errorHTML += `<p class="error-msg"><strong>Message:</strong> ${escapeHtml(errorData.errorMessage)}</p>`;
    }

    // Detailed message
    if (errorData.detailedMessage) {
        errorHTML += `<p class="error-detail">${escapeHtml(errorData.detailedMessage)}</p>`;
    }

    // Suggestion
    if (errorData.suggestion) {
        errorHTML += `<div class="error-suggestion">`;
        errorHTML += `<strong>💡 Suggestion:</strong><br>`;
        errorHTML += `${escapeHtml(errorData.suggestion)}`;
        errorHTML += `</div>`;
    }

    errorHTML += '</div>';
    errorMessage.innerHTML = errorHTML;

    // Don't auto-hide detailed errors - user needs time to read them
}

// Escape HTML to prevent XSS
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// Hide all messages
function hideAllMessages() {
    progressSection.classList.add('hidden');
    resultSection.classList.add('hidden');
    errorSection.classList.add('hidden');
}

// Make removeFile available globally
window.removeFile = removeFile;
