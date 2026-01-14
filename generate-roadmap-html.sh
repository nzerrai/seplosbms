#!/bin/bash

# Script de génération du HTML de la roadmap
# Peut être converti en PDF via navigateur (Ctrl+P → Enregistrer en PDF)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCS_DIR="$SCRIPT_DIR/docs"
INPUT_FILE="$DOCS_DIR/ROADMAP_TO_100_PERCENT.md"
OUTPUT_FILE="$DOCS_DIR/ROADMAP_TO_100_PERCENT.html"

echo "=================================================="
echo "  Génération du HTML - Roadmap vers 100%"
echo "=================================================="
echo ""

# Vérifier si le fichier source existe
if [ ! -f "$INPUT_FILE" ]; then
    echo "❌ Erreur: Fichier source introuvable: $INPUT_FILE"
    exit 1
fi

echo "✓ Fichier source trouvé: $INPUT_FILE"
echo "  Taille: $(wc -l < "$INPUT_FILE") lignes"
echo ""

# Fonction pour convertir Markdown en HTML (simple)
convert_md_to_html() {
    # Lire le contenu Markdown
    local content=$(cat "$INPUT_FILE")

    # Créer le HTML avec styles pour impression PDF
    cat > "$OUTPUT_FILE" << 'HTML_HEADER'
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Roadmap vers 100% de Conversion - COBOL → Java Spring Batch</title>
    <style>
        @media print {
            @page {
                size: A4;
                margin: 2cm;
            }
            body {
                font-size: 10pt;
            }
            h1 {
                page-break-before: always;
            }
            h1:first-of-type {
                page-break-before: avoid;
            }
            pre, code, table {
                page-break-inside: avoid;
            }
            .no-print {
                display: none;
            }
        }

        @media screen {
            body {
                max-width: 1200px;
                margin: 0 auto;
                padding: 20px;
                background: #f5f5f5;
            }
            .container {
                background: white;
                padding: 40px;
                box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            }
        }

        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            line-height: 1.6;
            color: #333;
        }

        h1 {
            color: #d32f2f;
            border-bottom: 3px solid #d32f2f;
            padding-bottom: 10px;
            margin-top: 30px;
            font-size: 2em;
        }

        h2 {
            color: #1976d2;
            border-bottom: 2px solid #1976d2;
            padding-bottom: 8px;
            margin-top: 25px;
            font-size: 1.6em;
        }

        h3 {
            color: #388e3c;
            margin-top: 20px;
            font-size: 1.3em;
        }

        h4 {
            color: #f57c00;
            margin-top: 15px;
            font-size: 1.1em;
        }

        h5 {
            color: #7b1fa2;
            margin-top: 12px;
            font-size: 1em;
        }

        code {
            background: #f4f4f4;
            padding: 2px 6px;
            border-radius: 3px;
            font-family: 'Courier New', monospace;
            font-size: 0.9em;
            color: #c7254e;
        }

        pre {
            background: #2d2d2d;
            color: #f8f8f2;
            padding: 15px;
            border-radius: 5px;
            overflow-x: auto;
            line-height: 1.4;
        }

        pre code {
            background: transparent;
            color: inherit;
            padding: 0;
        }

        table {
            border-collapse: collapse;
            width: 100%;
            margin: 20px 0;
        }

        th {
            background: #1976d2;
            color: white;
            padding: 12px;
            text-align: left;
            font-weight: bold;
        }

        td {
            border: 1px solid #ddd;
            padding: 10px;
        }

        tr:nth-child(even) {
            background: #f9f9f9;
        }

        tr:hover {
            background: #f0f0f0;
        }

        ul, ol {
            margin: 15px 0;
            padding-left: 30px;
        }

        li {
            margin: 8px 0;
        }

        blockquote {
            border-left: 4px solid #1976d2;
            margin: 20px 0;
            padding: 10px 20px;
            background: #e3f2fd;
            font-style: italic;
        }

        .priority-critical {
            color: #d32f2f;
            font-weight: bold;
        }

        .priority-high {
            color: #f57c00;
            font-weight: bold;
        }

        .priority-medium {
            color: #388e3c;
            font-weight: bold;
        }

        .epic-box {
            border: 2px solid #1976d2;
            border-radius: 8px;
            padding: 20px;
            margin: 25px 0;
            background: #e3f2fd;
        }

        .user-story-box {
            border: 1px solid #388e3c;
            border-radius: 5px;
            padding: 15px;
            margin: 15px 0;
            background: #f1f8e9;
        }

        .toc {
            background: #f5f5f5;
            border: 1px solid #ddd;
            border-radius: 5px;
            padding: 20px;
            margin: 30px 0;
        }

        .toc h2 {
            margin-top: 0;
            color: #333;
            border: none;
        }

        .toc ul {
            list-style-type: none;
            padding-left: 0;
        }

        .toc li {
            margin: 5px 0;
        }

        .toc a {
            color: #1976d2;
            text-decoration: none;
        }

        .toc a:hover {
            text-decoration: underline;
        }

        .cover-page {
            text-align: center;
            padding: 100px 0;
            page-break-after: always;
        }

        .cover-title {
            font-size: 3em;
            color: #d32f2f;
            margin-bottom: 20px;
        }

        .cover-subtitle {
            font-size: 1.5em;
            color: #1976d2;
            margin-bottom: 40px;
        }

        .cover-info {
            font-size: 1.2em;
            color: #666;
            line-height: 2;
        }

        .print-button {
            position: fixed;
            top: 20px;
            right: 20px;
            background: #d32f2f;
            color: white;
            padding: 12px 24px;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            font-size: 16px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.2);
            z-index: 1000;
        }

        .print-button:hover {
            background: #b71c1c;
        }

        @media print {
            .cover-page {
                page-break-after: always;
            }
        }

        /* Emojis et icônes */
        .emoji {
            font-size: 1.2em;
        }

        /* Checkbox styles */
        input[type="checkbox"] {
            margin-right: 8px;
        }
    </style>
</head>
<body>
    <button class="print-button no-print" onclick="window.print()">🖨️ Imprimer / Enregistrer en PDF</button>

    <div class="container">
        <!-- Page de couverture -->
        <div class="cover-page">
            <h1 class="cover-title">🎯 ROADMAP VERS 100%</h1>
            <p class="cover-subtitle">COBOL → Java Spring Batch</p>
            <p class="cover-subtitle">Plan d'Implémentation Technique Détaillé</p>
            <div class="cover-info">
                <p><strong>Version:</strong> 1.0</p>
                <p><strong>Date:</strong> 08 Janvier 2026</p>
                <p><strong>Équipe:</strong> Développement COBOL→Java Translator</p>
                <p style="margin-top: 60px; font-size: 0.9em; color: #999;">
                    Ce document détaille les 12 EPICs et 33 User Stories<br>
                    pour atteindre 99-100% de taux de conversion automatique
                </p>
            </div>
        </div>

        <!-- Contenu Markdown converti -->
        <div id="content">
HTML_HEADER

    # Convertir le Markdown en HTML basique
    # Cette conversion est simplifiée mais fonctionnelle
    python3 << 'PYTHON_SCRIPT'
import re
import sys

def markdown_to_html(md_content):
    html = md_content

    # Échapper les caractères HTML spéciaux dans le code
    def escape_code(match):
        code = match.group(1)
        code = code.replace('<', '&lt;').replace('>', '&gt;')
        return f'<code>{code}</code>'

    # Blocs de code (```)
    def escape_code_block(match):
        lang = match.group(1) or ''
        code = match.group(2)
        code = code.replace('<', '&lt;').replace('>', '&gt;')
        return f'<pre><code class="language-{lang}">{code}</code></pre>'

    # Traiter les blocs de code en premier
    html = re.sub(r'```(\w*)\n(.*?)```', escape_code_block, html, flags=re.DOTALL)

    # Code inline
    html = re.sub(r'`([^`]+)`', escape_code, html)

    # Titres
    html = re.sub(r'^# (.+)$', r'<h1>\1</h1>', html, flags=re.MULTILINE)
    html = re.sub(r'^## (.+)$', r'<h2>\1</h2>', html, flags=re.MULTILINE)
    html = re.sub(r'^### (.+)$', r'<h3>\1</h3>', html, flags=re.MULTILINE)
    html = re.sub(r'^#### (.+)$', r'<h4>\1</h4>', html, flags=re.MULTILINE)
    html = re.sub(r'^##### (.+)$', r'<h5>\1</h5>', html, flags=re.MULTILINE)

    # Gras et italique
    html = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', html)
    html = re.sub(r'\*(.+?)\*', r'<em>\1</em>', html)

    # Liens
    html = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'<a href="\2">\1</a>', html)

    # Listes à puces
    lines = html.split('\n')
    in_list = False
    result = []
    for line in lines:
        if re.match(r'^[\-\*]\s+', line):
            if not in_list:
                result.append('<ul>')
                in_list = True
            item = re.sub(r'^[\-\*]\s+', '', line)
            result.append(f'<li>{item}</li>')
        elif re.match(r'^\d+\.\s+', line):
            if not in_list:
                result.append('<ol>')
                in_list = True
            item = re.sub(r'^\d+\.\s+', '', line)
            result.append(f'<li>{item}</li>')
        else:
            if in_list:
                result.append('</ul>' if result[-1].startswith('<li>') else '</ol>')
                in_list = False
            result.append(line)

    if in_list:
        result.append('</ul>')

    html = '\n'.join(result)

    # Tableaux Markdown
    def convert_table(match):
        lines = match.group(0).strip().split('\n')
        table_html = '<table>\n'

        # En-tête
        headers = [h.strip() for h in lines[0].split('|')[1:-1]]
        table_html += '<thead><tr>\n'
        for h in headers:
            table_html += f'<th>{h}</th>\n'
        table_html += '</tr></thead>\n<tbody>\n'

        # Lignes de données (sauter la ligne de séparation)
        for line in lines[2:]:
            cells = [c.strip() for c in line.split('|')[1:-1]]
            table_html += '<tr>\n'
            for c in cells:
                table_html += f'<td>{c}</td>\n'
            table_html += '</tr>\n'

        table_html += '</tbody>\n</table>\n'
        return table_html

    html = re.sub(r'^\|.+\|$\n^\|[\s\-:|]+\|$\n(?:^\|.+\|$\n?)+', convert_table, html, flags=re.MULTILINE)

    # Checkboxes
    html = re.sub(r'\[ \]', r'<input type="checkbox" disabled>', html)
    html = re.sub(r'\[x\]', r'<input type="checkbox" checked disabled>', html)
    html = re.sub(r'\[X\]', r'<input type="checkbox" checked disabled>', html)

    # Paragraphes
    paragraphs = html.split('\n\n')
    result = []
    for p in paragraphs:
        p = p.strip()
        if p and not p.startswith('<'):
            result.append(f'<p>{p}</p>')
        else:
            result.append(p)

    html = '\n'.join(result)

    # Nettoyer les balises vides
    html = re.sub(r'<p>\s*</p>', '', html)
    html = re.sub(r'<p>(<[^>]+>)\s*</p>', r'\1', html)

    return html

# Lire le fichier Markdown
with open('DOCS_DIR/ROADMAP_TO_100_PERCENT.md'.replace('DOCS_DIR', '$DOCS_DIR'), 'r', encoding='utf-8') as f:
    md_content = f.read()

# Convertir en HTML
html_content = markdown_to_html(md_content)

# Écrire le résultat
print(html_content)
PYTHON_SCRIPT

    cat >> "$OUTPUT_FILE" << 'HTML_FOOTER'
        </div>
    </div>

    <script>
        // Améliorer la navigation
        document.addEventListener('DOMContentLoaded', function() {
            // Ajouter des IDs aux titres pour la navigation
            const headings = document.querySelectorAll('h1, h2, h3, h4');
            headings.forEach((heading, index) => {
                const id = 'section-' + index;
                heading.id = id;
            });

            // Ajouter un bouton "retour en haut" lors du scroll
            let backToTop = document.createElement('button');
            backToTop.innerHTML = '⬆️ Haut';
            backToTop.className = 'print-button no-print';
            backToTop.style.top = '70px';
            backToTop.style.display = 'none';
            backToTop.onclick = () => window.scrollTo({top: 0, behavior: 'smooth'});
            document.body.appendChild(backToTop);

            window.addEventListener('scroll', () => {
                if (window.scrollY > 300) {
                    backToTop.style.display = 'block';
                } else {
                    backToTop.style.display = 'none';
                }
            });

            // Colorier les priorités
            document.querySelectorAll('strong').forEach(el => {
                const text = el.textContent.toLowerCase();
                if (text.includes('critique') || text.includes('critical')) {
                    el.classList.add('priority-critical');
                } else if (text.includes('haute') || text.includes('high')) {
                    el.classList.add('priority-high');
                } else if (text.includes('moyenne') || text.includes('medium')) {
                    el.classList.add('priority-medium');
                }
            });
        });
    </script>
</body>
</html>
HTML_FOOTER
}

echo "🔄 Conversion Markdown → HTML en cours..."
echo ""

# Convertir le Markdown en HTML
python3 << PYTHON_CONVERT
import re

def markdown_to_html(md_file, html_file):
    with open(md_file, 'r', encoding='utf-8') as f:
        md_content = f.read()

    html = md_content

    # Échapper code blocks
    code_blocks = []
    def save_code_block(match):
        lang = match.group(1) or ''
        code = match.group(2)
        code = code.replace('<', '&lt;').replace('>', '&gt;')
        code_blocks.append(f'<pre><code class="language-{lang}">{code}</code></pre>')
        return f'___CODE_BLOCK_{len(code_blocks)-1}___'

    html = re.sub(r'\`\`\`(\w*)\n(.*?)\`\`\`', save_code_block, html, flags=re.DOTALL)

    # Code inline
    inline_codes = []
    def save_inline_code(match):
        code = match.group(1).replace('<', '&lt;').replace('>', '&gt;')
        inline_codes.append(f'<code>{code}</code>')
        return f'___INLINE_CODE_{len(inline_codes)-1}___'

    html = re.sub(r'\`([^\`]+)\`', save_inline_code, html)

    # Titres
    html = re.sub(r'^# (.+)$', r'<h1>\1</h1>', html, flags=re.MULTILINE)
    html = re.sub(r'^## (.+)$', r'<h2>\1</h2>', html, flags=re.MULTILINE)
    html = re.sub(r'^### (.+)$', r'<h3>\1</h3>', html, flags=re.MULTILINE)
    html = re.sub(r'^#### (.+)$', r'<h4>\1</h4>', html, flags=re.MULTILINE)
    html = re.sub(r'^##### (.+)$', r'<h5>\1</h5>', html, flags=re.MULTILINE)

    # Gras et italique
    html = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', html)
    html = re.sub(r'\*(.+?)\*', r'<em>\1</em>', html)

    # Liens
    html = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'<a href="\2">\1</a>', html)

    # Tableaux
    def convert_table(match):
        lines = match.group(0).strip().split('\n')
        if len(lines) < 3:
            return match.group(0)

        table = '<table>\n<thead>\n<tr>\n'
        headers = [h.strip() for h in lines[0].split('|') if h.strip()]
        for h in headers:
            table += f'<th>{h}</th>\n'
        table += '</tr>\n</thead>\n<tbody>\n'

        for line in lines[2:]:
            cells = [c.strip() for c in line.split('|') if c.strip()]
            if cells:
                table += '<tr>\n'
                for c in cells:
                    table += f'<td>{c}</td>\n'
                table += '</tr>\n'

        table += '</tbody>\n</table>\n'
        return table

    html = re.sub(r'^\|.+\|\n\|[\s\-:]+\|\n(?:\|.+\|\n?)+', convert_table, html, flags=re.MULTILINE)

    # Listes
    lines = html.split('\n')
    result = []
    in_ul = False
    in_ol = False

    for line in lines:
        ul_match = re.match(r'^[\-\*]\s+(.+)$', line)
        ol_match = re.match(r'^(\d+)\.\s+(.+)$', line)

        if ul_match:
            if not in_ul:
                if in_ol:
                    result.append('</ol>')
                    in_ol = False
                result.append('<ul>')
                in_ul = True
            result.append(f'<li>{ul_match.group(1)}</li>')
        elif ol_match:
            if not in_ol:
                if in_ul:
                    result.append('</ul>')
                    in_ul = False
                result.append('<ol>')
                in_ol = True
            result.append(f'<li>{ol_match.group(2)}</li>')
        else:
            if in_ul:
                result.append('</ul>')
                in_ul = False
            if in_ol:
                result.append('</ol>')
                in_ol = False
            result.append(line)

    if in_ul:
        result.append('</ul>')
    if in_ol:
        result.append('</ol>')

    html = '\n'.join(result)

    # Checkboxes
    html = re.sub(r'\[\s\]', '<input type="checkbox" disabled>', html)
    html = re.sub(r'\[[xX]\]', '<input type="checkbox" checked disabled>', html)

    # Paragraphes
    paragraphs = html.split('\n\n')
    final_result = []
    for p in paragraphs:
        p = p.strip()
        if p and not any(p.startswith(tag) for tag in ['<h', '<ul', '<ol', '<table', '<pre', '<div']):
            final_result.append(f'<p>{p}</p>')
        else:
            final_result.append(p)

    html = '\n\n'.join(final_result)

    # Restaurer les code blocks
    for i, block in enumerate(code_blocks):
        html = html.replace(f'___CODE_BLOCK_{i}___', block)

    for i, code in enumerate(inline_codes):
        html = html.replace(f'___INLINE_CODE_{i}___', code)

    # Créer le fichier HTML complet
    with open(html_file, 'w', encoding='utf-8') as f:
        f.write('''<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Roadmap vers 100% - COBOL → Java Spring Batch</title>
    <style>
        @page { size: A4; margin: 2cm; }
        @media print {
            body { font-size: 10pt; }
            h1 { page-break-before: always; }
            h1:first-of-type { page-break-before: avoid; }
            pre, table { page-break-inside: avoid; }
            .no-print { display: none; }
        }
        @media screen {
            body { max-width: 1200px; margin: 0 auto; padding: 20px; background: #f5f5f5; }
            .container { background: white; padding: 40px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        }
        body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; line-height: 1.6; color: #333; }
        h1 { color: #d32f2f; border-bottom: 3px solid #d32f2f; padding-bottom: 10px; margin-top: 30px; }
        h2 { color: #1976d2; border-bottom: 2px solid #1976d2; padding-bottom: 8px; margin-top: 25px; }
        h3 { color: #388e3c; margin-top: 20px; }
        h4 { color: #f57c00; margin-top: 15px; }
        code { background: #f4f4f4; padding: 2px 6px; border-radius: 3px; font-family: monospace; color: #c7254e; }
        pre { background: #2d2d2d; color: #f8f8f2; padding: 15px; border-radius: 5px; overflow-x: auto; }
        pre code { background: transparent; color: inherit; padding: 0; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th { background: #1976d2; color: white; padding: 12px; text-align: left; }
        td { border: 1px solid #ddd; padding: 10px; }
        tr:nth-child(even) { background: #f9f9f9; }
        ul, ol { margin: 15px 0; padding-left: 30px; }
        li { margin: 8px 0; }
        .print-button { position: fixed; top: 20px; right: 20px; background: #d32f2f; color: white; padding: 12px 24px; border: none; border-radius: 5px; cursor: pointer; font-size: 16px; box-shadow: 0 2px 5px rgba(0,0,0,0.2); }
        .print-button:hover { background: #b71c1c; }
        .cover-page { text-align: center; padding: 100px 0; page-break-after: always; }
        .cover-title { font-size: 3em; color: #d32f2f; margin-bottom: 20px; }
        .cover-subtitle { font-size: 1.5em; color: #1976d2; margin-bottom: 40px; }
    </style>
</head>
<body>
    <button class="print-button no-print" onclick="window.print()">🖨️ Imprimer / PDF</button>
    <div class="container">
        <div class="cover-page">
            <h1 class="cover-title">🎯 ROADMAP VERS 100%</h1>
            <p class="cover-subtitle">COBOL → Java Spring Batch</p>
            <p class="cover-subtitle">Plan d'Implémentation Technique Détaillé</p>
            <div style="margin-top: 60px;">
                <p><strong>Version:</strong> 1.0</p>
                <p><strong>Date:</strong> 08 Janvier 2026</p>
                <p><strong>Équipe:</strong> Développement COBOL→Java Translator</p>
            </div>
        </div>
        <div id="content">
''')
        f.write(html)
        f.write('''
        </div>
    </div>
</body>
</html>
''')

markdown_to_html('$INPUT_FILE', '$OUTPUT_FILE')
print("✅ Conversion réussie")
PYTHON_CONVERT

if [ $? -eq 0 ] && [ -f "$OUTPUT_FILE" ]; then
    HTML_SIZE=$(du -h "$OUTPUT_FILE" | cut -f1)

    echo ""
    echo "=================================================="
    echo "  ✅ HTML généré avec succès !"
    echo "=================================================="
    echo ""
    echo "📄 Fichier: $OUTPUT_FILE"
    echo "📊 Taille: $HTML_SIZE"
    echo ""
    echo "Pour ouvrir et convertir en PDF:"
    echo "  1. Ouvrir dans un navigateur:"
    echo "     firefox \"$OUTPUT_FILE\"  # ou chrome, etc."
    echo ""
    echo "  2. Imprimer (Ctrl+P)"
    echo ""
    echo "  3. Choisir 'Enregistrer au format PDF'"
    echo ""
    echo "  4. Enregistrer sous:"
    echo "     $DOCS_DIR/ROADMAP_TO_100_PERCENT.pdf"
    echo ""
else
    echo ""
    echo "❌ Erreur lors de la génération du HTML"
    exit 1
fi

echo "Génération terminée avec succès !"
