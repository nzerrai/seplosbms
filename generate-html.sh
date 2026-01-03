#!/bin/bash

################################################################################
# Script de génération de documentation HTML
# Convertit le fichier Markdown en HTML standalone avec Pandoc
################################################################################

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "════════════════════════════════════════════════════════════"
echo "  COBOL to Java Translator - Génération HTML"
echo "════════════════════════════════════════════════════════════"
echo

INPUT_FILE="docs/COMPLETE_DOCUMENTATION.md"
OUTPUT_FILE="docs/COBOL_to_Java_Translator_Documentation.html"

if [ ! -f "$INPUT_FILE" ]; then
    echo -e "${YELLOW}⚠️  Erreur: Fichier source non trouvé: $INPUT_FILE${NC}"
    exit 1
fi

echo "🔄 Génération du HTML en cours..."

# Générer HTML avec Pandoc (sans template personnalisé)
pandoc "$INPUT_FILE" \
    -o "$OUTPUT_FILE" \
    --standalone \
    --toc \
    --toc-depth=3 \
    --number-sections \
    --highlight-style=tango \
    --metadata title="COBOL to Java Spring Batch Translator - Documentation" \
    --css=https://cdn.jsdelivr.net/npm/github-markdown-css@5/github-markdown.css \
    --include-in-header=<(cat <<'EOF'
<style>
    body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
        line-height: 1.6;
        color: #24292e;
        background-color: #f6f8fa;
        margin: 0;
        padding: 20px;
    }
    .container {
        max-width: 980px;
        margin: 0 auto;
        padding: 45px;
        background: white;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
    }
    h1 {
        color: #0366d6;
        border-bottom: 2px solid #0366d6;
        padding-bottom: 10px;
    }
    h2 {
        color: #0366d6;
        border-bottom: 1px solid #eaecef;
        padding-bottom: 8px;
        margin-top: 24px;
    }
    h3 {
        color: #24292e;
        margin-top: 20px;
    }
    code {
        background-color: #f6f8fa;
        padding: 2px 6px;
        border-radius: 3px;
        font-family: 'Courier New', monospace;
        font-size: 0.9em;
    }
    pre {
        background-color: #f6f8fa;
        padding: 16px;
        border-radius: 6px;
        overflow-x: auto;
        border: 1px solid #e1e4e8;
    }
    pre code {
        background-color: transparent;
        padding: 0;
    }
    table {
        border-collapse: collapse;
        width: 100%;
        margin: 20px 0;
    }
    th, td {
        border: 1px solid #dfe2e5;
        padding: 12px;
        text-align: left;
    }
    th {
        background-color: #f6f8fa;
        font-weight: 600;
    }
    tr:nth-child(even) {
        background-color: #f9f9f9;
    }
    #TOC {
        background-color: #f6f8fa;
        padding: 20px;
        border-radius: 6px;
        margin-bottom: 30px;
        border: 1px solid #e1e4e8;
    }
    #TOC ul {
        list-style: none;
        padding-left: 20px;
    }
    #TOC > ul {
        padding-left: 0;
    }
    #TOC a {
        color: #0366d6;
        text-decoration: none;
    }
    #TOC a:hover {
        text-decoration: underline;
    }
    .mermaid {
        text-align: center;
        margin: 20px 0;
        background: white;
    }
    @media print {
        .container {
            box-shadow: none;
        }
    }
</style>
<script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
<script>
    mermaid.initialize({ startOnLoad: true, theme: 'default' });
</script>
EOF
)

if [ $? -eq 0 ]; then
    echo
    echo -e "${GREEN}✅ HTML généré avec succès!${NC}"
    echo
    echo "📄 Fichier: $OUTPUT_FILE"

    # Afficher la taille
    if [ -f "$OUTPUT_FILE" ]; then
        size=$(ls -lh "$OUTPUT_FILE" | awk '{print $5}')
        echo "📊 Taille: $size"
    fi

    echo
    echo "Pour ouvrir:"
    echo "  xdg-open $OUTPUT_FILE"
    echo
    echo "Ou dans votre navigateur favori:"
    echo "  firefox $OUTPUT_FILE"
    echo "  google-chrome $OUTPUT_FILE"
    echo
else
    echo
    echo -e "${YELLOW}❌ Erreur lors de la génération du HTML${NC}"
    exit 1
fi

echo "════════════════════════════════════════════════════════════"
