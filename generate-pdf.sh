#!/bin/bash

# Script de génération de PDF depuis le guide utilisateur Markdown
# Nécessite: pandoc et texlive-latex-base

echo "🔄 Génération du PDF du Guide Utilisateur..."

# Vérifier si pandoc est installé
if ! command -v pandoc &> /dev/null; then
    echo "❌ Erreur: pandoc n'est pas installé"
    echo "📦 Installation:"
    echo "   Ubuntu/Debian: sudo apt install pandoc texlive-latex-base texlive-fonts-recommended"
    echo "   macOS: brew install pandoc basictex"
    echo "   Windows: https://pandoc.org/installing.html"
    exit 1
fi

# Créer répertoire de sortie
mkdir -p docs/pdf

# Générer le PDF avec pandoc
# Strip emojis and other Unicode characters that LaTeX can't handle

# Create temporary file without emojis
TEMP_MD=$(mktemp --suffix=.md)

# Remove all emojis (Unicode characters in emoji ranges)
python3 -c "
import sys
import re

# Read the markdown file
with open('USER_GUIDE.md', 'r', encoding='utf-8') as f:
    content = f.read()

# Remove emojis using regex
# This covers most emoji ranges
emoji_pattern = re.compile('['
    u'\U0001F600-\U0001F64F'  # emoticons
    u'\U0001F300-\U0001F5FF'  # symbols & pictographs
    u'\U0001F680-\U0001F6FF'  # transport & map symbols
    u'\U0001F1E0-\U0001F1FF'  # flags (iOS)
    u'\U00002702-\U000027B0'
    u'\U000024C2-\U0001F251'
    ']+', flags=re.UNICODE)

clean_content = emoji_pattern.sub('', content)

# Write to temp file
with open('$TEMP_MD', 'w', encoding='utf-8') as f:
    f.write(clean_content)
"

pandoc "$TEMP_MD" \
    -o docs/pdf/COBOL-Translator-User-Guide.pdf \
    --pdf-engine=pdflatex \
    --variable geometry:margin=2cm \
    --variable fontsize=11pt \
    --variable documentclass=article \
    --variable lang=fr \
    --table-of-contents \
    --toc-depth=3 \
    --number-sections \
    --highlight-style=tango \
    --metadata title="Guide Utilisateur COBOL to Java Translator" \
    --metadata author="Projet COBOL Translator" \
    --metadata date="$(date +'%d/%m/%Y')" \
    2>&1

# Clean up temporary file
rm -f "$TEMP_MD"

if [ $? -eq 0 ]; then
    echo "✅ PDF généré avec succès!"
    echo "📄 Fichier: docs/pdf/COBOL-Translator-User-Guide.pdf"

    # Afficher la taille du fichier
    size=$(du -h docs/pdf/COBOL-Translator-User-Guide.pdf | cut -f1)
    echo "📊 Taille: $size"

    # Ouvrir le PDF si possible
    if command -v xdg-open &> /dev/null; then
        echo "📖 Ouverture du PDF..."
        xdg-open docs/pdf/COBOL-Translator-User-Guide.pdf
    elif command -v open &> /dev/null; then
        echo "📖 Ouverture du PDF..."
        open docs/pdf/COBOL-Translator-User-Guide.pdf
    fi
else
    echo "❌ Erreur lors de la génération du PDF"
    echo "💡 Essayez avec une alternative:"
    echo "   1. Utiliser un service en ligne: https://www.markdowntopdf.com/"
    echo "   2. Utiliser wkhtmltopdf: brew install wkhtmltopdf"
    echo "   3. Utiliser un éditeur Markdown avec export PDF"
    exit 1
fi
