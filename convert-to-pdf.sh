#!/bin/bash

# Script simple de conversion HTML → PDF
# Utilise wkhtmltopdf (plus facile à installer que pandoc+texlive)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HTML_FILE="$SCRIPT_DIR/docs/ROADMAP_TO_100_PERCENT.html"
PDF_FILE="$SCRIPT_DIR/docs/ROADMAP_TO_100_PERCENT.pdf"

echo "=================================================="
echo "  Conversion HTML → PDF"
echo "=================================================="
echo ""

# Vérifier si wkhtmltopdf est installé
if ! command -v wkhtmltopdf &> /dev/null; then
    echo "❌ wkhtmltopdf n'est pas installé"
    echo ""
    echo "Installation rapide:"
    echo "  sudo apt install -y wkhtmltopdf"
    echo ""
    echo "Alternative:"
    echo "  Ouvrir $HTML_FILE dans un navigateur"
    echo "  et utiliser Ctrl+P → Enregistrer en PDF"
    echo ""
    exit 1
fi

echo "✓ wkhtmltopdf trouvé: $(wkhtmltopdf --version | head -n 1)"
echo ""

# Vérifier que le fichier HTML existe
if [ ! -f "$HTML_FILE" ]; then
    echo "❌ Fichier HTML introuvable: $HTML_FILE"
    echo ""
    echo "Générer d'abord le HTML:"
    echo "  ./generate-roadmap-html.sh"
    echo ""
    exit 1
fi

echo "✓ Fichier HTML trouvé: $HTML_FILE"
echo ""

# Convertir en PDF
echo "🔄 Conversion en cours..."
echo ""

wkhtmltopdf \
    --page-size A4 \
    --orientation Portrait \
    --margin-top 20mm \
    --margin-bottom 20mm \
    --margin-left 25mm \
    --margin-right 25mm \
    --enable-local-file-access \
    --print-media-type \
    --no-stop-slow-scripts \
    --enable-javascript \
    --javascript-delay 1000 \
    --footer-center "Page [page] / [toPage]" \
    --footer-font-size 8 \
    --title "Roadmap vers 100% - COBOL → Java" \
    "$HTML_FILE" \
    "$PDF_FILE" \
    2>&1 | grep -v "QStandardPaths" | grep -v "Qt" || true

# Vérifier le succès
if [ $? -eq 0 ] && [ -f "$PDF_FILE" ]; then
    PDF_SIZE=$(du -h "$PDF_FILE" | cut -f1)

    echo ""
    echo "=================================================="
    echo "  ✅ PDF généré avec succès !"
    echo "=================================================="
    echo ""
    echo "📄 Fichier: $PDF_FILE"
    echo "📊 Taille: $PDF_SIZE"
    echo ""
    echo "Pour ouvrir le PDF:"
    echo "  xdg-open \"$PDF_FILE\"  # Linux"
    echo "  open \"$PDF_FILE\"      # macOS"
    echo ""
else
    echo ""
    echo "❌ Erreur lors de la conversion"
    echo ""
    echo "Alternative: Utiliser un navigateur"
    echo "  1. Ouvrir: firefox \"$HTML_FILE\""
    echo "  2. Ctrl+P"
    echo "  3. Enregistrer en PDF"
    echo ""
    exit 1
fi
