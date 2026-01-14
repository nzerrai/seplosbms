#!/bin/bash

# Script de génération du PDF de la roadmap
# Utilise pandoc pour convertir Markdown en PDF professionnel

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCS_DIR="$SCRIPT_DIR/docs"
INPUT_FILE="$DOCS_DIR/ROADMAP_TO_100_PERCENT.md"
OUTPUT_FILE="$DOCS_DIR/ROADMAP_TO_100_PERCENT.pdf"

echo "=================================================="
echo "  Génération du PDF - Roadmap vers 100%"
echo "=================================================="
echo ""

# Vérifier si pandoc est installé
if ! command -v pandoc &> /dev/null; then
    echo "❌ Erreur: pandoc n'est pas installé"
    echo ""
    echo "Installation sur Debian/Ubuntu:"
    echo "  sudo apt update"
    echo "  sudo apt install -y pandoc texlive-latex-base texlive-fonts-recommended texlive-latex-extra texlive-xetex"
    echo ""
    echo "Installation sur macOS:"
    echo "  brew install pandoc"
    echo "  brew install basictex"
    echo ""
    exit 1
fi

echo "✓ pandoc trouvé: $(pandoc --version | head -n 1)"
echo ""

# Vérifier si le fichier source existe
if [ ! -f "$INPUT_FILE" ]; then
    echo "❌ Erreur: Fichier source introuvable: $INPUT_FILE"
    exit 1
fi

echo "✓ Fichier source trouvé: $INPUT_FILE"
echo "  Taille: $(wc -l < "$INPUT_FILE") lignes"
echo ""

# Créer le fichier de métadonnées YAML pour le PDF
METADATA_FILE="$DOCS_DIR/pdf-metadata.yaml"
cat > "$METADATA_FILE" << 'EOF'
---
title: "Roadmap vers 100% de Conversion"
subtitle: "COBOL → Java Spring Batch - Plan d'Implémentation Technique"
author: "Équipe de Développement"
date: "08 Janvier 2026"
version: "1.0"
lang: fr
toc: true
toc-depth: 3
toc-title: "Table des Matières"
numbersections: true
fontsize: 11pt
geometry:
  - margin=2.5cm
  - a4paper
documentclass: report
classoption:
  - oneside
papersize: a4
header-includes:
  - \usepackage{fancyhdr}
  - \pagestyle{fancy}
  - \fancyhead[L]{Roadmap COBOL→Java}
  - \fancyhead[R]{Version 1.0}
  - \fancyfoot[C]{\thepage}
  - \usepackage{xcolor}
  - \definecolor{linkcolor}{RGB}{0,102,204}
  - \hypersetup{colorlinks=true,linkcolor=linkcolor,urlcolor=linkcolor}
  - \usepackage{listings}
  - \lstset{basicstyle=\ttfamily\footnotesize,breaklines=true,frame=single,backgroundcolor=\color{gray!10}}
---
EOF

echo "✓ Métadonnées PDF créées"
echo ""

# Générer le PDF avec pandoc
echo "🔄 Génération du PDF en cours..."
echo ""

pandoc "$INPUT_FILE" \
    --metadata-file="$METADATA_FILE" \
    --from markdown+yaml_metadata_block+implicit_figures \
    --to pdf \
    --pdf-engine=xelatex \
    --highlight-style=tango \
    --number-sections \
    --toc \
    --toc-depth=3 \
    -V geometry:margin=2.5cm \
    -V papersize=a4 \
    -V fontsize=11pt \
    -V mainfont="DejaVu Sans" \
    -V monofont="DejaVu Sans Mono" \
    -V linkcolor=blue \
    -V urlcolor=blue \
    -V toccolor=blue \
    --output "$OUTPUT_FILE" \
    2>&1 | tee /tmp/pandoc-log.txt

# Vérifier si la génération a réussi
if [ $? -eq 0 ] && [ -f "$OUTPUT_FILE" ]; then
    PDF_SIZE=$(du -h "$OUTPUT_FILE" | cut -f1)
    PDF_PAGES=$(pdfinfo "$OUTPUT_FILE" 2>/dev/null | grep "Pages:" | awk '{print $2}' || echo "?")

    echo ""
    echo "=================================================="
    echo "  ✅ PDF généré avec succès !"
    echo "=================================================="
    echo ""
    echo "📄 Fichier: $OUTPUT_FILE"
    echo "📊 Taille: $PDF_SIZE"
    echo "📖 Pages: $PDF_PAGES"
    echo ""
    echo "Pour ouvrir le PDF:"
    echo "  xdg-open \"$OUTPUT_FILE\"  # Linux"
    echo "  open \"$OUTPUT_FILE\"      # macOS"
    echo ""
else
    echo ""
    echo "=================================================="
    echo "  ❌ Erreur lors de la génération du PDF"
    echo "=================================================="
    echo ""
    echo "Consultez les logs: /tmp/pandoc-log.txt"
    echo ""
    exit 1
fi

# Nettoyer les fichiers temporaires
rm -f "$METADATA_FILE"

echo "✓ Nettoyage effectué"
echo ""
echo "Génération terminée avec succès !"
