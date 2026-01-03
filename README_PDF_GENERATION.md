# Génération de la documentation PDF

## Méthode 1 : Avec Pandoc (Recommandé)

### Installation des dépendances

**Sur Debian/Ubuntu:**
```bash
sudo apt-get update
sudo apt-get install -y pandoc texlive-latex-base texlive-fonts-recommended texlive-latex-extra texlive-lang-french

# Pour les diagrammes Mermaid (optionnel)
sudo apt-get install -y nodejs npm
sudo npm install -g mermaid-filter
```

**Sur macOS:**
```bash
brew install pandoc
brew install basictex

# Pour les diagrammes Mermaid (optionnel)
npm install -g mermaid-filter
```

### Génération du PDF

```bash
# Utiliser le script fourni
./generate-pdf.sh

# Le PDF sera généré dans: docs/COBOL_to_Java_Translator_Documentation.pdf
```

### Commande Pandoc manuelle

Si vous préférez lancer la commande manuellement:

```bash
pandoc docs/COMPLETE_DOCUMENTATION.md \
    -o docs/COBOL_to_Java_Translator_Documentation.pdf \
    --pdf-engine=pdflatex \
    --filter mermaid-filter \
    --toc \
    --toc-depth=3 \
    --number-sections \
    --highlight-style=tango \
    --variable colorlinks=true \
    --variable linkcolor=blue \
    --variable geometry:margin=2.5cm \
    --variable papersize=a4 \
    --variable fontsize=11pt \
    --variable documentclass=report
```

## Méthode 2 : Avec un éditeur Markdown

### Visual Studio Code

1. Installer l'extension **"Markdown PDF"**
2. Ouvrir `docs/COMPLETE_DOCUMENTATION.md`
3. Clic droit → "Markdown PDF: Export (pdf)"

### Typora

1. Télécharger Typora: https://typora.io/
2. Ouvrir `docs/COMPLETE_DOCUMENTATION.md`
3. File → Export → PDF

### Obsidian

1. Ouvrir le dossier dans Obsidian
2. Installer le plugin "Pandoc"
3. Exporter en PDF

## Méthode 3 : En ligne

### Markdown to PDF (dillinger.io)

1. Aller sur https://dillinger.io/
2. Copier le contenu de `docs/COMPLETE_DOCUMENTATION.md`
3. Export as → PDF

### HackMD

1. Aller sur https://hackmd.io/
2. Créer un nouveau document
3. Coller le contenu
4. Menu → Export → PDF

## Méthode 4 : Avec Docker

Si vous ne voulez pas installer Pandoc localement:

```bash
# Créer un conteneur avec Pandoc
docker run --rm \
    -v $(pwd):/workspace \
    -w /workspace \
    pandoc/latex:latest \
    pandoc docs/COMPLETE_DOCUMENTATION.md \
    -o docs/COBOL_to_Java_Translator_Documentation.pdf \
    --toc \
    --number-sections \
    --pdf-engine=pdflatex
```

## Résultat attendu

Le PDF généré contient:

- **Page de titre** avec métadonnées
- **Table des matières** (3 niveaux)
- **Sections numérotées** automatiquement
- **Graphiques Mermaid** rendus (si mermaid-filter installé)
- **Coloration syntaxique** pour le code
- **Liens cliquables** (table des matières, URLs)
- **Format A4** avec marges de 2.5cm

**Taille approximative:** 2-3 MB

**Nombre de pages:** ~80-100 pages

## Aperçu du contenu PDF

1. **Introduction** (5 pages)
   - Objectif du projet
   - Cas d'usage

2. **Architecture** (10 pages)
   - Vue d'ensemble avec diagrammes
   - Pipeline de traduction

3. **Algorithmes détaillés** (15 pages)
   - Parsing COBOL
   - Conversion de types
   - Génération de code

4. **Guide d'utilisation** (25 pages)
   - Installation
   - CLI et API
   - Exemples complets

5. **Limites et restrictions** (20 pages)
   - Constructions non supportées
   - Limites techniques
   - Recommandations

6. **Annexes** (25 pages)
   - Tables de correspondance
   - Configuration
   - Glossaire
   - Références

## Dépannage

### Erreur: "pandoc: command not found"

Pandoc n'est pas installé. Voir section Installation ci-dessus.

### Erreur: "pdflatex not found"

LaTeX n'est pas installé:
```bash
sudo apt-get install texlive-latex-base texlive-fonts-recommended texlive-latex-extra
```

### Diagrammes Mermaid non rendus

Si les diagrammes apparaissent en texte brut:
```bash
npm install -g mermaid-filter
```

Puis relancer la génération avec `--filter mermaid-filter`.

### Erreur: "Font not found"

Installer les polices supplémentaires:
```bash
sudo apt-get install texlive-fonts-extra fonts-dejavu
```

### PDF vide ou incomplet

Vérifier les logs:
```bash
./generate-pdf.sh 2>&1 | tee generation.log
cat generation.log
```

### Caractères français mal affichés

Ajouter le package babel français:
```bash
sudo apt-get install texlive-lang-french
```

## Alternative: Générer en HTML puis convertir

Si la génération PDF échoue, générer d'abord en HTML:

```bash
# Générer HTML
pandoc docs/COMPLETE_DOCUMENTATION.md \
    -o docs/COMPLETE_DOCUMENTATION.html \
    --standalone \
    --toc \
    --filter mermaid-filter

# Puis convertir HTML → PDF avec le navigateur
# Chrome/Chromium:
chromium --headless --print-to-pdf=docs/output.pdf docs/COMPLETE_DOCUMENTATION.html

# Ou ouvrir dans le navigateur et Ctrl+P → Enregistrer en PDF
```

## Support

Pour toute question sur la génération de PDF:
- Consulter la documentation Pandoc: https://pandoc.org/
- Issues GitHub du projet
