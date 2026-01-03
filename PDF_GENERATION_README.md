# 📄 Génération du PDF - Guide Utilisateur

## 🎯 Objectif

Convertir le guide utilisateur Markdown ([USER_GUIDE.md](USER_GUIDE.md)) en PDF professionnel.

---

## 🚀 Méthode 1 : Script automatique (Recommandé)

### Prérequis

**Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install pandoc texlive-latex-base texlive-fonts-recommended texlive-latex-extra
```

**macOS:**
```bash
brew install pandoc
brew install --cask basictex
```

**Windows:**
- Télécharger pandoc: https://pandoc.org/installing.html
- Télécharger MiKTeX: https://miktex.org/download

### Génération

```bash
./generate-pdf.sh
```

**Résultat:**
```
✅ PDF généré avec succès!
📄 Fichier: docs/pdf/COBOL-Translator-User-Guide.pdf
📊 Taille: 245K
```

Le PDF s'ouvre automatiquement (si possible).

---

## 🌐 Méthode 2 : Service en ligne (Sans installation)

### Option A : Markdown to PDF Online

1. Ouvrir https://www.markdowntopdf.com/
2. Copier le contenu de `USER_GUIDE.md`
3. Coller dans l'éditeur
4. Cliquer "Convert to PDF"
5. Télécharger le résultat

### Option B : GitHub

1. Pousser `USER_GUIDE.md` sur GitHub
2. Utiliser GitHub Actions avec pandoc
3. OU utiliser un service comme GitBook

---

## 🔧 Méthode 3 : Commande pandoc manuelle

Si le script ne fonctionne pas:

```bash
# Créer le répertoire
mkdir -p docs/pdf

# Générer le PDF
pandoc USER_GUIDE.md \
    -o docs/pdf/COBOL-Translator-User-Guide.pdf \
    --pdf-engine=pdflatex \
    --variable geometry:margin=2cm \
    --variable fontsize=11pt \
    --table-of-contents \
    --number-sections
```

---

## 📝 Méthode 4 : Éditeurs avec export PDF

### VS Code
1. Installer extension "Markdown PDF"
2. Ouvrir `USER_GUIDE.md`
3. Ctrl+Shift+P → "Markdown PDF: Export (pdf)"

### Typora
1. Ouvrir `USER_GUIDE.md` dans Typora
2. File → Export → PDF

### Obsidian
1. Ouvrir `USER_GUIDE.md` dans Obsidian
2. Installer plugin "Pandoc"
3. Exporter en PDF

---

## ✅ Vérification du PDF généré

Le PDF doit contenir:
- ✅ Table des matières cliquable
- ✅ Numérotation des sections
- ✅ Code coloré
- ✅ Tableaux formatés
- ✅ ~30-40 pages

**Taille attendue:** 200-300 KB

---

## 🎨 Personnalisation du PDF

### Modifier les marges

Éditez `generate-pdf.sh`:
```bash
--variable geometry:margin=1.5cm \
```

### Changer la police

```bash
--variable fontfamily=times \
```

### Ajouter un logo

```bash
--include-in-header=header.tex \
```

Créez `header.tex`:
```latex
\usepackage{graphicx}
\usepackage{fancyhdr}
\pagestyle{fancy}
\fancyhead[L]{\includegraphics[width=2cm]{logo.png}}
```

---

## 🐛 Dépannage

### Erreur: "pandoc not found"

**Solution:**
```bash
# Ubuntu
sudo apt install pandoc

# macOS
brew install pandoc
```

### Erreur: "pdflatex not found"

**Solution:**
```bash
# Ubuntu
sudo apt install texlive-latex-base

# macOS
brew install --cask basictex
```

### Erreur: "LaTeX Error: File not found"

**Solution:**
```bash
# Installer packages LaTeX supplémentaires
sudo apt install texlive-latex-extra texlive-fonts-extra
```

### PDF vide ou incomplet

**Solution:**
```bash
# Vérifier le fichier Markdown
cat USER_GUIDE.md | wc -l  # Doit retourner ~600+ lignes

# Générer avec debug
pandoc USER_GUIDE.md -o test.pdf --verbose
```

---

## 📦 Alternatives à Pandoc

### wkhtmltopdf

```bash
# Installation
sudo apt install wkhtmltopdf  # Ubuntu
brew install wkhtmltopdf       # macOS

# Génération
wkhtmltopdf USER_GUIDE.md docs/pdf/guide.pdf
```

### Grip (via HTML)

```bash
# Installation
pip install grip

# Générer HTML
grip USER_GUIDE.md --export guide.html

# Convertir HTML en PDF (avec navigateur)
# Chrome: Ctrl+P → Save as PDF
```

### Prince XML (Professionnel)

```bash
# Commercial mais version d'essai gratuite
prince USER_GUIDE.md -o guide.pdf
```

---

## 📧 Support

Si vous rencontrez des problèmes:

1. Vérifiez que `USER_GUIDE.md` existe
2. Vérifiez les dépendances installées
3. Essayez une méthode alternative
4. Consultez la documentation pandoc: https://pandoc.org/

---

**Version:** 1.0.0
**Date:** 2026-01-02
