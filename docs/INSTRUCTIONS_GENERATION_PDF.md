# 📄 Instructions pour Générer le PDF

## 🎯 Objectif

Convertir la roadmap complète (12 EPICs, 33 User Stories) en format PDF professionnel.

---

## ✅ Fichiers Déjà Disponibles

- ✅ **Markdown:** `docs/ROADMAP_TO_100_PERCENT.md` (60 Ko, 2248 lignes)
- ✅ **HTML:** `docs/ROADMAP_TO_100_PERCENT.html` (81 Ko) - **GÉNÉRÉ**

---

## 🚀 3 Méthodes de Conversion en PDF

### Méthode 1: Via Navigateur (La Plus Simple) ⭐ RECOMMANDÉE

**Avantages:** Aucune installation requise, meilleur rendu, gratuit

**Étapes:**

```bash
# 1. Ouvrir le fichier HTML dans un navigateur
firefox docs/ROADMAP_TO_100_PERCENT.html

# ou avec Google Chrome
google-chrome docs/ROADMAP_TO_100_PERCENT.html

# ou avec n'importe quel navigateur
xdg-open docs/ROADMAP_TO_100_PERCENT.html
```

**2. Dans le navigateur:**
- Appuyer sur `Ctrl+P` (ou Menu → Imprimer)
- **Destination:** Sélectionner "Enregistrer au format PDF"
- **Mise en page:**
  - Orientation: Portrait
  - Format: A4
  - Marges: Standard (ou Personnalisé: 2cm)
  - Échelle: 100%
- **Options:**
  - ✅ Cocher "Arrière-plans graphiques" (pour les couleurs)
  - ✅ Cocher "En-têtes et pieds de page" (optionnel)
- **Cliquer sur "Enregistrer"**
- **Nom du fichier:** `ROADMAP_TO_100_PERCENT.pdf`
- **Emplacement:** `docs/`

**Résultat:** PDF de ~80-100 pages, taille ~2-3 MB

---

### Méthode 2: Via wkhtmltopdf (Ligne de Commande)

**Avantages:** Automatisable, reproductible

**Prérequis:** Installer wkhtmltopdf

```bash
# Sur Debian/Ubuntu
sudo apt update
sudo apt install -y wkhtmltopdf

# Sur macOS
brew install wkhtmltopdf

# Sur Fedora/RHEL
sudo dnf install wkhtmltopdf
```

**Étapes:**

```bash
# Utiliser le script fourni
./convert-to-pdf.sh

# Ou directement avec wkhtmltopdf
wkhtmltopdf \
    --page-size A4 \
    --margin-top 20mm \
    --margin-bottom 20mm \
    --margin-left 25mm \
    --margin-right 25mm \
    --enable-local-file-access \
    --print-media-type \
    --footer-center "Page [page] / [toPage]" \
    docs/ROADMAP_TO_100_PERCENT.html \
    docs/ROADMAP_TO_100_PERCENT.pdf
```

**Résultat:** PDF automatiquement généré dans `docs/`

---

### Méthode 3: Via pandoc + LaTeX (Meilleure Qualité)

**Avantages:** Qualité professionnelle, table des matières automatique, numérotation

**Inconvénient:** Installation lourde (~500 MB)

**Prérequis:** Installer pandoc et LaTeX

```bash
# Sur Debian/Ubuntu (installation complète)
sudo apt update
sudo apt install -y \
    pandoc \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-latex-extra \
    texlive-xetex \
    texlive-lang-french

# Sur macOS
brew install pandoc
brew install basictex
```

**Étapes:**

```bash
# Utiliser le script fourni
./generate-roadmap-pdf.sh

# Ou directement avec pandoc
pandoc docs/ROADMAP_TO_100_PERCENT.md \
    --from markdown \
    --to pdf \
    --pdf-engine=xelatex \
    --toc \
    --toc-depth=3 \
    --number-sections \
    -V geometry:margin=2.5cm \
    -V papersize=a4 \
    -V fontsize=11pt \
    -V lang=fr \
    --output docs/ROADMAP_TO_100_PERCENT.pdf
```

**Résultat:** PDF haute qualité avec TOC, numérotation, bookmarks

---

## 📊 Comparaison des Méthodes

| Critère | Navigateur | wkhtmltopdf | pandoc |
|---------|-----------|-------------|--------|
| **Installation** | ✅ Aucune | ⚠️ Légère (~50 MB) | ❌ Lourde (~500 MB) |
| **Facilité** | ✅ Très facile | ✅ Facile | ⚠️ Moyen |
| **Qualité** | ✅ Excellente | ✅ Très bonne | ✅ Professionnelle |
| **Temps** | ⏱️ 1 min | ⏱️ 30 sec | ⏱️ 2 min |
| **Automatisation** | ❌ Manuelle | ✅ Scriptable | ✅ Scriptable |
| **Table matières** | ❌ Non | ❌ Non | ✅ Oui |
| **Bookmarks PDF** | ❌ Non | ❌ Non | ✅ Oui |
| **Rendu couleurs** | ✅ Excellent | ✅ Bon | ⚠️ Moyen |

**Recommandation:** Utiliser **Méthode 1 (Navigateur)** pour rapidité et simplicité.

---

## 🎨 Personnalisation du PDF

### Modifier les Styles (avant génération)

Éditer `docs/ROADMAP_TO_100_PERCENT.html` - section `<style>`:

```css
/* Changer la taille de police globale */
body { font-size: 11pt; }  /* Défaut: 10pt en print */

/* Changer les couleurs des titres */
h1 { color: #0066cc; }  /* Défaut: #d32f2f (rouge) */
h2 { color: #009900; }  /* Défaut: #1976d2 (bleu) */

/* Changer les marges d'impression */
@page {
    margin: 2.5cm;  /* Défaut: 2cm */
}
```

Puis re-générer le PDF.

---

## 🔍 Vérification du PDF Généré

```bash
# Vérifier la taille
ls -lh docs/ROADMAP_TO_100_PERCENT.pdf

# Compter les pages (si pdfinfo installé)
pdfinfo docs/ROADMAP_TO_100_PERCENT.pdf | grep Pages

# Ouvrir le PDF
xdg-open docs/ROADMAP_TO_100_PERCENT.pdf
```

**Attendu:**
- Taille: 2-4 MB
- Pages: ~80-100 pages
- Format: A4 (210x297 mm)

---

## 🐛 Dépannage

### Problème: Le PDF est trop grand (>10 MB)

**Solution:** Compresser le PDF

```bash
# Avec Ghostscript
gs -sDEVICE=pdfwrite \
   -dCompatibilityLevel=1.4 \
   -dPDFSETTINGS=/ebook \
   -dNOPAUSE -dQUIET -dBATCH \
   -sOutputFile=docs/ROADMAP_TO_100_PERCENT_compressed.pdf \
   docs/ROADMAP_TO_100_PERCENT.pdf

# Ou avec online tools: https://www.ilovepdf.com/compress_pdf
```

### Problème: Le texte est coupé sur les bords

**Solution:** Augmenter les marges

Dans le navigateur (Ctrl+P):
- Options avancées → Marges → Personnalisé
- Haut/Bas/Gauche/Droite: 2.5 cm

Ou dans `wkhtmltopdf`:
```bash
--margin-top 25mm --margin-bottom 25mm --margin-left 30mm --margin-right 30mm
```

### Problème: Les couleurs ne s'affichent pas

**Solution:** Activer l'impression des arrière-plans

Dans le navigateur (Ctrl+P):
- ✅ Cocher "Arrière-plans graphiques"

Dans `wkhtmltopdf`:
```bash
--print-media-type --enable-javascript
```

### Problème: Les blocs de code débordent

**Solution:** Réduire la taille de police du code

Éditer le HTML, section `<style>`:
```css
pre code {
    font-size: 8pt;  /* Réduire de 10pt à 8pt */
}
```

---

## 📦 Partage et Distribution

### Via Git

```bash
# Ajouter le PDF au repository (si <10 MB)
git add docs/ROADMAP_TO_100_PERCENT.pdf
git commit -m "docs: Add roadmap PDF"
git push
```

### Via Git LFS (si >10 MB)

```bash
# Installer Git LFS
sudo apt install git-lfs
git lfs install

# Tracker les PDFs
git lfs track "*.pdf"
git add .gitattributes

# Commit
git add docs/ROADMAP_TO_100_PERCENT.pdf
git commit -m "docs: Add roadmap PDF (via LFS)"
git push
```

### Via Cloud

- **Google Drive:** Upload + Partager lien
- **Dropbox:** Upload + Partager lien
- **OneDrive:** Upload + Partager lien
- **Email:** Attacher (si <10 MB)

---

## ✅ Checklist de Génération

- [ ] Fichier HTML généré (`docs/ROADMAP_TO_100_PERCENT.html`)
- [ ] Méthode de conversion choisie (Navigateur recommandé)
- [ ] PDF généré avec succès
- [ ] PDF vérifié (taille, nombre de pages, lisibilité)
- [ ] PDF enregistré dans `docs/ROADMAP_TO_100_PERCENT.pdf`
- [ ] PDF partagé avec l'équipe (Git, cloud, email)

---

## 📞 Support

**Questions ou problèmes ?**

1. Vérifier les prérequis installés
2. Consulter la section Dépannage
3. Essayer une méthode alternative
4. Contacter l'équipe de développement

---

**Dernière mise à jour:** 08 Janvier 2026
**Version:** 1.0
