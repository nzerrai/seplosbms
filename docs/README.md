# Documentation du projet COBOL to Java Translator

Ce répertoire contient la documentation complète du projet.

## 📚 Fichiers disponibles

### Documentation principale

- **[COMPLETE_DOCUMENTATION.md](COMPLETE_DOCUMENTATION.md)** - Documentation technique complète (source)
  - 80+ pages
  - Algorithmes détaillés avec diagrammes
  - Guide d'utilisation complet
  - Limites et restrictions
  - Annexes et références

### Formats générés

Après génération, vous trouverez:

- **COBOL_to_Java_Translator_Documentation.pdf** - Version PDF (recommandé pour impression)
- **COBOL_to_Java_Translator_Documentation.html** - Version HTML (pour consultation web)

## 🚀 Génération de la documentation

### Génération PDF

```bash
# Depuis la racine du projet
./generate-pdf.sh
```

**Prérequis:**
- Pandoc
- LaTeX (texlive)
- mermaid-filter (optionnel, pour les diagrammes)

Voir [README_PDF_GENERATION.md](../README_PDF_GENERATION.md) pour les instructions d'installation détaillées.

### Génération HTML

```bash
# Depuis la racine du projet
./generate-html.sh
```

**Avantages du HTML:**
- Pas besoin de LaTeX
- Diagrammes Mermaid rendus dynamiquement
- Navigation interactive
- Recherche dans le navigateur

## 📖 Structure de la documentation

### 1. Introduction (Chapitre 1)
- Objectif du projet
- Problématique de la migration COBOL
- Cas d'usage

### 2. Architecture (Chapitre 2)
- Vue d'ensemble du système
- Pipeline de traduction
- Diagrammes d'architecture

### 3. Algorithmes détaillés (Chapitre 3)
- **Algorithme de parsing COBOL**
  - Phase lexicale
  - Phase syntaxique
  - Construction de l'AST

- **Algorithme de conversion de types**
  - Mapping COBOL → Java
  - Détection de types

- **Algorithme de génération**
  - Génération d'entités
  - Génération de processors
  - Génération de configurations

- **Algorithme de traduction d'instructions**
  - Conversions MOVE, COMPUTE, IF, etc.

### 4. Guide d'utilisation (Chapitre 4)
- Installation
- Utilisation CLI
- Utilisation API Java
- Configuration avancée
- Exemples de traduction

### 5. Limites et restrictions (Chapitre 5)
- Constructions COBOL non supportées
  - ❌ CICS (transactions)
  - ❌ DB2 embedded SQL
  - ❌ SORT complexes
  - ⚠️ EVALUATE (partiel)
  - ⚠️ REDEFINES (partiel)

- Limites techniques
  - Taille de fichiers
  - Complexité cyclomatique
  - Précision de traduction (80% auto, 15% ajustement, 5% manuel)

- Recommandations
  - Workflow de migration
  - Checklist de validation
  - Tests obligatoires

### 6. Annexes (Chapitre 6)
- **Annexe A**: Table de correspondance complète COBOL ↔ Java
- **Annexe B**: Fichier de configuration (translator.properties)
- **Annexe C**: Glossaire
- **Annexe D**: Références
- **Annexe E**: Support et contact

## 📊 Diagrammes inclus

La documentation contient de nombreux diagrammes Mermaid:

1. **Architecture globale** - Vue d'ensemble du système
2. **Pipeline de traduction** - Flux de conversion
3. **Diagramme d'état** - Analyse lexicale
4. **Graphe de décision** - Conversion de types
5. **Diagramme de séquence** - Génération d'entités
6. **Graphe de traduction** - Instructions COBOL → Java
7. **Workflow de migration** - Processus complet
8. **Pie chart** - Taux de traduction
9. **Graphe des features** - Fonctionnalités supportées

## 🎯 Public cible

Cette documentation s'adresse à:

- **Développeurs** - Utiliser et étendre le traducteur
- **Architectes** - Comprendre l'architecture et les limitations
- **Chefs de projet** - Planifier une migration COBOL → Java
- **Équipes QA** - Valider les traductions
- **Décideurs techniques** - Évaluer la solution

## 📝 Sections importantes

### Pour les débutants
→ Lire: **Introduction** + **Guide d'utilisation**

### Pour les développeurs
→ Lire: **Architecture** + **Algorithmes** + **API**

### Pour les migrations critiques
→ Lire: **Limites et restrictions** + **Recommandations**

### Pour l'implémentation
→ Lire: **Guide d'utilisation** + **Configuration** + **Exemples**

## 🔄 Mise à jour de la documentation

Pour modifier la documentation:

1. Éditer le fichier **COMPLETE_DOCUMENTATION.md**
2. Regénérer les formats:
   ```bash
   ./generate-pdf.sh
   ./generate-html.sh
   ```
3. Vérifier le rendu

## 📄 Licence

Apache License 2.0 - Voir [LICENSE](../LICENSE)

## 📞 Support

- GitHub Issues: https://github.com/your-org/cobol-to-java-translator/issues
- Documentation: Ce répertoire
- Examples: [../examples/](../examples/)

---

**Dernière mise à jour:** 2026-01-01

**Version:** 1.0.0
