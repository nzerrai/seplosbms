# 🔧 Guide de Correction des Erreurs de Compilation

**Date**: 10 janvier 2026
**Projet**: customer-batch-processing (généré depuis BANKTRAN)
**Erreurs détectées**: 68 erreurs de compilation

---

## 📊 Vue d'Ensemble des Erreurs

| Type d'Erreur | Nombre | Sévérité | Fichiers Affectés |
|---------------|--------|----------|-------------------|
| Types incompatibles (int vs Long/BigDecimal) | 26 | 🔴 Critique | BanktranProcessor.java |
| Symboles introuvables (getters manquants) | 24 | 🔴 Critique | BanktranProcessor.java |
| Classes Spring Batch non trouvées | 4 | 🟡 Moyen | JobConfiguration files |
| Méthode dupliquée | 1 | 🟡 Moyen | CustprocJobConfiguration.java |
| Opérateurs incompatibles | 1 | 🟡 Moyen | CustprocProcessor.java |
| Autres | 12 | Variable | Divers |

---

## 🎯 Stratégie de Correction

### Approche Recommandée

**Phase 1**: Corrections automatiques dans le générateur (BusinessLogicTranslator.java)
**Phase 2**: Corrections manuelles dans le projet généré
**Phase 3**: Régénération complète du projet

**Temps estimé**: 2-3 heures

---

## 🔴 PHASE 1: Corriger le Générateur (PRIORITÉ HAUTE)

Ces corrections sont déjà partiellement faites, mais vérifions et complétons:

### 1.1 ✅ Correction du problème "NOT =" (DÉJÀ CORRIGÉ)

**Fichier**: `src/main/java/com/cobol/translator/generator/BusinessLogicTranslator.java`

**Ligne 639-660** - Ordre des remplacements corrigé:
```java
// ✅ CORRECT: Traiter "NOT =" AVANT de remplacer "NOT" seul
result = result.replaceAll("(?i)\\bNOT\\s*=", "!=");
result = result.replaceAll("(?i)\\bIS\\s+NOT\\s+EQUAL", "!=");
// ... puis après:
result = result.replaceAll("(?i)\\bNOT\\b", "!");
```

**Statut**: ✅ Déjà corrigé

### 1.2 ✅ Support des conditions NUMERIC (DÉJÀ CORRIGÉ)

**Ligne 640-646** - Support ajouté:
```java
// ✅ CORRECT: Support des class conditions
result = result.replaceAll("(?i)\\bIS\\s+NOT\\s+NUMERIC\\b", ".isNumeric() == false");
result = result.replaceAll("(?i)\\bNOT\\s+NUMERIC\\b", ".isNumeric() == false");
result = result.replaceAll("(?i)\\bIS\\s+NUMERIC\\b", ".isNumeric()");
```

**Statut**: ✅ Déjà corrigé

### 1.3 ❌ Types incompatibles (À CORRIGER)

**Problème**: Les littéraux numériques sont générés comme `int` alors qu'ils doivent être `Long` ou `BigDecimal`.

**Exemple d'erreur**:
```java
// ❌ ERREUR
this.wsTransRead = this.wsTransRead.add(1);  // 1 est int, pas BigDecimal
this.wsCurrAcctNum = 0;                      // 0 est int, pas Long
```

**Correction dans BusinessLogicTranslator.java**:

**Méthode `toJavaExpression` (ligne 747)**:
