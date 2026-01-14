# Programmes COBOL de Test Complets

Ce répertoire contient 3 programmes COBOL de test complets avec leurs JCL et données associées, conçus pour démontrer les capacités du convertisseur COBOL to Java Spring Batch.

## 📁 Structure

```
test-programs/
├── ORDER-PROCESSOR.cob      # Programme 1: Traitement de commandes
├── ORDER-PROCESSOR.jcl      # JCL associé
├── orders.dat               # Données de test
├── EMPLOYEE-PAYROLL.cob     # Programme 2: Calcul de paie
├── EMPLOYEE-PAYROLL.jcl     # JCL associé
├── employees.dat            # Données de test
├── DATA-TRANSFORMER.cob     # Programme 3: Transformation de données
├── DATA-TRANSFORMER.jcl     # JCL associé
└── rawdata.txt              # Données de test
```

## 🎯 Programme 1: ORDER-PROCESSOR

**Type**: Traitement de fichier séquentiel avec validations
**Complexité**: Moyenne
**Taux de conversion**: 100%

### Fonctionnalités

- Lecture séquentielle de commandes
- Validation multi-critères (quantité, prix, remise)
- Calculs d'agrégation (montants, totaux)
- Classification par statut (PENDING/APPROVED/REJECTED)
- Génération de rapport formaté
- Compteurs par catégorie

### Instructions COBOL Utilisées

- PERFORM UNTIL
- IF/THEN/ELSE avec OR
- EVALUATE TRUE
- ADD, COMPUTE
- MOVE
- DISPLAY
- 88-levels
- OPEN/CLOSE/READ/WRITE

### Génération

```bash
java -jar target/cobol-translator.jar translate \
  examples/test-programs/ORDER-PROCESSOR.cob \
  -p com.example.orderprocessor
```

### Résultat

- ✅ OrderFileRecord.java
- ✅ ReportFileRecord.java  
- ✅ OrderProcessor.java
- ✅ OrderJobConfiguration.java

---

## 💰 Programme 2: EMPLOYEE-PAYROLL

**Type**: Calcul de paie avec formules complexes
**Complexité**: Élevée
**Taux de conversion**: 100%

### Fonctionnalités

- Calcul salaire de base (heures × taux)
- Calcul heures supplémentaires (1.5x)
- Gestion bonus
- Calcul taxes variables (standard/réduit/exempté)
- Cotisations sociales
- Ajustements par niveau
- Construction de date (STRING)
- Statistiques détaillées

### Instructions COBOL Utilisées

- COMPUTE avec formules
- MULTIPLY/DIVIDE
- EVALUATE pour logique fiscale
- STRING DELIMITED BY SIZE
- ACCEPT FROM DATE
- SUBTRACT
- ON SIZE ERROR

### Génération

```bash
java -jar target/cobol-translator.jar translate \
  examples/test-programs/EMPLOYEE-PAYROLL.cob \
  -p com.example.payroll
```

### Résultat

- ✅ EmployeeFileRecord.java
- ✅ PayrollFileRecord.java
- ✅ EmployeeProcessor.java
- ✅ EmployeeJobConfiguration.java

---

## 🔄 Programme 3: DATA-TRANSFORMER

**Type**: Transformation et manipulation de chaînes
**Complexité**: Élevée
**Taux de conversion**: 84%

### Fonctionnalités

- Parsing de données délimitées (UNSTRING)
- Construction de chaînes (STRING)
- Comptage de caractères (INSPECT TALLYING)
- Remplacement de caractères (INSPECT REPLACING)
- Recherche dans table (SEARCH)
- Validation par table de codes
- Formatage de sortie complexe

### Instructions COBOL Utilisées

- UNSTRING DELIMITED BY
- STRING DELIMITED BY
- INSPECT TALLYING/REPLACING
- SEARCH avec WHEN
- OCCURS INDEXED BY
- SET
- Tableaux multidimensionnels

### Génération

```bash
java -jar target/cobol-translator.jar translate \
  examples/test-programs/DATA-TRANSFORMER.cob \
  -p com.example.datatransformer
```

### Résultat

- ✅ InputFileRecord.java
- ✅ OutputFileRecord.java
- ✅ DataProcessor.java
- ✅ DataJobConfiguration.java

---

## 📊 Comparaison

| Programme | LOC COBOL | LOC Java | Data Items | Statements | Conversion | Confidence |
|-----------|-----------|----------|------------|------------|------------|------------|
| ORDER-PROCESSOR | 280 | 818 | 70 | 38 | 100% | TRÈS HAUTE |
| EMPLOYEE-PAYROLL | 264 | 1,013 | 69 | 37 | 100% | TRÈS HAUTE |
| DATA-TRANSFORMER | 258 | 682 | 44 | 25 | 84% | HAUTE |
| **TOTAL** | **802** | **2,513** | **183** | **100** | **94.7%** | **HAUTE** |

---

## 🚀 Utilisation

### Convertir tous les programmes

```bash
# Programme 1
java -jar target/cobol-translator.jar translate \
  examples/test-programs/ORDER-PROCESSOR.cob

# Programme 2  
java -jar target/cobol-translator.jar translate \
  examples/test-programs/EMPLOYEE-PAYROLL.cob

# Programme 3
java -jar target/cobol-translator.jar translate \
  examples/test-programs/DATA-TRANSFORMER.cob
```

### Compiler le projet généré

```bash
cd ../generated-projects/customer-batch-processing
mvn clean compile
```

### Exécuter les tests

```bash
cd cobol-to-java-translator
mvn test
```

---

## 📝 Notes

- Les fichiers de données (.dat, .txt) sont en format COBOL fixe ou délimité
- Les JCL utilisent la syntaxe MVS standard
- Tous les programmes suivent le pattern batch standard COBOL
- Le code généré est compatible Spring Batch 5.x

---

## 📖 Documentation

- [Rapport Complet](../../docs/TEST_PROGRAMS_REPORT.md)
- [Guide d'Utilisation](../../README.md)
- [Documentation API](../../docs/)

