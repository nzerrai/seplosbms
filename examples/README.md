# Exemples et Données de Test COBOL to Java Translator

Ce répertoire contient des exemples de programmes COBOL et des outils pour tester la traduction.

## 📁 Fichiers Disponibles

### Programmes COBOL
- **`simple-customer.cob`** - Programme COBOL simple de traitement de fichier clients
  - Lit un fichier de clients
  - Compte les enregistrements traités
  - Affiche les clients avec montant > 1000€

### Données de Test
- **`customers.dat`** - Fichier de données au format COBOL (position fixe)
- **`customers.csv`** - Fichier de données au format CSV pour Java
- **`test_sample.dat`** / **`test_sample.csv`** - Exemples générés automatiquement

### Outils
- **`generate_test_data.py`** - Générateur de données de test aléatoires
- **`run-tests.sh`** - Script automatisé de test et validation
- **`TEST_DATA_README.md`** - Documentation détaillée du format des données

## 🚀 Utilisation Rapide

### 1. Traduire un Programme COBOL

```bash
# Depuis le répertoire racine du projet
java -jar target/cobol-translator.jar translate ./examples/simple-customer.cob
```

Le projet Java sera généré dans `../generated-projects/customer-batch-processing/`

### 2. Générer des Données de Test

```bash
cd examples

# Générer 100 enregistrements aléatoires
./generate_test_data.py -n 100

# Générer avec un seed spécifique (reproductible)
./generate_test_data.py -n 50 -o my_test --seed 12345

# Voir l'aide
./generate_test_data.py --help
```

**Options disponibles:**
- `-n, --num-records` : Nombre d'enregistrements (défaut: 100)
- `-o, --output` : Préfixe des fichiers de sortie
- `--seed` : Seed pour reproductibilité

### 3. Exécuter les Tests Automatisés

```bash
cd examples
./run-tests.sh
```

Ce script va:
1. ✅ Vérifier les prérequis (Java, Maven, GnuCOBOL optionnel)
2. ✅ Compiler et exécuter le programme COBOL (si disponible)
3. ✅ Générer le projet Java si nécessaire
4. ✅ Compiler le projet Java
5. ✅ Comparer les résultats
6. ✅ Afficher le rapport de conversion

## 📊 Données de Test Fournies

### `customers.dat` / `customers.csv` (10 enregistrements)

| ID     | Nom              | Montant   | Date       | HIGH VALUE? |
|--------|------------------|-----------|------------|-------------|
| 000001 | John Smith       | 500.00 €  | 2024-01-15 | Non         |
| 000002 | Marie Dupont     | 1500.00 € | 2024-01-16 | **Oui**     |
| 000003 | Pierre Martin    | 250.00 €  | 2024-01-17 | Non         |
| 000004 | Sophie Bernard   | 2000.00 € | 2024-01-18 | **Oui**     |
| 000005 | Luc Petit        | 750.00 €  | 2024-01-19 | Non         |
| 000006 | Emma Dubois      | 3000.00 € | 2024-01-20 | **Oui**     |
| 000007 | Thomas Moreau    | 125.00 €  | 2024-01-21 | Non         |
| 000008 | Julie Laurent    | 1750.00 € | 2024-01-22 | **Oui**     |
| 000009 | Antoine Simon    | 80.00 €   | 2024-01-23 | Non         |
| 000010 | Claire Michel    | 2500.00 € | 2024-01-24 | **Oui**     |

**Résultats attendus:**
- Total: 10 enregistrements
- HIGH VALUE (>1000€): 5 clients

## 🔍 Validation de la Traduction

Pour valider que la traduction COBOL → Java est correcte:

### Test COBOL (avec GnuCOBOL)

```bash
cd examples

# Compiler
cobc -x -free simple-customer.cob

# Exécuter
./simple-customer

# Sortie attendue:
# HIGH VALUE: Marie Dupont
# HIGH VALUE: Sophie Bernard
# HIGH VALUE: Emma Dubois
# HIGH VALUE: Julie Laurent
# HIGH VALUE: Claire Michel
# PROCESSED: 00010
```

### Test Java (Spring Batch)

```bash
# Aller dans le projet généré
cd ../generated-projects/customer-batch-processing

# Copier les données de test
cp ../../cobol-to-java-translator/examples/customers.csv src/main/resources/data/

# Compiler et exécuter
mvn clean package
mvn spring-boot:run

# Ou utiliser le JAR
java -jar target/customer-batch-processing-1.0.0-SNAPSHOT.jar
```

### Critères de Validation

Les deux versions (COBOL et Java) doivent produire:
1. ✅ Le même nombre d'enregistrements traités (10)
2. ✅ Les mêmes clients avec montant > 1000€ (5 clients)
3. ✅ Les mêmes noms affichés dans le même ordre

## 📋 Structure du Projet Généré

Après traduction, le projet Java est organisé ainsi:

```
generated-projects/customer-batch-processing/
├── src/
│   ├── main/
│   │   ├── java/com/nz/batch/
│   │   │   ├── model/
│   │   │   │   ├── CustomerFileRecord.java        # Entity
│   │   │   │   ├── CustprocProcessor.java         # Processor
│   │   │   │   └── CustprocJobConfiguration.java  # Job Config
│   │   │   └── Application.java
│   │   └── resources/
│   │       ├── application.properties
│   │       ├── data/
│   │       │   └── customers.csv                   # Données
│   │       └── cobol-original/
│   │           └── simple-customer.cob             # Source COBOL
│   └── test/
├── docs/
│   └── CUSTPROC_CONVERSION_REPORT.txt              # Rapport de conversion
├── pom.xml
└── README.md
```

## 🛠️ Personnaliser les Tests

### Modifier les Données Existantes

**Format COBOL (position fixe - 48 caractères):**
```
[ID:6][NAME:30 chars padded][AMOUNT:8 digits][DATE:8 digits]
000099Alice Johnson                   001234567820240201
```

**Format CSV:**
```csv
CUST_ID,CUST_NAME,CUST_AMOUNT,CUST_DATE
000099,Alice Johnson,1234.56,20240201
```

### Générer de Nouveaux Jeux de Données

```bash
# Petit jeu de test (20 enregistrements)
./generate_test_data.py -n 20 -o small_test

# Jeu de test moyen (500 enregistrements)
./generate_test_data.py -n 500 -o medium_test

# Grand jeu de test (10000 enregistrements)
./generate_test_data.py -n 10000 -o large_test

# Test reproductible
./generate_test_data.py -n 100 -o regression_test --seed 42
```

## 📖 Documentation Complète

Pour plus de détails sur:
- **Format des données**: Voir `TEST_DATA_README.md`
- **Rapport de conversion**: Voir le fichier généré dans `docs/CUSTPROC_CONVERSION_REPORT.txt`
- **Configuration du projet**: Voir le README du projet généré

## ⚠️ Prérequis

### Pour les Tests COBOL (optionnel)
```bash
# Ubuntu/Debian
sudo apt-get install gnucobol

# MacOS
brew install gnucobol

# Vérifier l'installation
cobc --version
```

### Pour les Tests Java (requis)
```bash
# Java 17+
java -version

# Maven 3.6+
mvn --version
```

### Pour le Générateur Python
```bash
# Python 3.6+
python3 --version
```

## 🎯 Cas d'Usage

### 1. Développement et Debug
Utilisez le petit jeu de données (10-20 enregistrements) pour:
- Développer et déboguer rapidement
- Vérifier visuellement les résultats
- Tester les cas limites

### 2. Tests d'Intégration
Utilisez un jeu moyen (500-1000 enregistrements) pour:
- Tester les performances
- Valider le traitement par lots (batch)
- Identifier les problèmes de mémoire

### 3. Tests de Charge
Utilisez un grand jeu (10000+ enregistrements) pour:
- Tester la scalabilité
- Mesurer les temps de traitement
- Valider les optimisations

## 🐛 Problèmes Connus

### COBOL: "File not found"
**Solution**: Assurez-vous que `customers.dat` est dans le répertoire d'exécution
```bash
ls -la customers.dat
```

### Java: Format de montant incorrect
**Solution**: Vérifiez que le CSV utilise le point comme séparateur décimal
```csv
1500.00  ✓ correct
1500,00  ✗ incorrect
```

### Différences de résultats COBOL vs Java
**Causes possibles**:
1. Ordre de tri différent → Vérifier la configuration du reader
2. Arrondis différents → Vérifier le type BigDecimal vs double
3. Fichiers différents → Vérifier que les données sont identiques

## 📞 Support

Pour signaler des bugs ou demander des fonctionnalités:
- Examinez le rapport de conversion généré
- Vérifiez les logs dans le projet Java
- Comparez les fichiers COBOL et Java côte à côte

## ✅ Checklist de Test

Avant de valider une traduction:

- [ ] Le programme COBOL compile et s'exécute
- [ ] Le code Java compile sans erreurs
- [ ] Les données de test sont au bon format
- [ ] Le nombre d'enregistrements traités est identique
- [ ] Les résultats métier sont identiques
- [ ] Le rapport de conversion a été examiné
- [ ] Les warnings ont été documentés
- [ ] Les cas non convertis ont été traités manuellement
