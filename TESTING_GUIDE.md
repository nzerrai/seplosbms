# Guide de Test - COBOL to Java Translator

Ce guide explique comment tester et valider les traductions COBOL vers Java.

## 📁 Structure des Fichiers de Test

```
examples/
├── simple-customer.cob          # Programme COBOL exemple
├── customers.dat                # Données test format COBOL
├── customers.csv                # Données test format CSV
├── generate_test_data.py        # Générateur de données
├── run-tests.sh                 # Tests automatisés
├── quick-demo.sh                # Démonstration interactive
├── README.md                    # Documentation complète
└── TEST_DATA_README.md          # Format des données
```

## 🚀 Démarrage Rapide

### 1. Génération du JAR Exécutable

```bash
mvn clean package -DskipTests
```

Le JAR sera créé dans `target/cobol-translator.jar`

### 2. Traduction d'un Programme COBOL

```bash
java -jar target/cobol-translator.jar translate ./examples/simple-customer.cob
```

**Options disponibles:**
```bash
# Avec package personnalisé
java -jar target/cobol-translator.jar translate ./examples/simple-customer.cob \
    -p com.mycompany.batch \
    -o ./output

# Sans tests ni rapport
java -jar target/cobol-translator.jar translate ./examples/simple-customer.cob \
    --no-tests --no-report

# Voir l'aide
java -jar target/cobol-translator.jar translate --help
```

### 3. Traduction de Tous les Fichiers d'un Répertoire

```bash
java -jar target/cobol-translator.jar translate-all ./examples
```

## 🎬 Démonstration Interactive

Pour une démonstration guidée pas à pas:

```bash
cd examples
./quick-demo.sh
```

Cette démonstration vous montrera:
1. Le code COBOL source
2. Les données de test
3. Le processus de traduction
4. Le code Java généré
5. Le rapport de conversion
6. La comparaison COBOL vs Java

## 🧪 Tests Automatisés

### Exécution des Tests

```bash
cd examples
./run-tests.sh
```

Ce script effectue:
- ✅ Vérification des prérequis (Java, Maven, GnuCOBOL)
- ✅ Compilation et exécution du COBOL (si disponible)
- ✅ Génération et compilation du projet Java
- ✅ Comparaison des résultats
- ✅ Affichage du rapport de conversion

### Résultats Attendus

Pour `customers.dat` (10 enregistrements):

| Métrique | Valeur Attendue |
|----------|----------------|
| Total d'enregistrements | 10 |
| HIGH VALUE (>1000€) | 5 |
| Noms affichés | Marie Dupont, Sophie Bernard, Emma Dubois, Julie Laurent, Claire Michel |

## 📊 Génération de Données de Test

### Utilisation du Générateur

```bash
cd examples

# Génération basique (100 enregistrements)
./generate_test_data.py

# Nombre personnalisé
./generate_test_data.py -n 500 -o medium_test

# Avec seed pour reproductibilité
./generate_test_data.py -n 100 --seed 42

# Aide complète
./generate_test_data.py --help
```

### Exemples de Jeux de Données

```bash
# Petit (20) - pour développement/debug
./generate_test_data.py -n 20 -o dev_test

# Moyen (500) - pour tests d'intégration
./generate_test_data.py -n 500 -o integration_test

# Grand (10000) - pour tests de charge
./generate_test_data.py -n 10000 -o load_test
```

## 📋 Format des Données

### Format COBOL (Position Fixe)

Chaque enregistrement fait **48 caractères**:

```
Position  Longueur  Champ         Type
────────────────────────────────────────
1-6       6         CUST-ID       Numérique
7-36      30        CUST-NAME     Alphanumérique
37-44     8         CUST-AMOUNT   Numérique (centimes)
45-52     8         CUST-DATE     AAAAMMJJ
```

**Exemple:**
```
000001John Smith                      000050000020240115
```

### Format CSV (pour Java)

```csv
CUST_ID,CUST_NAME,CUST_AMOUNT,CUST_DATE
000001,John Smith,500.00,20240115
```

## 🔍 Validation des Résultats

### Critères de Validation

Pour qu'une traduction soit considérée comme correcte:

1. **Exactitude fonctionnelle**
   - [ ] Même nombre d'enregistrements traités
   - [ ] Mêmes résultats métier (HIGH VALUE)
   - [ ] Même ordre de traitement

2. **Qualité du code**
   - [ ] Compilation sans erreurs
   - [ ] Pas de warnings critiques
   - [ ] Code lisible et maintenable

3. **Performance**
   - [ ] Temps d'exécution acceptable
   - [ ] Utilisation mémoire raisonnable

### Exemple de Validation

**COBOL Output:**
```
HIGH VALUE: Marie Dupont
HIGH VALUE: Sophie Bernard
HIGH VALUE: Emma Dubois
HIGH VALUE: Julie Laurent
HIGH VALUE: Claire Michel
PROCESSED: 00010
```

**Java Output (attendu):**
```
Processing customer: Marie Dupont (1500.00)
Processing customer: Sophie Bernard (2000.00)
Processing customer: Emma Dubois (3000.00)
Processing customer: Julie Laurent (1750.00)
Processing customer: Claire Michel (2500.00)
Job completed: 10 records processed
```

## 📖 Rapport de Conversion

Après chaque traduction, un rapport détaillé est généré:

```
../generated-projects/<project>/docs/<PROGRAM>_CONVERSION_REPORT.txt
```

Ce rapport contient:
- **Statistiques**: Taux de conversion, confiance
- **Instructions converties**: MOVE, COMPUTE, IF, etc.
- **Cas non convertis**: EXEC CICS, EXEC SQL, etc.
- **Recommandations**: Actions manuelles nécessaires
- **Graphique visuel**: Représentation du taux de conversion

### Niveaux de Confiance

| Niveau | Taux | Description |
|--------|------|-------------|
| 🟢 TRÈS HAUTE | >90% | Utilisable en production avec révision minimale |
| 🟢 HAUTE | 75-90% | Bonne qualité, révision standard |
| 🟡 MOYENNE | 60-75% | Révision approfondie requise |
| 🟠 FAIBLE | 40-60% | Travail manuel important |
| 🔴 TRÈS FAIBLE | <40% | Conversion automatique non recommandée |

## 🛠️ Tests avec GnuCOBOL (Optionnel)

### Installation de GnuCOBOL

```bash
# Ubuntu/Debian
sudo apt-get install gnucobol

# Fedora/RHEL
sudo dnf install gnucobol

# macOS
brew install gnucobol
```

### Compilation et Exécution COBOL

```bash
cd examples

# Compiler
cobc -x -free simple-customer.cob

# Exécuter
./simple-customer
```

## 🐛 Dépannage

### Problème: JAR non trouvé

```bash
# Rebuild le JAR
mvn clean package -DskipTests
```

### Problème: Fichier de données non trouvé (COBOL)

```bash
# Vérifier que customers.dat est présent
ls -la customers.dat

# Copier depuis examples si nécessaire
cp examples/customers.dat .
```

### Problème: Erreurs de compilation Java

```bash
# Vérifier la version de Java
java -version  # Doit être 17+

# Clean rebuild
cd ../generated-projects/<project>
mvn clean compile
```

### Problème: Résultats différents COBOL vs Java

**Causes possibles:**
1. Fichiers de données différents → Comparer customers.dat et customers.csv
2. Arrondis différents → Vérifier les types (BigDecimal)
3. Logique de conversion incomplète → Consulter le rapport

## 📚 Documentation Complète

Pour plus de détails:

- **[examples/README.md](examples/README.md)** - Guide complet des exemples
- **[examples/TEST_DATA_README.md](examples/TEST_DATA_README.md)** - Format détaillé des données
- **Rapport de conversion** - Généré après chaque traduction

## ✅ Checklist de Test Complète

Avant de valider une traduction en production:

### Préparation
- [ ] JAR du traducteur compilé
- [ ] Données de test préparées
- [ ] Programme COBOL source vérifié

### Traduction
- [ ] Traduction exécutée sans erreur
- [ ] Projet Java généré
- [ ] Rapport de conversion examiné

### Compilation
- [ ] Code COBOL compile (si test COBOL)
- [ ] Code Java compile sans erreurs
- [ ] Pas de warnings critiques

### Tests Fonctionnels
- [ ] COBOL exécuté (si disponible)
- [ ] Java exécuté
- [ ] Résultats identiques (nombre d'enregistrements)
- [ ] Résultats métier identiques (HIGH VALUE)
- [ ] Ordre de traitement identique

### Analyse du Rapport
- [ ] Taux de conversion acceptable (>60%)
- [ ] Niveau de confiance documenté
- [ ] Cas non convertis identifiés
- [ ] Plan d'action pour cas manuels

### Documentation
- [ ] Code commenté
- [ ] Tests unitaires ajoutés
- [ ] README projet mis à jour
- [ ] Limitations documentées

### Performance
- [ ] Tests de charge effectués
- [ ] Utilisation mémoire acceptable
- [ ] Temps de traitement acceptable

## 🎯 Bonnes Pratiques

1. **Toujours tester avec de vraies données**
   - Utilisez des extraits de production (anonymisés)
   - Testez avec différents volumes

2. **Comparer systématiquement les résultats**
   - COBOL vs Java sur les mêmes données
   - Automatiser la comparaison

3. **Examiner le code généré**
   - Ne pas déployer sans revue
   - Adapter aux standards de l'équipe

4. **Tests progressifs**
   - Petit jeu de données d'abord
   - Augmenter progressivement
   - Tests de charge en dernier

5. **Documenter les différences**
   - Noter les adaptations nécessaires
   - Documenter les limitations
   - Créer des tests de régression

## 🔗 Ressources Additionnelles

- **Spring Batch Documentation**: https://spring.io/projects/spring-batch
- **GnuCOBOL Manual**: https://gnucobol.sourceforge.io/
- **Maven Documentation**: https://maven.apache.org/

---

**Dernière mise à jour**: Janvier 2026
**Version du traducteur**: 1.0.0-SNAPSHOT
