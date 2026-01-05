# 🚀 Quick Reference - COBOL to Java Translator

## 📋 Commandes Essentielles

### Build & Tests

```bash
# Build complet
mvn clean package

# Build sans tests
mvn clean package -DskipTests

# Tests uniquement
mvn test

# Test spécifique
mvn test -Dtest=BusinessLogicTranslatorTest

# Voir les résultats de tests
cat target/surefire-reports/*.txt
```

### Utilisation CLI

```bash
# Traduction d'un fichier COBOL
java -jar target/cobol-translator.jar translate \
  examples/banking-transaction.cob \
  -o /tmp/output

# Traduction avec options
java -jar target/cobol-translator.jar translate \
  examples/banking-transaction.cob \
  --output /path/to/project \
  --package com.company.batch \
  --no-tests

# Traduction de plusieurs fichiers
java -jar target/cobol-translator.jar translate-all \
  --input-dir cobol-sources/ \
  --output ../generated-projects

# Aide
java -jar target/cobol-translator.jar --help
```

### Interface Web

```bash
# Démarrer l'interface web
mvn spring-boot:run

# Ou avec le JAR
java -jar target/cobol-translator.jar

# Avec port personnalisé
mvn spring-boot:run -Dspring-boot.run.arguments="--server.port=8080"

# Accéder à l'interface
open http://localhost:9090
```

### Vérifications

```bash
# Vérifier que tout compile
mvn clean compile

# Voir les erreurs de compilation
mvn clean compile 2>&1 | grep -A5 "ERROR"

# Lister les tests
mvn test -DfailIfNoTests=false 2>&1 | grep "Running"

# Statistiques du projet
find src/main/java -name "*.java" | xargs wc -l | tail -1
find src/test/java -name "*.java" | xargs wc -l | tail -1
```

---

## 🧪 Tests Rapides

### Exemple Banking Transaction

```bash
# Générer le projet
java -jar target/cobol-translator.jar translate \
  examples/banking-transaction.cob \
  -o /tmp/banking-test

# Vérifier les fichiers générés
ls -R /tmp/banking-test/src/

# Voir le processor généré
cat /tmp/banking-test/src/main/java/com/nz/batch/processor/BanktranProcessor.java

# Voir le rapport de conversion
cat /tmp/banking-test/docs/BANKTRAN_CONVERSION_REPORT.txt
```

### Exemple Customer Processing

```bash
# Traduction
java -jar target/cobol-translator.jar translate \
  examples/simple-customer.cob \
  -o /tmp/customer-test

# Compiler le projet généré
cd /tmp/customer-test
mvn clean package

# Exécuter les tests
mvn test
```

---

## 📊 Métriques et Rapports

### Statistiques du Code

```bash
# Lignes de code source
echo "Java main:" && find src/main/java -name "*.java" -exec wc -l {} + | tail -1
echo "Java test:" && find src/test/java -name "*.java" -exec wc -l {} + | tail -1

# Nombre de classes
echo "Classes:" && find src/main/java -name "*.java" | wc -l
echo "Tests:" && find src/test/java -name "*Test.java" | wc -l

# Documentation
echo "Docs:" && find . -maxdepth 1 -name "*.md" | wc -l
```

### Résultats de Tests

```bash
# Résumé des tests
mvn test 2>&1 | grep -E "(Tests run:|BUILD SUCCESS|BUILD FAILURE)"

# Tests détaillés
mvn test -DfailIfNoTests=false

# Coverage (si configuré)
mvn jacoco:report
open target/site/jacoco/index.html
```

---

## 🔧 Configuration

### Modifier le Port Web

```bash
# application.properties
echo "server.port=8080" >> src/main/resources/application.properties
```

### Modifier la Configuration de Traduction

```bash
# translator.properties
nano translator.properties

# Ou directement en ligne de commande
sed -i 's/target.project.name=.*/target.project.name=my-project/' translator.properties
```

---

## 🐛 Debugging

### Logs Détaillés

```bash
# Activer le mode debug
mvn spring-boot:run -Dlogging.level.com.cobol.translator=DEBUG

# Voir les logs pendant traduction
java -jar target/cobol-translator.jar translate \
  examples/banking-transaction.cob \
  -o /tmp/test 2>&1 | tee translation.log
```

### Problèmes Courants

```bash
# Port déjà utilisé
lsof -i :9090
kill <PID>

# Nettoyer le build
mvn clean
rm -rf target/

# Réinitialiser les tests
rm -rf target/surefire-reports/
mvn clean test

# Vérifier Java version
java -version
mvn -version
```

---

## 📦 Génération de Projet

### Projet Maven Standard

```bash
java -jar target/cobol-translator.jar translate \
  my-program.cob \
  --output ../generated-projects/my-project \
  --package com.company.batch
```

### Avec Tests et Dockerfile

```bash
# Éditer translator.properties
cat >> translator.properties << EOF
generate.tests=true
generate.dockerfile=true
generate.repositories=true
EOF

# Générer
java -jar target/cobol-translator.jar translate my-program.cob
```

### Build du Projet Généré

```bash
cd ../generated-projects/my-project

# Build
mvn clean package

# Run tests
mvn test

# Run application
mvn spring-boot:run

# Build Docker image (si Dockerfile généré)
docker build -t my-project:latest .
docker run -p 8080:8080 my-project:latest
```

---

## 🔍 Inspection du Code Généré

### Voir les Entités

```bash
ls -la ../generated-projects/*/src/main/java/*/model/
cat ../generated-projects/*/src/main/java/*/model/*Record.java
```

### Voir les Processors

```bash
cat ../generated-projects/*/src/main/java/*/processor/*Processor.java
```

### Voir la Configuration Batch

```bash
cat ../generated-projects/*/src/main/java/*/config/*JobConfiguration.java
```

### Voir les Repositories

```bash
ls -la ../generated-projects/*/src/main/java/*/repository/
cat ../generated-projects/*/src/main/java/*/repository/*.java
```

---

## 📝 Documentation

### Générer la Documentation

```bash
# HTML Guide (si script existe)
./generate-html-guide.sh

# Ouvrir la documentation
open GUIDE_UTILISATEUR_COMPLET.html
```

### Lire la Documentation

```bash
# README principal
cat README_COMPLETE.md

# Phase 3 - Business Logic Translator
cat PHASE3_BUSINESS_LOGIC_TRANSLATOR.md

# Synthèse du projet
cat PROJECT_SYNTHESIS.md

# Guide technique du translator
cat BUSINESS_LOGIC_TRANSLATOR.md
```

---

## 🎯 Workflows Complets

### Workflow 1: Conversion Simple

```bash
# 1. Build le translator
mvn clean package -DskipTests

# 2. Convertir un fichier COBOL
java -jar target/cobol-translator.jar translate \
  examples/banking-transaction.cob \
  -o /tmp/banking-project

# 3. Vérifier le résultat
ls -R /tmp/banking-project/
cat /tmp/banking-project/docs/BANKTRAN_CONVERSION_REPORT.txt

# 4. Compiler le projet généré
cd /tmp/banking-project
mvn clean package

# 5. Voir les tests
mvn test
```

### Workflow 2: Conversion Batch

```bash
# 1. Préparer les fichiers COBOL
mkdir -p /tmp/cobol-sources
cp *.cob /tmp/cobol-sources/

# 2. Configurer la traduction
cat > translator.properties << EOF
target.project.name=batch-processing
target.projects.directory=../generated-projects
target.package.base=com.company.batch
generate.tests=true
generate.dockerfile=true
EOF

# 3. Conversion batch
java -jar target/cobol-translator.jar translate-all \
  --input-dir /tmp/cobol-sources/ \
  --output ../generated-projects

# 4. Build tous les projets
cd ../generated-projects
for dir in */; do
    echo "Building $dir"
    (cd "$dir" && mvn clean package)
done
```

### Workflow 3: Développement avec Web UI

```bash
# 1. Démarrer le serveur web
mvn spring-boot:run &

# 2. Ouvrir le browser
open http://localhost:9090

# 3. Upload fichiers COBOL via UI

# 4. Télécharger le ZIP généré

# 5. Extraire et tester
unzip downloaded-project.zip
cd extracted-project/
mvn clean test
```

---

## 🚀 Scripts Utiles

### Script de Test Complet

```bash
#!/bin/bash
# test-all.sh

echo "=== Building translator ==="
mvn clean package -DskipTests

echo "=== Running tests ==="
mvn test

echo "=== Testing examples ==="
for example in examples/*.cob; do
    echo "Testing $example"
    java -jar target/cobol-translator.jar translate "$example" \
        -o "/tmp/test-$(basename $example .cob)"
done

echo "=== All tests completed ==="
```

### Script de Déploiement

```bash
#!/bin/bash
# deploy.sh

# Build
mvn clean package -DskipTests

# Copy JAR
cp target/cobol-translator.jar /opt/cobol-translator/

# Start service
systemctl restart cobol-translator

# Check status
systemctl status cobol-translator
```

---

## 📚 Ressources

### Documentation Clé
- [README_COMPLETE.md](README_COMPLETE.md) - Vue d'ensemble complète
- [PROJECT_SYNTHESIS.md](PROJECT_SYNTHESIS.md) - Synthèse du projet
- [PHASE3_BUSINESS_LOGIC_TRANSLATOR.md](PHASE3_BUSINESS_LOGIC_TRANSLATOR.md) - Phase 3
- [USER_GUIDE.md](USER_GUIDE.md) - Guide utilisateur

### Exemples
- [examples/](examples/) - Programmes COBOL d'exemple
- [examples/README.md](examples/README.md) - Guide des exemples

### Configuration
- [translator.properties](translator.properties) - Configuration principale
- [application.properties](src/main/resources/application.properties) - Config Spring Boot

---

## 🆘 Support

### Problème rencontré ?

1. **Vérifier les logs**
   ```bash
   tail -f logs/translator.log
   ```

2. **Recompiler proprement**
   ```bash
   mvn clean compile
   ```

3. **Vérifier la configuration**
   ```bash
   cat translator.properties
   cat src/main/resources/application.properties
   ```

4. **Tester avec un exemple simple**
   ```bash
   java -jar target/cobol-translator.jar translate examples/simple-customer.cob
   ```

### Obtenir de l'aide

- 📧 Email: support@seplos-bms.com
- 🐛 GitHub Issues: https://github.com/nzerrai/seplosbms/issues
- 📖 Wiki: https://github.com/nzerrai/seplosbms/wiki

---

**Quick Reference** - Version 1.0.0  
*Last updated: 5 janvier 2026*
