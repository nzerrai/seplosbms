# Separation des Projets - Architecture

## Vue d'ensemble

Le systeme est maintenant compose de **deux projets distincts et separes** :

1. **Le Convertisseur** (`cobol-to-java-translator/`) - L'outil de traduction
2. **Le(s) Projet(s) Genere(s)** (`../generated-projects/<nom-projet>/`) - Les applications Spring Batch resultantes

---

## Architecture

```
workspace/
├── cobol-to-java-translator/          # PROJET DU CONVERTISSEUR
│   ├── src/                            # Code source du traducteur
│   ├── pom.xml                         # Dependances du traducteur
│   ├── translator.properties           # FICHIER DE CONFIGURATION
│   └── README.md
│
└── generated-projects/                 # PROJETS GENERES
    ├── customer-batch-processing/      # Projet 1 (genere)
    │   ├── src/
    │   │   ├── main/java/com/mycompany/batch/
    │   │   │   ├── model/
    │   │   │   ├── processor/
    │   │   │   ├── config/
    │   │   │   ├── tasklet/
    │   │   │   └── listener/
    │   │   └── main/resources/
    │   │       ├── application.properties
    │   │       └── cobol-original/     # Sources COBOL (optionnel)
    │   ├── data/
    │   │   ├── input/
    │   │   ├── output/
    │   │   └── archive/
    │   ├── docs/
    │   │   └── CUSTOMER_CONVERSION_REPORT.txt
    │   ├── pom.xml                     # POM independant
    │   ├── README.md
    │   ├── .gitignore
    │   ├── build.sh
    │   └── Dockerfile
    │
    ├── order-processing/               # Projet 2 (genere)
    │   └── ...
    │
    └── inventory-batch/                # Projet 3 (genere)
        └── ...
```

---

## Configuration via `translator.properties`

### Emplacement du fichier

Le fichier de configuration `translator.properties` se trouve a la racine du projet du convertisseur :

```
cobol-to-java-translator/translator.properties
```

### Parametres du projet cible

```properties
# Nom du projet cible Java genere
target.project.name=customer-batch-processing

# Repertoire de base pour les projets generes
target.projects.directory=../generated-projects

# Group ID Maven pour le projet genere
target.project.groupId=com.mycompany.batch

# Package de base pour le code genere
target.package.base=com.mycompany.batch
```

### Resultat

Avec cette configuration, le projet genere sera cree dans :

```
../generated-projects/customer-batch-processing/
```

Avec le package :

```
com.mycompany.batch.model
com.mycompany.batch.processor
com.mycompany.batch.config
```

---

## Workflow de conversion

### 1. Configuration du projet cible

Editer `translator.properties` :

```properties
target.project.name=my-new-project
target.project.groupId=com.company.batch
target.package.base=com.company.batch
```

### 2. Lancer la conversion

```bash
cd cobol-to-java-translator

# Conversion d'un fichier
java -jar target/cobol-translator.jar translate \
    --input src/cobol/CUSTOMER.cob

# Conversion d'un repertoire
java -jar target/cobol-translator.jar translate-all \
    --input-dir src/cobol
```

### 3. Resultat

Le projet cible est genere dans :

```
../generated-projects/my-new-project/
```

Avec :
- Structure Maven complete
- Code Java genere
- Configuration Spring Boot
- Tests (optionnel)
- Documentation
- Rapports de conversion

### 4. Utiliser le projet genere

```bash
cd ../generated-projects/my-new-project

# Compiler
mvn clean install

# Executer
java -jar target/my-new-project-1.0.0-SNAPSHOT.jar
```

---

## Avantages de la separation

### Independance complete

- Le projet genere est **autonome**
- Aucune dependance vers le convertisseur
- Peut etre versionne separement (git init)
- Peut etre deploye independamment

### Plusieurs projets generes

Vous pouvez generer plusieurs projets differents :

```bash
# Projet 1 : Clients
vim translator.properties
# target.project.name=customer-batch
java -jar cobol-translator.jar translate-all --input-dir cobol/customers

# Projet 2 : Commandes
vim translator.properties
# target.project.name=order-batch
java -jar cobol-translator.jar translate-all --input-dir cobol/orders

# Projet 3 : Inventaire
vim translator.properties
# target.project.name=inventory-batch
java -jar cobol-translator.jar translate-all --input-dir cobol/inventory
```

Resultat :

```
generated-projects/
├── customer-batch/
├── order-batch/
└── inventory-batch/
```

### Personnalisation par projet

Chaque projet genere a :
- Son propre `pom.xml`
- Sa propre configuration Spring (`application.properties`)
- Sa propre structure de packages
- Ses propres dependances

### Facilite de maintenance

- Le convertisseur peut evoluer sans impacter les projets generes
- Les projets generes peuvent etre modifies independamment
- Pas de couplage entre l'outil et le resultat

---

## Fichiers generes

### Structure complete d'un projet genere

```
my-project/
├── pom.xml                     # Maven POM avec Spring Boot parent
├── README.md                   # Documentation du projet
├── .gitignore                  # Git ignore
├── build.sh                    # Script de build Unix
├── build.bat                   # Script de build Windows
├── Dockerfile                  # Conteneurisation (optionnel)
│
├── src/main/java/com/company/batch/
│   ├── model/
│   │   ├── CustomerRecord.java
│   │   └── OrderRecord.java
│   ├── processor/
│   │   ├── CustomerProcessor.java
│   │   └── OrderProcessor.java
│   ├── config/
│   │   ├── CustomerJobConfig.java
│   │   └── BatchConfiguration.java
│   ├── tasklet/
│   │   └── ValidationTasklet.java
│   └── listener/
│       └── JobCompletionListener.java
│
├── src/main/resources/
│   ├── application.properties
│   └── cobol-original/         # Sources COBOL (si copy.cobol.sources=true)
│       ├── CUSTOMER.cob
│       └── ORDER.cob
│
├── src/test/java/com/company/batch/
│   ├── CustomerProcessorTest.java
│   └── OrderProcessorTest.java
│
├── data/
│   ├── input/                  # Fichiers d'entree
│   ├── output/                 # Fichiers de sortie
│   └── archive/                # Archives
│
└── docs/
    ├── CUSTOMER_CONVERSION_REPORT.txt
    └── ORDER_CONVERSION_REPORT.txt
```

---

## Options de configuration avancees

### Personnalisation des packages

```properties
# Package de base
target.package.base=com.mycompany.batch

# Sous-packages (relatifs au package de base)
target.package.model=domain
target.package.processor=processing
target.package.config=configuration
target.package.tasklet=tasks
target.package.listener=listeners
```

Resultat :

```
com.mycompany.batch.domain
com.mycompany.batch.processing
com.mycompany.batch.configuration
com.mycompany.batch.tasks
com.mycompany.batch.listeners
```

### Personnalisation des versions

```properties
# Version de Spring Boot
spring.boot.version=3.2.0

# Version de Spring Batch
spring.batch.version=5.1.0

# Version de Java
java.version=17
```

### Personnalisation de la base de donnees

```properties
# Type de base de donnees (H2, POSTGRESQL, MYSQL, ORACLE, DB2)
database.type=POSTGRESQL

# Configuration (sera dans application.properties du projet genere)
database.url=jdbc:postgresql://localhost:5432/batch_db
database.username=batch_user
database.password=changeme
```

### Fichiers optionnels

```properties
# Generer les tests unitaires
generate.tests=true

# Generer la documentation Javadoc
generate.docs=true

# Generer le rapport de conversion
generate.report=true

# Generer un README
generate.readme=true

# Generer un .gitignore
generate.gitignore=true

# Generer la configuration Spring
generate.spring.config=true

# Generer un Dockerfile
generate.dockerfile=true

# Generer les scripts de build
generate.build.scripts=true
```

### Copie des sources COBOL

```properties
# Copier les fichiers COBOL sources dans le projet genere
copy.cobol.sources=true

# Repertoire pour les fichiers COBOL
cobol.sources.directory=src/main/resources/cobol-original

# Inclure les commentaires COBOL dans le code Java
include.cobol.comments=true
```

---

## Utilisation en ligne de commande

### Specifier un fichier de configuration different

```bash
java -jar cobol-translator.jar translate \
    --config custom-config.properties \
    --input CUSTOMER.cob
```

### Surcharger des parametres

```bash
java -jar cobol-translator.jar translate \
    --input CUSTOMER.cob \
    --project-name custom-project \
    --package com.custom.batch
```

---

## Migration d'un projet existant

Si vous avez deja un projet Spring Boot et voulez integrer le code genere :

### Option 1 : Generer dans un projet existant

```properties
# Pointer vers le projet existant
target.projects.directory=/path/to/existing/projects
target.project.name=my-existing-project

# Le code sera ajoute dans le projet existant
```

### Option 2 : Generer separement puis copier

```bash
# 1. Generer le projet
java -jar cobol-translator.jar translate-all --input-dir cobol/

# 2. Copier uniquement le code source
cp -r generated-projects/new-project/src/main/java/* existing-project/src/main/java/

# 3. Copier les fichiers de donnees
cp -r generated-projects/new-project/data/* existing-project/data/
```

---

## Versionnement

### Convertisseur (Git)

```bash
cd cobol-to-java-translator
git init
git add .
git commit -m "COBOL to Java Translator v1.0.0"
```

### Projet genere (Git separe)

```bash
cd ../generated-projects/customer-batch
git init
git add .
git commit -m "Initial migration from COBOL"
```

Les deux projets ont leur propre historique Git.

---

## Exemple complet

### 1. Configuration

Fichier `translator.properties` :

```properties
target.project.name=payroll-batch
target.projects.directory=../generated-projects
target.project.groupId=com.acme.payroll
target.package.base=com.acme.payroll.batch
java.version=17
database.type=POSTGRESQL
generate.tests=true
generate.report=true
```

### 2. Conversion

```bash
cd cobol-to-java-translator
java -jar target/cobol-translator.jar translate-all \
    --input-dir ../cobol-sources/payroll
```

### 3. Resultat

```
generated-projects/payroll-batch/
├── pom.xml
├── README.md
├── src/main/java/com/acme/payroll/batch/
│   ├── model/
│   ├── processor/
│   └── config/
└── docs/
    └── PAYROLL_CONVERSION_REPORT.txt
```

### 4. Compilation et execution

```bash
cd ../generated-projects/payroll-batch
mvn clean install
java -jar target/payroll-batch-1.0.0-SNAPSHOT.jar
```

---

## FAQ

### Q: Puis-je generer plusieurs fois dans le meme projet ?

Oui, mais attention :
- Les fichiers existants seront **ecrases**
- Utilisez le versionnement Git pour tracer les changements
- Sauvegardez vos modifications manuelles avant de regenerer

### Q: Puis-je modifier le code genere ?

Oui, totalement ! Le projet genere est **autonome** et peut etre modifie librement.

### Q: Comment mettre a jour le convertisseur sans impacter les projets generes ?

Les projets generes sont independants. Vous pouvez mettre a jour le convertisseur sans impact.

### Q: Puis-je partager le projet genere sans le convertisseur ?

Oui ! Le projet genere est un projet Maven Spring Boot standard qui peut etre partage, versionne, et deploye independamment.

---

## Voir aussi

- [translator.properties](../translator.properties) - Fichier de configuration complet
- [COMPLETE_DOCUMENTATION.md](COMPLETE_DOCUMENTATION.md) - Documentation technique du traducteur
- [CONVERSION_REPORT_FEATURE.md](CONVERSION_REPORT_FEATURE.md) - Systeme de rapport de conversion
