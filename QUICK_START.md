# Guide de Démarrage Rapide

## Vue d'ensemble

Ce convertisseur COBOL vers Java génère un **projet Maven Spring Batch complet et séparé** dont le nom et la configuration sont définis dans `translator.properties`.

## Architecture

```
workspace/
├── cobol-to-java-translator/     # CONVERTISSEUR (cet outil)
│   └── translator.properties      # Configuration du projet cible
│
└── generated-projects/            # PROJETS GÉNÉRÉS
    └── <nom-du-projet>/           # Projet Maven autonome
        ├── pom.xml
        ├── src/main/java/
        ├── src/main/resources/
        └── docs/
```

---

## Étape 1 : Configuration

Éditer le fichier `translator.properties` pour définir le projet cible :

```properties
# Nom du projet Java à générer
target.project.name=customer-batch-processing

# Où créer le projet
target.projects.directory=../generated-projects

# Package Java
target.package.base=com.mycompany.batch

# GroupId Maven
target.project.groupId=com.mycompany.batch
```

---

## Étape 2 : Préparer les fichiers COBOL

Placer vos fichiers COBOL dans un répertoire, par exemple :

```
cobol-sources/
├── CUSTOMER.cob
├── ORDER.cob
└── INVOICE.cob
```

---

## Étape 3 : Lancer la conversion

### Option A : Convertir un fichier unique

```bash
cd cobol-to-java-translator

java -jar target/cobol-translator.jar translate \
    --input ../cobol-sources/CUSTOMER.cob
```

### Option B : Convertir tout un répertoire

```bash
java -jar target/cobol-translator.jar translate-all \
    --input-dir ../cobol-sources/
```

---

## Étape 4 : Vérifier le résultat

Le projet est créé dans :

```
../generated-projects/customer-batch-processing/
```

Avec la structure :

```
customer-batch-processing/
├── pom.xml                          # Maven POM complet
├── README.md                        # Documentation
├── .gitignore
├── build.sh
├── src/main/java/com/mycompany/batch/
│   ├── model/
│   │   ├── CustomerRecord.java
│   │   └── OrderRecord.java
│   ├── processor/
│   │   ├── CustomerProcessor.java
│   │   └── OrderProcessor.java
│   └── config/
│       ├── CustomerJobConfig.java
│       └── BatchConfiguration.java
├── src/main/resources/
│   ├── application.properties       # Config Spring Boot
│   └── cobol-original/              # Sources COBOL (optionnel)
│       ├── CUSTOMER.cob
│       └── ORDER.cob
├── data/
│   ├── input/
│   ├── output/
│   └── archive/
└── docs/
    ├── CUSTOMER_CONVERSION_REPORT.txt
    └── ORDER_CONVERSION_REPORT.txt
```

---

## Étape 5 : Compiler et exécuter le projet généré

```bash
cd ../generated-projects/customer-batch-processing

# Compiler
mvn clean install

# Exécuter
java -jar target/customer-batch-processing-1.0.0-SNAPSHOT.jar
```

---

## Exemple Complet

### 1. Configuration (`translator.properties`)

```properties
target.project.name=payroll-batch
target.projects.directory=../generated-projects
target.project.groupId=com.acme.payroll
target.package.base=com.acme.payroll.batch
database.type=POSTGRESQL
generate.tests=true
generate.report=true
copy.cobol.sources=true
```

### 2. Conversion

```bash
cd cobol-to-java-translator

java -jar target/cobol-translator.jar translate-all \
    --input-dir ../mainframe-cobol/payroll/
```

### 3. Sortie

```
Starting translation...
Target project: payroll-batch
Target location: ../generated-projects/payroll-batch

Creating new target project...
Generated pom.xml
Generated application.properties
Generated README.md
Generated .gitignore

Parsing COBOL program: PAYROLL.cob
Parsed program: PAYROLL with 45 data items and 125 statements

Generating entity classes...
Generating processor...
Generating job configuration...
Generating tests...
Generating conversion report...

Conversion rate: 76.0%
Confidence level: HAUTE

Translation completed successfully!
Files generated in: ../generated-projects/payroll-batch/

✅ Generated files:
   ✓ PayrollRecord.java
   ✓ PayrollProcessor.java
   ✓ PayrollJob.java
   ✓ PayrollProcessorTest.java
   ✓ PAYROLL_CONVERSION_REPORT.txt
```

### 4. Utilisation du projet généré

```bash
cd ../generated-projects/payroll-batch

# Consulter le rapport de conversion
cat docs/PAYROLL_CONVERSION_REPORT.txt

# Compiler
./build.sh

# Exécuter
java -jar target/payroll-batch-1.0.0-SNAPSHOT.jar
```

---

## Générer Plusieurs Projets

Vous pouvez générer plusieurs projets différents en changeant la configuration :

```bash
# Projet 1 : Clients
vim translator.properties
# target.project.name=customer-batch
java -jar target/cobol-translator.jar translate-all --input-dir cobol/customers

# Projet 2 : Commandes
vim translator.properties
# target.project.name=order-batch
java -jar target/cobol-translator.jar translate-all --input-dir cobol/orders

# Projet 3 : Inventaire
vim translator.properties
# target.project.name=inventory-batch
java -jar target/cobol-translator.jar translate-all --input-dir cobol/inventory
```

Résultat :

```
generated-projects/
├── customer-batch/
├── order-batch/
└── inventory-batch/
```

Chaque projet est **totalement indépendant** avec son propre :
- `pom.xml`
- Configuration Spring Boot
- Structure de packages
- Repository Git (potentiel)

---

## Options de Configuration Principales

### Projet

```properties
target.project.name=mon-projet          # Nom du projet
target.projects.directory=../projects   # Où créer les projets
target.project.groupId=com.company      # GroupId Maven
target.package.base=com.company.batch   # Package de base
```

### Versions

```properties
spring.boot.version=3.2.0
java.version=17
database.type=POSTGRESQL
```

### Options de génération

```properties
generate.tests=true              # Générer les tests
generate.report=true             # Générer le rapport de conversion
generate.readme=true             # Générer le README
generate.gitignore=true          # Générer .gitignore
generate.spring.config=true      # Générer application.properties
generate.build.scripts=true      # Générer build.sh/bat
copy.cobol.sources=true          # Copier les .cob dans le projet
```

### Base de données

```properties
database.type=POSTGRESQL
database.url=jdbc:postgresql://localhost:5432/batch_db
database.username=batch_user
database.password=changeme
```

### Batch

```properties
batch.chunk.size=100
batch.thread.pool.size=4
batch.skip.limit=10
```

---

## Fichiers Importants

| Fichier | Description |
|---------|-------------|
| `translator.properties` | **Configuration principale** (nom projet, packages, etc.) |
| `docs/PROJECT_SEPARATION.md` | Documentation complète de l'architecture |
| `docs/CONVERSION_REPORT_FEATURE.md` | Système de rapport de conversion |
| `ARCHITECTURE_SEPARATION.txt` | Vue d'ensemble de la séparation |

---

## Versionnement Git

### Convertisseur (Repository 1)

```bash
cd cobol-to-java-translator
git init
git add .
git commit -m "COBOL Translator v1.0.0"
git remote add origin https://github.com/company/cobol-translator.git
git push
```

### Projet Généré (Repository 2 - SÉPARÉ)

```bash
cd ../generated-projects/payroll-batch
git init
git add .
git commit -m "Initial migration from COBOL"
git remote add origin https://github.com/company/payroll-batch.git
git push
```

Les deux projets ont des repositories Git **totalement indépendants**.

---

## Rapport de Conversion

Chaque conversion génère un rapport détaillé dans `docs/<PROGRAM>_CONVERSION_REPORT.txt` :

```
+===========================================================================+
|           COMPTE RENDU DE CONVERSION COBOL -> JAVA                       |
+===========================================================================+

STATISTIQUES DE CONVERSION
===========================================================================
Instructions totales        : 125
  * Converties            : 95 (76.0%)
  * Partielles           : 18 (14.4%)
  * Non converties        : 12 (9.6%)

INDICATEUR DE CONFIANCE
===========================================================================
Niveau de confiance : HAUTE

CAS NON CONVERTIS ET ALTERNATIVES
===========================================================================

1. EXEC SQL (DB2 embedded SQL)
   Alternative  : Utiliser Spring Data JPA
   Exemple      :
      @Repository
      public interface CustomerRepository extends JpaRepository<Customer, Long> {
          @Query("SELECT c FROM Customer c WHERE c.status = :status")
          List<Customer> findByStatus(@Param("status") String status);
      }
```

---

## Dépannage

### Erreur : "Configuration file not found"

Vérifier que `translator.properties` existe à la racine du projet convertisseur.

### Erreur : "Target directory already exists"

Le projet cible existe déjà. Options :
1. Supprimer le projet existant
2. Changer `target.project.name` dans `translator.properties`
3. Les fichiers seront écrasés (utiliser Git pour versionner)

### Projet généré ne compile pas

Vérifier :
1. Java version correspond à celle dans `translator.properties`
2. Base de données accessible (ou utiliser H2 pour les tests)
3. Consulter le rapport de conversion pour les cas non convertis

---

## Support

- **Documentation complète** : `docs/PROJECT_SEPARATION.md`
- **Rapports de conversion** : `<projet-genere>/docs/*.txt`
- **Code source COBOL** : `<projet-genere>/src/main/resources/cobol-original/`

---

**Prêt à démarrer !** 🚀

Modifiez `translator.properties` et lancez votre première conversion.
