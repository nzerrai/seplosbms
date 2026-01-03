# COBOL to Java Spring Batch Translator

## 📋 Description

Outil de traduction automatique de programmes COBOL vers **projets Java Spring Batch complets et séparés**.

**Architecture en 2 projets distincts** :
1. **Convertisseur** (`cobol-to-java-translator/`) - L'outil de traduction réutilisable
2. **Projet(s) généré(s)** (`../generated-projects/<nom-projet>/`) - Applications Maven autonomes

Le nom et la configuration du projet cible sont définis dans **`translator.properties`**.

### Le système génère automatiquement :
- ✅ **Projet Maven complet** avec pom.xml, structure, configuration Spring Boot
- ✅ **Code Java** (entités, processors, configurations Spring Batch)
- ✅ **Tests unitaires** avec Spring Batch Test
- ✅ **Documentation** (README, rapports de conversion détaillés)
- ✅ **Scripts** (.gitignore, build.sh/bat, Dockerfile optionnel)
- ✅ **Compte Rendu (CR)** avec taux de traduction, confiance, et alternatives

---

## ⚡ Démarrage Rapide

### 1. Configuration

Éditer `translator.properties` pour définir le projet cible :

```properties
# Nom du projet Java à générer
target.project.name=customer-batch-processing

# Où créer le projet
target.projects.directory=../generated-projects

# Package Java
target.package.base=com.mycompany.batch
```

### 2. Conversion

```bash
cd cobol-to-java-translator

# Fichier unique
java -jar target/cobol-translator.jar translate --input CUSTOMER.cob

# Répertoire complet
java -jar target/cobol-translator.jar translate-all --input-dir ../cobol-sources/
```

### 3. Résultat

Le projet est créé dans `../generated-projects/customer-batch-processing/` avec :

```
customer-batch-processing/
├── pom.xml                     # Maven POM complet
├── README.md
├── src/main/java/              # Code Java généré
├── src/main/resources/
│   ├── application.properties  # Config Spring Boot
│   └── cobol-original/         # Sources COBOL (optionnel)
├── data/
│   ├── input/
│   ├── output/
│   └── archive/
└── docs/
    └── *_CONVERSION_REPORT.txt # Rapports détaillés
```

### 4. Compilation et Exécution

```bash
cd ../generated-projects/customer-batch-processing

mvn clean install
java -jar target/customer-batch-processing-1.0.0-SNAPSHOT.jar
```

---

## 🎯 Fonctionnalités

### Phase 1 - Analyse COBOL
- Parse les fichiers COBOL (.cob, .cbl)
- Analyse les copybooks (.cpy)
- Identifie les structures de données (FILE SECTION, WORKING-STORAGE)
- Détecte les opérations (READ, WRITE, COMPUTE, IF, EVALUATE)
- Extrait les règles métier

### Phase 2 - Génération de Projet Maven
- **Crée un projet Maven complet et séparé**
- Structure de répertoires standard
- pom.xml avec dépendances Spring Boot/Batch
- application.properties configuré
- README, .gitignore, scripts de build
- Dockerfile (optionnel)

### Phase 3 - Génération Java
- Génère les entités Java à partir des structures COBOL
- Convertit les opérations COBOL en logique Java
- Crée les configurations Spring Batch
- Génère les ItemReader, ItemProcessor, ItemWriter

### Phase 4 - Optimisation
- Applique les best practices Java
- Utilise BigDecimal pour calculs financiers
- Gère les conversions de types (COMP-3, PIC, dates)
- Ajoute la gestion d'erreurs

### Phase 5 - Rapport de Conversion
- **Compte Rendu (CR)** automatique pour chaque conversion
- Calcul du **taux de traduction** (converti / partiel / non converti)
- **Indicateur de confiance** sur 5 niveaux (TRÈS HAUTE → TRÈS FAIBLE)
- Liste des **cas non convertis** avec alternatives et exemples
- **Recommandations** personnalisées selon le niveau de confiance
- Rapport sauvegardé au format texte dans `docs/`

---

## 📂 Architecture

```
workspace/
│
├── cobol-to-java-translator/       # CONVERTISSEUR (cet outil)
│   ├── translator.properties       # ⭐ CONFIGURATION PRINCIPALE
│   ├── src/main/java/
│   ├── pom.xml
│   └── docs/
│
└── generated-projects/              # PROJETS GÉNÉRÉS
    ├── customer-batch/              # Projet 1 (autonome)
    │   ├── pom.xml
    │   ├── src/main/java/
    │   └── docs/
    ├── order-batch/                 # Projet 2 (autonome)
    └── inventory-batch/             # Projet 3 (autonome)
```

---

## 🚀 Installation

```bash
# Cloner le projet
git clone <repo-url>
cd cobol-to-java-translator

# Compiler
mvn clean install
```

---

## 📖 Usage

### Fichier de Configuration : `translator.properties`

**Paramètres essentiels** :

```properties
# PROJET CIBLE
target.project.name=mon-projet              # Nom du projet à générer
target.projects.directory=../projects       # Où créer les projets
target.project.groupId=com.company.batch    # GroupId Maven
target.package.base=com.company.batch       # Package de base

# VERSIONS
spring.boot.version=3.2.0
java.version=17
database.type=POSTGRESQL

# OPTIONS DE GÉNÉRATION
generate.tests=true                         # Générer les tests
generate.report=true                        # Générer le rapport
generate.readme=true                        # Générer le README
generate.spring.config=true                 # Générer application.properties
copy.cobol.sources=true                     # Copier les .cob
```

Voir `translator.properties` pour la liste complète des 90+ paramètres.

### Mode CLI

```bash
# Traduire un fichier COBOL
java -jar cobol-translator.jar translate \
  --input ../cobol/CUSTPROC.cob

# Traduire un répertoire
java -jar cobol-translator.jar translate-all \
  --input-dir ../cobol/
```

### Mode API

```java
// Charge automatiquement translator.properties
CobolTranslator translator = new CobolTranslator();

TranslationConfig config = TranslationConfig.builder()
    .sourceFile("CUSTPROC.cob")
    .outputPackage("com.mycompany.batch")
    .targetDirectory("src/main/java")
    .generateTests(true)
    .generateReport(true)
    .build();

TranslationResult result = translator.translate(config);

if (result.isSuccess()) {
    System.out.println("Generated files:");
    result.getGeneratedFiles().forEach(System.out::println);

    // Accès au Compte Rendu
    ConversionReport report = result.getConversionReport();
    System.out.printf("Taux de conversion : %.1f%%\n", report.getConversionPercentage());
    System.out.println("Confiance : " + report.getOverallConfidence().getLabel());
}
```

---

## 📊 Exemple de Rapport de Conversion

Chaque conversion génère un rapport détaillé :

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

Interprétation : Le code généré est de bonne qualité et nécessite une
                 révision standard.

CAS NON CONVERTIS ET ALTERNATIVES
===========================================================================

1. EXEC SQL (DB2 embedded SQL)
   Raison       : SQL embarqué nécessite conversion vers JDBC ou JPA
   Alternative  : Utiliser Spring Data JPA ou JdbcTemplate
   Exemple      :
      @Repository
      public interface CustomerRepository extends JpaRepository<Customer, Long> {
          @Query("SELECT c FROM Customer c WHERE c.status = :status")
          List<Customer> findByStatus(@Param("status") String status);
      }

[... plus d'alternatives ...]

RECOMMANDATIONS
===========================================================================
Le code généré peut être utilisé avec un minimum de révision.
   - Effectuer une revue de code standard
   - Tester avec des données réelles
   - Valider les calculs financiers
```

---

## 🔧 Multi-Projets

Générer plusieurs projets différents :

```bash
# Projet 1 : Clients
vim translator.properties  # target.project.name=customer-batch
java -jar cobol-translator.jar translate-all --input-dir cobol/customers

# Projet 2 : Commandes
vim translator.properties  # target.project.name=order-batch
java -jar cobol-translator.jar translate-all --input-dir cobol/orders

# Projet 3 : Inventaire
vim translator.properties  # target.project.name=inventory-batch
java -jar cobol-translator.jar translate-all --input-dir cobol/inventory
```

Résultat :

```
generated-projects/
├── customer-batch/      # Totalement indépendant
├── order-batch/         # Totalement indépendant
└── inventory-batch/     # Totalement indépendant
```

Chaque projet est **autonome** avec son propre pom.xml, configuration, et repository Git potentiel.

---

## 📚 Documentation

| Fichier | Description |
|---------|-------------|
| **[QUICK_START.md](QUICK_START.md)** | ⭐ Guide de démarrage rapide |
| **[INSTRUCTIONS_FINALES.md](INSTRUCTIONS_FINALES.md)** | ⭐ Instructions complètes |
| **[PROJECT_SEPARATION.md](docs/PROJECT_SEPARATION.md)** | Architecture de séparation |
| **[CONVERSION_REPORT_FEATURE.md](docs/CONVERSION_REPORT_FEATURE.md)** | Système de rapport |
| **[ARCHITECTURE_SEPARATION.txt](ARCHITECTURE_SEPARATION.txt)** | Vue technique |
| **[translator.properties](translator.properties)** | Configuration (avec commentaires) |

---

## ✨ Avantages

### Séparation des Projets

- **Convertisseur** : Outil réutilisable pour toutes vos conversions
- **Projets générés** : Applications Maven standard totalement autonomes
- **Aucun couplage** : Les projets générés fonctionnent sans le convertisseur

### Configuration Centralisée

- **Un seul fichier** à éditer : `translator.properties`
- **90+ paramètres** configurables
- **Valeurs par défaut** intelligentes

### Projets Prêts à l'Emploi

- **Compilation immédiate** : `mvn clean install`
- **Structure standard** : Familière pour tous les développeurs Java
- **Documentation incluse** : README, rapports, sources COBOL
- **Déploiement direct** : JAR exécutable Spring Boot

### Traçabilité

- **Rapports détaillés** : Taux de conversion, confiance, alternatives
- **Sources préservées** : Fichiers COBOL originaux dans le projet
- **Versionnement** : Chaque projet peut avoir son propre Git

---

## 🎓 Support

### Questions Fréquentes

**Q: Où modifier le nom du projet cible ?**
R: Dans `translator.properties` → `target.project.name`

**Q: Le projet généré peut-il fonctionner sans le convertisseur ?**
R: **OUI !** C'est un projet Maven standard totalement autonome.

**Q: Puis-je générer plusieurs projets ?**
R: Oui, changez `target.project.name` entre les conversions.

**Q: Comment personnaliser la base de données ?**
R: Dans `translator.properties` → `database.type`, `database.url`, etc.

---

## 📝 Licence

[Votre licence]

---

## 🚀 Prêt à Démarrer

1. **Éditez** `translator.properties` avec le nom de votre projet
2. **Lancez** la conversion de vos fichiers COBOL
3. **Compilez** le projet généré avec `mvn clean install`
4. **Exécutez** votre application Spring Batch !

Pour plus de détails, consultez [QUICK_START.md](QUICK_START.md) ou [INSTRUCTIONS_FINALES.md](INSTRUCTIONS_FINALES.md).
# seplosbms
