# Correction : Classe Principale Manquante dans le Projet Généré

## 🐛 Problème Identifié

Le projet Java généré par le convertisseur ne pouvait pas démarrer avec Maven Spring Boot :

```bash
$ mvn spring-boot:run

[ERROR] Failed to execute goal org.springframework.boot:spring-boot-maven-plugin:3.2.0:run
(default-cli) on project final: Unable to find a suitable main class,
please add a 'mainClass' property
```

## 🔍 Cause Racine

Le `ProjectGenerator` générait la structure complète du projet **SAUF** la classe principale Spring Boot avec l'annotation `@SpringBootApplication` et la méthode `main()`.

### Ce qui était généré :

✅ `pom.xml` avec dépendances Spring Boot
✅ `application.properties`
✅ Structure de packages (model, processor, config)
✅ README.md, .gitignore, build scripts
❌ **Classe principale Application** (MANQUANTE)

### Contenu du projet généré (avant) :

```
mon-projet/
├── pom.xml                           ✅
├── README.md                         ✅
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/example/batch/
│   │   │       ├── model/            ✅
│   │   │       ├── processor/        ✅
│   │   │       └── config/           ✅
│   │   │       └── ??? Application.java   ❌ MANQUANTE !
│   │   └── resources/
│   │       └── application.properties ✅
│   └── test/
└── docs/
```

## ✅ Solution Implémentée

### Modification de `ProjectGenerator.java`

Ajout de la méthode `generateMainApplicationClass()` qui génère automatiquement la classe principale.

#### 1. Appel dans `generateProject()`

```java
// Generer le Dockerfile
if (config.isGenerateDockerfile()) {
    generateDockerfile(projectPath);
}

// Generer la classe principale Spring Boot Application
generateMainApplicationClass(projectPath);  // ✅ NOUVEAU

logger.info("Project structure created successfully at: {}", projectPath);
```

#### 2. Nouvelle méthode `generateMainApplicationClass()`

```java
/**
 * Genere la classe principale Spring Boot Application.
 */
private void generateMainApplicationClass(Path projectPath) throws IOException {
    // Convertir le nom du projet en PascalCase
    String className = toPascalCase(config.getTargetProjectName()) + "Application";

    String applicationClass = """
        package %s;

        import org.springframework.boot.SpringApplication;
        import org.springframework.boot.autoconfigure.SpringBootApplication;

        /**
         * Application principale Spring Boot.
         * Generee automatiquement par le traducteur COBOL vers Java.
         */
        @SpringBootApplication
        public class %s {

            public static void main(String[] args) {
                SpringApplication.run(%s.class, args);
            }
        }
        """.formatted(
        config.getTargetPackageBase(),
        className,
        className
    );

    // Creer le chemin du fichier
    String packagePath = config.getTargetPackageBase().replace('.', '/');
    Path applicationClassPath = projectPath
        .resolve("src/main/java")
        .resolve(packagePath)
        .resolve(className + ".java");

    Files.writeString(applicationClassPath, applicationClass);
    logger.info("Generated main application class: {}", className);
}
```

#### 3. Méthode utilitaire `toPascalCase()`

```java
/**
 * Convertit une chaine en PascalCase (ex: "mon-projet" -> "MonProjet").
 */
private String toPascalCase(String input) {
    if (input == null || input.isEmpty()) {
        return "Application";
    }

    String[] parts = input.split("[-\\s_]+");
    StringBuilder result = new StringBuilder();

    for (String part : parts) {
        if (!part.isEmpty()) {
            result.append(part.substring(0, 1).toUpperCase());
            if (part.length() > 1) {
                result.append(part.substring(1).toLowerCase());
            }
        }
    }

    return result.length() > 0 ? result.toString() : "Application";
}
```

## 📊 Résultat

### Avant

```
mon-projet/
├── src/main/java/com/example/batch/
│   ├── model/
│   ├── processor/
│   └── config/
└── ❌ Pas de classe Application → mvn spring-boot:run ÉCHOUE
```

### Après

```
mon-projet/
├── src/main/java/com/example/batch/
│   ├── MonProjetApplication.java    ✅ NOUVEAU !
│   ├── model/
│   ├── processor/
│   └── config/
└── ✅ mvn spring-boot:run FONCTIONNE
```

### Classe Générée (Exemple)

**Fichier** : `src/main/java/com/example/batch/MonProjetApplication.java`

```java
package com.example.batch;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Application principale Spring Boot.
 * Generee automatiquement par le traducteur COBOL vers Java.
 */
@SpringBootApplication
public class MonProjetApplication {

    public static void main(String[] args) {
        SpringApplication.run(MonProjetApplication.class, args);
    }
}
```

## 🎯 Exemples de Nommage

Le nom de la classe est automatiquement généré en **PascalCase** à partir du nom du projet :

| Nom du Projet (Input) | Classe Générée (Output) |
|------------------------|-------------------------|
| `mon-projet` | `MonProjetApplication.java` |
| `customer-batch` | `CustomerBatchApplication.java` |
| `final` | `FinalApplication.java` |
| `banking_transaction` | `BankingTransactionApplication.java` |
| `UPPER CASE` | `UpperCaseApplication.java` |
| *(vide ou null)* | `Application.java` *(fallback)* |

## ✅ Tests de Validation

### 1. Test de Compilation

```bash
# Recompiler le traducteur
mvn clean compile

# Résultat attendu :
[INFO] BUILD SUCCESS
```

### 2. Test de Génération via Web

```bash
# 1. Démarrer l'interface Web
mvn spring-boot:run

# 2. Uploader un fichier COBOL
# 3. Télécharger le ZIP
# 4. Extraire et vérifier

unzip mon-projet.zip
cd mon-projet

# Vérifier que la classe existe
ls -la src/main/java/com/example/batch/*Application.java

# Résultat attendu :
# -rw-r--r-- 1 user user 456 Jan  4 18:30 MonProjetApplication.java
```

### 3. Test de Démarrage du Projet Généré

```bash
cd mon-projet

# Compiler le projet généré
mvn clean package

# Résultat attendu :
[INFO] BUILD SUCCESS

# Démarrer l'application
mvn spring-boot:run

# Résultat attendu :
Started MonProjetApplication in 2.345 seconds
```

### 4. Test via CLI

```bash
# Tester aussi avec la CLI
java -jar target/cobol-translator.jar translate exemples/CUSTPROC.cob \
    -p com.test.batch

# Vérifier la classe générée
ls -la ../generated-projects/*/src/main/java/com/test/batch/*Application.java
```

## 🔍 Vérification du Code Généré

### Contenu Minimal de la Classe

La classe générée doit contenir :

1. ✅ Le bon package (`package com.example.batch;`)
2. ✅ L'import `SpringApplication`
3. ✅ L'import `SpringBootApplication`
4. ✅ L'annotation `@SpringBootApplication`
5. ✅ La méthode `public static void main(String[] args)`
6. ✅ L'appel `SpringApplication.run()`
7. ✅ Javadoc explicative

### Vérification Automatique

```bash
# Vérifier que la classe contient l'annotation
grep -q "@SpringBootApplication" src/main/java/com/example/batch/*Application.java
echo $? # Devrait afficher 0 (trouvé)

# Vérifier la méthode main
grep -q "public static void main" src/main/java/com/example/batch/*Application.java
echo $? # Devrait afficher 0 (trouvé)

# Vérifier SpringApplication.run
grep -q "SpringApplication.run" src/main/java/com/example/batch/*Application.java
echo $? # Devrait afficher 0 (trouvé)
```

## 📝 Fichiers Modifiés

- ✏️ `src/main/java/com/cobol/translator/project/ProjectGenerator.java`
  - Ajout de `generateMainApplicationClass(Path projectPath)`
  - Ajout de `toPascalCase(String input)`
  - Appel de `generateMainApplicationClass()` dans `generateProject()`
  - Suppression de l'import inutilisé `StandardOpenOption`

## 🚀 Impact

### Avant
- ❌ Projet généré ne démarre pas
- ❌ Erreur "Unable to find a suitable main class"
- ❌ Utilisateur doit créer manuellement la classe Application
- ❌ Expérience utilisateur frustrante

### Après
- ✅ Projet généré démarre immédiatement
- ✅ Classe Application créée automatiquement
- ✅ Nom de classe intelligent (PascalCase)
- ✅ Prêt à l'emploi ("batteries included")

## 💡 Améliorations Futures (Optionnel)

### 1. Configuration de la Classe Principale dans pom.xml

Actuellement, le plugin Spring Boot détecte automatiquement la classe principale. On pourrait l'expliciter :

```xml
<build>
    <plugins>
        <plugin>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-maven-plugin</artifactId>
            <configuration>
                <mainClass>com.example.batch.MonProjetApplication</mainClass>
            </configuration>
        </plugin>
    </plugins>
</build>
```

### 2. Classe Application avec Configuration Batch

Ajouter `@EnableBatchProcessing` directement dans la classe principale :

```java
@SpringBootApplication
@EnableBatchProcessing  // ← Peut être ajouté ici
public class MonProjetApplication {
    // ...
}
```

### 3. CommandLineRunner pour Lancer un Job au Démarrage

```java
@SpringBootApplication
public class MonProjetApplication implements CommandLineRunner {

    @Autowired
    private JobLauncher jobLauncher;

    @Autowired
    private Job myJob;

    public static void main(String[] args) {
        SpringApplication.run(MonProjetApplication.class, args);
    }

    @Override
    public void run(String... args) throws Exception {
        // Lancer le job au démarrage si nécessaire
    }
}
```

## ✨ Conclusion

Cette correction garantit que **tous les projets générés** (Web et CLI) contiennent une classe principale Spring Boot valide et peuvent démarrer immédiatement avec `mvn spring-boot:run`.

---

**Date de correction** : 2026-01-04
**Version** : 1.0.0-SNAPSHOT
**Status** : ✅ Corrigé et testé
**Compilé avec succès** : ✅ Oui
**Impact** : Critique - Projets générés maintenant fonctionnels
