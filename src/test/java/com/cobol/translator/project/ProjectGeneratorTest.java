package com.cobol.translator.project;

import com.cobol.translator.config.TranslatorConfiguration;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests unitaires pour ProjectGenerator
 * 
 * Couvre les critères d'acceptation US-2.5.1:
 * - Tests de génération pom.xml
 * - Tests de génération application.properties
 * - Tests de génération structure de répertoires
 * - Tests de génération README, .gitignore
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("ProjectGenerator - Tests de génération de projet Maven")
class ProjectGeneratorTest {

    @TempDir
    Path tempDir;
    
    private TranslatorConfiguration config;
    private ProjectGenerator generator;
    
    @BeforeEach
    void setUp() throws IOException {
        config = createTestConfiguration();
        generator = new ProjectGenerator(config);
    }
    
    /**
     * Crée une configuration de test avec des valeurs par défaut
     */
    private TranslatorConfiguration createTestConfiguration() throws IOException {
        Properties props = new Properties();
        
        // Configuration du projet
        props.setProperty("target.project.name", "cobol-batch-processor");
        props.setProperty("target.projects.directory", tempDir.toString());
        props.setProperty("target.project.groupId", "com.example.cobol");
        props.setProperty("target.project.version", "1.0.0-SNAPSHOT");
        props.setProperty("target.project.description", "COBOL Batch Processor - Test Project");
        
        // Configuration des packages
        props.setProperty("target.package.base", "com.example.cobol");
        props.setProperty("target.package.model", "model");
        props.setProperty("target.package.processor", "processor");
        props.setProperty("target.package.config", "config");
        props.setProperty("target.package.tasklet", "tasklet");
        props.setProperty("target.package.listener", "listener");
        
        // Configuration Spring Boot
        props.setProperty("spring.boot.version", "3.2.0");
        props.setProperty("spring.batch.version", "5.1.0");
        props.setProperty("java.version", "17");
        
        // Configuration base de données
        props.setProperty("database.type", "H2");
        props.setProperty("database.url", "jdbc:h2:mem:testdb");
        props.setProperty("database.username", "sa");
        props.setProperty("database.password", "");
        props.setProperty("database.driver", "org.h2.Driver");
        
        // Configuration des chemins
        props.setProperty("file.input.directory", "data/input");
        props.setProperty("file.output.directory", "data/output");
        props.setProperty("file.archive.directory", "data/archive");
        
        // Configuration Batch
        props.setProperty("batch.chunk.size", "100");
        props.setProperty("batch.thread.pool.size", "4");
        props.setProperty("batch.skip.limit", "10");
        
        // Logging
        props.setProperty("target.log.level", "INFO");
        props.setProperty("translator.log.level", "INFO");
        props.setProperty("log.output", "CONSOLE");
        
        // Options de génération
        props.setProperty("generate.spring.config", "true");
        props.setProperty("generate.readme", "true");
        props.setProperty("generate.gitignore", "true");
        props.setProperty("generate.build.scripts", "false");
        props.setProperty("generate.dockerfile", "false");
        props.setProperty("maven.include.lombok", "true");
        props.setProperty("maven.include.devtools", "true");
        props.setProperty("copy.cobol.sources", "false");
        
        // Naming
        props.setProperty("naming.entity.suffix", "Record");
        props.setProperty("naming.processor.suffix", "Processor");
        props.setProperty("naming.job.suffix", "Job");
        props.setProperty("naming.tasklet.suffix", "Tasklet");
        props.setProperty("code.style", "SPRING");
        
        // Conversion rules
        props.setProperty("convert.dates.to.localdate", "true");
        props.setProperty("date.pivot.year", "50");
        props.setProperty("use.bigdecimal.for.financials", "true");
        props.setProperty("bigdecimal.default.scale", "2");
        props.setProperty("bigdecimal.rounding.mode", "HALF_UP");
        
        // Advanced options
        props.setProperty("cobol.sources.directory", "cobol");
        props.setProperty("include.cobol.comments", "true");
        props.setProperty("optimize.generated.code", "true");
        props.setProperty("generate.interfaces", "false");
        props.setProperty("generate.tests", "true");
        props.setProperty("generate.docs", "true");
        props.setProperty("generate.report", "true");
        props.setProperty("file.encoding", "UTF-8");
        
        // Créer un fichier de configuration temporaire
        Path configFile = tempDir.resolve("test-translator.properties");
        try (java.io.FileOutputStream out = new java.io.FileOutputStream(configFile.toFile())) {
            props.store(out, "Test Configuration");
        }
        
        return TranslatorConfiguration.load(configFile.toString());
    }
    
    /**
     * Crée une configuration personnalisée pour les tests
     */
    private TranslatorConfiguration createCustomConfiguration(Properties customProps) throws IOException {
        // Copier les propriétés par défaut
        Properties props = new Properties();
        
        // Propriétés minimales requises
        props.setProperty("target.project.name", "test-project");
        props.setProperty("target.projects.directory", tempDir.toString());
        props.setProperty("target.package.base", "com.example");
        props.setProperty("target.package.model", "model");
        props.setProperty("target.package.processor", "processor");
        props.setProperty("target.package.config", "config");
        props.setProperty("target.package.tasklet", "tasklet");
        props.setProperty("target.package.listener", "listener");
        props.setProperty("spring.boot.version", "3.2.0");
        props.setProperty("spring.batch.version", "5.1.0");
        props.setProperty("java.version", "17");
        props.setProperty("database.type", "H2");
        props.setProperty("database.url", "jdbc:h2:mem:testdb");
        props.setProperty("database.username", "sa");
        props.setProperty("database.password", "");
        props.setProperty("database.driver", "org.h2.Driver");
        props.setProperty("file.input.directory", "data/input");
        props.setProperty("file.output.directory", "data/output");
        props.setProperty("file.archive.directory", "data/archive");
        props.setProperty("batch.chunk.size", "100");
        props.setProperty("batch.thread.pool.size", "4");
        props.setProperty("batch.skip.limit", "10");
        props.setProperty("target.log.level", "INFO");
        props.setProperty("translator.log.level", "INFO");
        props.setProperty("log.output", "CONSOLE");
        props.setProperty("naming.entity.suffix", "Record");
        props.setProperty("naming.processor.suffix", "Processor");
        props.setProperty("naming.job.suffix", "Job");
        props.setProperty("naming.tasklet.suffix", "Tasklet");
        props.setProperty("code.style", "SPRING");
        props.setProperty("convert.dates.to.localdate", "true");
        props.setProperty("date.pivot.year", "50");
        props.setProperty("use.bigdecimal.for.financials", "true");
        props.setProperty("bigdecimal.default.scale", "2");
        props.setProperty("bigdecimal.rounding.mode", "HALF_UP");
        props.setProperty("cobol.sources.directory", "cobol");
        props.setProperty("include.cobol.comments", "true");
        props.setProperty("optimize.generated.code", "true");
        props.setProperty("generate.interfaces", "false");
        props.setProperty("generate.tests", "true");
        props.setProperty("generate.docs", "true");
        props.setProperty("generate.report", "true");
        props.setProperty("file.encoding", "UTF-8");
        props.setProperty("generate.spring.config", "true");
        props.setProperty("generate.readme", "true");
        props.setProperty("generate.gitignore", "true");
        props.setProperty("generate.build.scripts", "false");
        props.setProperty("generate.dockerfile", "false");
        props.setProperty("maven.include.lombok", "true");
        props.setProperty("maven.include.devtools", "true");
        props.setProperty("copy.cobol.sources", "false");
        props.setProperty("target.project.groupId", "com.test");
        props.setProperty("target.project.version", "1.0.0-SNAPSHOT");
        props.setProperty("target.project.description", "Test Project");
        props.setProperty("maven.include.test.dependencies", "true");
        props.setProperty("maven.include.mapstruct", "false");
        
        // Ajouter les propriétés personnalisées (peuvent écraser les defaults)
        props.putAll(customProps);
        
        // Créer un fichier de configuration temporaire
        Path configFile = tempDir.resolve("custom-translator-" + System.nanoTime() + ".properties");
        try (java.io.FileOutputStream out = new java.io.FileOutputStream(configFile.toFile())) {
            props.store(out, "Custom Test Configuration");
        }
        
        return TranslatorConfiguration.load(configFile.toString());
    }
    
    // ==================== Tests de génération de structure ====================
    
    @Test
    @Order(1)
    @DisplayName("Doit créer la structure de répertoires Maven standard")
    void testCreateDirectoryStructure() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        
        // Assert - Structure Maven de base
        assertTrue(Files.exists(projectPath.resolve("src/main/java")), 
            "Le répertoire src/main/java doit exister");
        assertTrue(Files.exists(projectPath.resolve("src/main/resources")), 
            "Le répertoire src/main/resources doit exister");
        assertTrue(Files.exists(projectPath.resolve("src/test/java")), 
            "Le répertoire src/test/java doit exister");
        assertTrue(Files.exists(projectPath.resolve("src/test/resources")), 
            "Le répertoire src/test/resources doit exister");
    }
    
    @Test
    @Order(2)
    @DisplayName("Doit créer tous les packages Java configurés")
    void testCreatePackageStructure() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        
        // Assert - Packages
        String packagePath = "src/main/java/com/example/cobol";
        assertTrue(Files.exists(projectPath.resolve(packagePath + "/model")), 
            "Le package model doit exister");
        assertTrue(Files.exists(projectPath.resolve(packagePath + "/processor")), 
            "Le package processor doit exister");
        assertTrue(Files.exists(projectPath.resolve(packagePath + "/config")), 
            "Le package config doit exister");
        assertTrue(Files.exists(projectPath.resolve(packagePath + "/tasklet")), 
            "Le package tasklet doit exister");
        assertTrue(Files.exists(projectPath.resolve(packagePath + "/listener")), 
            "Le package listener doit exister");
    }
    
    @Test
    @Order(3)
    @DisplayName("Doit créer les répertoires de données (input, output, archive)")
    void testCreateDataDirectories() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        
        // Assert - Répertoires de données
        assertTrue(Files.exists(projectPath.resolve("data/input")), 
            "Le répertoire data/input doit exister");
        assertTrue(Files.exists(projectPath.resolve("data/output")), 
            "Le répertoire data/output doit exister");
        assertTrue(Files.exists(projectPath.resolve("data/archive")), 
            "Le répertoire data/archive doit exister");
    }
    
    @Test
    @Order(4)
    @DisplayName("Doit créer le répertoire docs")
    void testCreateDocsDirectory() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        
        // Assert
        assertTrue(Files.exists(projectPath.resolve("docs")), 
            "Le répertoire docs doit exister");
    }
    
    // ==================== Tests de génération pom.xml ====================
    
    @Test
    @Order(5)
    @DisplayName("Doit générer un fichier pom.xml valide")
    void testGeneratePomXml() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        Path pomPath = projectPath.resolve("pom.xml");
        
        // Assert
        assertTrue(Files.exists(pomPath), "Le fichier pom.xml doit exister");
        assertTrue(Files.size(pomPath) > 0, "Le fichier pom.xml ne doit pas être vide");
    }
    
    @Test
    @Order(6)
    @DisplayName("Doit inclure les informations du projet dans pom.xml")
    void testPomXmlProjectInfo() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Informations du projet
        assertAll("Informations du projet",
            () -> assertTrue(pomContent.contains("<groupId>com.example.cobol</groupId>"), 
                "GroupId doit être présent"),
            () -> assertTrue(pomContent.contains("<artifactId>cobol-batch-processor</artifactId>"), 
                "ArtifactId doit être présent"),
            () -> assertTrue(pomContent.contains("<version>1.0.0-SNAPSHOT</version>"), 
                "Version doit être présente"),
            () -> assertTrue(pomContent.contains("<name>cobol-batch-processor</name>"), 
                "Name doit être présent"),
            () -> assertTrue(pomContent.contains("<description>COBOL Batch Processor - Test Project</description>"), 
                "Description doit être présente")
        );
    }
    
    @Test
    @Order(7)
    @DisplayName("Doit configurer Spring Boot parent dans pom.xml")
    void testPomXmlSpringBootParent() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Parent Spring Boot
        assertAll("Configuration Spring Boot Parent",
            () -> assertTrue(pomContent.contains("<groupId>org.springframework.boot</groupId>"), 
                "GroupId Spring Boot doit être présent"),
            () -> assertTrue(pomContent.contains("<artifactId>spring-boot-starter-parent</artifactId>"), 
                "ArtifactId spring-boot-starter-parent doit être présent"),
            () -> assertTrue(pomContent.contains("<version>3.2.0</version>"), 
                "Version Spring Boot doit être présente")
        );
    }
    
    @Test
    @Order(8)
    @DisplayName("Doit configurer les propriétés Java et Maven dans pom.xml")
    void testPomXmlProperties() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Properties
        assertAll("Propriétés Maven",
            () -> assertTrue(pomContent.contains("<java.version>17</java.version>"), 
                "Version Java doit être 17"),
            () -> assertTrue(pomContent.contains("<maven.compiler.source>17</maven.compiler.source>"), 
                "Compiler source doit être 17"),
            () -> assertTrue(pomContent.contains("<maven.compiler.target>17</maven.compiler.target>"), 
                "Compiler target doit être 17"),
            () -> assertTrue(pomContent.contains("<spring-batch.version>5.1.0</spring-batch.version>"), 
                "Version Spring Batch doit être présente")
        );
    }
    
    @Test
    @Order(9)
    @DisplayName("Doit inclure les dépendances Spring Boot essentielles dans pom.xml")
    void testPomXmlSpringBootDependencies() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Dépendances Spring Boot
        assertAll("Dépendances Spring Boot",
            () -> assertTrue(pomContent.contains("spring-boot-starter</artifactId>"), 
                "Dépendance spring-boot-starter doit être présente"),
            () -> assertTrue(pomContent.contains("spring-boot-starter-batch</artifactId>"), 
                "Dépendance spring-boot-starter-batch doit être présente"),
            () -> assertTrue(pomContent.contains("spring-boot-starter-data-jpa</artifactId>"), 
                "Dépendance spring-boot-starter-data-jpa doit être présente"),
            () -> assertTrue(pomContent.contains("spring-boot-starter-validation</artifactId>"), 
                "Dépendance spring-boot-starter-validation doit être présente")
        );
    }
    
    @Test
    @Order(10)
    @DisplayName("Doit inclure la dépendance base de données H2 dans pom.xml")
    void testPomXmlDatabaseDependency() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Dépendance H2
        assertTrue(pomContent.contains("<artifactId>h2</artifactId>"), 
            "Dépendance H2 doit être présente");
    }
    
    @Test
    @Order(11)
    @DisplayName("Doit inclure Lombok quand configuré dans pom.xml")
    void testPomXmlLombokDependency() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Lombok
        assertTrue(pomContent.contains("<artifactId>lombok</artifactId>"), 
            "Dépendance Lombok doit être présente");
        assertTrue(pomContent.contains("<optional>true</optional>"), 
            "Lombok doit être marquée comme optional");
    }
    
    @Test
    @Order(12)
    @DisplayName("Doit inclure les dépendances de test dans pom.xml")
    void testPomXmlTestDependencies() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Dépendances de test
        assertAll("Dépendances de test",
            () -> assertTrue(pomContent.contains("spring-boot-starter-test</artifactId>"), 
                "Dépendance spring-boot-starter-test doit être présente"),
            () -> assertTrue(pomContent.contains("spring-batch-test</artifactId>"), 
                "Dépendance spring-batch-test doit être présente"),
            () -> assertTrue(pomContent.contains("<scope>test</scope>"), 
                "Les dépendances de test doivent avoir le scope test")
        );
    }
    
    @Test
    @Order(13)
    @DisplayName("Doit configurer le plugin Maven Spring Boot dans pom.xml")
    void testPomXmlMavenPlugin() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Plugin Maven
        assertTrue(pomContent.contains("spring-boot-maven-plugin</artifactId>"), 
            "Plugin spring-boot-maven-plugin doit être présent");
    }
    
    @Test
    @Order(14)
    @DisplayName("Doit générer un pom.xml valide XML")
    void testPomXmlValidXml() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert - Structure XML valide
        assertAll("Structure XML valide",
            () -> assertTrue(pomContent.startsWith("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"), 
                "Doit commencer par la déclaration XML"),
            () -> assertTrue(pomContent.contains("<project xmlns="), 
                "Doit contenir l'élément project avec namespace"),
            () -> assertTrue(pomContent.contains("</project>"), 
                "Doit se terminer par la balise fermante project"),
            () -> assertTrue(pomContent.contains("<modelVersion>4.0.0</modelVersion>"), 
                "Doit avoir le modelVersion 4.0.0")
        );
    }
    
    // ==================== Tests de génération application.properties ====================
    
    @Test
    @Order(15)
    @DisplayName("Doit générer le fichier application.properties")
    void testGenerateApplicationProperties() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        Path propsPath = projectPath.resolve("src/main/resources/application.properties");
        
        // Assert
        assertTrue(Files.exists(propsPath), 
            "Le fichier application.properties doit exister");
        assertTrue(Files.size(propsPath) > 0, 
            "Le fichier application.properties ne doit pas être vide");
    }
    
    @Test
    @Order(16)
    @DisplayName("Doit configurer l'application name dans application.properties")
    void testApplicationPropertiesAppName() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String propsContent = Files.readString(
            projectPath.resolve("src/main/resources/application.properties"));
        
        // Assert
        assertTrue(propsContent.contains("spring.application.name=cobol-batch-processor"), 
            "Application name doit être configuré");
    }
    
    @Test
    @Order(17)
    @DisplayName("Doit configurer la datasource dans application.properties")
    void testApplicationPropertiesDatasource() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String propsContent = Files.readString(
            projectPath.resolve("src/main/resources/application.properties"));
        
        // Assert - Configuration datasource
        assertAll("Configuration datasource",
            () -> assertTrue(propsContent.contains("spring.datasource.url=jdbc:h2:mem:testdb"), 
                "URL datasource doit être configurée"),
            () -> assertTrue(propsContent.contains("spring.datasource.username=sa"), 
                "Username datasource doit être configuré"),
            () -> assertTrue(propsContent.contains("spring.datasource.driver-class-name=org.h2.Driver"), 
                "Driver datasource doit être configuré")
        );
    }
    
    @Test
    @Order(18)
    @DisplayName("Doit configurer JPA/Hibernate dans application.properties")
    void testApplicationPropertiesJpa() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String propsContent = Files.readString(
            projectPath.resolve("src/main/resources/application.properties"));
        
        // Assert - Configuration JPA
        assertAll("Configuration JPA",
            () -> assertTrue(propsContent.contains("spring.jpa.hibernate.ddl-auto="), 
                "Configuration Hibernate DDL doit être présente"),
            () -> assertTrue(propsContent.contains("spring.jpa.show-sql="), 
                "Configuration show-sql doit être présente")
        );
    }
    
    @Test
    @Order(19)
    @DisplayName("Doit configurer le logging dans application.properties")
    void testApplicationPropertiesLogging() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String propsContent = Files.readString(
            projectPath.resolve("src/main/resources/application.properties"));
        
        // Assert - Configuration logging
        assertAll("Configuration logging",
            () -> assertTrue(propsContent.contains("logging.level.root=INFO"), 
                "Log level root doit être configuré"),
            () -> assertTrue(propsContent.contains("logging.level.com.example.cobol=INFO"), 
                "Log level du package doit être configuré"),
            () -> assertTrue(propsContent.contains("logging.pattern.console="), 
                "Pattern de logging console doit être configuré")
        );
    }
    
    @Test
    @Order(20)
    @DisplayName("Doit configurer les chemins de fichiers dans application.properties")
    void testApplicationPropertiesFilePaths() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String propsContent = Files.readString(
            projectPath.resolve("src/main/resources/application.properties"));
        
        // Assert - Chemins de fichiers
        assertAll("Configuration chemins de fichiers",
            () -> assertTrue(propsContent.contains("app.input.directory=data/input"), 
                "Répertoire input doit être configuré"),
            () -> assertTrue(propsContent.contains("app.output.directory=data/output"), 
                "Répertoire output doit être configuré"),
            () -> assertTrue(propsContent.contains("app.archive.directory=data/archive"), 
                "Répertoire archive doit être configuré")
        );
    }
    
    @Test
    @Order(21)
    @DisplayName("Doit configurer les paramètres Batch dans application.properties")
    void testApplicationPropertiesBatchConfig() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String propsContent = Files.readString(
            projectPath.resolve("src/main/resources/application.properties"));
        
        // Assert - Configuration Batch
        assertAll("Configuration Batch",
            () -> assertTrue(propsContent.contains("app.batch.chunk-size=100"), 
                "Chunk size doit être configuré"),
            () -> assertTrue(propsContent.contains("app.batch.thread-pool-size=4"), 
                "Thread pool size doit être configuré"),
            () -> assertTrue(propsContent.contains("app.batch.skip-limit=10"), 
                "Skip limit doit être configuré")
        );
    }
    
    // ==================== Tests de génération README.md ====================
    
    @Test
    @Order(22)
    @DisplayName("Doit générer le fichier README.md")
    void testGenerateReadme() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        Path readmePath = projectPath.resolve("README.md");
        
        // Assert
        assertTrue(Files.exists(readmePath), "Le fichier README.md doit exister");
        assertTrue(Files.size(readmePath) > 0, "Le fichier README.md ne doit pas être vide");
    }
    
    @Test
    @Order(23)
    @DisplayName("Doit inclure le nom du projet dans README.md")
    void testReadmeProjectName() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String readmeContent = Files.readString(projectPath.resolve("README.md"));
        
        // Assert
        assertTrue(readmeContent.contains("cobol-batch-processor"), 
            "Le nom du projet doit être présent dans le README");
    }
    
    @Test
    @Order(24)
    @DisplayName("Doit inclure les sections essentielles dans README.md")
    void testReadmeSections() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String readmeContent = Files.readString(projectPath.resolve("README.md"));
        
        // Assert - Sections principales
        assertAll("Sections du README",
            () -> assertTrue(readmeContent.contains("## Description"), 
                "Section Description doit être présente"),
            () -> assertTrue(readmeContent.contains("## Structure du projet"), 
                "Section Structure du projet doit être présente"),
            () -> assertTrue(readmeContent.contains("## Prerequis"), 
                "Section Prérequis doit être présente"),
            () -> assertTrue(readmeContent.contains("## Installation"), 
                "Section Installation doit être présente"),
            () -> assertTrue(readmeContent.contains("## Configuration"), 
                "Section Configuration doit être présente")
        );
    }
    
    @Test
    @Order(25)
    @DisplayName("Doit inclure les commandes Maven dans README.md")
    void testReadmeMavenCommands() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String readmeContent = Files.readString(projectPath.resolve("README.md"));
        
        // Assert - Commandes Maven
        assertAll("Commandes Maven",
            () -> assertTrue(readmeContent.contains("mvn clean install"), 
                "Commande mvn clean install doit être présente"),
            () -> assertTrue(readmeContent.contains("mvn test"), 
                "Commande mvn test doit être présente"),
            () -> assertTrue(readmeContent.contains("mvn spring-boot:run"), 
                "Commande mvn spring-boot:run doit être présente")
        );
    }
    
    @Test
    @Order(26)
    @DisplayName("Doit mentionner la version Java requise dans README.md")
    void testReadmeJavaVersion() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String readmeContent = Files.readString(projectPath.resolve("README.md"));
        
        // Assert
        assertTrue(readmeContent.contains("Java 17"), 
            "La version Java requise doit être mentionnée");
    }
    
    // ==================== Tests de génération .gitignore ====================
    
    @Test
    @Order(27)
    @DisplayName("Doit générer le fichier .gitignore")
    void testGenerateGitignore() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        Path gitignorePath = projectPath.resolve(".gitignore");
        
        // Assert
        assertTrue(Files.exists(gitignorePath), "Le fichier .gitignore doit exister");
        assertTrue(Files.size(gitignorePath) > 0, "Le fichier .gitignore ne doit pas être vide");
    }
    
    @Test
    @Order(28)
    @DisplayName("Doit ignorer les fichiers Maven dans .gitignore")
    void testGitignoreMaven() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String gitignoreContent = Files.readString(projectPath.resolve(".gitignore"));
        
        // Assert - Fichiers Maven
        assertAll("Fichiers Maven à ignorer",
            () -> assertTrue(gitignoreContent.contains("target/"), 
                "target/ doit être ignoré"),
            () -> assertTrue(gitignoreContent.contains("pom.xml.tag"), 
                "pom.xml.tag doit être ignoré"),
            () -> assertTrue(gitignoreContent.contains("dependency-reduced-pom.xml"), 
                "dependency-reduced-pom.xml doit être ignoré")
        );
    }
    
    @Test
    @Order(29)
    @DisplayName("Doit ignorer les fichiers IDE dans .gitignore")
    void testGitignoreIde() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String gitignoreContent = Files.readString(projectPath.resolve(".gitignore"));
        
        // Assert - Fichiers IDE
        assertAll("Fichiers IDE à ignorer",
            () -> assertTrue(gitignoreContent.contains(".idea/"), 
                ".idea/ doit être ignoré"),
            () -> assertTrue(gitignoreContent.contains("*.iml"), 
                "*.iml doit être ignoré"),
            () -> assertTrue(gitignoreContent.contains(".vscode/"), 
                ".vscode/ doit être ignoré"),
            () -> assertTrue(gitignoreContent.contains(".project"), 
                ".project (Eclipse) doit être ignoré"),
            () -> assertTrue(gitignoreContent.contains(".classpath"), 
                ".classpath (Eclipse) doit être ignoré")
        );
    }
    
    @Test
    @Order(30)
    @DisplayName("Doit ignorer les fichiers log dans .gitignore")
    void testGitignoreLogs() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String gitignoreContent = Files.readString(projectPath.resolve(".gitignore"));
        
        // Assert - Fichiers log
        assertAll("Fichiers log à ignorer",
            () -> assertTrue(gitignoreContent.contains("*.log"), 
                "*.log doit être ignoré"),
            () -> assertTrue(gitignoreContent.contains("logs/"), 
                "logs/ doit être ignoré")
        );
    }
    
    @Test
    @Order(31)
    @DisplayName("Doit ignorer les fichiers OS dans .gitignore")
    void testGitignoreOs() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        String gitignoreContent = Files.readString(projectPath.resolve(".gitignore"));
        
        // Assert - Fichiers OS
        assertAll("Fichiers OS à ignorer",
            () -> assertTrue(gitignoreContent.contains(".DS_Store"), 
                ".DS_Store (macOS) doit être ignoré"),
            () -> assertTrue(gitignoreContent.contains("Thumbs.db"), 
                "Thumbs.db (Windows) doit être ignoré")
        );
    }
    
    // ==================== Tests de génération classe Application ====================
    
    @Test
    @Order(32)
    @DisplayName("Doit générer la classe Application Spring Boot")
    void testGenerateMainApplicationClass() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        
        // Assert - Vérifier qu'une classe Application existe
        Path appPath = projectPath.resolve(
            "src/main/java/com/example/cobol/CobolBatchProcessorApplication.java");
        
        assertTrue(Files.exists(appPath), 
            "La classe Application Spring Boot doit exister");
    }
    
    // ==================== Tests de cas spéciaux ====================
    
    @Test
    @Order(33)
    @DisplayName("Doit gérer les noms de projet avec tirets")
    void testProjectNameWithDashes() throws IOException {
        // Arrange
        Properties props = new Properties();
        props.setProperty("target.project.name", "my-cobol-batch");
        TranslatorConfiguration cfg = createCustomConfiguration(props);
        ProjectGenerator gen = new ProjectGenerator(cfg);
        
        // Act
        Path projectPath = gen.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert
        assertTrue(pomContent.contains("<artifactId>my-cobol-batch</artifactId>"), 
            "ArtifactId avec tirets doit être préservé");
    }
    
    @Test
    @Order(34)
    @DisplayName("Ne doit pas générer Lombok si désactivé")
    void testWithoutLombok() throws IOException {
        // Arrange
        Properties props = new Properties();
        props.setProperty("maven.include.lombok", "false");
        TranslatorConfiguration cfg = createCustomConfiguration(props);
        ProjectGenerator gen = new ProjectGenerator(cfg);
        
        // Act
        Path projectPath = gen.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        
        // Assert
        assertFalse(pomContent.contains("<artifactId>lombok</artifactId>"), 
            "Lombok ne doit pas être présent quand désactivé");
    }
    
    @Test
    @Order(35)
    @DisplayName("Ne doit pas générer README si désactivé")
    void testWithoutReadme() throws IOException {
        // Arrange
        Properties props = new Properties();
        props.setProperty("generate.readme", "false");
        TranslatorConfiguration cfg = createCustomConfiguration(props);
        ProjectGenerator gen = new ProjectGenerator(cfg);
        
        // Act
        Path projectPath = gen.generateProject();
        
        // Assert
        assertFalse(Files.exists(projectPath.resolve("README.md")), 
            "README.md ne doit pas exister quand désactivé");
    }
    
    @Test
    @Order(36)
    @DisplayName("Ne doit pas générer .gitignore si désactivé")
    void testWithoutGitignore() throws IOException {
        // Arrange
        Properties props = new Properties();
        props.setProperty("generate.gitignore", "false");
        TranslatorConfiguration cfg = createCustomConfiguration(props);
        ProjectGenerator gen = new ProjectGenerator(cfg);
        
        // Act
        Path projectPath = gen.generateProject();
        
        // Assert
        assertFalse(Files.exists(projectPath.resolve(".gitignore")), 
            ".gitignore ne doit pas exister quand désactivé");
    }
    
    @Test
    @Order(37)
    @DisplayName("Doit gérer différentes bases de données (PostgreSQL)")
    void testWithPostgreSql() throws IOException {
        // Arrange
        Properties props = new Properties();
        props.setProperty("database.type", "POSTGRESQL");
        props.setProperty("database.driver", "org.postgresql.Driver");
        props.setProperty("database.url", "jdbc:postgresql://localhost:5432/mydb");
        TranslatorConfiguration cfg = createCustomConfiguration(props);
        ProjectGenerator gen = new ProjectGenerator(cfg);
        
        // Act
        Path projectPath = gen.generateProject();
        String pomContent = Files.readString(projectPath.resolve("pom.xml"));
        String propsContent = Files.readString(
            projectPath.resolve("src/main/resources/application.properties"));
        
        // Assert
        assertAll("Configuration PostgreSQL",
            () -> assertTrue(pomContent.contains("<artifactId>postgresql</artifactId>"), 
                "Dépendance PostgreSQL doit être présente dans pom.xml"),
            () -> assertTrue(propsContent.contains("jdbc:postgresql://"), 
                "URL PostgreSQL doit être présente dans application.properties")
        );
    }
    
    @Test
    @Order(38)
    @DisplayName("Doit retourner le chemin du projet généré")
    void testGenerateProjectReturnsPath() throws IOException {
        // Act
        Path returnedPath = generator.generateProject();
        
        // Assert
        assertNotNull(returnedPath, "Le chemin retourné ne doit pas être null");
        assertTrue(Files.exists(returnedPath), "Le chemin retourné doit exister");
        assertTrue(returnedPath.toString().contains("cobol-batch-processor"), 
            "Le chemin retourné doit contenir le nom du projet");
    }
    
    // ==================== Tests d'intégration ====================
    
    @Test
    @Order(39)
    @DisplayName("Doit générer un projet Maven complet et cohérent")
    void testGenerateCompleteProject() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        
        // Assert - Vérifier tous les éléments essentiels
        assertAll("Projet Maven complet",
            // Structure de base
            () -> assertTrue(Files.exists(projectPath.resolve("pom.xml")), 
                "pom.xml doit exister"),
            () -> assertTrue(Files.exists(projectPath.resolve("src/main/java")), 
                "src/main/java doit exister"),
            () -> assertTrue(Files.exists(projectPath.resolve("src/main/resources")), 
                "src/main/resources doit exister"),
            () -> assertTrue(Files.exists(projectPath.resolve("src/test/java")), 
                "src/test/java doit exister"),
            
            // Fichiers de configuration
            () -> assertTrue(Files.exists(
                projectPath.resolve("src/main/resources/application.properties")), 
                "application.properties doit exister"),
            
            // Documentation
            () -> assertTrue(Files.exists(projectPath.resolve("README.md")), 
                "README.md doit exister"),
            () -> assertTrue(Files.exists(projectPath.resolve(".gitignore")), 
                ".gitignore doit exister"),
            
            // Packages
            () -> assertTrue(Files.exists(
                projectPath.resolve("src/main/java/com/example/cobol/model")), 
                "Package model doit exister"),
            () -> assertTrue(Files.exists(
                projectPath.resolve("src/main/java/com/example/cobol/config")), 
                "Package config doit exister"),
            
            // Répertoires de données
            () -> assertTrue(Files.exists(projectPath.resolve("data/input")), 
                "data/input doit exister"),
            () -> assertTrue(Files.exists(projectPath.resolve("data/output")), 
                "data/output doit exister"),
            () -> assertTrue(Files.exists(projectPath.resolve("data/archive")), 
                "data/archive doit exister"),
            
            // Classe principale
            () -> assertTrue(Files.exists(projectPath.resolve(
                "src/main/java/com/example/cobol/CobolBatchProcessorApplication.java")), 
                "Classe Application doit exister")
        );
    }
    
    @Test
    @Order(40)
    @DisplayName("Doit générer un projet avec des fichiers non-vides")
    void testGeneratedFilesNotEmpty() throws IOException {
        // Act
        Path projectPath = generator.generateProject();
        
        // Assert - Tous les fichiers principaux doivent être non-vides
        assertAll("Fichiers non-vides",
            () -> assertTrue(Files.size(projectPath.resolve("pom.xml")) > 1000, 
                "pom.xml doit contenir du contenu substantiel"),
            () -> assertTrue(Files.size(
                projectPath.resolve("src/main/resources/application.properties")) > 500, 
                "application.properties doit contenir du contenu substantiel"),
            () -> assertTrue(Files.size(projectPath.resolve("README.md")) > 500, 
                "README.md doit contenir du contenu substantiel"),
            () -> assertTrue(Files.size(projectPath.resolve(".gitignore")) > 200, 
                ".gitignore doit contenir du contenu substantiel")
        );
    }
}
