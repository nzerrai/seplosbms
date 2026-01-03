package com.cobol.translator.project;

import com.cobol.translator.config.TranslatorConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * Generateur de structure de projet Maven Spring Boot.
 *
 * Cree un projet Java complet et separe du convertisseur avec :
 * - Structure Maven standard
 * - pom.xml avec dependances Spring Boot/Batch
 * - Structure de packages
 * - Fichiers de configuration
 * - README, .gitignore, etc.
 */
public class ProjectGenerator {

    private static final Logger logger = LoggerFactory.getLogger(ProjectGenerator.class);

    private final TranslatorConfiguration config;

    public ProjectGenerator(TranslatorConfiguration config) {
        this.config = config;
    }

    /**
     * Genere la structure complete du projet cible.
     */
    public Path generateProject() throws IOException {
        Path projectPath = config.getTargetProjectPath();

        logger.info("Creating target project: {}", projectPath);

        // Creer la structure de repertoires
        createDirectoryStructure(projectPath);

        // Generer le pom.xml
        generatePomXml(projectPath);

        // Generer les fichiers de configuration Spring
        if (config.isGenerateSpringConfig()) {
            generateSpringConfiguration(projectPath);
        }

        // Generer le README
        if (config.isGenerateReadme()) {
            generateReadme(projectPath);
        }

        // Generer le .gitignore
        if (config.isGenerateGitignore()) {
            generateGitignore(projectPath);
        }

        // Generer les scripts de build
        if (config.isGenerateBuildScripts()) {
            generateBuildScripts(projectPath);
        }

        // Generer le Dockerfile
        if (config.isGenerateDockerfile()) {
            generateDockerfile(projectPath);
        }

        logger.info("Project structure created successfully at: {}", projectPath);

        return projectPath;
    }

    /**
     * Cree la structure de repertoires Maven standard.
     */
    private void createDirectoryStructure(Path projectPath) throws IOException {
        // Structure Maven standard
        Files.createDirectories(projectPath.resolve("src/main/java"));
        Files.createDirectories(projectPath.resolve("src/main/resources"));
        Files.createDirectories(projectPath.resolve("src/test/java"));
        Files.createDirectories(projectPath.resolve("src/test/resources"));

        // Creer les packages
        String packagePath = config.getTargetPackageBase().replace('.', '/');
        Files.createDirectories(projectPath.resolve("src/main/java/" + packagePath + "/" +
            config.getTargetPackageModel()));
        Files.createDirectories(projectPath.resolve("src/main/java/" + packagePath + "/" +
            config.getTargetPackageProcessor()));
        Files.createDirectories(projectPath.resolve("src/main/java/" + packagePath + "/" +
            config.getTargetPackageConfig()));
        Files.createDirectories(projectPath.resolve("src/main/java/" + packagePath + "/" +
            config.getTargetPackageTasklet()));
        Files.createDirectories(projectPath.resolve("src/main/java/" + packagePath + "/" +
            config.getTargetPackageListener()));

        // Repertoires de donnees
        Files.createDirectories(projectPath.resolve(config.getFileInputDirectory()));
        Files.createDirectories(projectPath.resolve(config.getFileOutputDirectory()));
        Files.createDirectories(projectPath.resolve(config.getFileArchiveDirectory()));

        // Repertoire pour les sources COBOL originales (optionnel)
        if (config.isCopyCobolSources()) {
            Files.createDirectories(projectPath.resolve(config.getCobolSourcesDirectory()));
        }

        // Repertoire docs
        Files.createDirectories(projectPath.resolve("docs"));

        logger.info("Directory structure created");
    }

    /**
     * Genere le fichier pom.xml.
     */
    private void generatePomXml(Path projectPath) throws IOException {
        String pom = """
            <?xml version="1.0" encoding="UTF-8"?>
            <project xmlns="http://maven.apache.org/POM/4.0.0"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                     http://maven.apache.org/xsd/maven-4.0.0.xsd">
                <modelVersion>4.0.0</modelVersion>

                <groupId>%s</groupId>
                <artifactId>%s</artifactId>
                <version>%s</version>
                <packaging>jar</packaging>

                <name>%s</name>
                <description>%s</description>

                <parent>
                    <groupId>org.springframework.boot</groupId>
                    <artifactId>spring-boot-starter-parent</artifactId>
                    <version>%s</version>
                    <relativePath/>
                </parent>

                <properties>
                    <java.version>%s</java.version>
                    <maven.compiler.source>%s</maven.compiler.source>
                    <maven.compiler.target>%s</maven.compiler.target>
                    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
                    <spring-batch.version>%s</spring-batch.version>
                </properties>

                <dependencies>
                    <!-- Spring Boot Starter -->
                    <dependency>
                        <groupId>org.springframework.boot</groupId>
                        <artifactId>spring-boot-starter</artifactId>
                    </dependency>

                    <!-- Spring Batch -->
                    <dependency>
                        <groupId>org.springframework.boot</groupId>
                        <artifactId>spring-boot-starter-batch</artifactId>
                    </dependency>

                    <!-- Database -->
                    <dependency>
                        <groupId>org.springframework.boot</groupId>
                        <artifactId>spring-boot-starter-data-jpa</artifactId>
                    </dependency>

                    %s

                    <!-- Validation -->
                    <dependency>
                        <groupId>org.springframework.boot</groupId>
                        <artifactId>spring-boot-starter-validation</artifactId>
                    </dependency>

                    %s

                    %s

                    <!-- Logging -->
                    <dependency>
                        <groupId>org.springframework.boot</groupId>
                        <artifactId>spring-boot-starter-logging</artifactId>
                    </dependency>

                    <!-- Test -->
                    %s
                </dependencies>

                <build>
                    <plugins>
                        <plugin>
                            <groupId>org.springframework.boot</groupId>
                            <artifactId>spring-boot-maven-plugin</artifactId>
                        </plugin>
                    </plugins>
                </build>
            </project>
            """.formatted(
            config.getTargetProjectGroupId(),
            config.getTargetProjectName(),
            config.getTargetProjectVersion(),
            config.getTargetProjectName(),
            config.getTargetProjectDescription(),
            config.getSpringBootVersion(),
            config.getJavaVersion(),
            config.getJavaVersion(),
            config.getJavaVersion(),
            config.getSpringBatchVersion(),
            getDatabaseDependency(),
            getLombokDependency(),
            getDevtoolsDependency(),
            getTestDependencies()
        );

        Files.writeString(projectPath.resolve("pom.xml"), pom);
        logger.info("Generated pom.xml");
    }

    private String getDatabaseDependency() {
        return switch (config.getDatabaseType().toUpperCase()) {
            case "H2" -> """
                <dependency>
                    <groupId>com.h2database</groupId>
                    <artifactId>h2</artifactId>
                    <scope>runtime</scope>
                </dependency>
                """;
            case "POSTGRESQL" -> """
                <dependency>
                    <groupId>org.postgresql</groupId>
                    <artifactId>postgresql</artifactId>
                    <scope>runtime</scope>
                </dependency>
                """;
            case "MYSQL" -> """
                <dependency>
                    <groupId>com.mysql</groupId>
                    <artifactId>mysql-connector-j</artifactId>
                    <scope>runtime</scope>
                </dependency>
                """;
            case "ORACLE" -> """
                <dependency>
                    <groupId>com.oracle.database.jdbc</groupId>
                    <artifactId>ojdbc11</artifactId>
                    <scope>runtime</scope>
                </dependency>
                """;
            case "DB2" -> """
                <dependency>
                    <groupId>com.ibm.db2</groupId>
                    <artifactId>jcc</artifactId>
                    <scope>runtime</scope>
                </dependency>
                """;
            default -> "<!-- Database dependency not configured -->";
        };
    }

    private String getLombokDependency() {
        if (!config.isMavenIncludeLombok()) {
            return "";
        }
        return """
            <!-- Lombok -->
            <dependency>
                <groupId>org.projectlombok</groupId>
                <artifactId>lombok</artifactId>
                <optional>true</optional>
            </dependency>
            """;
    }

    private String getDevtoolsDependency() {
        if (!config.isMavenIncludeDevtools()) {
            return "";
        }
        return """
            <!-- DevTools -->
            <dependency>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-devtools</artifactId>
                <scope>runtime</scope>
                <optional>true</optional>
            </dependency>
            """;
    }

    private String getTestDependencies() {
        if (!config.isMavenIncludeTestDependencies()) {
            return "";
        }
        return """
            <dependency>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-starter-test</artifactId>
                <scope>test</scope>
            </dependency>
            <dependency>
                <groupId>org.springframework.batch</groupId>
                <artifactId>spring-batch-test</artifactId>
                <scope>test</scope>
            </dependency>
            """;
    }

    /**
     * Genere les fichiers de configuration Spring.
     */
    private void generateSpringConfiguration(Path projectPath) throws IOException {
        String applicationProperties = """
            # Application
            spring.application.name=%s

            # Batch
            spring.batch.job.enabled=false
            spring.batch.jdbc.initialize-schema=always

            # Database
            spring.datasource.url=%s
            spring.datasource.username=%s
            spring.datasource.password=%s
            spring.datasource.driver-class-name=%s

            # JPA
            spring.jpa.hibernate.ddl-auto=update
            spring.jpa.show-sql=false
            spring.jpa.properties.hibernate.format_sql=true

            # Logging
            logging.level.root=%s
            logging.level.%s=%s
            logging.pattern.console=%%d{yyyy-MM-dd HH:mm:ss} - %%msg%%n

            # File paths
            app.input.directory=%s
            app.output.directory=%s
            app.archive.directory=%s

            # Batch configuration
            app.batch.chunk-size=%d
            app.batch.thread-pool-size=%d
            app.batch.skip-limit=%d
            """.formatted(
            config.getTargetProjectName(),
            config.getDatabaseUrl(),
            config.getDatabaseUsername(),
            config.getDatabasePassword(),
            config.getDatabaseDriver(),
            config.getTargetLogLevel(),
            config.getTargetPackageBase(),
            config.getTargetLogLevel(),
            config.getFileInputDirectory(),
            config.getFileOutputDirectory(),
            config.getFileArchiveDirectory(),
            config.getBatchChunkSize(),
            config.getBatchThreadPoolSize(),
            config.getBatchSkipLimit()
        );

        Files.writeString(
            projectPath.resolve("src/main/resources/application.properties"),
            applicationProperties
        );

        logger.info("Generated application.properties");
    }

    /**
     * Genere le README.md du projet.
     */
    private void generateReadme(Path projectPath) throws IOException {
        String readme = """
            # %s

            %s

            ## Description

            Ce projet a ete genere automatiquement par le traducteur COBOL vers Java Spring Batch.

            ## Structure du projet

            ```
            %s/
            ├── src/main/java/%s/
            │   ├── model/          # Entites (Records)
            │   ├── processor/      # ItemProcessors
            │   ├── config/         # Configuration Spring Batch
            │   ├── tasklet/        # Tasklets
            │   └── listener/       # Listeners
            ├── src/main/resources/
            │   └── application.properties
            ├── src/test/java/      # Tests unitaires
            ├── data/
            │   ├── input/          # Fichiers d'entree
            │   ├── output/         # Fichiers de sortie
            │   └── archive/        # Archives
            └── docs/               # Documentation
            ```

            ## Prerequis

            - Java %s ou superieur
            - Maven 3.6+
            - Base de donnees %s

            ## Installation

            ```bash
            # Compiler le projet
            mvn clean install

            # Lancer les tests
            mvn test

            # Executer l'application
            mvn spring-boot:run
            ```

            ## Configuration

            Modifier le fichier `src/main/resources/application.properties` pour adapter :
            - La connexion a la base de donnees
            - Les chemins des fichiers
            - Les parametres du batch

            ## Execution d'un job

            ```bash
            java -jar target/%s-%s.jar --job.name=<nom-du-job>
            ```

            ## Documentation

            - Voir le repertoire `docs/` pour la documentation complete
            - Consulter les rapports de conversion pour les details de la migration COBOL

            ## Support

            Pour toute question sur la migration COBOL, consulter :
            - Les fichiers COBOL originaux (si inclus) dans `%s/`
            - Les rapports de conversion dans `docs/`

            ---

            Genere par : COBOL to Java Spring Batch Translator v1.0.0
            Date : %s
            """.formatted(
            config.getTargetProjectName(),
            config.getTargetProjectDescription(),
            config.getTargetProjectName(),
            config.getTargetPackageBase().replace('.', '/'),
            config.getJavaVersion(),
            config.getDatabaseType(),
            config.getTargetProjectName(),
            config.getTargetProjectVersion(),
            config.getCobolSourcesDirectory(),
            java.time.LocalDateTime.now().format(
                java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
        );

        Files.writeString(projectPath.resolve("README.md"), readme);
        logger.info("Generated README.md");
    }

    /**
     * Genere le fichier .gitignore.
     */
    private void generateGitignore(Path projectPath) throws IOException {
        String gitignore = """
            # Maven
            target/
            pom.xml.tag
            pom.xml.releaseBackup
            pom.xml.versionsBackup
            pom.xml.next
            release.properties
            dependency-reduced-pom.xml
            buildNumber.properties
            .mvn/timing.properties

            # IDE
            .idea/
            *.iml
            .vscode/
            *.swp
            *.swo
            *~

            # Eclipse
            .project
            .classpath
            .settings/

            # Logs
            *.log
            logs/

            # OS
            .DS_Store
            Thumbs.db

            # Spring Boot
            spring-boot-*.log

            # Data files (si sensibles)
            # data/input/*.dat
            # data/output/*.dat
            """;

        Files.writeString(projectPath.resolve(".gitignore"), gitignore);
        logger.info("Generated .gitignore");
    }

    /**
     * Genere les scripts de build.
     */
    private void generateBuildScripts(Path projectPath) throws IOException {
        // build.sh (Linux/Mac)
        String buildSh = """
            #!/bin/bash
            set -e

            echo "Building %s..."

            mvn clean package

            echo "Build complete!"
            echo "Run with: java -jar target/%s-%s.jar"
            """.formatted(
            config.getTargetProjectName(),
            config.getTargetProjectName(),
            config.getTargetProjectVersion()
        );

        Path buildShPath = projectPath.resolve("build.sh");
        Files.writeString(buildShPath, buildSh);
        buildShPath.toFile().setExecutable(true);

        // build.bat (Windows)
        String buildBat = """
            @echo off
            echo Building %s...

            mvn clean package

            echo Build complete!
            echo Run with: java -jar target\\%s-%s.jar
            """.formatted(
            config.getTargetProjectName(),
            config.getTargetProjectName(),
            config.getTargetProjectVersion()
        );

        Files.writeString(projectPath.resolve("build.bat"), buildBat);

        logger.info("Generated build scripts");
    }

    /**
     * Genere le Dockerfile.
     */
    private void generateDockerfile(Path projectPath) throws IOException {
        String dockerfile = """
            FROM openjdk:%s-jdk-slim

            WORKDIR /app

            COPY target/%s-%s.jar app.jar

            EXPOSE 8080

            ENTRYPOINT ["java", "-jar", "app.jar"]
            """.formatted(
            config.getJavaVersion(),
            config.getTargetProjectName(),
            config.getTargetProjectVersion()
        );

        Files.writeString(projectPath.resolve("Dockerfile"), dockerfile);
        logger.info("Generated Dockerfile");
    }
}
