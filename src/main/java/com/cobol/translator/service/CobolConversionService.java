package com.cobol.translator.service;

import com.cobol.translator.parser.CobolParser;
import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.generator.JobConfigGenerator;
import com.cobol.translator.generator.EntityGenerator;
import com.cobol.translator.generator.ProcessorGenerator;
import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.jcl.parser.JCLParser;
import com.cobol.translator.jcl.model.JCLJob;
import com.cobol.translator.jcl.generator.JCLSpringBatchGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.ArrayList;

/**
 * Service for converting COBOL files to Spring Batch Java project
 */
@Service
public class CobolConversionService {

    private static final Logger logger = LoggerFactory.getLogger(CobolConversionService.class);

    @Value("${cobol.translator.temp.output-dir}")
    private String outputTempDir;

    /**
     * Convert COBOL files to a complete Spring Batch project
     *
     * @param cobolFiles List of COBOL source files
     * @param projectName Name of the target project
     * @param basePackage Base package for generated Java classes
     * @return Path to the generated project directory
     */
    public Path convertToSpringBatchProject(List<Path> cobolFiles,
                                            String projectName,
                                            String basePackage) throws IOException {

        logger.info("Starting conversion - Project: {}, Package: {}, Files: {}",
                    projectName, basePackage, cobolFiles.size());

        // Create output directory structure using configured path
        Path projectDir = createOutputDirectory(projectName);
        Path srcMainJava = projectDir.resolve("src/main/java");
        Path srcMainResources = projectDir.resolve("src/main/resources");
        Path srcTestJava = projectDir.resolve("src/test/java");

        Files.createDirectories(srcMainJava);
        Files.createDirectories(srcMainResources);
        Files.createDirectories(srcTestJava);

        // Convert base package to directory structure
        String packagePath = basePackage.replace('.', '/');
        Path packageDir = srcMainJava.resolve(packagePath);
        Files.createDirectories(packageDir);

        // Create sub-packages for organization
        Path batchDir = packageDir.resolve("batch");
        Path modelDir = packageDir.resolve("model");
        Path configDir = packageDir.resolve("config");

        Files.createDirectories(batchDir);
        Files.createDirectories(modelDir);
        Files.createDirectories(configDir);

        // Initialize generators
        CobolParser parser = new CobolParser();
        JobConfigGenerator jobGenerator = new JobConfigGenerator();
        EntityGenerator entityGenerator = new EntityGenerator();
        ProcessorGenerator processorGenerator = new ProcessorGenerator();

        for (Path cobolFile : cobolFiles) {
            try {
                logger.info("Converting COBOL file: {}", cobolFile.getFileName());

                // Parse COBOL file
                String cobolSource = Files.readString(cobolFile);
                CobolProgram program = parser.parse(cobolSource);

                String programName = program.getProgramName();
                if (programName == null || programName.isEmpty()) {
                    programName = cobolFile.getFileName().toString().replace(".cob", "").replace(".cbl", "");
                }

                // Create translation configuration for this file
                TranslationConfig config = TranslationConfig.builder()
                        .sourceFile(cobolFile.toString())
                        .outputPackage(basePackage + ".batch")
                        .targetDirectory(batchDir.toString())
                        .generateTests(false)
                        .generateDocs(false)
                        .generateReport(false)
                        .build();

                // Generate Spring Batch Job configuration
                jobGenerator.generate(program, config, batchDir);
                logger.info("Generated Job config for: {}", programName);

                // Generate Entity class if working storage exists
                if (!program.getWorkingStorageItems().isEmpty()) {
                    TranslationConfig modelConfig = config.toBuilder()
                            .outputPackage(basePackage + ".model")
                            .targetDirectory(modelDir.toString())
                            .build();
                    entityGenerator.generate(program, modelConfig, modelDir);
                    logger.info("Generated Entity for: {}", programName);
                }

                // Generate Item Processor
                processorGenerator.generate(program, config, batchDir);
                logger.info("Generated Processor for: {}", programName);

            } catch (Exception e) {
                logger.error("Error converting file: {}", cobolFile.getFileName(), e);
                throw new IOException("Failed to convert " + cobolFile.getFileName() + ": " + e.getMessage(), e);
            }
        }

        // Generate Spring Boot configuration files
        generatePomXml(projectDir, projectName, basePackage);
        generateApplicationProperties(srcMainResources);
        generateBatchConfiguration(configDir, basePackage + ".config");
        generateMainApplication(packageDir, basePackage, projectName);
        generateReadme(projectDir, projectName);

        logger.info("Conversion completed successfully. Project directory: {}", projectDir);

        return projectDir;
    }

    /**
     * Generate pom.xml for the Spring Batch project
     */
    private void generatePomXml(Path projectDir, String projectName, String basePackage) throws IOException {
        String artifactId = projectName.toLowerCase().replace(" ", "-");
        String groupId = basePackage.substring(0, basePackage.lastIndexOf('.'));

        String pomContent = """
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>3.2.0</version>
        <relativePath/>
    </parent>

    <groupId>%s</groupId>
    <artifactId>%s</artifactId>
    <version>1.0.0-SNAPSHOT</version>
    <name>%s</name>
    <description>Spring Batch project generated from COBOL</description>

    <properties>
        <java.version>17</java.version>
    </properties>

    <dependencies>
        <!-- Spring Batch -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-batch</artifactId>
        </dependency>

        <!-- Spring Boot Web (for monitoring) -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>

        <!-- H2 Database -->
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <scope>runtime</scope>
        </dependency>

        <!-- Lombok (optional) -->
        <dependency>
            <groupId>org.projectlombok</groupId>
            <artifactId>lombok</artifactId>
            <optional>true</optional>
        </dependency>

        <!-- Test -->
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
""".formatted(groupId, artifactId, projectName);

        Files.writeString(projectDir.resolve("pom.xml"), pomContent);
    }

    /**
     * Generate application.properties
     */
    private void generateApplicationProperties(Path resourcesDir) throws IOException {
        String content = """
# Application Configuration
spring.application.name=cobol-batch-conversion

# H2 Database Configuration
spring.datasource.url=jdbc:h2:mem:batchdb
spring.datasource.driverClassName=org.h2.Driver
spring.datasource.username=sa
spring.datasource.password=

# H2 Console
spring.h2.console.enabled=true
spring.h2.console.path=/h2-console

# Batch Configuration
spring.batch.jdbc.initialize-schema=always
spring.batch.job.enabled=true

# Server Port
server.port=8080

# Logging
logging.level.org.springframework.batch=INFO
""";

        Files.writeString(resourcesDir.resolve("application.properties"), content);
    }

    /**
     * Generate Spring Batch configuration class
     */
    private void generateBatchConfiguration(Path configDir, String packageName) throws IOException {
        String content = """
package %s;

import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.context.annotation.Configuration;

/**
 * Spring Batch Configuration
 */
@Configuration
@EnableBatchProcessing
public class BatchConfiguration {

    // Additional batch configuration can be added here

}
""".formatted(packageName);

        Files.writeString(configDir.resolve("BatchConfiguration.java"), content);
    }

    /**
     * Generate main Spring Boot application class
     */
    private void generateMainApplication(Path packageDir, String basePackage, String projectName) throws IOException {
        String className = toPascalCase(projectName) + "Application";

        String content = """
package %s;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Main application class for %s
 * Generated from COBOL source files
 */
@SpringBootApplication
public class %s {

    public static void main(String[] args) {
        SpringApplication.run(%s.class, args);
    }
}
""".formatted(basePackage, projectName, className, className);

        Files.writeString(packageDir.resolve(className + ".java"), content);
    }

    /**
     * Generate README.md
     */
    private void generateReadme(Path projectDir, String projectName) throws IOException {
        String content = """
# %s

This Spring Batch project was automatically generated from COBOL source files.

## Building the Project

```bash
mvn clean package
```

## Running the Application

```bash
mvn spring-boot:run
```

Or run the JAR file:

```bash
java -jar target/*.jar
```

## H2 Console

Access the H2 database console at: http://localhost:8080/h2-console

- JDBC URL: jdbc:h2:mem:batchdb
- Username: sa
- Password: (empty)

## Project Structure

- `src/main/java/.../batch/` - Spring Batch job implementations
- `src/main/java/.../model/` - Data model classes
- `src/main/java/.../config/` - Spring configuration classes

## Generated from COBOL

This project maintains the business logic from the original COBOL programs
while implementing them as modern Spring Batch jobs.
""".formatted(projectName);

        Files.writeString(projectDir.resolve("README.md"), content);
    }

    /**
     * Convert string to PascalCase
     */
    private String toPascalCase(String input) {
        String[] parts = input.split("[-\\s_]+");
        StringBuilder result = new StringBuilder();

        for (String part : parts) {
            if (!part.isEmpty()) {
                result.append(part.substring(0, 1).toUpperCase());
                result.append(part.substring(1).toLowerCase());
            }
        }

        return result.toString();
    }

    /**
     * Convert COBOL files with JCL to a complete Spring Batch project
     * This method uses JCL to automatically configure readers, writers, and job flow
     *
     * @param cobolFiles List of COBOL source files
     * @param jclFile Optional JCL file for job configuration
     * @param projectName Name of the target project
     * @param basePackage Base package for generated Java classes
     * @return Path to the generated project directory
     */
    public Path convertWithJCL(List<Path> cobolFiles,
                               Path jclFile,
                               String projectName,
                               String basePackage) throws IOException {

        logger.info("Starting conversion with JCL - Project: {}, Package: {}, COBOL Files: {}, JCL: {}",
                    projectName, basePackage, cobolFiles.size(), jclFile != null ? jclFile.getFileName() : "none");

        // Create base project structure using configured path
        Path projectDir = createOutputDirectory(projectName);
        Path srcMainJava = projectDir.resolve("src/main/java");
        Path srcMainResources = projectDir.resolve("src/main/resources");

        Files.createDirectories(srcMainJava);
        Files.createDirectories(srcMainResources);

        String packagePath = basePackage.replace('.', '/');
        Path packageDir = srcMainJava.resolve(packagePath);
        Files.createDirectories(packageDir);

        // Parse and generate from COBOL files first
        CobolParser cobolParser = new CobolParser();
        EntityGenerator entityGenerator = new EntityGenerator();

        for (Path cobolFile : cobolFiles) {
            try {
                logger.info("Processing COBOL file: {}", cobolFile.getFileName());

                String cobolSource = Files.readString(cobolFile);
                CobolProgram program = cobolParser.parse(cobolSource);

                TranslationConfig config = TranslationConfig.builder()
                        .sourceFile(cobolFile.toString())
                        .outputPackage(basePackage + ".model")
                        .targetDirectory(packageDir.resolve("model").toString())
                        .generateTests(false)
                        .generateDocs(false)
                        .generateReport(false)
                        .build();

                // Generate entities from COBOL
                if (!program.getWorkingStorageItems().isEmpty()) {
                    Files.createDirectories(packageDir.resolve("model"));
                    entityGenerator.generate(program, config, packageDir.resolve("model"));
                }

            } catch (Exception e) {
                logger.error("Error processing COBOL file: {}", cobolFile.getFileName(), e);
            }
        }

        // If JCL file provided, use it to generate Spring Batch configuration
        if (jclFile != null) {
            try {
                logger.info("Processing JCL file: {}", jclFile.getFileName());

                JCLParser jclParser = new JCLParser();
                JCLJob jclJob = jclParser.parse(jclFile);

                JCLSpringBatchGenerator jclGenerator = new JCLSpringBatchGenerator(basePackage + ".batch");
                Path batchDir = packageDir.resolve("batch");
                Files.createDirectories(batchDir);

                List<Path> generatedFiles = jclGenerator.generateJobConfiguration(jclJob, srcMainJava);

                logger.info("Generated {} files from JCL", generatedFiles.size());

            } catch (Exception e) {
                logger.error("Error processing JCL file: {}", jclFile.getFileName(), e);
                // Continue without JCL - fall back to standard conversion
            }
        }

        // Generate standard project files
        Path configDir = packageDir.resolve("config");
        Files.createDirectories(configDir);

        generatePomXml(projectDir, projectName, basePackage);
        generateApplicationProperties(srcMainResources);
        generateBatchConfiguration(configDir, basePackage + ".config");
        generateMainApplication(packageDir, basePackage, projectName);
        generateReadme(projectDir, projectName);

        logger.info("Conversion with JCL completed successfully. Project directory: {}", projectDir);

        return projectDir;
    }

    /**
     * Create output directory using the configured base path
     */
    private Path createOutputDirectory(String projectName) throws IOException {
        // Create base directory if it doesn't exist
        Path basePath = Paths.get(outputTempDir);
        if (!Files.exists(basePath)) {
            Files.createDirectories(basePath);
            logger.info("Created output base directory: {}", basePath);
        }

        // Create unique project directory with timestamp
        String timestamp = String.valueOf(System.currentTimeMillis());
        String dirName = projectName + "-" + timestamp;
        Path projectDir = basePath.resolve(dirName);
        Files.createDirectories(projectDir);

        logger.info("Created output directory: {}", projectDir);
        return projectDir;
    }
}
