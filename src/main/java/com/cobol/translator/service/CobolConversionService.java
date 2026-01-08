package com.cobol.translator.service;

import com.cobol.translator.CobolTranslator;
import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.config.TranslatorConfiguration;
import com.cobol.translator.result.TranslationResult;
import com.cobol.translator.report.ConversionReport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.ArrayList;
import java.util.Properties;

/**
 * Service for converting COBOL files to Spring Batch Java project
 * Uses the full CobolTranslator to ensure same results as CLI
 */
@Service
public class CobolConversionService {

    private static final Logger logger = LoggerFactory.getLogger(CobolConversionService.class);

    @Value("${cobol.translator.temp.output-dir}")
    private String outputTempDir;

    /**
     * Convert COBOL files to a complete Spring Batch project
     * Now uses the full CobolTranslator to ensure same results as CLI
     *
     * @param cobolFiles List of COBOL source files
     * @param projectName Name of the target project
     * @param basePackage Base package for generated Java classes
     * @return ConversionResult with project path and conversion report
     */
    public ConversionResult convertToSpringBatchProject(List<Path> cobolFiles,
                                            String projectName,
                                            String basePackage) throws IOException {

        logger.info("Starting conversion - Project: {}, Package: {}, Files: {}",
                    projectName, basePackage, cobolFiles.size());

        // Create output directory structure using configured path
        // IMPORTANT: Don't create a timestamp subdirectory - use projectName directly
        Path baseOutputDir = Paths.get(outputTempDir);
        if (!Files.exists(baseOutputDir)) {
            Files.createDirectories(baseOutputDir);
            logger.info("Created output base directory: {}", baseOutputDir);
        }

        // Use a simpler name without timestamp to avoid confusion
        Path projectDir = baseOutputDir.resolve(projectName);
        Files.createDirectories(projectDir);

        // Create custom properties for this web request to match CLI behavior
        Properties webProperties = createWebTranslatorProperties(baseOutputDir, projectName, basePackage);

        // Save properties to temp file for CobolTranslator to load
        Path tempPropertiesFile = baseOutputDir.resolve("translator-web-temp-" + projectName + ".properties");
        try (FileOutputStream fos = new FileOutputStream(tempPropertiesFile.toFile())) {
            webProperties.store(fos, "Temporary configuration for web request");
        }

        // Load translator with custom configuration
        TranslatorConfiguration customConfig = TranslatorConfiguration.load(tempPropertiesFile.toString());
        CobolTranslator customTranslator = new CobolTranslator(customConfig);

        try {
            // Convert each COBOL file using the full translator
            List<TranslationResult> results = new ArrayList<>();
            List<ConversionResult.FileConversionReport> allReports = new ArrayList<>();
            for (Path cobolFile : cobolFiles) {
                try {
                    logger.info("Converting COBOL file: {}", cobolFile.getFileName());

                    // Create translation configuration for this file
                    TranslationConfig config = TranslationConfig.builder()
                            .sourceFile(cobolFile.toString())
                            .outputPackage(basePackage)
                            .targetDirectory(projectDir.toString())
                            .generateTests(true)
                            .generateDocs(true)
                            .generateReport(true)
                            .build();

                    // Use the full translator - same as CLI!
                    TranslationResult result = customTranslator.translate(config);
                    results.add(result);
                    
                    // Collect report for this file
                    if (result.getConversionReport() != null) {
                        allReports.add(new ConversionResult.FileConversionReport(
                            cobolFile.getFileName().toString(),
                            result.getConversionReport()
                        ));
                    }

                    if (!result.isSuccess()) {
                        logger.error("Translation failed for: {} - {}", cobolFile.getFileName(), result.getErrorMessage());
                        throw new IOException("Failed to convert " + cobolFile.getFileName() + ": " + result.getErrorMessage());
                    }

                    logger.info("Successfully converted: {} ({} files generated)",
                        cobolFile.getFileName(),
                        result.getGeneratedFiles().size());

                } catch (Exception e) {
                    logger.error("Error converting file: {}", cobolFile.getFileName(), e);
                    throw new IOException("Failed to convert " + cobolFile.getFileName() + ": " + e.getMessage(), e);
                }
            }

            // The actual project directory created by CobolTranslator
            Path actualProjectDir = customConfig.getTargetProjectPath();

            logger.info("Conversion completed successfully.");
            logger.info("Configured project directory: {}", projectDir);
            logger.info("Actual project directory: {}", actualProjectDir);
            logger.info("Total files generated: {}", results.stream().mapToInt(r -> r.getGeneratedFiles().size()).sum());

            // Return the actual directory where files were created along with the report
            Path finalProjectDir;
            if (Files.exists(actualProjectDir) && Files.list(actualProjectDir).findAny().isPresent()) {
                logger.info("Using actual project directory with files: {}", actualProjectDir);
                finalProjectDir = actualProjectDir;
            } else {
                logger.info("Using configured project directory: {}", projectDir);
                finalProjectDir = projectDir;
            }
            
            return new ConversionResult(finalProjectDir, allReports);

        } finally {
            // Clean up temp properties file
            try {
                Files.deleteIfExists(tempPropertiesFile);
            } catch (IOException e) {
                logger.warn("Could not delete temp properties file: {}", tempPropertiesFile);
            }
        }
    }

    /**
     * Create translator properties for web requests to match CLI behavior
     */
    private Properties createWebTranslatorProperties(Path projectDir, String projectName, String basePackage) {
        Properties props = new Properties();

        // Project Configuration
        props.setProperty("target.project.name", projectName);
        props.setProperty("target.projects.directory", projectDir.getParent().toString());
        props.setProperty("target.project.groupId", basePackage);
        props.setProperty("target.project.version", "1.0.0-SNAPSHOT");
        props.setProperty("target.project.description", "Spring Batch project generated from COBOL via Web Interface");

        // Package Configuration
        props.setProperty("target.package.base", basePackage);
        props.setProperty("target.package.model", "model");
        props.setProperty("target.package.processor", "processor");
        props.setProperty("target.package.config", "config");

        // Spring Boot versions
        props.setProperty("spring.boot.version", "3.2.0");
        props.setProperty("spring.batch.version", "5.1.0");
        props.setProperty("java.version", "17");

        // Enable all features to match CLI
        props.setProperty("generate.tests", "true");
        props.setProperty("generate.docs", "true");
        props.setProperty("generate.report", "true");
        props.setProperty("generate.readme", "true");
        props.setProperty("generate.gitignore", "true");
        props.setProperty("generate.spring.config", "true");
        props.setProperty("copy.cobol.sources", "true");

        return props;
    }

    /**
     * Generate pom.xml for the Spring Batch project
     * @deprecated This method is no longer used - ProjectGenerator handles this now
     */
    @Deprecated
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
     * @deprecated This method is no longer used - ProjectGenerator handles this now
     */
    @Deprecated
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
     * @deprecated This method is no longer used - ProjectGenerator handles this now
     */
    @Deprecated
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
     * @deprecated This method is no longer used - ProjectGenerator handles this now
     */
    @Deprecated
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
     * @deprecated This method is no longer used - ProjectGenerator handles this now
     */
    @Deprecated
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
     * Now uses the full CobolTranslator to ensure same results as CLI
     *
     * @param cobolFiles List of COBOL source files
     * @param jclFile Optional JCL file for job configuration
     * @param projectName Name of the target project
     * @param basePackage Base package for generated Java classes
     * @return ConversionResult with project path and conversion report
     */
    public ConversionResult convertWithJCL(List<Path> cobolFiles,
                               Path jclFile,
                               String projectName,
                               String basePackage) throws IOException {

        logger.info("Starting conversion with JCL - Project: {}, Package: {}, COBOL Files: {}, JCL: {}",
                    projectName, basePackage, cobolFiles.size(), jclFile != null ? jclFile.getFileName() : "none");

        // For now, JCL processing is handled by CobolTranslator automatically
        // If a JCL file exists with the same name as COBOL file, it will be detected and used
        // So we just need to ensure the JCL file is in the same directory as COBOL files

        // Create temp directory and copy JCL there if needed
        if (jclFile != null && cobolFiles.size() > 0) {
            Path cobolDir = cobolFiles.get(0).getParent();
            Path jclTarget = cobolDir.resolve(jclFile.getFileName());
            if (!jclFile.equals(jclTarget)) {
                Files.copy(jclFile, jclTarget, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                logger.info("Copied JCL file to COBOL directory for auto-detection: {}", jclTarget);
            }
        }

        // Use the standard conversion which will auto-detect and use JCL
        return convertToSpringBatchProject(cobolFiles, projectName, basePackage);
    }

    /**
     * Create output directory using the configured base path
     * @deprecated This method is no longer used - directories are created inline in convertToSpringBatchProject
     */
    @Deprecated
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
