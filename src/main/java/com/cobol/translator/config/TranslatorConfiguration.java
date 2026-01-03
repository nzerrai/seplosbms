package com.cobol.translator.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.RoundingMode;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;

/**
 * Configuration du traducteur COBOL vers Java chargee depuis translator.properties.
 *
 * Cette classe gere la separation entre :
 * - Le projet du convertisseur (cet outil)
 * - Les projets Java generes (resultats de conversion)
 */
public class TranslatorConfiguration {

    private static final Logger logger = LoggerFactory.getLogger(TranslatorConfiguration.class);
    private static final String DEFAULT_CONFIG_FILE = "translator.properties";

    private final Properties properties;

    // Project Configuration
    private final String targetProjectName;
    private final Path targetProjectsDirectory;
    private final String targetProjectGroupId;
    private final String targetProjectVersion;
    private final String targetProjectDescription;

    // Package Configuration
    private final String targetPackageBase;
    private final String targetPackageModel;
    private final String targetPackageProcessor;
    private final String targetPackageConfig;
    private final String targetPackageTasklet;
    private final String targetPackageListener;

    // Naming
    private final String namingEntitySuffix;
    private final String namingProcessorSuffix;
    private final String namingJobSuffix;
    private final String namingTaskletSuffix;
    private final CodeStyle codeStyle;

    // Spring Boot
    private final String springBootVersion;
    private final String springBatchVersion;
    private final String javaVersion;

    // Generation Options
    private final boolean generateTests;
    private final boolean generateDocs;
    private final boolean generateReport;
    private final boolean generateReadme;
    private final boolean generateGitignore;
    private final boolean generateSpringConfig;
    private final boolean generateDockerfile;
    private final boolean generateBuildScripts;

    // Database
    private final String databaseType;
    private final String databaseDriver;
    private final String databaseUrl;
    private final String databaseUsername;
    private final String databasePassword;

    // Batch Processing
    private final int batchChunkSize;
    private final int batchThreadPoolSize;
    private final int batchSkipLimit;

    // File Processing
    private final String fileEncoding;
    private final String fileInputDirectory;
    private final String fileOutputDirectory;
    private final String fileArchiveDirectory;

    // Logging
    private final String translatorLogLevel;
    private final String targetLogLevel;
    private final String logOutput;

    // Conversion Rules
    private final boolean convertDatesToLocalDate;
    private final int datePivotYear;
    private final boolean useBigDecimalForFinancials;
    private final int bigDecimalDefaultScale;
    private final RoundingMode bigDecimalRoundingMode;

    // Advanced Options
    private final boolean copyCobolSources;
    private final String cobolSourcesDirectory;
    private final boolean includeCobolComments;
    private final boolean optimizeGeneratedCode;
    private final boolean generateInterfaces;

    // Report
    private final String reportFormat;
    private final boolean reportIncludeSource;
    private final String reportDetailLevel;

    // Maven
    private final boolean mavenIncludeTestDependencies;
    private final boolean mavenIncludeDevtools;
    private final boolean mavenIncludeLombok;
    private final boolean mavenIncludeMapstruct;

    private TranslatorConfiguration(Properties properties) {
        this.properties = properties;

        // Project Configuration
        this.targetProjectName = getProperty("target.project.name", "generated-batch-project");
        this.targetProjectsDirectory = Paths.get(getProperty("target.projects.directory", "../generated-projects"));
        this.targetProjectGroupId = getProperty("target.project.groupId", "com.generated.batch");
        this.targetProjectVersion = getProperty("target.project.version", "1.0.0-SNAPSHOT");
        this.targetProjectDescription = getProperty("target.project.description",
            "Batch processing application migrated from COBOL");

        // Package Configuration
        this.targetPackageBase = getProperty("target.package.base", "com.generated.batch");
        this.targetPackageModel = getProperty("target.package.model", "model");
        this.targetPackageProcessor = getProperty("target.package.processor", "processor");
        this.targetPackageConfig = getProperty("target.package.config", "config");
        this.targetPackageTasklet = getProperty("target.package.tasklet", "tasklet");
        this.targetPackageListener = getProperty("target.package.listener", "listener");

        // Naming
        this.namingEntitySuffix = getProperty("naming.entity.suffix", "Record");
        this.namingProcessorSuffix = getProperty("naming.processor.suffix", "Processor");
        this.namingJobSuffix = getProperty("naming.job.suffix", "Job");
        this.namingTaskletSuffix = getProperty("naming.tasklet.suffix", "Tasklet");
        this.codeStyle = CodeStyle.valueOf(getProperty("code.style", "SPRING").toUpperCase());

        // Spring Boot
        this.springBootVersion = getProperty("spring.boot.version", "3.2.0");
        this.springBatchVersion = getProperty("spring.batch.version", "5.1.0");
        this.javaVersion = getProperty("java.version", "17");

        // Generation Options
        this.generateTests = getBooleanProperty("generate.tests", true);
        this.generateDocs = getBooleanProperty("generate.docs", true);
        this.generateReport = getBooleanProperty("generate.report", true);
        this.generateReadme = getBooleanProperty("generate.readme", true);
        this.generateGitignore = getBooleanProperty("generate.gitignore", true);
        this.generateSpringConfig = getBooleanProperty("generate.spring.config", true);
        this.generateDockerfile = getBooleanProperty("generate.dockerfile", false);
        this.generateBuildScripts = getBooleanProperty("generate.build.scripts", true);

        // Database
        this.databaseType = getProperty("database.type", "POSTGRESQL");
        this.databaseDriver = getProperty("database.driver", "org.postgresql.Driver");
        this.databaseUrl = getProperty("database.url", "jdbc:postgresql://localhost:5432/batch_db");
        this.databaseUsername = getProperty("database.username", "batch_user");
        this.databasePassword = getProperty("database.password", "changeme");

        // Batch Processing
        this.batchChunkSize = getIntProperty("batch.chunk.size", 100);
        this.batchThreadPoolSize = getIntProperty("batch.thread.pool.size", 4);
        this.batchSkipLimit = getIntProperty("batch.skip.limit", 10);

        // File Processing
        this.fileEncoding = getProperty("file.encoding", "UTF-8");
        this.fileInputDirectory = getProperty("file.input.directory", "data/input");
        this.fileOutputDirectory = getProperty("file.output.directory", "data/output");
        this.fileArchiveDirectory = getProperty("file.archive.directory", "data/archive");

        // Logging
        this.translatorLogLevel = getProperty("translator.log.level", "INFO");
        this.targetLogLevel = getProperty("target.log.level", "INFO");
        this.logOutput = getProperty("log.output", "BOTH");

        // Conversion Rules
        this.convertDatesToLocalDate = getBooleanProperty("convert.dates.to.localdate", true);
        this.datePivotYear = getIntProperty("date.pivot.year", 50);
        this.useBigDecimalForFinancials = getBooleanProperty("use.bigdecimal.for.financials", true);
        this.bigDecimalDefaultScale = getIntProperty("bigdecimal.default.scale", 2);
        this.bigDecimalRoundingMode = RoundingMode.valueOf(
            getProperty("bigdecimal.rounding.mode", "HALF_UP"));

        // Advanced Options
        this.copyCobolSources = getBooleanProperty("copy.cobol.sources", true);
        this.cobolSourcesDirectory = getProperty("cobol.sources.directory", "src/main/resources/cobol-original");
        this.includeCobolComments = getBooleanProperty("include.cobol.comments", true);
        this.optimizeGeneratedCode = getBooleanProperty("optimize.generated.code", false);
        this.generateInterfaces = getBooleanProperty("generate.interfaces", true);

        // Report
        this.reportFormat = getProperty("report.format", "TXT");
        this.reportIncludeSource = getBooleanProperty("report.include.source", true);
        this.reportDetailLevel = getProperty("report.detail.level", "DETAILED");

        // Maven
        this.mavenIncludeTestDependencies = getBooleanProperty("maven.include.test.dependencies", true);
        this.mavenIncludeDevtools = getBooleanProperty("maven.include.devtools", true);
        this.mavenIncludeLombok = getBooleanProperty("maven.include.lombok", true);
        this.mavenIncludeMapstruct = getBooleanProperty("maven.include.mapstruct", false);
    }

    /**
     * Charge la configuration depuis le fichier par defaut (translator.properties).
     */
    public static TranslatorConfiguration load() throws IOException {
        return load(DEFAULT_CONFIG_FILE);
    }

    /**
     * Charge la configuration depuis un fichier specifique.
     */
    public static TranslatorConfiguration load(String configFile) throws IOException {
        Properties props = new Properties();

        try (InputStream input = new FileInputStream(configFile)) {
            props.load(input);
            logger.info("Configuration loaded from: {}", configFile);
        } catch (IOException e) {
            logger.warn("Could not load config file: {}. Using defaults.", configFile);
            // Continue with empty properties (defaults will be used)
        }

        return new TranslatorConfiguration(props);
    }

    /**
     * Retourne le chemin complet du projet cible.
     */
    public Path getTargetProjectPath() {
        return targetProjectsDirectory.resolve(targetProjectName);
    }

    /**
     * Retourne le package complet pour un sous-package.
     */
    public String getFullPackage(String subPackage) {
        return targetPackageBase + "." + subPackage;
    }

    public String getModelPackage() {
        return getFullPackage(targetPackageModel);
    }

    public String getProcessorPackage() {
        return getFullPackage(targetPackageProcessor);
    }

    public String getConfigPackage() {
        return getFullPackage(targetPackageConfig);
    }

    public String getTaskletPackage() {
        return getFullPackage(targetPackageTasklet);
    }

    public String getListenerPackage() {
        return getFullPackage(targetPackageListener);
    }

    // Helper methods
    private String getProperty(String key, String defaultValue) {
        return properties.getProperty(key, defaultValue);
    }

    private boolean getBooleanProperty(String key, boolean defaultValue) {
        String value = properties.getProperty(key);
        return value != null ? Boolean.parseBoolean(value) : defaultValue;
    }

    private int getIntProperty(String key, int defaultValue) {
        String value = properties.getProperty(key);
        if (value != null) {
            try {
                return Integer.parseInt(value);
            } catch (NumberFormatException e) {
                logger.warn("Invalid integer value for {}: {}. Using default: {}",
                    key, value, defaultValue);
            }
        }
        return defaultValue;
    }

    // Getters
    public String getTargetProjectName() { return targetProjectName; }
    public Path getTargetProjectsDirectory() { return targetProjectsDirectory; }
    public String getTargetProjectGroupId() { return targetProjectGroupId; }
    public String getTargetProjectVersion() { return targetProjectVersion; }
    public String getTargetProjectDescription() { return targetProjectDescription; }
    public String getTargetPackageBase() { return targetPackageBase; }
    public String getTargetPackageModel() { return targetPackageModel; }
    public String getTargetPackageProcessor() { return targetPackageProcessor; }
    public String getTargetPackageConfig() { return targetPackageConfig; }
    public String getTargetPackageTasklet() { return targetPackageTasklet; }
    public String getTargetPackageListener() { return targetPackageListener; }
    public String getNamingEntitySuffix() { return namingEntitySuffix; }
    public String getNamingProcessorSuffix() { return namingProcessorSuffix; }
    public String getNamingJobSuffix() { return namingJobSuffix; }
    public String getNamingTaskletSuffix() { return namingTaskletSuffix; }
    public CodeStyle getCodeStyle() { return codeStyle; }
    public String getSpringBootVersion() { return springBootVersion; }
    public String getSpringBatchVersion() { return springBatchVersion; }
    public String getJavaVersion() { return javaVersion; }
    public boolean isGenerateTests() { return generateTests; }
    public boolean isGenerateDocs() { return generateDocs; }
    public boolean isGenerateReport() { return generateReport; }
    public boolean isGenerateReadme() { return generateReadme; }
    public boolean isGenerateGitignore() { return generateGitignore; }
    public boolean isGenerateSpringConfig() { return generateSpringConfig; }
    public boolean isGenerateDockerfile() { return generateDockerfile; }
    public boolean isGenerateBuildScripts() { return generateBuildScripts; }
    public String getDatabaseType() { return databaseType; }
    public String getDatabaseDriver() { return databaseDriver; }
    public String getDatabaseUrl() { return databaseUrl; }
    public String getDatabaseUsername() { return databaseUsername; }
    public String getDatabasePassword() { return databasePassword; }
    public int getBatchChunkSize() { return batchChunkSize; }
    public int getBatchThreadPoolSize() { return batchThreadPoolSize; }
    public int getBatchSkipLimit() { return batchSkipLimit; }
    public String getFileEncoding() { return fileEncoding; }
    public String getFileInputDirectory() { return fileInputDirectory; }
    public String getFileOutputDirectory() { return fileOutputDirectory; }
    public String getFileArchiveDirectory() { return fileArchiveDirectory; }
    public String getTranslatorLogLevel() { return translatorLogLevel; }
    public String getTargetLogLevel() { return targetLogLevel; }
    public String getLogOutput() { return logOutput; }
    public boolean isConvertDatesToLocalDate() { return convertDatesToLocalDate; }
    public int getDatePivotYear() { return datePivotYear; }
    public boolean isUseBigDecimalForFinancials() { return useBigDecimalForFinancials; }
    public int getBigDecimalDefaultScale() { return bigDecimalDefaultScale; }
    public RoundingMode getBigDecimalRoundingMode() { return bigDecimalRoundingMode; }
    public boolean isCopyCobolSources() { return copyCobolSources; }
    public String getCobolSourcesDirectory() { return cobolSourcesDirectory; }
    public boolean isIncludeCobolComments() { return includeCobolComments; }
    public boolean isOptimizeGeneratedCode() { return optimizeGeneratedCode; }
    public boolean isGenerateInterfaces() { return generateInterfaces; }
    public String getReportFormat() { return reportFormat; }
    public boolean isReportIncludeSource() { return reportIncludeSource; }
    public String getReportDetailLevel() { return reportDetailLevel; }
    public boolean isMavenIncludeTestDependencies() { return mavenIncludeTestDependencies; }
    public boolean isMavenIncludeDevtools() { return mavenIncludeDevtools; }
    public boolean isMavenIncludeLombok() { return mavenIncludeLombok; }
    public boolean isMavenIncludeMapstruct() { return mavenIncludeMapstruct; }

    public enum CodeStyle {
        GOOGLE,
        ORACLE,
        SPRING
    }

    @Override
    public String toString() {
        return String.format("TranslatorConfiguration[project=%s, location=%s, package=%s]",
            targetProjectName, getTargetProjectPath(), targetPackageBase);
    }
}
