package com.cobol.translator;

import com.cobol.translator.analyzer.CobolContextAnalyzer;
import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.config.TranslatorConfiguration;
import com.cobol.translator.copybook.CopybookResolver;
import com.cobol.translator.generator.*;
import com.cobol.translator.jcl.generator.JCLSpringBatchGenerator;
import com.cobol.translator.jcl.model.JCLJob;
import com.cobol.translator.jcl.parser.JCLParser;
import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.parser.CobolASTParser;
import com.cobol.translator.parser.CobolParser;
import com.cobol.translator.ast.ProgramNode;
import com.cobol.translator.project.ProjectGenerator;
import com.cobol.translator.report.ConversionReport;
import com.cobol.translator.report.ReportGenerator;
import com.cobol.translator.result.TranslationResult;
import com.cobol.translator.vsam.VsamFileAnalyzer;
import com.cobol.translator.vsam.VsamFileInfo;
import com.cobol.translator.vsam.VsamToJdbcMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Main translator class that orchestrates COBOL to Java Spring Batch translation.
 *
 * Usage:
 * <pre>
 * CobolTranslator translator = new CobolTranslator();
 * TranslationConfig config = TranslationConfig.builder()
 *     .sourceFile("CUSTPROC.cob")
 *     .outputPackage("com.mycompany.batch")
 *     .build();
 * TranslationResult result = translator.translate(config);
 * </pre>
 */
public class CobolTranslator {

    private static final Logger logger = LoggerFactory.getLogger(CobolTranslator.class);

    private final CobolParser parser;
    private final EntityGenerator entityGenerator;
    private final ProcessorGenerator processorGenerator;
    private final JobConfigGenerator jobConfigGenerator;
    private final TestGenerator testGenerator;
    private final TranslatorConfiguration translatorConfig;
    private final CopybookResolver copybookResolver;
    private final VsamFileAnalyzer vsamAnalyzer;
    private final VsamToJdbcMapper vsamMapper;

    /**
     * Constructeur par defaut - charge la configuration depuis translator.properties
     */
    public CobolTranslator() throws IOException {
        this(TranslatorConfiguration.load());
    }

    /**
     * Constructeur avec configuration personnalisee
     */
    public CobolTranslator(TranslatorConfiguration translatorConfig) {
        this.translatorConfig = translatorConfig;
        this.parser = new CobolParser();
        this.entityGenerator = new EntityGenerator();
        this.processorGenerator = new ProcessorGenerator();
        this.jobConfigGenerator = new JobConfigGenerator();
        this.testGenerator = new TestGenerator();
        
        // Initialize copybook resolver
        this.copybookResolver = new CopybookResolver();
        
        // Initialize VSAM support
        this.vsamAnalyzer = new VsamFileAnalyzer();
        this.vsamMapper = new VsamToJdbcMapper(TranslationConfig.builder()
            .outputPackage(translatorConfig.getTargetPackageBase())
            .build());

        logger.info("CobolTranslator initialized with configuration: {}", translatorConfig);
        logger.info("Copybook resolver and VSAM support enabled");
    }

    /**
     * Translates a single COBOL program to Java Spring Batch.
     *0: Resolve copybooks (if any)
            logger.info("Resolving copybooks...");
            String cobolSource = readFile(config.getSourceFile());
            
            // Configure copybook search paths
            Path sourceDir = Paths.get(config.getSourceFile()).getParent();
            if (sourceDir != null) {
                copybookResolver.addSearchPath(sourceDir);
                // Also check for copybooks subdirectory
                Path copybooksDir = sourceDir.resolve("copybooks");
                if (Files.exists(copybooksDir)) {
                    copybookResolver.addSearchPath(copybooksDir);
                }
            }
            
            // Expand copybooks
            cobolSource = copybookResolver.resolveAllCopybooks(cobolSource);
            
            if (!copybookResolver.getResolvedCopybooks().isEmpty()) {
                logger.info("Resolved {} copybooks: {}", 
                    copybookResolver.getResolvedCopybooks().size(),
                    copybookResolver.getResolvedCopybooks());
            }
            
            // Step 1: Parse COBOL program
            logger.info("Parsing COBOL program..."cs
     */
    public TranslationResult translate(TranslationConfig config) {
        logger.info("Starting translation of: {}", config.getSourceFile());
        logger.info("Target project: {}", translatorConfig.getTargetProjectName());
        logger.info("Target location: {}", translatorConfig.getTargetProjectPath());

        TranslationResult.Builder resultBuilder = TranslationResult.builder()
                .sourceFile(config.getSourceFile());

        try {
            // Step 1: Parse COBOL program
            logger.info("Parsing COBOL program...");
            String cobolSource = readFile(config.getSourceFile());
            CobolProgram program = null;

            // First, try the richer ANTLR parser to validate syntax and recover metadata
            ProgramNode ast = null;
            try {
                CobolASTParser astParser = new CobolASTParser();
                ast = astParser.parseString(cobolSource, config.getSourceFile());
                if (ast != null && ast.getProgramName() != null && !ast.getProgramName().isEmpty()) {
                    logger.info("ANTLR parse success - program name: {}", ast.getProgramName());
                }
            } catch (Exception e) {
                logger.warn("ANTLR parse failed, falling back to legacy parser: {}", e.getMessage());
            }

            program = parser.parse(cobolSource);
            if (ast != null && ast.getProgramName() != null && !ast.getProgramName().isEmpty()) {
                // If legacy parser missed the program name, patch it from the AST
                if (program.getProgramName() == null || program.getProgramName().isEmpty()) {
                    program.setProgramName(ast.getProgramName());
                    program.setProgramId(ast.getProgramName());
                }
            }
            program.setSourceFile(config.getSourceFile());
            resultBuilder.cobolProgram(program);

            logger.info("Parsed program: {} with {} data items and {} statements",
                    program.getProgramName(),
                    program.getDataItems().size(),
                    program.getStatements().size());

            // Step 1.5: Perform contextual analysis
            logger.info("Performing contextual analysis...");
            CobolContextAnalyzer analyzer = new CobolContextAnalyzer(program);
            CobolContextAnalyzer.AnalysisResult analysisResult = analyzer.analyze();

            // Log analysis warnings
            for (CobolContextAnalyzer.AnalysisWarning warning : analysisResult.getWarnings()) {
                logger.warn("Analysis warning: {}", warning);
            }

            logger.info("Contextual analysis completed with {} warnings ({} high priority)",
                    analysisResult.getWarnings().size(),
                    analysisResult.getWarningsByLevel(CobolContextAnalyzer.WarningLevel.HIGH).size());

            // Step 2: Generer le projet cible (si pas deja fait)
            logger.info("Ensuring target project exists...");
            Path projectPath = ensureProjectExists();

            // Step 3: Creer la structure de packages dans le projet cible
            Path modelDir = createOutputDirectory(projectPath, "model");
            Path processorDir = createOutputDirectory(projectPath, "processor");
            Path configDir = createOutputDirectory(projectPath, "config");

            // Step 4: Copier les sources COBOL (si configure)
            if (translatorConfig.isCopyCobolSources()) {
                copyCobolSource(config.getSourceFile(), projectPath, program.getProgramName());
            }

            // Step 5: Generate entity classes
            logger.info("Generating entity classes...");
            List<File> entityFiles = entityGenerator.generate(program, config, modelDir);
            resultBuilder.addGeneratedFiles(entityFiles);

            // Step 6: Generate processor
            logger.info("Generating processor...");
            File processorFile = processorGenerator.generate(program, config, processorDir);
            resultBuilder.addGeneratedFile(processorFile);

            // Step 7: Check for corresponding JCL file and translate if found
            Path jclFile = findCorrespondingJCLFile(config.getSourceFile());
            if (jclFile != null && Files.exists(jclFile)) {
                logger.info("Found corresponding JCL file: {}", jclFile.getFileName());
                translateJCL(jclFile, program, config, configDir, resultBuilder);
            }

            // Step 8: Generate Spring Batch job configuration
            logger.info("Generating job configuration...");
            File jobConfigFile = jobConfigGenerator.generate(program, config, configDir);
            resultBuilder.addGeneratedFile(jobConfigFile);

            // Step 9: Generate tests (if enabled)
            if (translatorConfig.isGenerateTests()) {
                logger.info("Generating tests...");
                Path testDir = projectPath.resolve("src/test/java").resolve(translatorConfig.getTargetPackageBase().replace('.', '/'));
                Files.createDirectories(testDir);
                List<File> testFiles = testGenerator.generate(program, config, testDir);
                resultBuilder.addGeneratedFiles(testFiles);
            }

            // Step 9: Generate conversion report
            if (translatorConfig.isGenerateReport()) {
                logger.info("Generating conversion report...");
                ConversionReport report = generateReport(program, projectPath);
                resultBuilder.conversionReport(report);

                logger.info("Conversion rate: {:.1f}%", report.getConversionPercentage());
                logger.info("Confidence level: {}", report.getOverallConfidence().getLabel());
            }

            // Step 10: Calculate metrics
            resultBuilder.metrics(calculateMetrics(program, resultBuilder.build()));

            logger.info("Translation completed successfully!");
            logger.info("Files generated in: {}", projectPath);

            return resultBuilder.success(true).build();

        } catch (Exception e) {
            logger.error("Translation failed: {}", e.getMessage(), e);
            return resultBuilder
                    .success(false)
                    .errorMessage(e.getMessage())
                    .build();
        }
    }

    /**
     * Translates all COBOL programs in a directory.
     *
     * @param inputDir Input directory containing COBOL files
     * @param config Base translation configuration
     * @return List of translation results
     */
    public List<TranslationResult> translateDirectory(String inputDir, TranslationConfig config) {
        logger.info("Translating all COBOL files in: {}", inputDir);

        List<TranslationResult> results = new ArrayList<>();
        File dir = new File(inputDir);

        if (!dir.exists() || !dir.isDirectory()) {
            logger.error("Invalid input directory: {}", inputDir);
            return results;
        }

        File[] cobolFiles = dir.listFiles((d, name) ->
                name.toLowerCase().endsWith(".cob") ||
                name.toLowerCase().endsWith(".cbl") ||
                name.toLowerCase().endsWith(".cobol")
        );

        if (cobolFiles == null || cobolFiles.length == 0) {
            logger.warn("No COBOL files found in: {}", inputDir);
            return results;
        }

        logger.info("Found {} COBOL files", cobolFiles.length);

        for (File cobolFile : cobolFiles) {
            logger.info("Translating: {}", cobolFile.getName());

            TranslationConfig fileConfig = config.toBuilder()
                    .sourceFile(cobolFile.getAbsolutePath())
                    .build();

            TranslationResult result = translate(fileConfig);
            results.add(result);
        }

        return results;
    }

    /**
     * Reads a file into a string.
     */
    private String readFile(String filePath) throws IOException {
        return Files.readString(Paths.get(filePath));
    }

    /**
     * S'assure que le projet cible existe (le cree si necessaire).
     */
    private Path ensureProjectExists() throws IOException {
        Path projectPath = translatorConfig.getTargetProjectPath();

        if (!Files.exists(projectPath)) {
            logger.info("Creating new target project: {}", projectPath);
            ProjectGenerator projectGen = new ProjectGenerator(translatorConfig);
            projectGen.generateProject();
        } else {
            logger.info("Target project already exists: {}", projectPath);
        }

        return projectPath;
    }

    /**
     * Creates output directory structure based on package name in the target project.
     */
    private Path createOutputDirectory(Path projectPath) throws IOException {
        return createOutputDirectory(projectPath, translatorConfig.getTargetPackageModel());
    }

    /**
     * Creates output directory for a specific sub-package.
     */
    private Path createOutputDirectory(Path projectPath, String subPackage) throws IOException {
        // Creer le chemin complet vers les packages Java
        String packagePath = translatorConfig.getTargetPackageBase().replace('.', '/');
        Path outputPath = projectPath.resolve("src/main/java").resolve(packagePath)
            .resolve(subPackage);

        if (!Files.exists(outputPath)) {
            Files.createDirectories(outputPath);
            logger.info("Created output directory: {}", outputPath);
        }

        return outputPath;
    }

    /**
     * Copie le fichier source COBOL dans le projet genere.
     */
    private void copyCobolSource(String sourceFile, Path projectPath, String programName) throws IOException {
        Path cobolDir = projectPath.resolve(translatorConfig.getCobolSourcesDirectory());
        Files.createDirectories(cobolDir);

        Path sourcePath = Paths.get(sourceFile);
        Path targetPath = cobolDir.resolve(sourcePath.getFileName());

        Files.copy(sourcePath, targetPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        logger.info("Copied COBOL source to: {}", targetPath);
    }

    /**
     * Finds the corresponding JCL file for a COBOL source file.
     * Looks for files with same base name but .jcl extension.
     */
    private Path findCorrespondingJCLFile(String cobolSourceFile) {
        Path cobolPath = Paths.get(cobolSourceFile);
        String fileName = cobolPath.getFileName().toString();

        // Remove extension (.cob, .cbl, .cobol)
        String baseName = fileName.replaceAll("\\.(cob|cbl|cobol)$", "");

        // Look for .jcl file in same directory
        Path jclPath = cobolPath.getParent().resolve(baseName + ".jcl");

        if (Files.exists(jclPath)) {
            return jclPath;
        }

        return null;
    }

    /**
     * Translates a JCL file to Spring Batch configuration.
     */
    private void translateJCL(Path jclFile, CobolProgram program, TranslationConfig config,
                             Path outputDir, TranslationResult.Builder resultBuilder) {
        try {
            logger.info("Parsing JCL file: {}", jclFile.getFileName());

            JCLParser jclParser = new JCLParser();
            JCLJob jclJob = jclParser.parse(jclFile);

            logger.info("Parsed JCL job: {} with {} steps",
                jclJob.getJobName(), jclJob.getSteps().size());

            // Generate Spring Batch configuration from JCL
            logger.info("Generating Spring Batch configuration from JCL...");
            String basePackage = config.getOutputPackage();
            JCLSpringBatchGenerator jclGenerator = new JCLSpringBatchGenerator(basePackage);
            List<Path> generatedFiles = jclGenerator.generateJobConfiguration(jclJob, outputDir);

            for (Path generatedFile : generatedFiles) {
                resultBuilder.addGeneratedFile(generatedFile.toFile());
                logger.info("Generated JCL-based file: {}", generatedFile.getFileName());
            }

        } catch (Exception e) {
            logger.error("Error parsing JCL: {}", e.getMessage());
            logger.warn("Continuing without JCL translation");
        }
    }

    /**
     * Calculates translation metrics.
     */
    private TranslationMetrics calculateMetrics(CobolProgram program, TranslationResult partialResult) {
        return TranslationMetrics.builder()
                .cobolLines(program.getTotalLines())
                .javaLines(countJavaLines(partialResult.getGeneratedFiles()))
                .dataItemsTranslated(program.getDataItems().size())
                .statementsTranslated(program.getStatements().size())
                .filesGenerated(partialResult.getGeneratedFiles().size())
                .build();
    }

    /**
     * Counts total lines in generated Java files.
     */
    private int countJavaLines(List<File> files) {
        int total = 0;
        for (File file : files) {
            try {
                total += Files.readAllLines(file.toPath()).size();
            } catch (IOException e) {
                logger.warn("Could not count lines in: {}", file.getName());
            }
        }
        return total;
    }

    /**
     * Generates conversion report and saves it to file.
     */
    private ConversionReport generateReport(CobolProgram program, Path projectPath) throws IOException {
        // Generate report
        ReportGenerator reportGen = new ReportGenerator(program);
        ConversionReport report = reportGen.generate();

        // Save report to docs/ directory
        String reportFileName = program.getProgramName() + "_CONVERSION_REPORT.txt";
        Path docsDir = projectPath.resolve("docs");
        Files.createDirectories(docsDir);
        Path reportPath = docsDir.resolve(reportFileName);
        Files.writeString(reportPath, report.generateTextReport());
        logger.info("Conversion report saved to: {}", reportPath);

        return report;
    }

    /**
     * Translation metrics.
     */
    public static class TranslationMetrics {
        private final int cobolLines;
        private final int javaLines;
        private final int dataItemsTranslated;
        private final int statementsTranslated;
        private final int filesGenerated;

        private TranslationMetrics(Builder builder) {
            this.cobolLines = builder.cobolLines;
            this.javaLines = builder.javaLines;
            this.dataItemsTranslated = builder.dataItemsTranslated;
            this.statementsTranslated = builder.statementsTranslated;
            this.filesGenerated = builder.filesGenerated;
        }

        public static Builder builder() {
            return new Builder();
        }

        // Getters
        public int getCobolLines() { return cobolLines; }
        public int getJavaLines() { return javaLines; }
        public int getDataItemsTranslated() { return dataItemsTranslated; }
        public int getStatementsTranslated() { return statementsTranslated; }
        public int getFilesGenerated() { return filesGenerated; }

        @Override
        public String toString() {
            return String.format(
                    "Metrics: COBOL lines=%d, Java lines=%d, Data items=%d, Statements=%d, Files=%d",
                    cobolLines, javaLines, dataItemsTranslated, statementsTranslated, filesGenerated
            );
        }

        public static class Builder {
            private int cobolLines;
            private int javaLines;
            private int dataItemsTranslated;
            private int statementsTranslated;
            private int filesGenerated;

            public Builder cobolLines(int val) { cobolLines = val; return this; }
            public Builder javaLines(int val) { javaLines = val; return this; }
            public Builder dataItemsTranslated(int val) { dataItemsTranslated = val; return this; }
            public Builder statementsTranslated(int val) { statementsTranslated = val; return this; }
            public Builder filesGenerated(int val) { filesGenerated = val; return this; }
            public TranslationMetrics build() { return new TranslationMetrics(this); }
        }
    }
}
