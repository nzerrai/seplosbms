package com.cobol.translator.generator;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import com.cobol.translator.model.Paragraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Generates Spring Batch job configuration.
 */
public class JobConfigGenerator {

    private static final Logger logger = LoggerFactory.getLogger(JobConfigGenerator.class);
    private final IOOptimizer ioOptimizer = new IOOptimizer();

    public File generate(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {

        String configName = toJavaClassName(program.getProgramName()) +
                          config.getNamingJobSuffix() + "Configuration";
        File outputFile = outputDir.resolve(configName + ".java").toFile();

        logger.info("Generating job configuration: {}", configName);

        StringBuilder code = new StringBuilder();

        // Package - derive from output directory structure
        String packageName = derivePackageFromPath(outputDir);
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import org.springframework.batch.core.Job;\n");
        code.append("import org.springframework.batch.core.Step;\n");
        code.append("import org.springframework.batch.core.job.builder.JobBuilder;\n");
        code.append("import org.springframework.batch.core.repository.JobRepository;\n");
        code.append("import org.springframework.batch.core.step.builder.StepBuilder;\n");
        code.append("import org.springframework.batch.item.ItemProcessor;\n");
        code.append("import org.springframework.batch.item.ItemReader;\n");
        code.append("import org.springframework.batch.item.ItemWriter;\n");
        code.append("import org.springframework.batch.item.file.builder.FlatFileItemReaderBuilder;\n");
        code.append("import org.springframework.batch.item.file.builder.FlatFileItemWriterBuilder;\n");
        code.append("import org.springframework.context.annotation.Bean;\n");
        code.append("import org.springframework.context.annotation.Configuration;\n");
        code.append("import org.springframework.core.io.FileSystemResource;\n");
        code.append("import org.springframework.transaction.PlatformTransactionManager;\n");
        code.append("import org.slf4j.Logger;\n");
        code.append("import org.slf4j.LoggerFactory;\n");
        
        // Import entity classes from model package
        String modelPackage = deriveModelPackage(packageName);
        if (modelPackage != null && !modelPackage.equals(packageName)) {
            code.append("import ").append(modelPackage).append(".*;\n");
        }
        
        // Import processor classes from processor package  
        String processorPackage = deriveProcessorPackage(packageName);
        if (processorPackage != null && !processorPackage.equals(packageName)) {
            code.append("import ").append(processorPackage).append(".*;\n");
        }
        code.append("\n");

        // Javadoc
        code.append("/**\n");
        code.append(" * Spring Batch job configuration for COBOL program: ")
            .append(program.getProgramName()).append("\n");
        code.append(" */\n");

        // Class declaration
        code.append("@Configuration\n");
        code.append("public class ").append(configName).append(" {\n\n");
        
        code.append("    private static final Logger logger = LoggerFactory.getLogger(")
            .append(configName).append(".class);\n\n");

        String jobName = toJavaFieldName(program.getProgramName()) + config.getNamingJobSuffix();

        String entityType = resolveEntityType(program, config);
        String processorType = toJavaClassName(program.getProgramName()) + config.getNamingProcessorSuffix();

        // Check if we have paragraphs to generate separate steps
        List<Paragraph> majorParagraphs = program.getParagraphs().stream()
                .filter(Paragraph::isMajorParagraph)
                .toList();

        boolean useMultiStep = !majorParagraphs.isEmpty();

        // Job bean
        code.append("    @Bean\n");
        code.append("    public Job ").append(jobName)
            .append("(JobRepository jobRepository");
        
        if (useMultiStep) {
            // Multi-step job with paragraph-based steps
            for (Paragraph para : majorParagraphs) {
                code.append(",\n            Step ").append(toJavaFieldName(para.getName())).append("Step");
            }
            code.append(") {\n");
            code.append("        return new JobBuilder(\"").append(jobName).append("\", jobRepository)\n");
            for (int i = 0; i < majorParagraphs.size(); i++) {
                Paragraph para = majorParagraphs.get(i);
                if (i == 0) {
                    code.append("                .start(").append(toJavaFieldName(para.getName())).append("Step)\n");
                } else {
                    code.append("                .next(").append(toJavaFieldName(para.getName())).append("Step)\n");
                }
            }
            code.append("                .build();\n");
        } else {
            // Single step job
            code.append(", Step ").append(jobName).append("Step) {\n");
            code.append("        return new JobBuilder(\"").append(jobName).append("\", jobRepository)\n");
            code.append("                .start(").append(jobName).append("Step)\n");
            code.append("                .build();\n");
        }
        code.append("    }\n\n");

        // Reader bean - use IOOptimizer if file definitions available
        List<String> fieldNames = extractFieldNames(program);

        if (!program.getFileDefinitions().isEmpty() && program.getFileDefinitions().get(0).getRecordLayout() != null) {
            // Use optimized reader generation
            FileDefinition inputFile = program.getFileDefinitions().get(0);

            logger.info("Using IOOptimizer for file: {}", inputFile.getFileName());
            IOOptimizer.FileIOMetadata metadata = ioOptimizer.analyzeFileDefinition(inputFile, program);
            String readerCode = ioOptimizer.generateOptimizedReader(metadata, entityType, jobName + "Reader");
            code.append(readerCode);
        } else {
            // Fallback to basic reader
            String fieldNamesStr = String.join("\", \"", fieldNames);

            code.append("    @Bean\n");
            code.append("    public ItemReader<").append(entityType).append("> ")
                .append(jobName).append("Reader() {\n");
            code.append("        return new FlatFileItemReaderBuilder<").append(entityType).append(">()\n");
            code.append("                .name(\"").append(jobName).append("-reader\")\n");
            code.append("                .resource(new FileSystemResource(\"data/input/input.csv\"))\n");
            code.append("                .delimited()\n");
            code.append("                .delimiter(\",\")\n");
            if (!fieldNames.isEmpty()) {
                code.append("                .names(\"").append(fieldNamesStr).append("\")\n");
            } else {
                code.append("                .names(\"TODO-MAP-FIELDS\")\n");
            }
            code.append("                .targetType(").append(entityType).append(".class)\n");
            code.append("                .build();\n");
            code.append("    }\n\n");
        }

        // Writer bean - use IOOptimizer if file definitions available
        if (!program.getFileDefinitions().isEmpty() && program.getFileDefinitions().get(0).getRecordLayout() != null) {
            // Use optimized writer generation
            FileDefinition writerFile = program.getFileDefinitions().get(0);

            IOOptimizer.FileIOMetadata writerMetadata = ioOptimizer.analyzeFileDefinition(writerFile, program);
            String writerCode = ioOptimizer.generateOptimizedWriter(writerMetadata, entityType, jobName + "Writer");
            code.append(writerCode);
        } else {
            // Fallback to basic writer
            code.append("    @Bean\n");
            code.append("    public ItemWriter<").append(entityType).append("> ")
                .append(jobName).append("Writer() {\n");
            code.append("        return new FlatFileItemWriterBuilder<").append(entityType).append(">()\n");
            code.append("                .name(\"").append(jobName).append("-writer\")\n");
            code.append("                .resource(new FileSystemResource(\"data/output/output.csv\"))\n");
            code.append("                .delimited()\n");
            code.append("                .delimiter(\",\")\n");
            if (!fieldNames.isEmpty()) {
                code.append("                .names(new String[]{");
                for (int i = 0; i < fieldNames.size(); i++) {
                    if (i > 0) code.append(", ");
                    code.append("\"").append(fieldNames.get(i)).append("\"");
                }
                code.append("})\n");
            }
            code.append("                .build();\n");
            code.append("    }\n\n");
        }

        // Generate audit trail writer
        generateAuditTrailWriter(code, program, config);
        
        // Generate error report writer
        generateErrorReportWriter(code, program, config);

        // Step bean with reader/processor/writer skeleton
        code.append("    @Bean\n");
        code.append("    public Step ").append(jobName).append("Step(\n");
        code.append("            JobRepository jobRepository,\n");
        code.append("            PlatformTransactionManager transactionManager,\n");
        code.append("            ItemReader<").append(entityType).append("> ").append(jobName).append("Reader,\n");
        code.append("            ").append(processorType).append(" processor,\n");
        code.append("            ItemWriter<").append(entityType).append("> ").append(jobName).append("Writer) {\n");
        code.append("        return new StepBuilder(\"").append(jobName).append("Step\", jobRepository)\n");
        code.append("                .<").append(entityType).append(", ").append(entityType).append(">chunk(100, transactionManager)\n");
        code.append("                .reader(").append(jobName).append("Reader)\n");
        code.append("                .processor(processor)\n");
        code.append("                .writer(").append(jobName).append("Writer)\n");
        code.append("                .build();\n");
        code.append("    }\n\n");

        // Generate paragraph-based steps if we have major paragraphs
        if (useMultiStep) {
            for (Paragraph para : majorParagraphs) {
                generateParagraphStep(code, para, jobName, entityType, processorType);
            }
        } else {
            // Generate single default step
            generateDefaultStep(code, jobName, entityType, processorType);
        }

        // Close class
        code.append("}\n");

        // Write to file
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write(code.toString());
        }

        logger.info("Generated: {}", outputFile.getAbsolutePath());
        return outputFile;
    }

    private String toJavaClassName(String cobolName) {
        String[] parts = cobolName.split("-");
        StringBuilder result = new StringBuilder();
        for (String part : parts) {
            // Remove leading digits from each part
            String cleanPart = part.replaceAll("^[0-9]+", "");
            if (!cleanPart.isEmpty()) {
                result.append(Character.toUpperCase(cleanPart.charAt(0)));
                result.append(cleanPart.substring(1).toLowerCase());
            }
        }
        // If result is empty, return a default name
        if (result.length() == 0) {
            result.append("Step");
        }
        return result.toString();
    }

    private String toJavaFieldName(String cobolName) {
        String[] parts = cobolName.toLowerCase().split("-");
        StringBuilder result = new StringBuilder();
        
        for (int i = 0; i < parts.length; i++) {
            // Remove leading digits from each part
            String cleanPart = parts[i].replaceAll("^[0-9]+", "");
            if (!cleanPart.isEmpty()) {
                if (i == 0) {
                    result.append(cleanPart);
                } else {
                    result.append(Character.toUpperCase(cleanPart.charAt(0)));
                    result.append(cleanPart.substring(1));
                }
            }
        }
        
        // If result is empty, return a default name
        if (result.length() == 0) {
            result.append("step");
        }
        return result.toString();
    }

    private String resolveEntityType(CobolProgram program, TranslationConfig config) {
        if (program.getFileDefinitions() != null && !program.getFileDefinitions().isEmpty()) {
            String name = program.getFileDefinitions().get(0).getFileName();
            return toJavaClassName(name) + config.getNamingEntitySuffix();
        }
        return toJavaClassName(program.getProgramName()) + config.getNamingEntitySuffix();
    }

    /**
     * Extracts field names from the record layout for reader/writer configuration.
     * Returns Java field names (camelCase) suitable for CSV column mapping.
     */
    private List<String> extractFieldNames(CobolProgram program) {
        List<String> fieldNames = new ArrayList<>();
        
        if (program.getFileDefinitions() != null && !program.getFileDefinitions().isEmpty()) {
            FileDefinition fd = program.getFileDefinitions().get(0);
            DataItem recordLayout = fd.getRecordLayout();
            
            if (recordLayout != null) {
                // Find elementary fields under this record
                List<DataItem> allItems = program.getDataItems();
                int parentIdx = allItems.indexOf(recordLayout);
                
                if (parentIdx != -1) {
                    for (int i = parentIdx + 1; i < allItems.size(); i++) {
                        DataItem item = allItems.get(i);
                        
                        // Stop at next top-level item
                        if (item.getLevel() <= recordLayout.getLevel()) {
                            break;
                        }
                        
                        // Include elementary fields (skip groups and fillers for CSV)
                        if (item.isElementary() && !item.isFiller() && item.getJavaFieldName() != null) {
                            fieldNames.add(item.getJavaFieldName());
                        }
                    }
                }
            }
        }
        
        return fieldNames;
    }

    /**
     * Generates a default single-step configuration with reader/processor/writer.
     */
    private void generateDefaultStep(StringBuilder code, String jobName, String entityType, String processorType) {
        // Step bean with reader/processor/writer skeleton
        code.append("    @Bean\n");
        code.append("    public Step ").append(jobName).append("Step(\n");
        code.append("            JobRepository jobRepository,\n");
        code.append("            PlatformTransactionManager transactionManager,\n");
        code.append("            ItemReader<").append(entityType).append("> ").append(jobName).append("Reader,\n");
        code.append("            ").append(processorType).append(" processor,\n");
        code.append("            ItemWriter<").append(entityType).append("> ").append(jobName).append("Writer) {\n");
        code.append("        return new StepBuilder(\"").append(jobName).append("Step\", jobRepository)\n");
        code.append("                .<").append(entityType).append(", ").append(entityType).append(">chunk(100, transactionManager)\n");
        code.append("                .reader(").append(jobName).append("Reader)\n");
        code.append("                .processor(processor)\n");
        code.append("                .writer(").append(jobName).append("Writer)\n");
        code.append("                .build();\n");
        code.append("    }\n\n");
    }

    /**
     * Generates a tasklet step for a COBOL paragraph.
     */
    private void generateParagraphStep(StringBuilder code, Paragraph para, String jobName, 
                                       String entityType, String processorType) {
        String stepName = toJavaFieldName(para.getName()) + "Step";
        String stepLabel = para.getName().replace("-", " ");
        
        code.append("    /**\n");
        code.append("     * Step for COBOL paragraph: ").append(para.getName()).append("\n");
        code.append("     * Lines ").append(para.getStartLine()).append("-").append(para.getEndLine()).append("\n");
        code.append("     * Statements: ").append(para.getStatements().size()).append("\n");
        code.append("     */\n");
        code.append("    @Bean\n");
        code.append("    public Step ").append(stepName).append("(\n");
        code.append("            JobRepository jobRepository,\n");
        code.append("            PlatformTransactionManager transactionManager) {\n");
        code.append("        return new StepBuilder(\"").append(stepName).append("\", jobRepository)\n");
        code.append("                .tasklet((contribution, chunkContext) -> {\n");
        code.append("                    // TODO: Implement logic from COBOL paragraph ").append(para.getName()).append("\n");
        code.append("                    logger.info(\"Executing ").append(stepLabel).append("\");\n");
        
        // Add comment showing some statements
        if (!para.getStatements().isEmpty()) {
            code.append("                    // Original COBOL statements:\n");
            int count = Math.min(3, para.getStatements().size());
            for (int i = 0; i < count; i++) {
                String cobolLine = para.getStatements().get(i).getOriginalCobol();
                if (cobolLine != null && !cobolLine.isEmpty()) {
                    code.append("                    // - ").append(cobolLine).append("\n");
                }
            }
            if (para.getStatements().size() > 3) {
                code.append("                    // ... and ").append(para.getStatements().size() - 3).append(" more\n");
            }
        }
        
        code.append("                    return org.springframework.batch.repeat.RepeatStatus.FINISHED;\n");
        code.append("                }, transactionManager)\n");
        code.append("                .build();\n");
        code.append("    }\n\n");
    }

    /**
     * Generates audit trail item writer bean.
     */
    private void generateAuditTrailWriter(StringBuilder code, CobolProgram program, TranslationConfig config) {
        String auditEntityType = findAuditEntityType(program);
        if (auditEntityType == null) {
            auditEntityType = "AuditTrailFileRecord";
        }
        
        code.append("    /**\n");
        code.append("     * ItemWriter for audit trail records\n");
        code.append("     * COBOL: AUDIT-TRAIL file\n");
        code.append("     */\n");
        code.append("    @Bean\n");
        code.append("    public ItemWriter<").append(auditEntityType).append("> auditTrailWriter() {\n");
        code.append("        return new FlatFileItemWriterBuilder<").append(auditEntityType).append(">()\n");
        code.append("                .name(\"auditTrailWriter\")\n");
        code.append("                .resource(new FileSystemResource(\"output/audit-trail.txt\"))\n");
        code.append("                .lineAggregator(item -> {\n");
        code.append("                    // TODO: Format audit trail record\n");
        code.append("                    return item.toString();\n");
        code.append("                })\n");
        code.append("                .build();\n");
        code.append("    }\n\n");
    }
    
    /**
     * Generates error report item writer bean.
     */
    private void generateErrorReportWriter(StringBuilder code, CobolProgram program, TranslationConfig config) {
        String errorEntityType = findErrorEntityType(program);
        if (errorEntityType == null) {
            errorEntityType = "ErrorReportFileRecord";
        }
        
        code.append("    /**\n");
        code.append("     * ItemWriter for error report records\n");
        code.append("     * COBOL: ERROR-REPORT file\n");
        code.append("     */\n");
        code.append("    @Bean\n");
        code.append("    public ItemWriter<").append(errorEntityType).append("> errorReportWriter() {\n");
        code.append("        return new FlatFileItemWriterBuilder<").append(errorEntityType).append(">()\n");
        code.append("                .name(\"errorReportWriter\")\n");
        code.append("                .resource(new FileSystemResource(\"output/error-report.txt\"))\n");
        code.append("                .lineAggregator(item -> {\n");
        code.append("                    // TODO: Format error report record\n");
        code.append("                    return item.toString();\n");
        code.append("                })\n");
        code.append("                .build();\n");
        code.append("    }\n\n");
    }
    
    /**
     * Finds the audit entity type from file definitions.
     */
    private String findAuditEntityType(CobolProgram program) {
        return program.getFileDefinitions().stream()
            .filter(f -> f.getFileName().toUpperCase().contains("AUDIT"))
            .map(f -> toJavaClassName(f.getFileName()) + "Record")
            .findFirst()
            .orElse(null);
    }
    
    /**
     * Finds the error entity type from file definitions.
     */
    private String findErrorEntityType(CobolProgram program) {
        return program.getFileDefinitions().stream()
            .filter(f -> f.getFileName().toUpperCase().contains("ERROR"))
            .map(f -> toJavaClassName(f.getFileName()) + "Record")
            .findFirst()
            .orElse(null);
    }
    
    /**
     * Derives the model package from current package.
     */
    private String deriveModelPackage(String currentPackage) {
        if (currentPackage == null) return null;
        int lastDot = currentPackage.lastIndexOf('.');
        if (lastDot > 0) {
            return currentPackage.substring(0, lastDot) + ".model";
        }
        return null;
    }
    
    /**
     * Derives the processor package from current package.
     */
    private String deriveProcessorPackage(String currentPackage) {
        if (currentPackage == null) return null;
        int lastDot = currentPackage.lastIndexOf('.');
        if (lastDot > 0) {
            return currentPackage.substring(0, lastDot) + ".processor";
        }
        return null;
    }

    /**
     * Derives the Java package name from the output directory path.
     * Extracts the package structure from the path after "src/main/java/"
     */
    private String derivePackageFromPath(Path outputDir) {
        String pathStr = outputDir.toString().replace('\\', '/');
        int javaIndex = pathStr.indexOf("src/main/java/");
        if (javaIndex >= 0) {
            String packagePath = pathStr.substring(javaIndex + "src/main/java/".length());
            return packagePath.replace('/', '.');
        }
        // Fallback to last parts of the path if src/main/java not found
        String[] parts = pathStr.split("[/\\\\]");
        if (parts.length >= 3) {
            return parts[parts.length - 3] + "." + parts[parts.length - 2] + "." + parts[parts.length - 1];
        }
        return "com.example.batch";
    }
}
