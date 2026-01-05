package com.cobol.translator.generator;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.CobolProgram;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;

/**
 * Generates Spring Batch ItemProcessor from COBOL procedure logic.
 * Now includes business rules translation from COBOL validation paragraphs.
 */
public class ProcessorGenerator {

    private static final Logger logger = LoggerFactory.getLogger(ProcessorGenerator.class);
    private final BusinessRuleGenerator businessRuleGenerator = new BusinessRuleGenerator();
    private final BusinessLogicTranslator logicTranslator = new BusinessLogicTranslator();

    public File generate(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {

        String processorName = toJavaClassName(program.getProgramName()) +
                             config.getNamingProcessorSuffix();
        File outputFile = outputDir.resolve(processorName + ".java").toFile();

        logger.info("Generating processor: {}", processorName);

        StringBuilder code = new StringBuilder();

        // Package - derive from output directory structure
        String packageName = derivePackageFromPath(outputDir);
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
            code.append("import org.springframework.batch.item.ItemProcessor;\n");
            code.append("import org.springframework.beans.factory.annotation.Autowired;\n");
            code.append("import org.springframework.stereotype.Component;\n");
            code.append("import org.slf4j.Logger;\n");
            code.append("import org.slf4j.LoggerFactory;\n");
            code.append("import java.math.BigDecimal;\n");
            code.append("import java.math.RoundingMode;\n");

        // Import entity classes from model package
        String modelPackage = deriveModelPackage(packageName);
        if (modelPackage != null && !modelPackage.equals(packageName)) {
            code.append("import ").append(modelPackage).append(".*;\n");
        }
        code.append("\n");

        // Javadoc
        code.append("/**\n");
        code.append(" * Processor for COBOL program: ").append(program.getProgramName()).append("\n");
        code.append(" * Auto-generated from PROCEDURE DIVISION logic\n");
        code.append(" *\n");
        code.append(" * Implements business logic from COBOL paragraphs:\n");
        code.append(" * - 200-PROCESS-TRANSACTIONS: Main processing loop\n");
        code.append(" * - 210-VALIDATE-TRANSACTION: Input validation\n");
        code.append(" * - 220-PROCESS-VALID-TRANSACTION: Business rule processing\n");
        code.append(" * - 223-UPDATE-ACCOUNT-BALANCE: Balance calculations\n");
        code.append(" */\n");

        // Class declaration
        String entityName = toJavaClassName(program.getProgramName());

        // Determine the input record type from the first input file
        String inputRecordType = determineInputRecordType(program, config);

            code.append("@Component\n");
        code.append("public class ").append(processorName)
            .append(" implements ItemProcessor<").append(inputRecordType)
            .append(", ").append(inputRecordType).append("> {\n\n");

        code.append("    private static final Logger logger = LoggerFactory.getLogger(")
            .append(processorName).append(".class);\n\n");

        // Inject validator
        String validatorName = entityName + "Validator";
        code.append("    @Autowired\n");
        code.append("    private ").append(validatorName).append(" validator;\n\n");

        // Process method with business logic
        code.append("    /**\n");
        code.append("     * Main processing method - corresponds to COBOL paragraph 200-PROCESS-TRANSACTIONS\n");
        code.append("     * \n");
        code.append("     * COBOL Logic:\n");
        code.append("     * 1. PERFORM 210-VALIDATE-TRANSACTION\n");
        code.append("     * 2. IF VALID-TRANS\n");
        code.append("     * 3.    PERFORM 220-PROCESS-VALID-TRANSACTION\n");
        code.append("     * 4. ELSE\n");
        code.append("     * 5.    PERFORM 230-LOG-ERROR\n");
        code.append("     */\n");
        code.append("    @Override\n");
        code.append("    public ").append(inputRecordType).append(" process(")
            .append(inputRecordType).append(" record) throws Exception {\n");
        code.append("        logger.debug(\"Processing record: {}\", record);\n\n");

        // Step 1: Validate transaction (210-VALIDATE-TRANSACTION)
        code.append("        // Step 1: Validate transaction (COBOL: 210-VALIDATE-TRANSACTION)\n");
        code.append("        ").append(validatorName).append(".ValidationResult validationResult = \n");
        code.append("            validator.validateTransaction(record);\n\n");

        code.append("        if (!validationResult.isValid()) {\n");
        code.append("            // COBOL: PERFORM 230-LOG-ERROR\n");
        code.append("            logger.warn(\"Transaction validation failed: {}\", validationResult.getErrors());\n");
        code.append("            // In a real implementation, this would write to error file\n");
        code.append("            // For now, we'll return null to filter out invalid records\n");
        code.append("            return null;\n");
        code.append("        }\n\n");

        // Step 2: Process valid transaction (220-PROCESS-VALID-TRANSACTION)
        code.append("        // Step 2: Process valid transaction (COBOL: 220-PROCESS-VALID-TRANSACTION)\n");
        code.append("        logger.debug(\"Transaction validated successfully, processing business rules\");\n\n");

        // Step 3: Translate COBOL paragraphs into Java business logic
        code.append("        // Step 3: Business logic from COBOL PROCEDURE DIVISION\n");
        
        // Find main processing paragraph
        var processingParagraph = program.getParagraphs().stream()
            .filter(p -> p.getName().contains("PROCESS-VALID") || p.getName().contains("220"))
            .findFirst();
        
        if (processingParagraph.isPresent()) {
            String translatedCode = logicTranslator.translateParagraph(processingParagraph.get(), inputRecordType);
            code.append(translatedCode);
        } else {
            code.append("        // TODO: No specific processing paragraph found\n");
            code.append("        // Implement balance update logic here\n");
            code.append("        logger.debug(\"Transaction processed: {}\", record);\n");
        }
        code.append("\n");

        code.append("        return record;\n");
        code.append("    }\n\n");

        // Helper methods
        generateHelperMethods(code, validatorName);

        // Close class
        code.append("}\n");

        // Also generate the validator class (in same package as processor)
        businessRuleGenerator.generateValidator(program, packageName, outputDir, inputRecordType);

        // Write to file
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write(code.toString());
        }

        logger.info("Generated: {}", outputFile.getAbsolutePath());
        return outputFile;
    }

    private void generateHelperMethods(StringBuilder code, String validatorName) {
        code.append("    // ========================================\n");
        code.append("    // Helper Methods\n");
        code.append("    // ========================================\n\n");

        code.append("    /**\n");
        code.append("     * Helper method to check if transaction is a debit\n");
        code.append("     * COBOL: Level-88 TR-DEBIT VALUE 'DB'\n");
        code.append("     */\n");
        code.append("    private boolean isDebit(String transactionType) {\n");
        code.append("        return \"DB\".equals(transactionType);\n");
        code.append("    }\n\n");

        code.append("    /**\n");
        code.append("     * Helper method to check if transaction is a credit\n");
        code.append("     * COBOL: Level-88 TR-CREDIT VALUE 'CR'\n");
        code.append("     */\n");
        code.append("    private boolean isCredit(String transactionType) {\n");
        code.append("        return \"CR\".equals(transactionType);\n");
        code.append("    }\n\n");

        code.append("    /**\n");
        code.append("     * Helper method to check if transaction is a transfer\n");
        code.append("     * COBOL: Level-88 TR-TRANSFER VALUE 'TF'\n");
        code.append("     */\n");
        code.append("    private boolean isTransfer(String transactionType) {\n");
        code.append("        return \"TF\".equals(transactionType);\n");
        code.append("    }\n\n");

        code.append("    /**\n");
        code.append("     * Calculate new balance based on transaction type and amount\n");
        code.append("     * COBOL: Paragraph 223-UPDATE-ACCOUNT-BALANCE\n");
        code.append("     */\n");
        code.append("    private BigDecimal calculateNewBalance(\n");
        code.append("            BigDecimal currentBalance, \n");
        code.append("            BigDecimal transactionAmount, \n");
        code.append("            String transactionType) {\n");
        code.append("        \n");
        code.append("        if (isDebit(transactionType) || isTransfer(transactionType)) {\n");
        code.append("            return currentBalance.subtract(transactionAmount);\n");
        code.append("        } else if (isCredit(transactionType)) {\n");
        code.append("            return currentBalance.add(transactionAmount);\n");
        code.append("        }\n");
        code.append("        return currentBalance;\n");
        code.append("    }\n\n");
    }

    private String toJavaClassName(String cobolName) {
        String[] parts = cobolName.split("-");
        StringBuilder result = new StringBuilder();
        for (String part : parts) {
            if (!part.isEmpty()) {
                result.append(Character.toUpperCase(part.charAt(0)));
                result.append(part.substring(1).toLowerCase());
            }
        }
        return result.toString();
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

    /**
     * Derives the model package from the current package.
     * For example: com.nz.batch.processor -> com.nz.batch.model
     */
    private String deriveModelPackage(String currentPackage) {
        if (currentPackage == null) {
            return null;
        }
        // Replace last component (processor/config) with "model"
        int lastDot = currentPackage.lastIndexOf('.');
        if (lastDot > 0) {
            String basePackage = currentPackage.substring(0, lastDot);
            return basePackage + ".model";
        }
        return null;
    }

    /**
     * Determines the input record type from the program's file definitions.
     * Returns the name of the entity class for the first input file.
     */
    private String determineInputRecordType(CobolProgram program, TranslationConfig config) {
        // Find first input file (usually the transaction file)
        if (program.getFileDefinitions() != null && !program.getFileDefinitions().isEmpty()) {
            // Get the first file definition (typically the input file)
            String fileName = program.getFileDefinitions().get(0).getFileName();
            return toJavaClassName(fileName) + config.getNamingEntitySuffix();
        }
        // Fallback to generic name
        return "Record";
    }
}
