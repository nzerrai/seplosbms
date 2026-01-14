package com.cobol.translator.generator;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Generates Java validation and business rule methods from COBOL logic.
 * Translates Level-88 conditions, validation paragraphs, and business rules.
 */
public class BusinessRuleGenerator {

    private static final Logger logger = LoggerFactory.getLogger(BusinessRuleGenerator.class);

    /**
     * Generates a validator class with business rules from COBOL program.
     */
    public File generateValidator(CobolProgram program, String packageName, Path outputDir, String recordType) throws IOException {
        String validatorName = toJavaClassName(program.getProgramName()) + "Validator";
        File outputFile = outputDir.resolve(validatorName + ".java").toFile();

        logger.info("Generating business rule validator: {}", validatorName);

        StringBuilder code = new StringBuilder();

        // Package
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import org.springframework.stereotype.Component;\n");
        code.append("import org.slf4j.Logger;\n");
        code.append("import org.slf4j.LoggerFactory;\n");
        code.append("import java.math.BigDecimal;\n");
        code.append("import java.util.ArrayList;\n");
        code.append("import java.util.List;\n");

        // Import entity classes from model package
        String modelPackage = deriveModelPackage(packageName);
        if (modelPackage != null && !modelPackage.equals(packageName)) {
            code.append("import ").append(modelPackage).append(".*;\n");
        }
        code.append("\n");

        // Javadoc
        code.append("/**\n");
        code.append(" * Business rule validator for COBOL program: ").append(program.getProgramName()).append("\n");
        code.append(" * Auto-generated from COBOL validation logic\n");
        code.append(" *\n");
        code.append(" * Translates COBOL validation paragraphs and Level-88 conditions\n");
        code.append(" * into Java validation methods.\n");
        code.append(" */\n");

        // Class declaration
        code.append("@Component\n");
        code.append("public class ").append(validatorName).append(" {\n\n");

        code.append("    private static final Logger logger = LoggerFactory.getLogger(")
            .append(validatorName).append(".class);\n\n");

        // Error codes constants
        code.append("    // Error codes from COBOL program\n");
        generateErrorCodeConstants(code);

        // Validation result class
        generateValidationResultClass(code);

        // Generate Level-88 condition check methods
        generateLevel88Methods(program, code);

        // Generate main validation method
        generateMainValidationMethod(program, code, recordType);

        // Generate individual validation methods based on COBOL logic
        generateValidationMethods(program, code, recordType);

        // Close class
        code.append("}\n");

        // Write to file
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write(code.toString());
        }

        logger.info("Generated: {}", outputFile.getAbsolutePath());
        return outputFile;
    }

    private void generateErrorCodeConstants(StringBuilder code) {
        code.append("    public static final String ERR_INVALID_ACCOUNT = \"E001\";\n");
        code.append("    public static final String ERR_INVALID_TRANSACTION_TYPE = \"E002\";\n");
        code.append("    public static final String ERR_INVALID_AMOUNT = \"E003\";\n");
        code.append("    public static final String ERR_INVALID_DATE = \"E004\";\n");
        code.append("    public static final String ERR_ACCOUNT_NOT_FOUND = \"E005\";\n");
        code.append("    public static final String ERR_ACCOUNT_CLOSED = \"E006\";\n");
        code.append("    public static final String ERR_ACCOUNT_FROZEN = \"E007\";\n");
        code.append("    public static final String ERR_OVERDRAFT_EXCEEDED = \"E008\";\n\n");
    }

    private void generateValidationResultClass(StringBuilder code) {
        code.append("    /**\n");
        code.append("     * Validation result containing validation status and error details\n");
        code.append("     */\n");
        code.append("    public static class ValidationResult {\n");
        code.append("        private boolean valid = true;\n");
        code.append("        private List<ValidationError> errors = new ArrayList<>();\n\n");

        code.append("        public boolean isValid() { return valid; }\n");
        code.append("        public void setValid(boolean valid) { this.valid = valid; }\n\n");

        code.append("        public List<ValidationError> getErrors() { return errors; }\n\n");

        code.append("        public void addError(String code, String message) {\n");
        code.append("            this.valid = false;\n");
        code.append("            this.errors.add(new ValidationError(code, message));\n");
        code.append("        }\n");
        code.append("    }\n\n");

        code.append("    /**\n");
        code.append("     * Individual validation error\n");
        code.append("     */\n");
        code.append("    public static class ValidationError {\n");
        code.append("        private final String code;\n");
        code.append("        private final String message;\n\n");

        code.append("        public ValidationError(String code, String message) {\n");
        code.append("            this.code = code;\n");
        code.append("            this.message = message;\n");
        code.append("        }\n\n");

        code.append("        public String getCode() { return code; }\n");
        code.append("        public String getMessage() { return message; }\n");
        code.append("    }\n\n");
    }

    private void generateLevel88Methods(CobolProgram program, StringBuilder code) {
        // Find all Level-88 condition names
        List<DataItem> level88Items = program.getDataItems().stream()
            .filter(DataItem::isConditionName)
            .collect(Collectors.toList());

        if (level88Items.isEmpty()) {
            return;
        }

        code.append("    // ========================================\n");
        code.append("    // Level-88 Condition Name Methods\n");
        code.append("    // ========================================\n\n");

        for (DataItem condition : level88Items) {
            String methodName = "is" + toPascalCase(condition.getName());
            String parentField = deriveIntelligentParameterName(condition);
            String conditionValue = condition.getConditionValue();

            code.append("    /**\n");
            code.append("     * COBOL Level-88: ").append(condition.getName())
                .append(" VALUE '").append(conditionValue).append("'\n");
            if (condition.getConditionParent() != null) {
                code.append("     * @param ").append(parentField)
                    .append(" The value of ").append(condition.getConditionParent().getName()).append("\n");
            }
            code.append("     */\n");
            code.append("    public boolean ").append(methodName).append("(String ").append(parentField).append(") {\n");
            code.append("        return \"").append(conditionValue).append("\".equals(").append(parentField).append(");\n");
            code.append("    }\n\n");
        }
    }

    private void generateMainValidationMethod(CobolProgram program, StringBuilder code, String recordType) {
        code.append("    // ========================================\n");
        code.append("    // Main Validation Method\n");
        code.append("    // ========================================\n\n");

        code.append("    /**\n");
        code.append("     * Main validation method - validates record fields based on COBOL data structure\n");
        code.append("     * \n");
        code.append("     * @param record The record to validate\n");
        code.append("     * @return ValidationResult containing validation status and any errors\n");
        code.append("     */\n");
        code.append("    public ValidationResult validateTransaction(").append(recordType).append(" record) {\n");
        code.append("        logger.debug(\"Validating record: {}\", record);\n");
        code.append("        ValidationResult result = new ValidationResult();\n\n");

        // Get the fields from the specific file definition that matches this recordType
        String fileName = extractFileName(recordType);
        List<DataItem> fields = getRecordFields(program, fileName);
        
        if (fields.isEmpty()) {
            code.append("        // No fields found for validation\n");
            code.append("        // All records pass validation by default\n\n");
        } else {
            code.append("        // Auto-generated validation logic based on COBOL data items\n\n");
            
            for (DataItem field : fields) {
                if (field.isFiller()) {
                    continue; // Skip FILLER fields
                }
                
                String javaFieldName = field.getJavaFieldName();
                String getterName = "get" + capitalize(javaFieldName);
                String javaType = field.getJavaType();
                
                // Generate validation based on field type
                if ("String".equals(javaType)) {
                    code.append("        // Validate ").append(field.getName()).append(" (").append(field.getPictureClause()).append(")\n");
                    code.append("        if (record.").append(getterName).append("() == null || record.").append(getterName).append("().trim().isEmpty()) {\n");
                    code.append("            result.addError(\"VAL_").append(field.getName().toUpperCase().replace("-", "_")).append("\", ");
                    code.append("\"").append(field.getName()).append(" is required\");\n");
                    code.append("        }\n\n");
                } else if ("BigDecimal".equals(javaType)) {
                    code.append("        // Validate ").append(field.getName()).append(" (").append(field.getPictureClause()).append(")\n");
                    code.append("        if (record.").append(getterName).append("() == null || record.").append(getterName).append("().compareTo(BigDecimal.ZERO) < 0) {\n");
                    code.append("            result.addError(\"VAL_").append(field.getName().toUpperCase().replace("-", "_")).append("\", ");
                    code.append("\"").append(field.getName()).append(" must be a positive value\");\n");
                    code.append("        }\n\n");
                } else if ("Integer".equals(javaType) || "Long".equals(javaType)) {
                    code.append("        // Validate ").append(field.getName()).append(" (").append(field.getPictureClause()).append(")\n");
                    code.append("        if (record.").append(getterName).append("() == null || record.").append(getterName).append("() <= 0) {\n");
                    code.append("            result.addError(\"VAL_").append(field.getName().toUpperCase().replace("-", "_")).append("\", ");
                    code.append("\"").append(field.getName()).append(" must be a positive value\");\n");
                    code.append("        }\n\n");
                } else if ("LocalDate".equals(javaType)) {
                    code.append("        // Validate ").append(field.getName()).append(" (").append(field.getPictureClause()).append(")\n");
                    code.append("        if (record.").append(getterName).append("() == null) {\n");
                    code.append("            result.addError(\"VAL_").append(field.getName().toUpperCase().replace("-", "_")).append("\", ");
                    code.append("\"").append(field.getName()).append(" is required\");\n");
                    code.append("        }\n\n");
                }
            }
            
            code.append("        // Additional custom validation rules can be added here\n\n");
        }

        code.append("        logger.debug(\"Validation result: valid={}, errors={}\", result.isValid(), result.getErrors().size());\n");
        code.append("        return result;\n");
        code.append("    }\n\n");
    }
    
    /**
     * Gets the record fields from a specific file definition
     * @param program The COBOL program
     * @param fileName The name of the file (e.g., "TRANSACTION-FILE")
     * @return List of fields belonging to that specific FD record
     */
    private List<DataItem> getRecordFields(CobolProgram program, String fileName) {
        if (program.getFileDefinitions() == null || program.getFileDefinitions().isEmpty()) {
            return List.of();
        }
        
        // Find the specific file definition by name
        var fileDefinition = program.getFileDefinitions().stream()
            .filter(fd -> fd.getFileName().equalsIgnoreCase(fileName))
            .findFirst();
        
        if (fileDefinition.isEmpty() || fileDefinition.get().getRecordLayout() == null) {
            logger.warn("No file definition found for: {}", fileName);
            return List.of();
        }
        
        DataItem recordLayout = fileDefinition.get().getRecordLayout();
        String recordName = recordLayout.getName();
        
        // Find elementary fields that belong to this specific record
        // They must be direct children of the record (level > recordLayout.level)
        // and have a parent hierarchy that includes the record
        return program.getDataItems().stream()
            .filter(item -> item.getLevel() > recordLayout.getLevel())
            .filter(DataItem::isElementary)
            .filter(item -> item.getJavaType() != null)
            .filter(item -> isChildOfRecord(item, recordName, program))
            .collect(Collectors.toList());
    }
    
    /**
     * Checks if a data item is a child of a specific record
     */
    private boolean isChildOfRecord(DataItem item, String recordName, CobolProgram program) {
        // Check if this item's hierarchy includes the record name
        // The item's full name or parent structure should reference the record
        String itemName = item.getName();
        if (itemName == null) {
            return false;
        }
        
        // Simple heuristic: check if the item name starts with the record prefix
        // e.g., TR-* fields belong to TRANSACTION-RECORD
        // This works for most COBOL naming conventions where record fields share a prefix
        String recordPrefix = extractPrefix(recordName);
        String itemPrefix = extractPrefix(itemName);
        
        return recordPrefix.equals(itemPrefix);
    }
    
    /**
     * Extracts the prefix from a COBOL name (e.g., "TR-ACCOUNT-NUMBER" -> "TR")
     */
    private String extractPrefix(String cobolName) {
        if (cobolName == null || !cobolName.contains("-")) {
            return cobolName;
        }
        return cobolName.substring(0, cobolName.indexOf("-"));
    }
    
    /**
     * Extracts the file name from a record type (e.g., "TransactionFileRecord" -> "TRANSACTION-FILE")
     */
    private String extractFileName(String recordType) {
        // Remove "Record" suffix if present
        String baseName = recordType.replaceAll("Record$", "");
        
        // Convert from PascalCase to COBOL naming (e.g., TransactionFile -> TRANSACTION-FILE)
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < baseName.length(); i++) {
            char c = baseName.charAt(i);
            if (Character.isUpperCase(c) && i > 0) {
                result.append("-");
            }
            result.append(Character.toUpperCase(c));
        }
        return result.toString();
    }

    private void generateValidationMethods(CobolProgram program, StringBuilder code, String recordType) {
        code.append("    // ========================================\n");
        code.append("    // Individual Validation Methods\n");
        code.append("    // TODO: Implement validation methods based on COBOL logic\n");
        code.append("    // ========================================\n\n");

        code.append("    /**\n");
        code.append("     * TODO: Add validation helper methods as needed for your business rules\n");
        code.append("     * \n");
        code.append("     * Example pattern from COBOL validation:\n");
        code.append("     * COBOL: IF FIELD-NAME = ZERO\n");
        code.append("     *        MOVE 'E001' TO WS-ERR-CODE\n");
        code.append("     * \n");
        code.append("     * Java equivalent:\n");
        code.append("     * private boolean validateFieldName(").append(recordType).append(" record, ValidationResult result) {\n");
        code.append("     *     if (record.getFieldName() == null || record.getFieldName().equals(0)) {\n");
        code.append("     *         result.addError(\"ERR001\", \"Field name is required\");\n");
        code.append("     *         return false;\n");
        code.append("     *     }\n");
        code.append("     *     return true;\n");
        code.append("     * }\n");
        code.append("     * \n");
        code.append("     * Access record fields using getter methods from ").append(recordType).append(".\n");
        code.append("     * Check the entity class to see available getters.\n");
        code.append("     */\n\n");

        // Account status validation
        code.append("    /**\n");
        code.append("     * COBOL: IF MA-CLOSED or IF MA-FROZEN\n");
        code.append("     *        Business rule validation for account status\n");
        code.append("     */\n");
        code.append("    public boolean validateAccountStatus(String accountStatus, ValidationResult result) {\n");
        code.append("        if (\"C\".equals(accountStatus)) {\n");
        code.append("            result.addError(ERR_ACCOUNT_CLOSED, \"COMPTE FERME\");\n");
        code.append("            return false;\n");
        code.append("        }\n");
        code.append("        if (\"F\".equals(accountStatus)) {\n");
        code.append("            result.addError(ERR_ACCOUNT_FROZEN, \"COMPTE GELE\");\n");
        code.append("            return false;\n");
        code.append("        }\n");
        code.append("        return true;\n");
        code.append("    }\n\n");

        // Overdraft validation
        code.append("    /**\n");
        code.append("     * COBOL: IF MA-CURRENT-BALANCE < (MA-OVERDRAFT-LIMIT * -1)\n");
        code.append("     *        Business rule for overdraft limit check\n");
        code.append("     */\n");
        code.append("    public boolean validateOverdraft(BigDecimal currentBalance, BigDecimal overdraftLimit, ValidationResult result) {\n");
        code.append("        BigDecimal minAllowedBalance = overdraftLimit.negate();\n");
        code.append("        if (currentBalance.compareTo(minAllowedBalance) < 0) {\n");
        code.append("            result.addError(ERR_OVERDRAFT_EXCEEDED, \"DEPASSEMENT DECOUVERT AUTORISE\");\n");
        code.append("            return false;\n");
        code.append("        }\n");
        code.append("        return true;\n");
        code.append("    }\n");
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

    private String toPascalCase(String cobolName) {
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
     * Derives an intelligent parameter name from a Level-88 condition.
     * Examples:
     * - TR-DEBIT/TR-CREDIT/TR-TRANSFER (parent: TR-TRANSACTION-TYPE) -> "transactionType"
     * - MA-ACTIVE/MA-CLOSED/MA-FROZEN (parent: MA-ACCOUNT-STATUS) -> "accountStatus"
     * - Default: use parent's Java field name
     */
    private String deriveIntelligentParameterName(DataItem condition) {
        if (condition.getConditionParent() == null) {
            return "value";
        }
        
        String parentName = condition.getConditionParent().getName();
        String conditionName = condition.getName();
        
        // Extract common semantic patterns from COBOL naming
        if (parentName.contains("TRANSACTION-TYPE") || conditionName.contains("DEBIT") || conditionName.contains("CREDIT")) {
            return "transactionType";
        }
        if (parentName.contains("STATUS") || conditionName.contains("ACTIVE") || conditionName.contains("CLOSED") || conditionName.contains("FROZEN")) {
            return "accountStatus";
        }
        if (parentName.contains("TYPE")) {
            return "type";
        }
        if (parentName.contains("CODE")) {
            return "code";
        }
        if (parentName.contains("FLAG")) {
            return "flag";
        }
        
        // Fallback: use parent's Java field name
        return condition.getConditionParent().getJavaFieldName();
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
     * Capitalizes the first letter of a string
     */
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return Character.toUpperCase(str.charAt(0)) + str.substring(1);
    }

    /**
     * Placeholder class for TransactionRecord - would be generated by EntityGenerator
     */
    public static class TransactionRecord {
        private Long accountNumber;
        private String transactionType;
        private java.math.BigDecimal amount;
        private Integer transactionDate;

        public Long getAccountNumber() { return accountNumber; }
        public String getTransactionType() { return transactionType; }
        public java.math.BigDecimal getAmount() { return amount; }
        public Integer getTransactionDate() { return transactionDate; }
    }
}
