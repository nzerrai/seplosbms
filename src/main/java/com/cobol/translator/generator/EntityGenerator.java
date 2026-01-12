package com.cobol.translator.generator;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.Set;

/**
 * Generates Java entity classes from COBOL file definitions.
 * Enhanced with type inference to generate fields even when COBOL layout is incomplete.
 */
public class EntityGenerator {

    private static final Logger logger = LoggerFactory.getLogger(EntityGenerator.class);

    public List<File> generate(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        return generate(program, config, outputDir, new LinkedHashMap<>());
    }

    /**
     * Generates entities with additional inferred fields.
     * 
     * @param additionalFields Map of entityName → (fieldName → javaType) for fields to add
     */
    public List<File> generate(CobolProgram program, TranslationConfig config, Path outputDir,
                               Map<String, Map<String, String>> additionalFields) throws IOException {
        List<File> generatedFiles = new ArrayList<>();

        // Generate entity for each file definition
        for (FileDefinition fileDef : program.getFileDefinitions()) {
            String entityName = toJavaClassName(fileDef.getFileName()) + config.getNamingEntitySuffix();
            Map<String, String> extraFields = additionalFields.getOrDefault(entityName, new LinkedHashMap<>());
            
            File entityFile = generateEntityForFile(fileDef, program, config, outputDir, extraFields);
            generatedFiles.add(entityFile);
        }

        return generatedFiles;
    }

    private File generateEntityForFile(FileDefinition fileDef, CobolProgram program,
                                       TranslationConfig config, Path outputDir,
                                       Map<String, String> additionalFields) throws IOException {

        String entityName = toJavaClassName(fileDef.getFileName()) + config.getNamingEntitySuffix();
        File outputFile = outputDir.resolve(entityName + ".java").toFile();

        logger.info("Generating entity: {} (with {} additional fields)", 
            entityName, additionalFields.size());

        StringBuilder code = new StringBuilder();

        // Package declaration - derive from output directory structure
        String packageName = derivePackageFromPath(outputDir);
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import java.math.BigDecimal;\n");
        code.append("import java.time.LocalDate;\n\n");

        // Javadoc
        code.append("/**\n");
        code.append(" * Entity for COBOL file: ").append(fileDef.getFileName()).append("\n");
        code.append(" * Generated from program: ").append(program.getProgramName()).append("\n");
        code.append(" */\n");

        // Class declaration
        code.append("public class ").append(entityName).append(" {\n\n");

        // Collect existing field names from layout to avoid duplicates
        Set<String> existingFields = new HashSet<>();
        DataItem recordLayout = fileDef.getRecordLayout();
        if (recordLayout != null) {
            List<DataItem> elementaryFields = findElementaryFields(recordLayout, program);
            for (DataItem field : elementaryFields) {
                String fieldName = field.getJavaFieldName();
                if (fieldName != null && !fieldName.isEmpty()) {
                    // Convert camelCase to PascalCase to match inferred field keys
                    String pascalCase = Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
                    existingFields.add(pascalCase);
                }
            }
        }
        
        // Filter out duplicate fields from additionalFields
        Map<String, String> uniqueAdditionalFields = new LinkedHashMap<>();
        for (Map.Entry<String, String> entry : additionalFields.entrySet()) {
            if (!existingFields.contains(entry.getKey())) {
                uniqueAdditionalFields.put(entry.getKey(), entry.getValue());
            } else {
                logger.debug("Skipping duplicate field from layout: {}", entry.getKey());
            }
        }

        // Generate fields from record layout
        if (recordLayout != null) {
            generateFields(recordLayout, program, code);
        }

        // Generate additional inferred fields (only unique ones)
        if (!uniqueAdditionalFields.isEmpty()) {
            generateAdditionalFields(uniqueAdditionalFields, code);
        }

        // Generate getters and setters
        code.append("    // Getters and Setters\n\n");
        if (recordLayout != null) {
            generateGettersSetters(recordLayout, program, code);
        }

        // Generate getters/setters for additional fields (only unique ones)
        if (!uniqueAdditionalFields.isEmpty()) {
            generateAdditionalGettersSetters(uniqueAdditionalFields, code);
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

    private void generateFields(DataItem item, CobolProgram program, StringBuilder code) {
        // Find all elementary fields under this record
        List<DataItem> elementaryFields = findElementaryFields(item, program);

        for (DataItem field : elementaryFields) {
            // Default to String when the analyzer could not infer a type to avoid missing accessors.
            String javaType = field.getJavaType() != null ? field.getJavaType() : "String";
            if (field.getJavaType() == null) {
                field.setJavaType(javaType);
            }

            // Add FILLER annotation if this is a filler field
            if (field.isFiller()) {
                code.append("    /**\n");
                code.append("     * FILLER field - reserved/unused space in COBOL record\n");
                code.append("     * This field is not used in COBOL logic but maintains record structure\n");
                code.append("     */\n");
            }

            code.append("    private ").append(javaType).append(" ")
                .append(field.getJavaFieldName()).append(";\n");

            // Add comment with COBOL picture
            if (field.getPictureClause() != null) {
                code.append("    // COBOL: ");
                if (field.isFiller()) {
                    code.append("FILLER ");
                }
                code.append("PIC ").append(field.getPictureClause());
                if (field.getUsage() != null) {
                    code.append(" ").append(field.getUsage());
                }
                code.append("\n");
            }
        }
        code.append("\n");
    }

    private void generateGettersSetters(DataItem item, CobolProgram program, StringBuilder code) {
        List<DataItem> elementaryFields = findElementaryFields(item, program);

        for (DataItem field : elementaryFields) {
            // Default to String when type inference failed so getters/setters always exist
            String type = field.getJavaType() != null ? field.getJavaType() : "String";
            if (field.getJavaType() == null) {
                field.setJavaType(type);
            }

            String fieldName = field.getJavaFieldName();
            String methodName = Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);

            // Getter
            code.append("    public ").append(type).append(" get").append(methodName).append("() {\n");
            code.append("        return ").append(fieldName).append(";\n");
            code.append("    }\n\n");

            // Setter
            code.append("    public void set").append(methodName).append("(").append(type).append(" ")
                .append(fieldName).append(") {\n");
            code.append("        this.").append(fieldName).append(" = ").append(fieldName).append(";\n");
            code.append("    }\n\n");
        }

        // Generate 88-level condition name helper methods
        generate88LevelMethods(item, program, code);
    }

    /**
     * Generates helper methods for 88-level condition names.
     * For each 88-level, creates an is*() method that checks if the parent field equals the condition value.
     * 
     * Example COBOL:
     *   05  TR-TRANSACTION-TYPE     PIC X(02).
     *       88  TR-DEBIT            VALUE 'DB'.
     *       88  TR-CREDIT           VALUE 'CR'.
     * 
     * Generated Java:
     *   public boolean isTrDebit() {
     *       return "DB".equals(this.trTransactionType);
     *   }
     *   public boolean isTrCredit() {
     *       return "CR".equals(this.trTransactionType);
     *   }
     */
    private void generate88LevelMethods(DataItem item, CobolProgram program, StringBuilder code) {
        List<DataItem> allItems = program.getDataItems();
        if (allItems == null || allItems.isEmpty()) {
            return;
        }

        int parentIdx = allItems.indexOf(item);
        if (parentIdx == -1) {
            return;
        }

        // Look for 88-level items within this record's scope
        for (int i = parentIdx + 1; i < allItems.size(); i++) {
            DataItem currentItem = allItems.get(i);

            // Stop when we reach another top-level or sibling record
            if (currentItem.getLevel() <= item.getLevel()) {
                break;
            }

            // Check if this is an 88-level condition name
            if (currentItem.getLevel() == 88 && currentItem.isConditionName()) {
                // Find the parent field (the field immediately before with level < 88)
                DataItem parentField = findConditionParent(allItems, i);
                
                if (parentField != null && parentField.getJavaFieldName() != null) {
                    String conditionName = currentItem.getName();
                    String conditionValue = currentItem.getConditionValue();
                    String parentFieldName = parentField.getJavaFieldName();

                    if (conditionName != null && conditionValue != null) {
                        // Generate is*() method
                        String methodName = toConditionMethodName(conditionName);
                        
                        code.append("    /**\n");
                        code.append("     * COBOL 88-level condition: ").append(conditionName);
                        code.append(" VALUE ").append(conditionValue).append("\n");
                        code.append("     * Checks if ").append(parentField.getName());
                        code.append(" equals ").append(conditionValue).append("\n");
                        code.append("     */\n");
                        code.append("    public boolean ").append(methodName).append("() {\n");
                        
                        // Clean the condition value (remove quotes if present)
                        String cleanValue = conditionValue.replaceAll("^['\"]|['\"]$", "");
                        
                        code.append("        return \"").append(cleanValue).append("\".equals(this.");
                        code.append(parentFieldName).append(");\n");
                        code.append("    }\n\n");
                    }
                }
            }
        }
    }

    /**
     * Finds the parent field for an 88-level condition.
     * The parent is the last field with level < 88 before the condition.
     */
    private DataItem findConditionParent(List<DataItem> allItems, int conditionIndex) {
        for (int i = conditionIndex - 1; i >= 0; i--) {
            DataItem item = allItems.get(i);
            if (item.getLevel() < 88 && item.getLevel() > 0) {
                return item;
            }
        }
        return null;
    }

    /**
     * Converts an 88-level condition name to a Java method name.
     * Example: TR-DEBIT -> isTrDebit
     */
    private String toConditionMethodName(String conditionName) {
        String[] parts = conditionName.split("-");
        StringBuilder result = new StringBuilder("is");
        for (String part : parts) {
            if (part.length() > 0) {
                result.append(Character.toUpperCase(part.charAt(0)));
                result.append(part.substring(1).toLowerCase());
            }
        }
        return result.toString();
    }

    private List<DataItem> findElementaryFields(DataItem parentItem, CobolProgram program) {
        List<DataItem> result = new ArrayList<>();

        // Scope fields to the contiguous block following the parent 01 record until another
        // top-level record (level <= parent level) is encountered. This avoids pulling in
        // unrelated WORKING-STORAGE items into the entity.
        List<DataItem> allItems = program.getDataItems();
        int parentIdx = allItems.indexOf(parentItem);

        if (parentIdx == -1) {
            // Fallback to previous behavior if the parent is not found
            for (DataItem item : allItems) {
                if (item.getLevel() > 1 && item.isElementary() && item.getJavaType() != null) {
                    result.add(item);
                }
            }
            return result;
        }

        for (int i = parentIdx + 1; i < allItems.size(); i++) {
            DataItem item = allItems.get(i);

            // Stop when we reach another top-level or sibling record
            if (item.getLevel() <= parentItem.getLevel()) {
                break;
            }

            if (item.isElementary() && item.getJavaType() != null) {
                result.add(item);
            }
        }

        return result;
    }

    private String toJavaClassName(String cobolName) {
        String[] parts = cobolName.split("-");
        StringBuilder result = new StringBuilder();
        for (String part : parts) {
            result.append(Character.toUpperCase(part.charAt(0)));
            result.append(part.substring(1).toLowerCase());
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
     * Generates additional fields inferred from processor code analysis.
     */
    private void generateAdditionalFields(Map<String, String> additionalFields, StringBuilder code) {
        code.append("    // ========================================\n");
        code.append("    // Additional fields (inferred from usage)\n");
        code.append("    // ========================================\n");
        
        for (Map.Entry<String, String> entry : additionalFields.entrySet()) {
            String fieldName = toCamelCase(entry.getKey());
            String javaType = entry.getValue();
            String defaultValue = getDefaultValue(javaType);
            
            code.append("    private ").append(javaType).append(" ")
                .append(fieldName);
            
            if (defaultValue != null) {
                code.append(" = ").append(defaultValue);
            }
            
            code.append("; // Inferred: ").append(entry.getKey()).append("\n");
        }
        
        code.append("\n");
    }

    /**
     * Generates getters and setters for additional inferred fields.
     */
    private void generateAdditionalGettersSetters(Map<String, String> additionalFields, StringBuilder code) {
        code.append("    // ========================================\n");
        code.append("    // Getters/Setters for additional fields\n");
        code.append("    // ========================================\n\n");
        
        for (Map.Entry<String, String> entry : additionalFields.entrySet()) {
            String pascalCase = entry.getKey();
            String fieldName = toCamelCase(pascalCase);
            String javaType = entry.getValue();
            
            // Getter
            code.append("    public ").append(javaType).append(" get")
                .append(pascalCase).append("() {\n");
            code.append("        return this.").append(fieldName).append(";\n");
            code.append("    }\n\n");
            
            // Setter
            code.append("    public void set").append(pascalCase).append("(")
                .append(javaType).append(" value) {\n");
            code.append("        this.").append(fieldName).append(" = value;\n");
            code.append("    }\n\n");
        }
    }

    /**
     * Converts PascalCase to camelCase.
     */
    private String toCamelCase(String pascalCase) {
        if (pascalCase == null || pascalCase.isEmpty()) {
            return pascalCase;
        }
        return Character.toLowerCase(pascalCase.charAt(0)) + pascalCase.substring(1);
    }

    /**
     * Returns default value expression for Java type.
     */
    private String getDefaultValue(String javaType) {
        switch (javaType) {
            case "BigDecimal":
                return "BigDecimal.ZERO";
            case "Integer":
                return "0";
            case "Long":
                return "0L";
            case "String":
                return "\"\"";
            case "Boolean":
            case "boolean":
                return "false";
            default:
                return null; // null for LocalDate and complex types
        }
    }
}
