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
import java.util.List;

/**
 * Generates Java entity classes from COBOL file definitions.
 */
public class EntityGenerator {

    private static final Logger logger = LoggerFactory.getLogger(EntityGenerator.class);

    public List<File> generate(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        List<File> generatedFiles = new ArrayList<>();

        // Generate entity for each file definition
        for (FileDefinition fileDef : program.getFileDefinitions()) {
            File entityFile = generateEntityForFile(fileDef, program, config, outputDir);
            generatedFiles.add(entityFile);
        }

        return generatedFiles;
    }

    private File generateEntityForFile(FileDefinition fileDef, CobolProgram program,
                                       TranslationConfig config, Path outputDir) throws IOException {

        String entityName = toJavaClassName(fileDef.getFileName()) + config.getNamingEntitySuffix();
        File outputFile = outputDir.resolve(entityName + ".java").toFile();

        logger.info("Generating entity: {}", entityName);

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

        // Generate fields from record layout
        DataItem recordLayout = fileDef.getRecordLayout();
        if (recordLayout != null) {
            generateFields(recordLayout, program, code);
        }

        // Generate getters and setters
        code.append("    // Getters and Setters\n\n");
        if (recordLayout != null) {
            generateGettersSetters(recordLayout, program, code);
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
            if (field.getJavaType() != null) {
                // Add FILLER annotation if this is a filler field
                if (field.isFiller()) {
                    code.append("    /**\n");
                    code.append("     * FILLER field - reserved/unused space in COBOL record\n");
                    code.append("     * This field is not used in COBOL logic but maintains record structure\n");
                    code.append("     */\n");
                }

                code.append("    private ").append(field.getJavaType()).append(" ")
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
        }
        code.append("\n");
    }

    private void generateGettersSetters(DataItem item, CobolProgram program, StringBuilder code) {
        List<DataItem> elementaryFields = findElementaryFields(item, program);

        for (DataItem field : elementaryFields) {
            if (field.getJavaType() == null) continue;

            String fieldName = field.getJavaFieldName();
            String type = field.getJavaType();
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
}
