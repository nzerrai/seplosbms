package com.cobol.translator.generator;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.FileDefinition;
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
 * Generates Spring Data JPA repositories from COBOL file definitions.
 * Creates repository interfaces for indexed files with finder methods based on keys.
 */
public class RepositoryGenerator {

    private static final Logger logger = LoggerFactory.getLogger(RepositoryGenerator.class);

    /**
     * Generates JPA repositories for all indexed files in the COBOL program.
     */
    public void generateRepositories(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        
        // Find all indexed files (these need database access)
        List<FileDefinition> indexedFiles = program.getFileDefinitions().stream()
            .filter(this::isIndexedFile)
            .collect(Collectors.toList());

        if (indexedFiles.isEmpty()) {
            logger.info("No indexed files found, skipping repository generation");
            return;
        }

        for (FileDefinition file : indexedFiles) {
            generateRepository(file, program, config, outputDir);
        }
    }

    /**
     * Generates a single JPA repository for an indexed file.
     */
    private File generateRepository(FileDefinition file, CobolProgram program, 
                                   TranslationConfig config, Path outputDir) throws IOException {
        
        String entityName = toJavaClassName(file.getFileName()) + config.getNamingEntitySuffix();
        String repositoryName = toJavaClassName(file.getFileName()) + "Repository";
        File outputFile = outputDir.resolve(repositoryName + ".java").toFile();

        logger.info("Generating repository: {}", repositoryName);

        StringBuilder code = new StringBuilder();

        // Package
        String packageName = derivePackageFromPath(outputDir);
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import org.springframework.data.jpa.repository.JpaRepository;\n");
        code.append("import org.springframework.stereotype.Repository;\n");
        code.append("import java.util.List;\n");
        code.append("import java.util.Optional;\n");

        // Import entity from model package
        String modelPackage = deriveModelPackage(packageName);
        if (modelPackage != null && !modelPackage.equals(packageName)) {
            code.append("import ").append(modelPackage).append(".").append(entityName).append(";\n");
        }
        code.append("\n");

        // Javadoc
        code.append("/**\n");
        code.append(" * Repository for COBOL file: ").append(file.getFileName()).append("\n");
        code.append(" * Organization: ").append(file.getOrganization() != null ? file.getOrganization() : "SEQUENTIAL").append("\n");
        code.append(" * Access: ").append(file.getAccessMode() != null ? file.getAccessMode() : "SEQUENTIAL").append("\n");
        code.append(" *\n");
        code.append(" * Auto-generated JPA repository for data access operations.\n");
        code.append(" */\n");

        // Interface declaration
        code.append("@Repository\n");
        code.append("public interface ").append(repositoryName)
            .append(" extends JpaRepository<").append(entityName).append(", Long> {\n\n");

        // Generate finder methods based on record keys
        generateFinderMethods(code, file, program, entityName);

        // Close interface
        code.append("}\n");

        // Write to file
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write(code.toString());
        }

        logger.info("Generated: {}", outputFile.getAbsolutePath());
        return outputFile;
    }

    /**
     * Generates finder methods based on the file's record structure and keys.
     */
    private void generateFinderMethods(StringBuilder code, FileDefinition file, 
                                      CobolProgram program, String entityName) {
        
        // Get the record structure for this file
        List<DataItem> recordFields = getRecordFields(file, program);
        
        if (recordFields.isEmpty()) {
            code.append("    // No record fields found - add custom finder methods as needed\n\n");
            return;
        }

        // Find potential key fields (typically fields with PICTURE 9 or X and reasonable length)
        List<DataItem> keyFields = recordFields.stream()
            .filter(this::isPotentialKeyField)
            .limit(3) // Limit to first 3 key candidates
            .collect(Collectors.toList());

        if (keyFields.isEmpty()) {
            code.append("    // No key fields identified - add custom finder methods as needed\n\n");
            return;
        }

        // Generate finder methods for each key field
        for (DataItem keyField : keyFields) {
            String fieldName = keyField.getJavaFieldName();
            String methodName = "findBy" + toPascalCase(fieldName);
            String javaType = keyField.getJavaType();

            code.append("    /**\n");
            code.append("     * Find records by ").append(keyField.getName()).append("\n");
            code.append("     * COBOL: ").append(keyField.getPictureClause() != null ? keyField.getPictureClause() : "").append("\n");
            code.append("     */\n");
            code.append("    Optional<").append(entityName).append("> ").append(methodName)
                .append("(").append(javaType).append(" ").append(fieldName).append(");\n\n");
        }

        // Generate a findAll-like method with a common filter
        if (!keyFields.isEmpty()) {
            DataItem firstKey = keyFields.get(0);
            String fieldName = firstKey.getJavaFieldName();
            String javaType = firstKey.getJavaType();
            
            code.append("    /**\n");
            code.append("     * Find all records matching the given ").append(firstKey.getName()).append("\n");
            code.append("     */\n");
            code.append("    List<").append(entityName).append("> findAllBy")
                .append(toPascalCase(fieldName)).append("(").append(javaType).append(" ")
                .append(fieldName).append(");\n\n");
        }
    }

    /**
     * Gets the record fields for a file definition.
     */
    private List<DataItem> getRecordFields(FileDefinition file, CobolProgram program) {
        // Find the record structure associated with this file
        DataItem recordLayout = file.getRecordLayout();
        if (recordLayout == null) {
            return List.of();
        }
        
        String recordName = recordLayout.getName();
        return program.getDataItems().stream()
            .filter(item -> recordName.equalsIgnoreCase(item.getName()) || 
                           (item.getParent() != null && recordName.equalsIgnoreCase(item.getParent().getName())))
            .collect(Collectors.toList());
    }

    /**
     * Checks if a data item is a potential key field.
     */
    private boolean isPotentialKeyField(DataItem item) {
        // Key fields are typically:
        // - Elementary items (level 05-49, not 01, not 77, not 88)
        // - Numeric or alphanumeric
        // - Not too long (< 50 chars)
        // - Named with patterns like ID, NUMBER, CODE, KEY
        
        if (item.getLevel() <= 1 || item.getLevel() >= 77) {
            return false;
        }

        String name = item.getName().toUpperCase();
        if (name.contains("ID") || name.contains("NUMBER") || 
            name.contains("CODE") || name.contains("KEY") ||
            name.contains("ACCOUNT")) {
            return true;
        }

        // Check if it's a reasonably-sized numeric or alphanumeric field
        String pic = item.getPictureClause();
        if (pic != null) {
            if ((pic.startsWith("9") || pic.startsWith("X")) && pic.length() < 15) {
                return true;
            }
        }

        return false;
    }

    /**
     * Checks if a file is indexed (needs database access).
     */
    private boolean isIndexedFile(FileDefinition file) {
        String org = file.getOrganization();
        String access = file.getAccessMode();
        
        // Indexed files or files with random/dynamic access need repositories
        return (org != null && org.toUpperCase().contains("INDEXED")) ||
               (access != null && (access.toUpperCase().contains("RANDOM") || 
                                  access.toUpperCase().contains("DYNAMIC")));
    }

    private String toJavaClassName(String cobolName) {
        String[] parts = cobolName.split("-");
        StringBuilder result = new StringBuilder();
        for (String part : parts) {
            String cleanPart = part.replaceAll("^[0-9]+", "");
            if (!cleanPart.isEmpty()) {
                result.append(Character.toUpperCase(cleanPart.charAt(0)));
                result.append(cleanPart.substring(1).toLowerCase());
            }
        }
        return result.length() > 0 ? result.toString() : "Record";
    }

    private String toPascalCase(String fieldName) {
        if (fieldName == null || fieldName.isEmpty()) {
            return "";
        }
        return Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
    }

    private String derivePackageFromPath(Path outputDir) {
        String pathStr = outputDir.toString().replace('\\', '/');
        int javaIndex = pathStr.indexOf("src/main/java/");
        if (javaIndex >= 0) {
            String packagePath = pathStr.substring(javaIndex + "src/main/java/".length());
            return packagePath.replace('/', '.');
        }
        String[] parts = pathStr.split("[/\\\\]");
        if (parts.length >= 3) {
            return parts[parts.length - 3] + "." + parts[parts.length - 2] + "." + parts[parts.length - 1];
        }
        return "com.example.repository";
    }

    private String deriveModelPackage(String currentPackage) {
        if (currentPackage == null) return null;
        int lastDot = currentPackage.lastIndexOf('.');
        if (lastDot > 0) {
            return currentPackage.substring(0, lastDot) + ".model";
        }
        return null;
    }
}
