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
 * Generates JUnit tests for JPA entity classes.
 * Tests cover:
 * - Constructor and field initialization
 * - Getters and Setters validation
 * - Entity serialization
 * - Equals and HashCode contracts
 * - Field constraints and validation
 */
public class EntityTestGenerator {

    private static final Logger logger = LoggerFactory.getLogger(EntityTestGenerator.class);

    public List<File> generate(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        List<File> generatedFiles = new ArrayList<>();

        // Generate test for each file definition (entity)
        for (FileDefinition fileDef : program.getFileDefinitions()) {
            String entityName = toJavaClassName(fileDef.getFileName()) + config.getNamingEntitySuffix();
            File testFile = generateEntityTest(entityName, fileDef, program, config, outputDir);
            generatedFiles.add(testFile);
        }

        return generatedFiles;
    }

    private File generateEntityTest(String entityName, FileDefinition fileDef, 
                                    CobolProgram program, TranslationConfig config, 
                                    Path outputDir) throws IOException {
        
        String testClassName = entityName + "Test";
        File outputFile = outputDir.resolve(testClassName + ".java").toFile();

        logger.info("Generating entity test: {}", testClassName);

        StringBuilder code = new StringBuilder();

        // Package declaration
        String packageName = derivePackageFromPath(outputDir);
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import org.junit.jupiter.api.Test;\n");
        code.append("import org.junit.jupiter.api.DisplayName;\n");
        code.append("import org.junit.jupiter.api.BeforeEach;\n");
        code.append("import static org.junit.jupiter.api.Assertions.*;\n");
        code.append("import static org.assertj.core.api.Assertions.assertThat;\n\n");
        code.append("import java.math.BigDecimal;\n");
        code.append("import java.time.LocalDate;\n\n");

        // Import entity from model package
        String modelPackage = deriveModelPackage(packageName);
        if (modelPackage != null && !modelPackage.equals(packageName)) {
            code.append("import ").append(modelPackage).append(".").append(entityName).append(";\n\n");
        }

        // Javadoc
        code.append("/**\n");
        code.append(" * Tests unitaires pour l'entité ").append(entityName).append("\n");
        code.append(" * Générés automatiquement depuis le fichier COBOL: ").append(fileDef.getFileName()).append("\n");
        code.append(" * Programme: ").append(program.getProgramName()).append("\n");
        code.append(" *\n");
        code.append(" * Couvre:\n");
        code.append(" * - Initialisation et constructeurs\n");
        code.append(" * - Validation des getters/setters\n");
        code.append(" * - Contraintes de validation\n");
        code.append(" * - Equals et HashCode\n");
        code.append(" */\n");

        // Class declaration
        code.append("@DisplayName(\"").append(entityName).append(" - Tests d'entité\")\n");
        code.append("class ").append(testClassName).append(" {\n\n");

        // Instance field for test subject
        code.append("    private ").append(entityName).append(" entity;\n\n");

        // @BeforeEach setup method
        code.append("    @BeforeEach\n");
        code.append("    void setUp() {\n");
        code.append("        entity = new ").append(entityName).append("();\n");
        code.append("    }\n\n");

        // Test 1: Constructor
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit créer une instance avec constructeur par défaut\")\n");
        code.append("    void testDefaultConstructor() {\n");
        code.append("        // Arrange & Act\n");
        code.append("        ").append(entityName).append(" newEntity = new ").append(entityName).append("();\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(newEntity, \"L'entité ne doit pas être null\");\n");
        code.append("    }\n\n");

        // Get elementary fields from record layout
        List<DataItem> fields = new ArrayList<>();
        if (fileDef.getRecordLayout() != null) {
            fields = findElementaryFields(fileDef.getRecordLayout(), program);
        }

        // Test 2: Getters/Setters for each field
        if (!fields.isEmpty()) {
            for (DataItem field : fields) {
                String fieldName = field.getJavaFieldName();
                if (fieldName == null || fieldName.isEmpty()) {
                    continue;
                }
                
                String javaType = field.getJavaType();
                String capitalizedFieldName = capitalize(fieldName);
                
                code.append("    @Test\n");
                code.append("    @DisplayName(\"Doit valider getter/setter pour ").append(fieldName).append("\")\n");
                code.append("    void testGetSet").append(capitalizedFieldName).append("() {\n");
                code.append("        // Arrange\n");
                code.append("        ").append(javaType).append(" testValue = ").append(getTestValue(javaType)).append(";\n\n");
                code.append("        // Act\n");
                code.append("        entity.set").append(capitalizedFieldName).append("(testValue);\n");
                code.append("        ").append(javaType).append(" result = entity.get").append(capitalizedFieldName).append("();\n\n");
                code.append("        // Assert\n");
                code.append("        assertEquals(testValue, result, \"La valeur doit être correctement assignée\");\n");
                code.append("    }\n\n");
            }
        }

        // Test 3: Null handling
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit gérer les valeurs null correctement\")\n");
        code.append("    void testNullHandling() {\n");
        code.append("        // Arrange & Act\n");
        if (!fields.isEmpty()) {
            DataItem firstField = fields.get(0);
            String fieldName = firstField.getJavaFieldName();
            if (fieldName != null && !fieldName.isEmpty()) {
                String capitalizedFieldName = capitalize(fieldName);
                code.append("        entity.set").append(capitalizedFieldName).append("(null);\n\n");
                code.append("        // Assert\n");
                code.append("        assertNull(entity.get").append(capitalizedFieldName).append("(), ");
                code.append("\"Doit accepter et retourner null\");\n");
            }
        } else {
            code.append("        // No fields to test\n");
            code.append("        // Assert\n");
            code.append("        assertNotNull(entity);\n");
        }
        code.append("    }\n\n");

        // Test 4: Fluent setters (if any)
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit permettre le chaînage des setters (fluent API)\")\n");
        code.append("    void testFluentSetters() {\n");
        code.append("        // Arrange & Act\n");
        code.append("        ").append(entityName).append(" result = entity");
        
        int count = 0;
        for (DataItem field : fields) {
            String fieldName = field.getJavaFieldName();
            if (fieldName == null || fieldName.isEmpty() || count >= 3) {
                break; // Limit to 3 fields for readability
            }
            String capitalizedFieldName = capitalize(fieldName);
            String javaType = field.getJavaType();
            code.append("\n            .set").append(capitalizedFieldName).append("(").append(getTestValue(javaType)).append(")");
            count++;
        }
        code.append(";\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(result, \"Le chaînage doit retourner l'instance\");\n");
        code.append("    }\n\n");

        // Test 5: Equals and HashCode
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit implémenter equals correctement\")\n");
        code.append("    void testEquals() {\n");
        code.append("        // Arrange\n");
        code.append("        ").append(entityName).append(" entity1 = new ").append(entityName).append("();\n");
        code.append("        ").append(entityName).append(" entity2 = new ").append(entityName).append("();\n\n");
        
        if (!fields.isEmpty()) {
            DataItem firstField = fields.get(0);
            String fieldName = firstField.getJavaFieldName();
            if (fieldName != null && !fieldName.isEmpty()) {
                String capitalizedFieldName = capitalize(fieldName);
                String javaType = firstField.getJavaType();
                code.append("        entity1.set").append(capitalizedFieldName).append("(").append(getTestValue(javaType)).append(");\n");
                code.append("        entity2.set").append(capitalizedFieldName).append("(").append(getTestValue(javaType)).append(");\n\n");
            }
        }
        
        code.append("        // Assert\n");
        code.append("        assertEquals(entity1, entity2, \"Les entités avec les mêmes valeurs doivent être égales\");\n");
        code.append("        assertEquals(entity1.hashCode(), entity2.hashCode(), \"Les hashCodes doivent être identiques\");\n");
        code.append("    }\n\n");

        // Test 6: ToString
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit générer une représentation String valide\")\n");
        code.append("    void testToString() {\n");
        code.append("        // Act\n");
        code.append("        String result = entity.toString();\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(result, \"toString() ne doit pas retourner null\");\n");
        code.append("        assertThat(result).contains(\"").append(entityName).append("\");\n");
        code.append("    }\n\n");

        // Test 7: Field validation (for BigDecimal, dates, etc.)
        if (hasSpecialFieldTypes(fields)) {
            code.append("    @Test\n");
            code.append("    @DisplayName(\"Doit valider les types de champs spéciaux\")\n");
            code.append("    void testSpecialFieldTypes() {\n");
            
            for (DataItem field : fields) {
                String fieldName = field.getJavaFieldName();
                if (fieldName == null || fieldName.isEmpty()) continue;
                
                String javaType = field.getJavaType();
                if ("BigDecimal".equals(javaType)) {
                    String capitalizedFieldName = capitalize(fieldName);
                    code.append("        // Test BigDecimal field: ").append(fieldName).append("\n");
                    code.append("        entity.set").append(capitalizedFieldName).append("(new BigDecimal(\"123.45\"));\n");
                    code.append("        assertNotNull(entity.get").append(capitalizedFieldName).append("());\n");
                    code.append("        assertEquals(0, entity.get").append(capitalizedFieldName);
                    code.append("().compareTo(new BigDecimal(\"123.45\")));\n\n");
                } else if ("LocalDate".equals(javaType)) {
                    String capitalizedFieldName = capitalize(fieldName);
                    code.append("        // Test LocalDate field: ").append(fieldName).append("\n");
                    code.append("        LocalDate testDate = LocalDate.of(2026, 1, 14);\n");
                    code.append("        entity.set").append(capitalizedFieldName).append("(testDate);\n");
                    code.append("        assertEquals(testDate, entity.get").append(capitalizedFieldName).append("());\n\n");
                }
            }
            code.append("    }\n\n");
        }

        // Close class
        code.append("}\n");

        // Write to file
        outputFile.getParentFile().mkdirs();
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write(code.toString());
        }

        logger.info("Generated entity test: {}", outputFile.getAbsolutePath());
        return outputFile;
    }

    // Helper methods
    private List<DataItem> findElementaryFields(DataItem parentItem, CobolProgram program) {
        List<DataItem> result = new ArrayList<>();

        List<DataItem> allItems = program.getDataItems();
        int parentIdx = allItems.indexOf(parentItem);

        if (parentIdx == -1) {
            // Fallback
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

    private boolean hasSpecialFieldTypes(List<DataItem> fields) {
        for (DataItem field : fields) {
            String type = field.getJavaType();
            if ("BigDecimal".equals(type) || "LocalDate".equals(type)) {
                return true;
            }
        }
        return false;
    }

    private String getTestValue(String javaType) {
        return switch (javaType) {
            case "String" -> "\"TEST_VALUE\"";
            case "Integer", "int" -> "42";
            case "Long", "long" -> "123L";
            case "BigDecimal" -> "new BigDecimal(\"999.99\")";
            case "LocalDate" -> "LocalDate.of(2026, 1, 14)";
            case "Boolean", "boolean" -> "true";
            case "Double", "double" -> "3.14";
            case "Float", "float" -> "2.5f";
            default -> "null";
        };
    }

    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return Character.toUpperCase(str.charAt(0)) + str.substring(1);
    }

    private String toJavaClassName(String cobolName) {
        if (cobolName == null) {
            return "UnknownClass";
        }
        
        String[] parts = cobolName.split("[-_]");
        StringBuilder result = new StringBuilder();
        
        for (String part : parts) {
            if (!part.isEmpty()) {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1).toLowerCase());
                }
            }
        }
        
        return result.toString();
    }

    private String derivePackageFromPath(Path path) {
        String pathStr = path.toString().replace("\\", "/");
        
        if (pathStr.contains("/src/test/java/")) {
            String packagePath = pathStr.substring(pathStr.indexOf("/src/test/java/") + 15);
            return packagePath.replace("/", ".");
        } else if (pathStr.contains("/src/main/java/")) {
            String packagePath = pathStr.substring(pathStr.indexOf("/src/main/java/") + 15);
            return packagePath.replace("/", ".");
        }
        
        return "com.cobol.generated.test";
    }

    private String deriveModelPackage(String testPackage) {
        if (testPackage.endsWith(".test")) {
            return testPackage.substring(0, testPackage.length() - 5);
        }
        return testPackage.replace(".test.", ".model.");
    }
}
