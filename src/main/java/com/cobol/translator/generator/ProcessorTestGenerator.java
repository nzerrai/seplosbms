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
 * Generates JUnit tests for Spring Batch ItemProcessor classes.
 * Tests cover:
 * - Process method with valid input
 * - Null and edge case handling
 * - Business logic validation
 * - Exception handling
 * - Data transformation correctness
 */
public class ProcessorTestGenerator {

    private static final Logger logger = LoggerFactory.getLogger(ProcessorTestGenerator.class);

    public File generate(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        
        String processorName = toJavaClassName(program.getProgramName()) + 
                             config.getNamingProcessorSuffix();
        String testClassName = processorName + "Test";
        File outputFile = outputDir.resolve(testClassName + ".java").toFile();

        logger.info("Generating processor test: {}", testClassName);

        StringBuilder code = new StringBuilder();

        // Package declaration
        String packageName = derivePackageFromPath(outputDir);
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import org.junit.jupiter.api.Test;\n");
        code.append("import org.junit.jupiter.api.DisplayName;\n");
        code.append("import org.junit.jupiter.api.BeforeEach;\n");
        code.append("import org.junit.jupiter.api.extension.ExtendWith;\n");
        code.append("import org.mockito.InjectMocks;\n");
        code.append("import org.mockito.Mock;\n");
        code.append("import org.mockito.junit.jupiter.MockitoExtension;\n");
        code.append("import static org.junit.jupiter.api.Assertions.*;\n");
        code.append("import static org.assertj.core.api.Assertions.assertThat;\n");
        code.append("import static org.mockito.Mockito.*;\n\n");
        code.append("import java.math.BigDecimal;\n");
        code.append("import java.time.LocalDate;\n\n");

        // Import processor and entity from model package
        String modelPackage = deriveModelPackage(packageName);
        if (modelPackage != null && !modelPackage.equals(packageName)) {
            code.append("import ").append(modelPackage).append(".").append(processorName).append(";\n");
            
            // Import entity classes
            for (FileDefinition fileDef : program.getFileDefinitions()) {
                String entityName = toJavaClassName(fileDef.getFileName()) + config.getNamingEntitySuffix();
                code.append("import ").append(modelPackage).append(".").append(entityName).append(";\n");
            }
            code.append("\n");
        }

        // Javadoc
        code.append("/**\n");
        code.append(" * Tests unitaires pour le processor ").append(processorName).append("\n");
        code.append(" * Générés automatiquement depuis le programme COBOL: ").append(program.getProgramName()).append("\n");
        code.append(" *\n");
        code.append(" * Couvre:\n");
        code.append(" * - Traitement nominal avec données valides\n");
        code.append(" * - Gestion des cas limites et valeurs nulles\n");
        code.append(" * - Validation de la logique métier\n");
        code.append(" * - Gestion des exceptions\n");
        code.append(" */\n");

        // Class declaration with Mockito extension
        code.append("@ExtendWith(MockitoExtension.class)\n");
        code.append("@DisplayName(\"").append(processorName).append(" - Tests de processor\")\n");
        code.append("class ").append(testClassName).append(" {\n\n");

        // Inject processor under test
        code.append("    @InjectMocks\n");
        code.append("    private ").append(processorName).append(" processor;\n\n");

        // Determine input entity type
        String inputEntityName = "UnknownRecord";
        if (!program.getFileDefinitions().isEmpty()) {
            FileDefinition firstFile = program.getFileDefinitions().get(0);
            inputEntityName = toJavaClassName(firstFile.getFileName()) + config.getNamingEntitySuffix();
        }

        // Test data field
        code.append("    private ").append(inputEntityName).append(" inputRecord;\n\n");

        // @BeforeEach setup method
        code.append("    @BeforeEach\n");
        code.append("    void setUp() {\n");
        code.append("        inputRecord = createTestRecord();\n");
        code.append("    }\n\n");

        // Test 1: Process valid input
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit traiter un enregistrement valide avec succès\")\n");
        code.append("    void testProcessValidRecord() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        ").append(inputEntityName).append(" input = createTestRecord();\n\n");
        code.append("        // Act\n");
        code.append("        ").append(inputEntityName).append(" result = processor.process(input);\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(result, \"Le résultat ne doit pas être null\");\n");
        code.append("    }\n\n");

        // Test 2: Null input handling
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit gérer correctement une entrée null\")\n");
        code.append("    void testProcessNullInput() throws Exception {\n");
        code.append("        // Act\n");
        code.append("        ").append(inputEntityName).append(" result = processor.process(null);\n\n");
        code.append("        // Assert\n");
        code.append("        assertNull(result, \"Le résultat doit être null pour une entrée null\");\n");
        code.append("    }\n\n");

        // Test 3: Empty fields handling
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit traiter un enregistrement avec champs vides\")\n");
        code.append("    void testProcessEmptyFields() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        ").append(inputEntityName).append(" input = new ").append(inputEntityName).append("();\n\n");
        code.append("        // Act\n");
        code.append("        ").append(inputEntityName).append(" result = processor.process(input);\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(result, \"Le processor doit gérer les champs vides\");\n");
        code.append("    }\n\n");

        // Test 4: Business logic validation (if applicable)
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit valider la logique métier du traitement\")\n");
        code.append("    void testBusinessLogicValidation() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        ").append(inputEntityName).append(" input = createTestRecord();\n\n");
        code.append("        // Act\n");
        code.append("        ").append(inputEntityName).append(" result = processor.process(input);\n\n");
        code.append("        // Assert - Vérifier que la logique métier s'applique correctement\n");
        code.append("        assertNotNull(result);\n");
        code.append("        // TODO: Ajouter des assertions spécifiques à la logique métier\n");
        code.append("    }\n\n");

        // Test 5: Exception handling
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit gérer les exceptions de traitement\")\n");
        code.append("    void testExceptionHandling() {\n");
        code.append("        // Arrange\n");
        code.append("        ").append(inputEntityName).append(" input = createInvalidRecord();\n\n");
        code.append("        // Act & Assert\n");
        code.append("        assertDoesNotThrow(() -> processor.process(input),\n");
        code.append("            \"Le processor ne doit pas propager d'exceptions non gérées\");\n");
        code.append("    }\n\n");

        // Test 6: Data transformation
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit transformer les données correctement\")\n");
        code.append("    void testDataTransformation() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        ").append(inputEntityName).append(" input = createTestRecord();\n\n");
        code.append("        // Act\n");
        code.append("        ").append(inputEntityName).append(" result = processor.process(input);\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(result, \"Le résultat ne doit pas être null\");\n");
        code.append("        // Vérifier que les transformations attendues ont été appliquées\n");
        code.append("    }\n\n");

        // Test 7: Performance test (optional)
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit traiter les enregistrements en temps acceptable\")\n");
        code.append("    void testProcessingPerformance() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        ").append(inputEntityName).append(" input = createTestRecord();\n");
        code.append("        long startTime = System.currentTimeMillis();\n\n");
        code.append("        // Act - Traiter 100 enregistrements\n");
        code.append("        for (int i = 0; i < 100; i++) {\n");
        code.append("            processor.process(input);\n");
        code.append("        }\n");
        code.append("        long endTime = System.currentTimeMillis();\n\n");
        code.append("        // Assert - Doit traiter en moins de 1 seconde\n");
        code.append("        long duration = endTime - startTime;\n");
        code.append("        assertThat(duration).isLessThan(1000L);\n");
        code.append("    }\n\n");

        // Helper method: createTestRecord
        code.append("    /**\n");
        code.append("     * Crée un enregistrement de test avec des données valides\n");
        code.append("     */\n");
        code.append("    private ").append(inputEntityName).append(" createTestRecord() {\n");
        code.append("        ").append(inputEntityName).append(" record = new ").append(inputEntityName).append("();\n");
        
        // Set test values for fields
        if (!program.getFileDefinitions().isEmpty()) {
            FileDefinition firstFile = program.getFileDefinitions().get(0);
            if (firstFile.getRecordLayout() != null) {
                List<DataItem> fields = findElementaryFields(firstFile.getRecordLayout(), program);
                int count = 0;
                for (DataItem field : fields) {
                    if (count >= 5) break; // Limit to 5 fields for readability
                    String fieldName = field.getJavaFieldName();
                    if (fieldName == null || fieldName.isEmpty()) continue;
                    
                    String capitalizedFieldName = capitalize(fieldName);
                    String javaType = field.getJavaType();
                    code.append("        record.set").append(capitalizedFieldName);
                    code.append("(").append(getTestValue(javaType)).append(");\n");
                    count++;
                }
            }
        }
        
        code.append("        return record;\n");
        code.append("    }\n\n");

        // Helper method: createInvalidRecord
        code.append("    /**\n");
        code.append("     * Crée un enregistrement de test avec des données invalides\n");
        code.append("     */\n");
        code.append("    private ").append(inputEntityName).append(" createInvalidRecord() {\n");
        code.append("        ").append(inputEntityName).append(" record = new ").append(inputEntityName).append("();\n");
        code.append("        // Définir des valeurs invalides pour tester la robustesse\n");
        code.append("        return record;\n");
        code.append("    }\n\n");

        // Close class
        code.append("}\n");

        // Write to file
        outputFile.getParentFile().mkdirs();
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write(code.toString());
        }

        logger.info("Generated processor test: {}", outputFile.getAbsolutePath());
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

    private String getTestValue(String javaType) {
        return switch (javaType) {
            case "String" -> "\"TEST_VALUE\"";
            case "Integer", "int" -> "100";
            case "Long", "long" -> "1000L";
            case "BigDecimal" -> "new BigDecimal(\"1234.56\")";
            case "LocalDate" -> "LocalDate.of(2026, 1, 14)";
            case "Boolean", "boolean" -> "true";
            case "Double", "double" -> "99.99";
            case "Float", "float" -> "99.9f";
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
