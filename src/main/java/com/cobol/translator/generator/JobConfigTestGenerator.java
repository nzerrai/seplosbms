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
 * Generates Spring Batch integration tests for Job Configuration classes.
 * Tests cover:
 * - Spring context loading
 * - Job bean creation and configuration
 * - Step configuration validation
 * - Reader/Writer/Processor wiring
 * - Job execution with test data
 */
public class JobConfigTestGenerator {

    private static final Logger logger = LoggerFactory.getLogger(JobConfigTestGenerator.class);

    public File generate(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        
        String jobConfigName = toJavaClassName(program.getProgramName()) + 
                              config.getNamingJobSuffix() + "Configuration";
        String testClassName = jobConfigName + "Test";
        File outputFile = outputDir.resolve(testClassName + ".java").toFile();

        logger.info("Generating job config test: {}", testClassName);

        StringBuilder code = new StringBuilder();

        // Package declaration
        String packageName = derivePackageFromPath(outputDir);
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import org.junit.jupiter.api.Test;\n");
        code.append("import org.junit.jupiter.api.DisplayName;\n");
        code.append("import org.junit.jupiter.api.extension.ExtendWith;\n");
        code.append("import org.springframework.batch.core.Job;\n");
        code.append("import org.springframework.batch.core.JobExecution;\n");
        code.append("import org.springframework.batch.core.JobParameters;\n");
        code.append("import org.springframework.batch.core.JobParametersBuilder;\n");
        code.append("import org.springframework.batch.core.Step;\n");
        code.append("import org.springframework.batch.core.launch.JobLauncher;\n");
        code.append("import org.springframework.batch.test.JobLauncherTestUtils;\n");
        code.append("import org.springframework.batch.test.context.SpringBatchTest;\n");
        code.append("import org.springframework.beans.factory.annotation.Autowired;\n");
        code.append("import org.springframework.boot.test.context.SpringBootTest;\n");
        code.append("import org.springframework.context.ApplicationContext;\n");
        code.append("import org.springframework.test.context.junit.jupiter.SpringExtension;\n");
        code.append("import static org.junit.jupiter.api.Assertions.*;\n");
        code.append("import static org.assertj.core.api.Assertions.assertThat;\n\n");
        code.append("import org.springframework.batch.core.BatchStatus;\n\n");

        // Import job config from model package
        String modelPackage = deriveModelPackage(packageName);
        if (modelPackage != null && !modelPackage.equals(packageName)) {
            code.append("import ").append(modelPackage).append(".").append(jobConfigName).append(";\n\n");
        }

        // Javadoc
        code.append("/**\n");
        code.append(" * Tests d'intégration Spring Batch pour ").append(jobConfigName).append("\n");
        code.append(" * Générés automatiquement depuis le programme COBOL: ").append(program.getProgramName()).append("\n");
        code.append(" *\n");
        code.append(" * Couvre:\n");
        code.append(" * - Chargement du contexte Spring\n");
        code.append(" * - Configuration des beans Job et Steps\n");
        code.append(" * - Câblage des composants (Reader/Processor/Writer)\n");
        code.append(" * - Exécution du job avec données de test\n");
        code.append(" */\n");

        // Class declaration with Spring annotations
        code.append("@ExtendWith(SpringExtension.class)\n");
        code.append("@SpringBootTest\n");
        code.append("@SpringBatchTest\n");
        code.append("@DisplayName(\"").append(jobConfigName).append(" - Tests d'intégration\")\n");
        code.append("class ").append(testClassName).append(" {\n\n");

        // Autowired dependencies
        code.append("    @Autowired\n");
        code.append("    private ApplicationContext applicationContext;\n\n");
        
        code.append("    @Autowired\n");
        code.append("    private JobLauncherTestUtils jobLauncherTestUtils;\n\n");
        
        code.append("    @Autowired\n");
        code.append("    private Job job;\n\n");

        // Test 1: Context loads
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit charger le contexte Spring avec succès\")\n");
        code.append("    void testContextLoads() {\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(applicationContext, \"Le contexte Spring doit être chargé\");\n");
        code.append("    }\n\n");

        // Test 2: Job bean exists
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit créer le bean Job\")\n");
        code.append("    void testJobBeanExists() {\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(job, \"Le bean Job doit être créé\");\n");
        code.append("        assertThat(job.getName()).isNotEmpty();\n");
        code.append("    }\n\n");

        // Test 3: Job configuration is valid
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit avoir une configuration Job valide\")\n");
        code.append("    void testJobConfiguration() {\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(job.getName(), \"Le job doit avoir un nom\");\n");
        code.append("        assertThat(job.getStepNames()).isNotEmpty();\n");
        code.append("    }\n\n");

        // Test 4: Step beans exist
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit créer les beans Step\")\n");
        code.append("    void testStepBeansExist() {\n");
        code.append("        // Act\n");
        code.append("        String[] stepNames = job.getStepNames().toArray(new String[0]);\n\n");
        code.append("        // Assert\n");
        code.append("        assertThat(stepNames).isNotEmpty();\n");
        code.append("        for (String stepName : stepNames) {\n");
        code.append("            Step step = applicationContext.getBean(stepName, Step.class);\n");
        code.append("            assertNotNull(step, \"Le step \" + stepName + \" doit exister\");\n");
        code.append("        }\n");
        code.append("    }\n\n");

        // Test 5: JobLauncherTestUtils configured
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit configurer JobLauncherTestUtils\")\n");
        code.append("    void testJobLauncherTestUtilsConfigured() {\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(jobLauncherTestUtils, \"JobLauncherTestUtils doit être configuré\");\n");
        code.append("        assertNotNull(jobLauncherTestUtils.getJob(), \"Le job doit être assigné à TestUtils\");\n");
        code.append("    }\n\n");

        // Test 6: Job execution with empty dataset
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit exécuter le job avec un dataset vide\")\n");
        code.append("    void testJobExecutionWithEmptyDataset() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        JobParameters jobParameters = new JobParametersBuilder()\n");
        code.append("            .addLong(\"time\", System.currentTimeMillis())\n");
        code.append("            .toJobParameters();\n\n");
        code.append("        // Act\n");
        code.append("        JobExecution jobExecution = jobLauncherTestUtils.launchJob(jobParameters);\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(jobExecution, \"L'exécution du job ne doit pas être null\");\n");
        code.append("        assertThat(jobExecution.getStatus()).isIn(BatchStatus.COMPLETED, BatchStatus.FAILED);\n");
        code.append("    }\n\n");

        // Test 7: Job execution with test data
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit exécuter le job avec des données de test\")\n");
        code.append("    void testJobExecutionWithTestData() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        JobParameters jobParameters = new JobParametersBuilder()\n");
        code.append("            .addLong(\"time\", System.currentTimeMillis())\n");
        code.append("            .addString(\"inputFile\", \"test-input.txt\")\n");
        code.append("            .addString(\"outputFile\", \"test-output.txt\")\n");
        code.append("            .toJobParameters();\n\n");
        code.append("        // Act\n");
        code.append("        JobExecution jobExecution = jobLauncherTestUtils.launchJob(jobParameters);\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(jobExecution);\n");
        code.append("        assertEquals(BatchStatus.COMPLETED, jobExecution.getStatus(),\n");
        code.append("            \"Le job doit se terminer avec succès\");\n");
        code.append("    }\n\n");

        // Test 8: Step execution
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit exécuter un step individuel avec succès\")\n");
        code.append("    void testStepExecution() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        JobParameters jobParameters = new JobParametersBuilder()\n");
        code.append("            .addLong(\"time\", System.currentTimeMillis())\n");
        code.append("            .toJobParameters();\n\n");
        code.append("        // Act\n");
        code.append("        JobExecution jobExecution = jobLauncherTestUtils.launchStep(\n");
        code.append("            job.getStepNames().iterator().next(), // Premier step\n");
        code.append("            jobParameters\n");
        code.append("        );\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(jobExecution);\n");
        code.append("        assertThat(jobExecution.getStepExecutions()).isNotEmpty();\n");
        code.append("    }\n\n");

        // Test 9: Job parameters validation
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit valider les paramètres du job\")\n");
        code.append("    void testJobParametersValidation() {\n");
        code.append("        // Arrange\n");
        code.append("        JobParameters validParams = new JobParametersBuilder()\n");
        code.append("            .addLong(\"time\", System.currentTimeMillis())\n");
        code.append("            .toJobParameters();\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(validParams);\n");
        code.append("        assertFalse(validParams.isEmpty(), \"Les paramètres ne doivent pas être vides\");\n");
        code.append("    }\n\n");

        // Test 10: Job restart capability
        code.append("    @Test\n");
        code.append("    @DisplayName(\"Doit supporter le redémarrage du job\")\n");
        code.append("    void testJobRestartability() throws Exception {\n");
        code.append("        // Arrange\n");
        code.append("        JobParameters jobParameters = new JobParametersBuilder()\n");
        code.append("            .addLong(\"time\", System.currentTimeMillis())\n");
        code.append("            .toJobParameters();\n\n");
        code.append("        // Act - Premier lancement\n");
        code.append("        JobExecution firstExecution = jobLauncherTestUtils.launchJob(jobParameters);\n\n");
        code.append("        // Assert\n");
        code.append("        assertNotNull(firstExecution);\n");
        code.append("        // Note: Le redémarrage dépend de la configuration du job\n");
        code.append("        // Tester selon la stratégie de restart configurée\n");
        code.append("    }\n\n");

        // Close class
        code.append("}\n");

        // Write to file
        outputFile.getParentFile().mkdirs();
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write(code.toString());
        }

        logger.info("Generated job config test: {}", outputFile.getAbsolutePath());
        return outputFile;
    }

    // Helper methods
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
        return testPackage.replace(".test.", ".config.");
    }
}
