package com.cobol.translator.generator;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.CobolProgram;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Generates comprehensive JUnit tests for translated Java code.
 * 
 * Orchestrates the generation of three types of tests:
 * 1. Entity Tests - Unit tests for JPA entities (getters/setters, equals/hashCode, validation)
 * 2. Processor Tests - Unit tests for ItemProcessor business logic
 * 3. Job Config Tests - Integration tests for Spring Batch job configuration
 * 
 * All tests use JUnit 5 (Jupiter) with AssertJ for fluent assertions
 * and Mockito for mocking dependencies when needed.
 */
public class TestGenerator {

    private static final Logger logger = LoggerFactory.getLogger(TestGenerator.class);

    private final EntityTestGenerator entityTestGenerator;
    private final ProcessorTestGenerator processorTestGenerator;
    private final JobConfigTestGenerator jobConfigTestGenerator;

    public TestGenerator() {
        this.entityTestGenerator = new EntityTestGenerator();
        this.processorTestGenerator = new ProcessorTestGenerator();
        this.jobConfigTestGenerator = new JobConfigTestGenerator();
    }

    /**
     * Generates all test files for the given COBOL program.
     * 
     * @param program The parsed COBOL program
     * @param config Translation configuration
     * @param outputDir Output directory for test files (typically src/test/java)
     * @return List of all generated test files
     */
    public List<File> generate(CobolProgram program, TranslationConfig config, Path outputDir) {
        logger.info("Starting test generation for program: {}", program.getProgramName());
        
        List<File> generatedFiles = new ArrayList<>();
        int totalTests = 0;
        
        try {
            // 1. Generate Entity Tests
            logger.info("Generating entity tests...");
            List<File> entityTests = entityTestGenerator.generate(program, config, outputDir);
            generatedFiles.addAll(entityTests);
            totalTests += entityTests.size();
            logger.info("Generated {} entity test(s)", entityTests.size());
            
            // 2. Generate Processor Test
            logger.info("Generating processor test...");
            File processorTest = processorTestGenerator.generate(program, config, outputDir);
            generatedFiles.add(processorTest);
            totalTests++;
            logger.info("Generated processor test");
            
            // 3. Generate Job Config Test
            logger.info("Generating job configuration test...");
            File jobConfigTest = jobConfigTestGenerator.generate(program, config, outputDir);
            generatedFiles.add(jobConfigTest);
            totalTests++;
            logger.info("Generated job config test");
            
            // Summary
            logger.info("✅ Test generation completed successfully!");
            logger.info("   Total test files generated: {}", totalTests);
            logger.info("   Entity tests: {}", entityTests.size());
            logger.info("   Processor tests: 1");
            logger.info("   Job config tests: 1");
            
        } catch (IOException e) {
            logger.error("❌ Error during test generation: {}", e.getMessage(), e);
            // Return partial results if any were generated
        }
        
        return generatedFiles;
    }
    
    /**
     * Generates only entity tests.
     * Useful when you only need to test data model classes.
     */
    public List<File> generateEntityTests(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        logger.info("Generating entity tests only for program: {}", program.getProgramName());
        return entityTestGenerator.generate(program, config, outputDir);
    }
    
    /**
     * Generates only processor test.
     * Useful when you only need to test business logic.
     */
    public File generateProcessorTest(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        logger.info("Generating processor test only for program: {}", program.getProgramName());
        return processorTestGenerator.generate(program, config, outputDir);
    }
    
    /**
     * Generates only job configuration test.
     * Useful when you only need integration tests.
     */
    public File generateJobConfigTest(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {
        logger.info("Generating job config test only for program: {}", program.getProgramName());
        return jobConfigTestGenerator.generate(program, config, outputDir);
    }
}
