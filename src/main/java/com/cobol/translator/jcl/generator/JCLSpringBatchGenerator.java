package com.cobol.translator.jcl.generator;

import com.cobol.translator.jcl.model.DDStatement;
import com.cobol.translator.jcl.model.JCLJob;
import com.cobol.translator.jcl.model.JCLStep;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Generates Spring Batch configuration from JCL
 */
public class JCLSpringBatchGenerator {

    private static final Logger logger = LoggerFactory.getLogger(JCLSpringBatchGenerator.class);

    private final String basePackage;

    public JCLSpringBatchGenerator(String basePackage) {
        this.basePackage = basePackage;
    }

    /**
     * Generate Spring Batch Job configuration from JCL
     *
     * @param jclJob Parsed JCL Job
     * @param outputDir Output directory for generated code
     * @return List of generated file paths
     * @throws IOException if file writing fails
     */
    public List<Path> generateJobConfiguration(JCLJob jclJob, Path outputDir) throws IOException {
        logger.info("Generating Spring Batch configuration for job: {}", jclJob.getJobName());

        List<Path> generatedFiles = new ArrayList<>();

        // Create package directory
        String packagePath = basePackage.replace('.', '/');
        Path packageDir = outputDir.resolve(packagePath);
        Files.createDirectories(packageDir);

        // Generate main Job configuration class
        Path jobConfigFile = generateMainJobConfig(jclJob, packageDir);
        generatedFiles.add(jobConfigFile);

        // Generate ItemReader/ItemWriter for each step
        for (JCLStep step : jclJob.getSteps()) {
            Path readerFile = generateStepReader(step, packageDir);
            Path writerFile = generateStepWriter(step, packageDir);
            Path processorFile = generateStepProcessor(step, packageDir);

            if (readerFile != null) generatedFiles.add(readerFile);
            if (writerFile != null) generatedFiles.add(writerFile);
            if (processorFile != null) generatedFiles.add(processorFile);
        }

        logger.info("Generated {} files for JCL job '{}'", generatedFiles.size(), jclJob.getJobName());
        return generatedFiles;
    }

    /**
     * Generate main Job configuration class
     */
    private Path generateMainJobConfig(JCLJob jclJob, Path packageDir) throws IOException {
        String className = toPascalCase(jclJob.getJobName()) + "JobConfig";
        String content = generateJobConfigClass(jclJob, className);

        Path file = packageDir.resolve(className + ".java");
        Files.writeString(file, content);

        return file;
    }

    private String generateJobConfigClass(JCLJob jclJob, String className) {
        StringBuilder sb = new StringBuilder();

        // Package and imports
        sb.append("package ").append(basePackage).append(";\n\n");
        sb.append("import org.springframework.batch.core.Job;\n");
        sb.append("import org.springframework.batch.core.Step;\n");
        sb.append("import org.springframework.batch.core.job.builder.JobBuilder;\n");
        sb.append("import org.springframework.batch.core.repository.JobRepository;\n");
        sb.append("import org.springframework.context.annotation.Bean;\n");
        sb.append("import org.springframework.context.annotation.Configuration;\n");
        sb.append("import org.springframework.beans.factory.annotation.Autowired;\n\n");

        // Class declaration
        sb.append("/**\n");
        sb.append(" * Spring Batch Job configuration for JCL job: ").append(jclJob.getJobName()).append("\n");
        sb.append(" * Auto-generated from JCL\n");
        sb.append(" */\n");
        sb.append("@Configuration\n");
        sb.append("public class ").append(className).append(" {\n\n");

        // Job bean
        sb.append("    @Bean\n");
        sb.append("    public Job ").append(toCamelCase(jclJob.getJobName())).append("Job(\n");
        sb.append("            JobRepository jobRepository");

        // Add step dependencies
        for (int i = 0; i < jclJob.getSteps().size(); i++) {
            JCLStep step = jclJob.getSteps().get(i);
            sb.append(",\n            Step ").append(toCamelCase(step.getStepName())).append("Step");
        }

        sb.append(") {\n");
        sb.append("        return new JobBuilder(\"").append(toCamelCase(jclJob.getJobName())).append("Job\", jobRepository)\n");

        // Chain steps
        for (int i = 0; i < jclJob.getSteps().size(); i++) {
            JCLStep step = jclJob.getSteps().get(i);
            if (i == 0) {
                sb.append("                .start(").append(toCamelCase(step.getStepName())).append("Step)\n");
            } else {
                sb.append("                .next(").append(toCamelCase(step.getStepName())).append("Step)\n");
            }
        }

        sb.append("                .build();\n");
        sb.append("    }\n\n");

        // Generate comment about manual implementation needed
        sb.append("    // TODO: Implement Step beans\n");
        sb.append("    // For each step, you need to create:\n");
        sb.append("    // - ItemReader (configured from DD input files)\n");
        sb.append("    // - ItemProcessor (business logic from COBOL program)\n");
        sb.append("    // - ItemWriter (configured from DD output files)\n\n");

        for (JCLStep step : jclJob.getSteps()) {
            sb.append("    // Step: ").append(step.getStepName()).append("\n");
            sb.append("    // Program: ").append(step.getProgramName()).append("\n");

            // List input files
            sb.append("    // Input files:\n");
            for (DDStatement dd : step.getDdStatements()) {
                if (dd.isInputFile()) {
                    sb.append("    //   - ").append(dd.getDdName()).append(": ").append(dd.getDatasetName()).append("\n");
                }
            }

            // List output files
            sb.append("    // Output files:\n");
            for (DDStatement dd : step.getDdStatements()) {
                if (dd.isOutputFile()) {
                    sb.append("    //   - ").append(dd.getDdName()).append(": ").append(dd.getDatasetName()).append("\n");
                }
            }
            sb.append("\n");
        }

        sb.append("}\n");

        return sb.toString();
    }

    /**
     * Generate ItemReader for a step
     */
    private Path generateStepReader(JCLStep step, Path packageDir) throws IOException {
        // Find input DD statements
        List<DDStatement> inputDDs = new ArrayList<>();
        for (DDStatement dd : step.getDdStatements()) {
            if (dd.isInputFile() && !dd.isDummy()) {
                inputDDs.add(dd);
            }
        }

        if (inputDDs.isEmpty()) {
            return null; // No input files for this step
        }

        String className = toPascalCase(step.getStepName()) + "Reader";
        String content = generateReaderClass(step, inputDDs.get(0), className);

        Path file = packageDir.resolve(className + ".java");
        Files.writeString(file, content);

        return file;
    }

    private String generateReaderClass(JCLStep step, DDStatement inputDD, String className) {
        StringBuilder sb = new StringBuilder();

        sb.append("package ").append(basePackage).append(";\n\n");
        sb.append("import org.springframework.batch.item.file.FlatFileItemReader;\n");
        sb.append("import org.springframework.batch.item.file.builder.FlatFileItemReaderBuilder;\n");
        sb.append("import org.springframework.core.io.FileSystemResource;\n");
        sb.append("import org.springframework.context.annotation.Bean;\n");
        sb.append("import org.springframework.stereotype.Component;\n\n");

        sb.append("/**\n");
        sb.append(" * ItemReader for step: ").append(step.getStepName()).append("\n");
        sb.append(" * Reads from JCL DD: ").append(inputDD.getDdName()).append("\n");
        sb.append(" * Dataset: ").append(inputDD.getDatasetName()).append("\n");
        sb.append(" */\n");
        sb.append("@Component\n");
        sb.append("public class ").append(className).append(" {\n\n");

        sb.append("    @Bean\n");
        sb.append("    public FlatFileItemReader<String> ").append(toCamelCase(step.getStepName())).append("Reader() {\n");
        sb.append("        return new FlatFileItemReaderBuilder<String>()\n");
        sb.append("                .name(\"").append(toCamelCase(step.getStepName())).append("Reader\")\n");
        sb.append("                .resource(new FileSystemResource(\"data/input/").append(inputDD.getDatasetName()).append("\"))\n");

        // Add LRECL if available
        if (inputDD.getDcbInfo().getLrecl() != null) {
            sb.append("                // Record length: ").append(inputDD.getDcbInfo().getLrecl()).append(" bytes\n");
        }

        // Add RECFM if available
        if (inputDD.getDcbInfo().getRecfm() != null) {
            sb.append("                // Record format: ").append(inputDD.getDcbInfo().getRecfm()).append("\n");
        }

        sb.append("                .lineMapper((line, lineNumber) -> line)\n");
        sb.append("                .build();\n");
        sb.append("    }\n");
        sb.append("}\n");

        return sb.toString();
    }

    /**
     * Generate ItemWriter for a step
     */
    private Path generateStepWriter(JCLStep step, Path packageDir) throws IOException {
        // Find output DD statements
        List<DDStatement> outputDDs = new ArrayList<>();
        for (DDStatement dd : step.getDdStatements()) {
            if (dd.isOutputFile() && !dd.isDummy()) {
                outputDDs.add(dd);
            }
        }

        if (outputDDs.isEmpty()) {
            return null; // No output files for this step
        }

        String className = toPascalCase(step.getStepName()) + "Writer";
        String content = generateWriterClass(step, outputDDs.get(0), className);

        Path file = packageDir.resolve(className + ".java");
        Files.writeString(file, content);

        return file;
    }

    private String generateWriterClass(JCLStep step, DDStatement outputDD, String className) {
        StringBuilder sb = new StringBuilder();

        sb.append("package ").append(basePackage).append(";\n\n");
        sb.append("import org.springframework.batch.item.file.FlatFileItemWriter;\n");
        sb.append("import org.springframework.batch.item.file.builder.FlatFileItemWriterBuilder;\n");
        sb.append("import org.springframework.core.io.FileSystemResource;\n");
        sb.append("import org.springframework.context.annotation.Bean;\n");
        sb.append("import org.springframework.stereotype.Component;\n\n");

        sb.append("/**\n");
        sb.append(" * ItemWriter for step: ").append(step.getStepName()).append("\n");
        sb.append(" * Writes to JCL DD: ").append(outputDD.getDdName()).append("\n");
        sb.append(" * Dataset: ").append(outputDD.getDatasetName()).append("\n");
        sb.append(" */\n");
        sb.append("@Component\n");
        sb.append("public class ").append(className).append(" {\n\n");

        sb.append("    @Bean\n");
        sb.append("    public FlatFileItemWriter<String> ").append(toCamelCase(step.getStepName())).append("Writer() {\n");
        sb.append("        return new FlatFileItemWriterBuilder<String>()\n");
        sb.append("                .name(\"").append(toCamelCase(step.getStepName())).append("Writer\")\n");
        sb.append("                .resource(new FileSystemResource(\"data/output/").append(outputDD.getDatasetName()).append("\"))\n");

        // Add LRECL if available
        if (outputDD.getDcbInfo().getLrecl() != null) {
            sb.append("                // Record length: ").append(outputDD.getDcbInfo().getLrecl()).append(" bytes\n");
        }

        sb.append("                .lineAggregator(item -> item)\n");
        sb.append("                .build();\n");
        sb.append("    }\n");
        sb.append("}\n");

        return sb.toString();
    }

    /**
     * Generate ItemProcessor stub for a step
     */
    private Path generateStepProcessor(JCLStep step, Path packageDir) throws IOException {
        String className = toPascalCase(step.getStepName()) + "Processor";
        String content = generateProcessorClass(step, className);

        Path file = packageDir.resolve(className + ".java");
        Files.writeString(file, content);

        return file;
    }

    private String generateProcessorClass(JCLStep step, String className) {
        StringBuilder sb = new StringBuilder();

        sb.append("package ").append(basePackage).append(";\n\n");
        sb.append("import org.springframework.batch.item.ItemProcessor;\n");
        sb.append("import org.springframework.stereotype.Component;\n\n");

        sb.append("/**\n");
        sb.append(" * ItemProcessor for step: ").append(step.getStepName()).append("\n");
        sb.append(" * Implements business logic from program: ").append(step.getProgramName()).append("\n");
        sb.append(" */\n");
        sb.append("@Component\n");
        sb.append("public class ").append(className).append(" implements ItemProcessor<String, String> {\n\n");

        sb.append("    @Override\n");
        sb.append("    public String process(String item) throws Exception {\n");
        sb.append("        // TODO: Implement business logic from COBOL program: ").append(step.getProgramName()).append("\n");
        sb.append("        return item;\n");
        sb.append("    }\n");
        sb.append("}\n");

        return sb.toString();
    }

    // Utility methods

    private String toPascalCase(String input) {
        if (input == null || input.isEmpty()) {
            return input;
        }

        String[] parts = input.split("[-_\\s]+");
        StringBuilder result = new StringBuilder();

        for (String part : parts) {
            if (!part.isEmpty()) {
                result.append(part.substring(0, 1).toUpperCase());
                if (part.length() > 1) {
                    result.append(part.substring(1).toLowerCase());
                }
            }
        }

        return result.toString();
    }

    private String toCamelCase(String input) {
        String pascal = toPascalCase(input);
        if (pascal.isEmpty()) {
            return pascal;
        }
        return pascal.substring(0, 1).toLowerCase() + pascal.substring(1);
    }
}
