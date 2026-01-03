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
 * Generates Spring Batch job configuration.
 */
public class JobConfigGenerator {

    private static final Logger logger = LoggerFactory.getLogger(JobConfigGenerator.class);

    public File generate(CobolProgram program, TranslationConfig config, Path outputDir) throws IOException {

        String configName = toJavaClassName(program.getProgramName()) +
                          config.getNamingJobSuffix() + "Configuration";
        File outputFile = outputDir.resolve(configName + ".java").toFile();

        logger.info("Generating job configuration: {}", configName);

        StringBuilder code = new StringBuilder();

        // Package - derive from output directory structure
        String packageName = derivePackageFromPath(outputDir);
        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import org.springframework.batch.core.Job;\n");
        code.append("import org.springframework.batch.core.Step;\n");
        code.append("import org.springframework.batch.core.job.builder.JobBuilder;\n");
        code.append("import org.springframework.batch.core.repository.JobRepository;\n");
        code.append("import org.springframework.batch.core.step.builder.StepBuilder;\n");
        code.append("import org.springframework.context.annotation.Bean;\n");
        code.append("import org.springframework.context.annotation.Configuration;\n");
        code.append("import org.springframework.transaction.PlatformTransactionManager;\n\n");

        // Javadoc
        code.append("/**\n");
        code.append(" * Spring Batch job configuration for COBOL program: ")
            .append(program.getProgramName()).append("\n");
        code.append(" */\n");

        // Class declaration
        code.append("@Configuration\n");
        code.append("public class ").append(configName).append(" {\n\n");

        String jobName = toJavaFieldName(program.getProgramName()) + config.getNamingJobSuffix();

        // Job bean
        code.append("    @Bean\n");
        code.append("    public Job ").append(jobName)
            .append("(JobRepository jobRepository, Step ").append(jobName).append("Step) {\n");
        code.append("        return new JobBuilder(\"").append(jobName).append("\", jobRepository)\n");
        code.append("                .start(").append(jobName).append("Step)\n");
        code.append("                .build();\n");
        code.append("    }\n\n");

        // Step bean
        code.append("    @Bean\n");
        code.append("    public Step ").append(jobName).append("Step(\n");
        code.append("            JobRepository jobRepository,\n");
        code.append("            PlatformTransactionManager transactionManager) {\n");
        code.append("        return new StepBuilder(\"").append(jobName).append("Step\", jobRepository)\n");
        code.append("                .<Record, Record>chunk(100, transactionManager)\n");
        code.append("                // TODO: Add reader, processor, writer\n");
        code.append("                .build();\n");
        code.append("    }\n");

        // Close class
        code.append("}\n");

        // Write to file
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write(code.toString());
        }

        logger.info("Generated: {}", outputFile.getAbsolutePath());
        return outputFile;
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

    private String toJavaFieldName(String cobolName) {
        String[] parts = cobolName.toLowerCase().split("-");
        StringBuilder result = new StringBuilder(parts[0]);
        for (int i = 1; i < parts.length; i++) {
            result.append(Character.toUpperCase(parts[i].charAt(0)));
            result.append(parts[i].substring(1));
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
