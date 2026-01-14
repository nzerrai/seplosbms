package com.cobol.translator.jcl.translator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * Manages JCL temporary datasets (&&TEMP, &&WORK, etc.) in Spring Batch.
 *
 * JCL Temporary Dataset Characteristics:
 * - Prefix with && (double ampersand)
 * - Exist only for the duration of the job
 * - Automatically deleted at job completion
 * - Shared across steps within the same job
 *
 * Spring Batch Implementation:
 * - Use temporary files in OS temp directory
 * - Track via ExecutionContext for cross-step access
 * - Clean up in JobExecutionListener
 */
@Component
public class TemporaryDatasetManager {

    private static final Logger logger = LoggerFactory.getLogger(TemporaryDatasetManager.class);

    private static final String TEMP_DATASET_PREFIX = "springbatch_temp_";
    private static final String TEMP_DIR_KEY = "temp.dataset.directory";

    /**
     * Represents a temporary dataset
     */
    public static class TemporaryDataset {
        private String logicalName;      // e.g., "&&TEMP01"
        private String physicalPath;     // Actual file path
        private boolean persistent;       // Delete on job end?
        private long createdTimestamp;
        private String jobExecutionId;

        public TemporaryDataset(String logicalName, String physicalPath, String jobExecutionId) {
            this.logicalName = logicalName;
            this.physicalPath = physicalPath;
            this.jobExecutionId = jobExecutionId;
            this.persistent = false;
            this.createdTimestamp = System.currentTimeMillis();
        }

        public String getLogicalName() { return logicalName; }
        public String getPhysicalPath() { return physicalPath; }
        public boolean isPersistent() { return persistent; }
        public void setPersistent(boolean persistent) { this.persistent = persistent; }
        public long getCreatedTimestamp() { return createdTimestamp; }
        public String getJobExecutionId() { return jobExecutionId; }

        @Override
        public String toString() {
            return "TemporaryDataset{" +
                   "logical='" + logicalName + '\'' +
                   ", physical='" + physicalPath + '\'' +
                   ", persistent=" + persistent +
                   '}';
        }
    }

    // In-memory registry of temporary datasets
    private final Map<String, TemporaryDataset> datasetRegistry = new HashMap<>();

    /**
     * Creates a temporary dataset for a JCL job
     */
    public TemporaryDataset createTemporaryDataset(String logicalName, String jobExecutionId)
            throws IOException {

        logger.debug("Creating temporary dataset: {} for job {}", logicalName, jobExecutionId);

        // Generate unique physical file name
        String sanitizedName = sanitizeDatasetName(logicalName);
        String uniqueId = UUID.randomUUID().toString().substring(0, 8);
        String fileName = TEMP_DATASET_PREFIX + sanitizedName + "_" + uniqueId + ".dat";

        // Create in system temp directory
        Path tempDir = getTempDirectory();
        Path tempFilePath = tempDir.resolve(fileName);

        // Create the file
        Files.createFile(tempFilePath);
        logger.info("Created temporary dataset: {} -> {}", logicalName, tempFilePath);

        // Register it
        TemporaryDataset dataset = new TemporaryDataset(
            logicalName,
            tempFilePath.toString(),
            jobExecutionId
        );
        datasetRegistry.put(logicalName, dataset);

        return dataset;
    }

    /**
     * Retrieves an existing temporary dataset
     */
    public TemporaryDataset getTemporaryDataset(String logicalName) {
        TemporaryDataset dataset = datasetRegistry.get(logicalName);

        if (dataset == null) {
            logger.warn("Temporary dataset not found: {}", logicalName);
            return null;
        }

        // Verify file still exists
        File file = new File(dataset.getPhysicalPath());
        if (!file.exists()) {
            logger.warn("Temporary dataset file no longer exists: {}", dataset.getPhysicalPath());
            datasetRegistry.remove(logicalName);
            return null;
        }

        return dataset;
    }

    /**
     * Deletes a temporary dataset
     */
    public boolean deleteTemporaryDataset(String logicalName) {
        TemporaryDataset dataset = datasetRegistry.remove(logicalName);

        if (dataset == null) {
            logger.warn("Cannot delete - temporary dataset not found: {}", logicalName);
            return false;
        }

        File file = new File(dataset.getPhysicalPath());
        if (file.exists()) {
            boolean deleted = file.delete();
            if (deleted) {
                logger.info("Deleted temporary dataset: {} ({})", logicalName, dataset.getPhysicalPath());
            } else {
                logger.error("Failed to delete temporary dataset file: {}", dataset.getPhysicalPath());
            }
            return deleted;
        }

        return true;
    }

    /**
     * Cleans up all temporary datasets for a job
     */
    public void cleanupJobDatasets(String jobExecutionId) {
        logger.info("Cleaning up temporary datasets for job: {}", jobExecutionId);

        int cleanedCount = 0;
        int errorCount = 0;

        for (Map.Entry<String, TemporaryDataset> entry : datasetRegistry.entrySet()) {
            TemporaryDataset dataset = entry.getValue();

            if (jobExecutionId.equals(dataset.getJobExecutionId()) && !dataset.isPersistent()) {
                File file = new File(dataset.getPhysicalPath());
                if (file.exists()) {
                    boolean deleted = file.delete();
                    if (deleted) {
                        cleanedCount++;
                        logger.debug("Cleaned up: {}", dataset.getLogicalName());
                    } else {
                        errorCount++;
                        logger.error("Failed to clean up: {}", dataset.getPhysicalPath());
                    }
                }
            }
        }

        // Remove from registry
        datasetRegistry.entrySet().removeIf(entry ->
            jobExecutionId.equals(entry.getValue().getJobExecutionId()) &&
            !entry.getValue().isPersistent()
        );

        logger.info("Cleanup complete: {} datasets cleaned, {} errors", cleanedCount, errorCount);
    }

    /**
     * Generates Spring Batch ExecutionContext key for temp dataset
     */
    public static String getExecutionContextKey(String logicalName) {
        return "temp.dataset." + sanitizeDatasetName(logicalName);
    }

    /**
     * Generates code to access temporary dataset in Spring Batch Step
     */
    public String generateDatasetAccessCode(String logicalName, boolean isInput) {
        StringBuilder code = new StringBuilder();

        String sanitizedName = sanitizeDatasetName(logicalName);
        String variableName = sanitizedName.toLowerCase() + "Path";

        code.append("        // Access temporary dataset: ").append(logicalName).append("\n");
        code.append("        ExecutionContext executionContext = chunkContext.getStepContext()\n");
        code.append("            .getStepExecution().getJobExecution().getExecutionContext();\n\n");

        code.append("        String ").append(variableName).append(" = executionContext.getString(\"")
            .append(getExecutionContextKey(logicalName)).append("\");\n\n");

        code.append("        if (").append(variableName).append(" == null) {\n");
        if (isInput) {
            code.append("            throw new IllegalStateException(\"Temporary dataset not found: ")
                .append(logicalName).append("\");\n");
        } else {
            code.append("            // Create new temporary dataset\n");
            code.append("            ").append(variableName).append(" = temporaryDatasetManager\n");
            code.append("                .createTemporaryDataset(\"").append(logicalName).append("\", \n");
            code.append("                    String.valueOf(chunkContext.getStepContext().getJobExecutionId()))\n");
            code.append("                .getPhysicalPath();\n");
            code.append("            executionContext.putString(\"")
                .append(getExecutionContextKey(logicalName)).append("\", ").append(variableName).append(");\n");
        }
        code.append("        }\n\n");

        code.append("        logger.info(\"Using temporary dataset {} at path: {}\", \"")
            .append(logicalName).append("\", ").append(variableName).append(");\n");

        return code.toString();
    }

    /**
     * Generates JobExecutionListener to cleanup temp datasets
     */
    public String generateCleanupListenerCode(String packageName) {
        StringBuilder code = new StringBuilder();

        code.append("package ").append(packageName).append(";\n\n");
        code.append("import org.springframework.batch.core.JobExecution;\n");
        code.append("import org.springframework.batch.core.JobExecutionListener;\n");
        code.append("import org.springframework.beans.factory.annotation.Autowired;\n");
        code.append("import org.springframework.stereotype.Component;\n");
        code.append("import org.slf4j.Logger;\n");
        code.append("import org.slf4j.LoggerFactory;\n");
        code.append("import com.cobol.translator.jcl.translator.TemporaryDatasetManager;\n\n");

        code.append("/**\n");
        code.append(" * Cleans up JCL temporary datasets (&&TEMP) at job completion\n");
        code.append(" */\n");
        code.append("@Component\n");
        code.append("public class TemporaryDatasetCleanupListener implements JobExecutionListener {\n\n");

        code.append("    private static final Logger logger = LoggerFactory.getLogger(\n");
        code.append("        TemporaryDatasetCleanupListener.class);\n\n");

        code.append("    @Autowired\n");
        code.append("    private TemporaryDatasetManager temporaryDatasetManager;\n\n");

        code.append("    @Override\n");
        code.append("    public void beforeJob(JobExecution jobExecution) {\n");
        code.append("        logger.info(\"Job {} starting - temporary datasets will be managed\",\n");
        code.append("            jobExecution.getJobId());\n");
        code.append("    }\n\n");

        code.append("    @Override\n");
        code.append("    public void afterJob(JobExecution jobExecution) {\n");
        code.append("        logger.info(\"Job {} completed - cleaning up temporary datasets\",\n");
        code.append("            jobExecution.getJobId());\n\n");

        code.append("        try {\n");
        code.append("            temporaryDatasetManager.cleanupJobDatasets(\n");
        code.append("                String.valueOf(jobExecution.getJobId()));\n");
        code.append("        } catch (Exception e) {\n");
        code.append("            logger.error(\"Error cleaning up temporary datasets\", e);\n");
        code.append("        }\n");
        code.append("    }\n");
        code.append("}\n");

        return code.toString();
    }

    /**
     * Generates configuration code for temporary dataset handling
     */
    public String generateConfigurationCode(String datasetName, boolean createNew) {
        StringBuilder code = new StringBuilder();

        String sanitizedName = sanitizeDatasetName(datasetName);

        if (createNew) {
            code.append("        // Create temporary dataset: ").append(datasetName).append("\n");
            code.append("        String ").append(sanitizedName.toLowerCase()).append("Path = \n");
            code.append("            temporaryDatasetManager.createTemporaryDataset(\n");
            code.append("                \"").append(datasetName).append("\",\n");
            code.append("                String.valueOf(jobExecution.getJobId())\n");
            code.append("            ).getPhysicalPath();\n\n");

            code.append("        // Store in ExecutionContext for cross-step access\n");
            code.append("        jobExecution.getExecutionContext().putString(\n");
            code.append("            TemporaryDatasetManager.getExecutionContextKey(\"").append(datasetName).append("\"),\n");
            code.append("            ").append(sanitizedName.toLowerCase()).append("Path\n");
            code.append("        );\n");
        } else {
            code.append("        // Retrieve existing temporary dataset: ").append(datasetName).append("\n");
            code.append("        String ").append(sanitizedName.toLowerCase()).append("Path = \n");
            code.append("            jobExecution.getExecutionContext().getString(\n");
            code.append("                TemporaryDatasetManager.getExecutionContextKey(\"")
                .append(datasetName).append("\")\n");
            code.append("            );\n\n");

            code.append("        if (").append(sanitizedName.toLowerCase()).append("Path == null) {\n");
            code.append("            throw new IllegalStateException(\n");
            code.append("                \"Temporary dataset ").append(datasetName)
                .append(" not found in execution context\");\n");
            code.append("        }\n");
        }

        return code.toString();
    }

    private Path getTempDirectory() throws IOException {
        String tempDirProperty = System.getProperty(TEMP_DIR_KEY);

        if (tempDirProperty != null) {
            Path customTempDir = Paths.get(tempDirProperty);
            if (!Files.exists(customTempDir)) {
                Files.createDirectories(customTempDir);
            }
            return customTempDir;
        }

        // Use system temp directory
        return Paths.get(System.getProperty("java.io.tmpdir"));
    }

    private static String sanitizeDatasetName(String name) {
        // Remove && prefix and sanitize
        String sanitized = name.replaceAll("^&+", "");
        return sanitized.replaceAll("[^a-zA-Z0-9_]", "_");
    }

    /**
     * Get all registered temporary datasets (for monitoring/debugging)
     */
    public Map<String, TemporaryDataset> getAllDatasets() {
        return new HashMap<>(datasetRegistry);
    }

    /**
     * Check if a dataset name represents a temporary dataset
     */
    public static boolean isTemporaryDataset(String datasetName) {
        return datasetName != null && datasetName.startsWith("&&");
    }

    /**
     * Clean up orphaned temporary files (for maintenance)
     */
    public int cleanupOrphanedFiles() throws IOException {
        logger.info("Scanning for orphaned temporary dataset files");

        Path tempDir = getTempDirectory();
        File[] files = tempDir.toFile().listFiles((dir, name) ->
            name.startsWith(TEMP_DATASET_PREFIX));

        if (files == null) {
            return 0;
        }

        int deletedCount = 0;
        long cutoffTime = System.currentTimeMillis() - (24 * 60 * 60 * 1000); // 24 hours

        for (File file : files) {
            if (file.lastModified() < cutoffTime) {
                if (file.delete()) {
                    deletedCount++;
                    logger.debug("Deleted orphaned file: {}", file.getName());
                }
            }
        }

        logger.info("Cleaned up {} orphaned temporary dataset files", deletedCount);
        return deletedCount;
    }
}
