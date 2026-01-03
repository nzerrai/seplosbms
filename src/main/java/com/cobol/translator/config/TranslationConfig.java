package com.cobol.translator.config;

/**
 * Configuration for COBOL to Java translation.
 */
public class TranslationConfig {

    private final String sourceFile;
    private final String outputPackage;
    private final String targetDirectory;
    private final boolean generateTests;
    private final boolean generateDocs;
    private final boolean generateReport;
    private final String namingEntitySuffix;
    private final String namingProcessorSuffix;
    private final String namingJobSuffix;
    private final CodeStyle codeStyle;

    private TranslationConfig(Builder builder) {
        this.sourceFile = builder.sourceFile;
        this.outputPackage = builder.outputPackage;
        this.targetDirectory = builder.targetDirectory;
        this.generateTests = builder.generateTests;
        this.generateDocs = builder.generateDocs;
        this.generateReport = builder.generateReport;
        this.namingEntitySuffix = builder.namingEntitySuffix;
        this.namingProcessorSuffix = builder.namingProcessorSuffix;
        this.namingJobSuffix = builder.namingJobSuffix;
        this.codeStyle = builder.codeStyle;
    }

    public static Builder builder() {
        return new Builder();
    }

    public Builder toBuilder() {
        return new Builder()
                .sourceFile(this.sourceFile)
                .outputPackage(this.outputPackage)
                .targetDirectory(this.targetDirectory)
                .generateTests(this.generateTests)
                .generateDocs(this.generateDocs)
                .generateReport(this.generateReport)
                .namingEntitySuffix(this.namingEntitySuffix)
                .namingProcessorSuffix(this.namingProcessorSuffix)
                .namingJobSuffix(this.namingJobSuffix)
                .codeStyle(this.codeStyle);
    }

    // Getters
    public String getSourceFile() { return sourceFile; }
    public String getOutputPackage() { return outputPackage; }
    public String getTargetDirectory() { return targetDirectory; }
    public boolean isGenerateTests() { return generateTests; }
    public boolean isGenerateDocs() { return generateDocs; }
    public boolean isGenerateReport() { return generateReport; }
    public String getNamingEntitySuffix() { return namingEntitySuffix; }
    public String getNamingProcessorSuffix() { return namingProcessorSuffix; }
    public String getNamingJobSuffix() { return namingJobSuffix; }
    public CodeStyle getCodeStyle() { return codeStyle; }

    public static class Builder {
        private String sourceFile;
        private String outputPackage = "com.generated.batch";
        private String targetDirectory = "src/main/java";
        private boolean generateTests = true;
        private boolean generateDocs = true;
        private boolean generateReport = true;
        private String namingEntitySuffix = "Record";
        private String namingProcessorSuffix = "Processor";
        private String namingJobSuffix = "Job";
        private CodeStyle codeStyle = CodeStyle.GOOGLE;

        public Builder sourceFile(String val) { sourceFile = val; return this; }
        public Builder outputPackage(String val) { outputPackage = val; return this; }
        public Builder targetDirectory(String val) { targetDirectory = val; return this; }
        public Builder generateTests(boolean val) { generateTests = val; return this; }
        public Builder generateDocs(boolean val) { generateDocs = val; return this; }
        public Builder generateReport(boolean val) { generateReport = val; return this; }
        public Builder namingEntitySuffix(String val) { namingEntitySuffix = val; return this; }
        public Builder namingProcessorSuffix(String val) { namingProcessorSuffix = val; return this; }
        public Builder namingJobSuffix(String val) { namingJobSuffix = val; return this; }
        public Builder codeStyle(CodeStyle val) { codeStyle = val; return this; }

        public TranslationConfig build() {
            if (sourceFile == null || sourceFile.isEmpty()) {
                throw new IllegalArgumentException("Source file is required");
            }
            return new TranslationConfig(this);
        }
    }

    public enum CodeStyle {
        GOOGLE,
        ORACLE,
        SPRING
    }
}
