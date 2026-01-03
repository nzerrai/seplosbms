package com.cobol.translator.result;

import com.cobol.translator.CobolTranslator.TranslationMetrics;
import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.report.ConversionReport;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Result of a COBOL to Java translation.
 */
public class TranslationResult {

    private final String sourceFile;
    private final boolean success;
    private final String errorMessage;
    private final CobolProgram cobolProgram;
    private final List<File> generatedFiles;
    private final TranslationMetrics metrics;
    private final ConversionReport conversionReport;

    private TranslationResult(Builder builder) {
        this.sourceFile = builder.sourceFile;
        this.success = builder.success;
        this.errorMessage = builder.errorMessage;
        this.cobolProgram = builder.cobolProgram;
        this.generatedFiles = builder.generatedFiles;
        this.metrics = builder.metrics;
        this.conversionReport = builder.conversionReport;
    }

    public static Builder builder() {
        return new Builder();
    }

    // Getters

    public String getSourceFile() { return sourceFile; }
    public boolean isSuccess() { return success; }
    public String getErrorMessage() { return errorMessage; }
    public CobolProgram getCobolProgram() { return cobolProgram; }
    public List<File> getGeneratedFiles() { return generatedFiles; }
    public TranslationMetrics getMetrics() { return metrics; }
    public ConversionReport getConversionReport() { return conversionReport; }

    @Override
    public String toString() {
        if (success) {
            return String.format("Translation SUCCESS: %s -> %d files generated",
                    sourceFile, generatedFiles.size());
        } else {
            return String.format("Translation FAILED: %s - %s",
                    sourceFile, errorMessage);
        }
    }

    public static class Builder {
        private String sourceFile;
        private boolean success;
        private String errorMessage;
        private CobolProgram cobolProgram;
        private List<File> generatedFiles = new ArrayList<>();
        private TranslationMetrics metrics;
        private ConversionReport conversionReport;

        public Builder sourceFile(String val) { sourceFile = val; return this; }
        public Builder success(boolean val) { success = val; return this; }
        public Builder errorMessage(String val) { errorMessage = val; return this; }
        public Builder cobolProgram(CobolProgram val) { cobolProgram = val; return this; }
        public Builder generatedFiles(List<File> val) { generatedFiles = val; return this; }
        public Builder metrics(TranslationMetrics val) { metrics = val; return this; }
        public Builder conversionReport(ConversionReport val) {
            conversionReport = val;
            return this;
        }

        public Builder addGeneratedFile(File file) {
            generatedFiles.add(file);
            return this;
        }

        public Builder addGeneratedFiles(List<File> files) {
            generatedFiles.addAll(files);
            return this;
        }

        public TranslationResult build() {
            return new TranslationResult(this);
        }
    }
}
