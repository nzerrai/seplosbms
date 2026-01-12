package com.cobol.translator.generator;

import com.cobol.translator.report.InferenceReportData;
import java.io.File;
import java.util.Map;

/**
 * Result of processor generation including the generated file, inferred entity fields,
 * and complete inference report data for IHM display.
 * Used to support algorithmic field inference system.
 */
public class ProcessorGenerationResult {
    
    private final File processorFile;
    private final Map<String, String> inferredFields;
    private final String inputRecordType;
    private final InferenceReportData inferenceReportData;
    
    public ProcessorGenerationResult(File processorFile, 
                                    Map<String, String> inferredFields,
                                    String inputRecordType) {
        this(processorFile, inferredFields, inputRecordType, null);
    }
    
    public ProcessorGenerationResult(File processorFile, 
                                    Map<String, String> inferredFields,
                                    String inputRecordType,
                                    InferenceReportData inferenceReportData) {
        this.processorFile = processorFile;
        this.inferredFields = inferredFields;
        this.inputRecordType = inputRecordType;
        this.inferenceReportData = inferenceReportData;
    }
    
    public File getProcessorFile() {
        return processorFile;
    }
    
    public Map<String, String> getInferredFields() {
        return inferredFields;
    }
    
    public String getInputRecordType() {
        return inputRecordType;
    }
    
    public InferenceReportData getInferenceReportData() {
        return inferenceReportData;
    }
    
    public boolean hasInferredFields() {
        return inferredFields != null && !inferredFields.isEmpty();
    }
    
    public boolean hasInferenceReport() {
        return inferenceReportData != null;
    }
}

