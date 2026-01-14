package com.cobol.translator.model;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a COBOL file definition (FD) with complete metadata.
 * Extended to support all FD clauses and improve code generation.
 */
public class FileDefinition {

    private String fileName;
    private String physicalName;
    private String organization;  // SEQUENTIAL, INDEXED, RELATIVE, LINE SEQUENTIAL
    private String accessMode;    // SEQUENTIAL, RANDOM, DYNAMIC
    private DataItem recordLayout; // 01 level record structure
    
    // FD Clause metadata
    private String labelRecords;  // STANDARD, OMITTED
    private String recordKey;     // For INDEXED files
    private String fileStatus;    // FILE STATUS variable name
    private Integer minRecordLength;
    private Integer maxRecordLength;
    private Integer blockSizeBytes;
    private Integer blockSizeRecords;
    private boolean fixedLength = true;
    
    // Additional metadata
    private final Map<String, String> metadata = new HashMap<>();

    public FileDefinition() {
    }

    public FileDefinition(String fileName) {
        this.fileName = fileName;
    }

    // Getters and Setters

    public String getFileName() { return fileName; }
    public void setFileName(String fileName) { this.fileName = fileName; }

    public String getPhysicalName() { return physicalName; }
    public void setPhysicalName(String physicalName) { this.physicalName = physicalName; }

    public String getOrganization() { return organization; }
    public void setOrganization(String organization) { this.organization = organization; }

    public String getAccessMode() { return accessMode; }
    public void setAccessMode(String accessMode) { this.accessMode = accessMode; }

    public DataItem getRecordLayout() { return recordLayout; }
    public void setRecordLayout(DataItem recordLayout) { this.recordLayout = recordLayout; }
    
    public String getLabelRecords() { return labelRecords; }
    public void setLabelRecords(String labelRecords) { this.labelRecords = labelRecords; }
    
    public String getRecordKey() { return recordKey; }
    public void setRecordKey(String recordKey) { this.recordKey = recordKey; }
    
    public String getFileStatus() { return fileStatus; }
    public void setFileStatus(String fileStatus) { this.fileStatus = fileStatus; }
    
    public Integer getMinRecordLength() { return minRecordLength; }
    public void setMinRecordLength(Integer minRecordLength) { this.minRecordLength = minRecordLength; }
    
    public Integer getMaxRecordLength() { return maxRecordLength; }
    public void setMaxRecordLength(Integer maxRecordLength) { this.maxRecordLength = maxRecordLength; }
    
    public Integer getBlockSizeBytes() { return blockSizeBytes; }
    public void setBlockSizeBytes(Integer blockSizeBytes) { this.blockSizeBytes = blockSizeBytes; }
    
    public Integer getBlockSizeRecords() { return blockSizeRecords; }
    public void setBlockSizeRecords(Integer blockSizeRecords) { this.blockSizeRecords = blockSizeRecords; }
    
    public boolean isFixedLength() { return fixedLength; }
    public void setFixedLength(boolean fixedLength) { this.fixedLength = fixedLength; }
    
    // Metadata map for additional clauses
    public void addMetadata(String key, String value) {
        metadata.put(key, value);
    }
    
    public String getMetadata(String key) {
        return metadata.get(key);
    }
    
    public Map<String, String> getAllMetadata() {
        return new HashMap<>(metadata);
    }
    
    /**
     * Check if this is an indexed file (VSAM KSDS)
     */
    public boolean isIndexed() {
        return "INDEXED".equalsIgnoreCase(organization);
    }
    
    /**
     * Check if this is a sequential file
     */
    public boolean isSequential() {
        return organization == null || 
               "SEQUENTIAL".equalsIgnoreCase(organization) ||
               "LINE SEQUENTIAL".equalsIgnoreCase(organization);
    }
    
    /**
     * Check if this is a relative file
     */
    public boolean isRelative() {
        return "RELATIVE".equalsIgnoreCase(organization);
    }

    @Override
    public String toString() {
        return "FileDefinition{fileName='" + fileName + "', organization='" + organization + 
               "', fixedLength=" + fixedLength + "}";
    }
}
