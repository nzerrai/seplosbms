package com.cobol.translator.model;

/**
 * Represents a COBOL file definition (FD).
 */
public class FileDefinition {

    private String fileName;
    private String physicalName;
    private String organization;  // SEQUENTIAL, INDEXED, RELATIVE
    private String accessMode;    // SEQUENTIAL, RANDOM, DYNAMIC
    private DataItem recordLayout; // 01 level record structure

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

    @Override
    public String toString() {
        return "FileDefinition{fileName='" + fileName + "'}";
    }
}
