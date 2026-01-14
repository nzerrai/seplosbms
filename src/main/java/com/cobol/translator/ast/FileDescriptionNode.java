package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents a file description (FD) entry in the FILE SECTION.
 * Captures all COBOL FD clauses: BLOCK CONTAINS, RECORD CONTAINS, LABEL RECORDS, etc.
 */
public class FileDescriptionNode extends ASTNode {
    private String fileName;
    private final List<DataItemNode> records = new ArrayList<>();
    
    // FD Clauses metadata
    private String blockContains;        // BLOCK CONTAINS n RECORDS/CHARACTERS
    private String recordContains;       // RECORD CONTAINS n CHARACTERS
    private String labelRecords;         // LABEL RECORDS ARE STANDARD/OMITTED
    private String valueOfClause;        // VALUE OF clause
    private String dataRecords;          // DATA RECORDS ARE ...
    private String linageClause;         // LINAGE clause for reports
    private String recordingMode;        // RECORDING MODE clause
    private String codeSetClause;        // CODE-SET clause
    
    // Additional metadata (from SELECT/ASSIGN in Environment Division)
    private String organization;         // SEQUENTIAL, INDEXED, RELATIVE, LINE SEQUENTIAL
    private String accessMode;           // SEQUENTIAL, RANDOM, DYNAMIC
    private String recordKey;            // RECORD KEY for indexed files
    private String alternateKeys;        // ALTERNATE RECORD KEY
    private String fileStatus;           // FILE STATUS variable
    
    // Computed properties
    private Integer minRecordLength;
    private Integer maxRecordLength;
    private Integer blockSize;
    
    // Additional properties for better conversion
    private final Map<String, String> additionalClauses = new HashMap<>();

    public FileDescriptionNode(String fileName) {
        this.fileName = fileName;
    }

    @Override
    public String getNodeType() { return "FileDescription"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitFileDescriptionNode(this);
    }

    public String getFileName() { return fileName; }
    public void setFileName(String fileName) { this.fileName = fileName; }

    public void addRecord(DataItemNode record) {
        records.add(record);
        addChild(record);
    }

    public List<DataItemNode> getRecords() {
        return new ArrayList<>(records);
    }
    
    // FD Clauses getters/setters
    public String getBlockContains() { return blockContains; }
    public void setBlockContains(String blockContains) { this.blockContains = blockContains; }
    
    public String getRecordContains() { return recordContains; }
    public void setRecordContains(String recordContains) { this.recordContains = recordContains; }
    
    public String getLabelRecords() { return labelRecords; }
    public void setLabelRecords(String labelRecords) { this.labelRecords = labelRecords; }
    
    public String getValueOfClause() { return valueOfClause; }
    public void setValueOfClause(String valueOfClause) { this.valueOfClause = valueOfClause; }
    
    public String getDataRecords() { return dataRecords; }
    public void setDataRecords(String dataRecords) { this.dataRecords = dataRecords; }
    
    public String getLinageClause() { return linageClause; }
    public void setLinageClause(String linageClause) { this.linageClause = linageClause; }
    
    public String getRecordingMode() { return recordingMode; }
    public void setRecordingMode(String recordingMode) { this.recordingMode = recordingMode; }
    
    public String getCodeSetClause() { return codeSetClause; }
    public void setCodeSetClause(String codeSetClause) { this.codeSetClause = codeSetClause; }
    
    // Organization/Access metadata
    public String getOrganization() { return organization; }
    public void setOrganization(String organization) { this.organization = organization; }
    
    public String getAccessMode() { return accessMode; }
    public void setAccessMode(String accessMode) { this.accessMode = accessMode; }
    
    public String getRecordKey() { return recordKey; }
    public void setRecordKey(String recordKey) { this.recordKey = recordKey; }
    
    public String getAlternateKeys() { return alternateKeys; }
    public void setAlternateKeys(String alternateKeys) { this.alternateKeys = alternateKeys; }
    
    public String getFileStatus() { return fileStatus; }
    public void setFileStatus(String fileStatus) { this.fileStatus = fileStatus; }
    
    // Computed properties
    public Integer getMinRecordLength() { return minRecordLength; }
    public void setMinRecordLength(Integer minRecordLength) { this.minRecordLength = minRecordLength; }
    
    public Integer getMaxRecordLength() { return maxRecordLength; }
    public void setMaxRecordLength(Integer maxRecordLength) { this.maxRecordLength = maxRecordLength; }
    
    public Integer getBlockSize() { return blockSize; }
    public void setBlockSize(Integer blockSize) { this.blockSize = blockSize; }
    
    // Additional clauses
    public void addClause(String key, String value) {
        additionalClauses.put(key, value);
    }
    
    public String getClause(String key) {
        return additionalClauses.get(key);
    }
    
    public Map<String, String> getAdditionalClauses() {
        return new HashMap<>(additionalClauses);
    }
    
    /**
     * Check if this is a fixed-length file (from RECORD CONTAINS clause)
     */
    public boolean isFixedLength() {
        return recordContains != null && !recordContains.contains("TO");
    }
    
    /**
     * Check if this is a variable-length file
     */
    public boolean isVariableLength() {
        return recordContains != null && recordContains.contains("TO");
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
}
