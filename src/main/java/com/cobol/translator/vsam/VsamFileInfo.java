package com.cobol.translator.vsam;

import java.util.ArrayList;
import java.util.List;

/**
 * Information about a VSAM file extracted from COBOL
 */
public class VsamFileInfo {
    
    private String fileName;
    private VsamFileAnalyzer.VsamType vsamType;
    private String accessMode;
    private String primaryKey;
    private List<AlternateKeyInfo> alternateKeys;
    
    public VsamFileInfo() {
        this.alternateKeys = new ArrayList<>();
    }
    
    public String getFileName() {
        return fileName;
    }
    
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }
    
    public VsamFileAnalyzer.VsamType getVsamType() {
        return vsamType;
    }
    
    public void setVsamType(VsamFileAnalyzer.VsamType vsamType) {
        this.vsamType = vsamType;
    }
    
    public String getAccessMode() {
        return accessMode;
    }
    
    public void setAccessMode(String accessMode) {
        this.accessMode = accessMode;
    }
    
    public String getPrimaryKey() {
        return primaryKey;
    }
    
    public void setPrimaryKey(String primaryKey) {
        this.primaryKey = primaryKey;
    }
    
    public List<AlternateKeyInfo> getAlternateKeys() {
        return alternateKeys;
    }
    
    public void addAlternateKey(AlternateKeyInfo keyInfo) {
        this.alternateKeys.add(keyInfo);
    }
    
    public boolean hasAlternateKeys() {
        return !alternateKeys.isEmpty();
    }
    
    public boolean isIndexed() {
        return vsamType == VsamFileAnalyzer.VsamType.KSDS;
    }
}
