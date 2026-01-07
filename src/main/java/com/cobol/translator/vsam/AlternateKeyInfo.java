package com.cobol.translator.vsam;

/**
 * Information about an alternate key in a VSAM KSDS file
 */
public class AlternateKeyInfo {
    
    private final String keyName;
    private final boolean allowDuplicates;
    
    public AlternateKeyInfo(String keyName, boolean allowDuplicates) {
        this.keyName = keyName;
        this.allowDuplicates = allowDuplicates;
    }
    
    public String getKeyName() {
        return keyName;
    }
    
    public boolean isAllowDuplicates() {
        return allowDuplicates;
    }
    
    @Override
    public String toString() {
        return keyName + (allowDuplicates ? " (with duplicates)" : " (unique)");
    }
}
