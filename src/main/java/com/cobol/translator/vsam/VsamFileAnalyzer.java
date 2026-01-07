package com.cobol.translator.vsam;

import com.cobol.translator.model.FileDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Analyzes COBOL VSAM file definitions and extracts metadata for conversion to JDBC/JPA.
 */
public class VsamFileAnalyzer {
    
    private static final Logger logger = LoggerFactory.getLogger(VsamFileAnalyzer.class);
    
    // Pattern to identify VSAM file types
    private static final Pattern ORGANIZATION_PATTERN = Pattern.compile(
        "ORGANIZATION\\s+IS\\s+(INDEXED|SEQUENTIAL|RELATIVE)",
        Pattern.CASE_INSENSITIVE
    );
    
    private static final Pattern ACCESS_MODE_PATTERN = Pattern.compile(
        "ACCESS\\s+MODE\\s+IS\\s+(SEQUENTIAL|RANDOM|DYNAMIC)",
        Pattern.CASE_INSENSITIVE
    );
    
    private static final Pattern RECORD_KEY_PATTERN = Pattern.compile(
        "RECORD\\s+KEY\\s+IS\\s+([A-Z0-9-]+)",
        Pattern.CASE_INSENSITIVE
    );
    
    private static final Pattern ALTERNATE_KEY_PATTERN = Pattern.compile(
        "ALTERNATE\\s+(?:RECORD\\s+)?KEY\\s+IS\\s+([A-Z0-9-]+)(?:\\s+WITH\\s+DUPLICATES)?",
        Pattern.CASE_INSENSITIVE
    );
    
    /**
     * Analyze VSAM file and determine its characteristics
     */
    public VsamFileInfo analyzeVsamFile(FileDefinition fileDefinition, String sourceCode) {
        VsamFileInfo info = new VsamFileInfo();
        info.setFileName(fileDefinition.getFileName());
        
        // Determine VSAM type
        VsamType vsamType = detectVsamType(sourceCode, fileDefinition);
        info.setVsamType(vsamType);
        
        // Extract access mode
        String accessMode = extractAccessMode(sourceCode);
        info.setAccessMode(accessMode);
        
        // Extract keys
        if (vsamType == VsamType.KSDS) {
            extractKeys(sourceCode, info);
        }
        
        logger.info("Analyzed VSAM file: {} (Type: {}, Access: {})",
            fileDefinition.getFileName(), vsamType, accessMode);
        
        return info;
    }
    
    /**
     * Detect VSAM type from file definition
     */
    private VsamType detectVsamType(String sourceCode, FileDefinition fileDefinition) {
        String organization = fileDefinition.getOrganization();
        
        if (organization != null) {
            if (organization.equalsIgnoreCase("INDEXED")) {
                return VsamType.KSDS; // Key-Sequenced Data Set
            } else if (organization.equalsIgnoreCase("SEQUENTIAL")) {
                return VsamType.ESDS; // Entry-Sequenced Data Set
            } else if (organization.equalsIgnoreCase("RELATIVE")) {
                return VsamType.RRDS; // Relative-Record Data Set
            }
        }
        
        // Fallback to pattern matching
        Matcher matcher = ORGANIZATION_PATTERN.matcher(sourceCode);
        if (matcher.find()) {
            String orgType = matcher.group(1).toUpperCase();
            switch (orgType) {
                case "INDEXED":
                    return VsamType.KSDS;
                case "SEQUENTIAL":
                    return VsamType.ESDS;
                case "RELATIVE":
                    return VsamType.RRDS;
            }
        }
        
        return VsamType.ESDS; // Default
    }
    
    /**
     * Extract access mode
     */
    private String extractAccessMode(String sourceCode) {
        Matcher matcher = ACCESS_MODE_PATTERN.matcher(sourceCode);
        if (matcher.find()) {
            return matcher.group(1).toUpperCase();
        }
        return "SEQUENTIAL"; // Default
    }
    
    /**
     * Extract primary and alternate keys
     */
    private void extractKeys(String sourceCode, VsamFileInfo info) {
        // Extract primary key
        Matcher primaryMatcher = RECORD_KEY_PATTERN.matcher(sourceCode);
        if (primaryMatcher.find()) {
            String primaryKey = primaryMatcher.group(1);
            info.setPrimaryKey(primaryKey);
            logger.debug("Found primary key: {}", primaryKey);
        }
        
        // Extract alternate keys
        Matcher altMatcher = ALTERNATE_KEY_PATTERN.matcher(sourceCode);
        while (altMatcher.find()) {
            String altKey = altMatcher.group(1);
            boolean withDuplicates = altMatcher.group(0).toUpperCase().contains("WITH DUPLICATES");
            info.addAlternateKey(new AlternateKeyInfo(altKey, withDuplicates));
            logger.debug("Found alternate key: {} (duplicates: {})", altKey, withDuplicates);
        }
    }
    
    /**
     * VSAM file type enumeration
     */
    public enum VsamType {
        KSDS("Key-Sequenced Data Set"),
        ESDS("Entry-Sequenced Data Set"),
        RRDS("Relative-Record Data Set");
        
        private final String description;
        
        VsamType(String description) {
            this.description = description;
        }
        
        public String getDescription() {
            return description;
        }
    }
}
