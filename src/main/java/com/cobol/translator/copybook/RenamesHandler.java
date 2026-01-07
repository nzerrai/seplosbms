package com.cobol.translator.copybook;

import com.cobol.translator.model.DataItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Handles COBOL level 66 (RENAMES) clauses.
 * RENAMES allows creating an alias for a range of data items.
 */
public class RenamesHandler {
    
    private static final Logger logger = LoggerFactory.getLogger(RenamesHandler.class);
    
    // Pattern to match 66 level items
    private static final Pattern RENAMES_PATTERN = Pattern.compile(
        "\\s*66\\s+([A-Z0-9-]+)\\s+RENAMES\\s+([A-Z0-9-]+)(?:\\s+(?:THROUGH|THRU)\\s+([A-Z0-9-]+))?\\s*\\.",
        Pattern.CASE_INSENSITIVE
    );
    
    /**
     * Process RENAMES clauses in COBOL source
     * @param sourceCode COBOL source code
     * @param dataItems Parsed data items
     * @return Map of rename aliases to their target ranges
     */
    public Map<String, RenameInfo> processRenames(String sourceCode, List<DataItem> dataItems) {
        Map<String, RenameInfo> renames = new LinkedHashMap<>();
        Matcher matcher = RENAMES_PATTERN.matcher(sourceCode);
        
        while (matcher.find()) {
            String renameName = matcher.group(1);
            String firstField = matcher.group(2);
            String lastField = matcher.group(3); // May be null
            
            RenameInfo info = new RenameInfo(renameName, firstField, lastField);
            renames.put(renameName, info);
            
            logger.debug("Found RENAMES: {} -> {} {}",
                renameName, firstField,
                (lastField != null ? "THRU " + lastField : ""));
        }
        
        return renames;
    }
    
    /**
     * Generate Java field for a RENAMES clause
     */
    public String generateJavaField(RenameInfo rename, List<DataItem> dataItems) {
        StringBuilder java = new StringBuilder();
        
        java.append("    /**\n");
        java.append("     * COBOL RENAMES: ").append(rename.getRenameName());
        java.append(" RENAMES ").append(rename.getFirstField());
        if (rename.getLastField() != null) {
            java.append(" THRU ").append(rename.getLastField());
        }
        java.append("\n");
        java.append("     * This is a view over multiple fields\n");
        java.append("     */\n");
        
        // Generate getter that combines the renamed fields
        String methodName = toJavaMethodName(rename.getRenameName());
        java.append("    public String get").append(methodName).append("() {\n");
        java.append("        // Combine fields: ").append(rename.getFirstField());
        if (rename.getLastField() != null) {
            java.append(" through ").append(rename.getLastField());
        }
        java.append("\n");
        java.append("        // TODO: Implement field concatenation\n");
        java.append("        return \"\";\n");
        java.append("    }\n\n");
        
        return java.toString();
    }
    
    /**
     * Convert COBOL name to Java method name
     */
    private String toJavaMethodName(String cobolName) {
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;
        
        for (char c : cobolName.toCharArray()) {
            if (c == '-') {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                result.append(Character.toLowerCase(c));
            }
        }
        
        return result.toString();
    }
    
    /**
     * Information about a RENAMES clause
     */
    public static class RenameInfo {
        private final String renameName;
        private final String firstField;
        private final String lastField; // May be null for single field
        
        public RenameInfo(String renameName, String firstField, String lastField) {
            this.renameName = renameName;
            this.firstField = firstField;
            this.lastField = lastField;
        }
        
        public String getRenameName() {
            return renameName;
        }
        
        public String getFirstField() {
            return firstField;
        }
        
        public String getLastField() {
            return lastField;
        }
        
        public boolean isRange() {
            return lastField != null;
        }
    }
}
