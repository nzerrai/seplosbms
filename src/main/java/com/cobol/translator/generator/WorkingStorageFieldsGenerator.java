package com.cobol.translator.generator;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Generates Java fields from COBOL WORKING-STORAGE SECTION.
 *
 * This generator creates instance variables in the Processor class
 * for all WORKING-STORAGE variables, enabling access to file status,
 * counters, flags, and other working variables.
 */
public class WorkingStorageFieldsGenerator {

    private static final Logger logger = LoggerFactory.getLogger(WorkingStorageFieldsGenerator.class);

    /**
     * Generates WORKING-STORAGE fields as Java instance variables
     */
    public String generateWorkingStorageFields(CobolProgram program) {
        StringBuilder fields = new StringBuilder();
        List<DataItem> wsItems = findWorkingStorageItems(program);

        if (wsItems.isEmpty()) {
            logger.debug("No WORKING-STORAGE items found for program {}", program.getProgramName());
            return "";
        }

        fields.append("\n    // ========================================\n");
        fields.append("    // WORKING-STORAGE Fields\n");
        fields.append("    // ========================================\n");

        for (DataItem item : wsItems) {
            String javaType = mapCobolTypeToJava(item);
            String fieldName = toJavaFieldName(item.getName());
            String initialValue = getInitialValue(item, javaType);

            // Generate field with comment
            fields.append("    // COBOL: ").append(item.getName());
            if (item.getPictureClause() != null) {
                fields.append(" PIC ").append(item.getPictureClause());
            }
            fields.append("\n");

            fields.append("    private ").append(javaType)
                  .append(" ").append(fieldName)
                  .append(" = ").append(initialValue).append(";\n\n");
        }

        // Generate getters/setters
        fields.append(generateGettersSetters(wsItems));

        logger.info("Generated {} WORKING-STORAGE fields", wsItems.size());
        return fields.toString();
    }

    /**
     * Find all WORKING-STORAGE items
     */
    private List<DataItem> findWorkingStorageItems(CobolProgram program) {
        List<DataItem> wsItems = new ArrayList<>();

        for (DataItem item : program.getDataItems()) {
            if (isWorkingStorageItem(item)) {
                // Only include level 01, 05, and 77 (elementary items)
                // Skip group items that only contain sub-items
                int level = item.getLevel();
                if ((level == 1 || level == 5 || level == 77) &&
                    item.getPictureClause() != null) {
                    wsItems.add(item);
                }
            }
        }

        return wsItems;
    }

    /**
     * Check if item belongs to WORKING-STORAGE SECTION
     */
    private boolean isWorkingStorageItem(DataItem item) {
        // First, check if section metadata is available (preferred method)
        if (item.getSection() != null) {
            return "WORKING-STORAGE".equals(item.getSection());
        }

        // Fallback to heuristics if section metadata is missing
        // (for backwards compatibility with older parsed programs)
        if (item.getPictureClause() == null) {
            return false; // Not an elementary item
        }

        // Skip items that look like file records
        if (isFileDescriptionItem(item)) {
            return false;
        }

        // Check if name suggests it's a working storage variable
        String name = item.getName().toUpperCase();
        if (name.startsWith("WS-") || name.startsWith("W-") ||
            name.contains("-STATUS") || name.contains("-FLAG") ||
            name.contains("-COUNT") || name.contains("-TOTAL") ||
            name.contains("-COUNTER")) {
            return true;
        }

        // Include items at level 77 (they're always WORKING-STORAGE)
        if (item.getLevel() == 77) {
            return true;
        }

        return false;
    }

    /**
     * Check if item is part of a FILE SECTION (FD)
     */
    private boolean isFileDescriptionItem(DataItem item) {
        // First, check section metadata if available
        if (item.getSection() != null) {
            return "FILE".equals(item.getSection());
        }

        // Fallback to heuristic - items in FILE SECTION typically don't need
        // to be in WORKING-STORAGE as they're already in the Record classes
        String name = item.getName().toUpperCase();
        return name.contains("-FILE") || name.contains("-RECORD");
    }

    /**
     * Map COBOL type to Java type based on PICTURE clause
     */
    private String mapCobolTypeToJava(DataItem item) {
        String pic = item.getPictureClause();
        if (pic == null) {
            return "String"; // Default
        }

        pic = pic.toUpperCase().replaceAll("\\s+", "");

        // Numeric with decimals (handles both V and . decimal separators)
        // Patterns: 9V9, 9(5)V9(2), S9V99, S9(11)V99, etc.
        if (pic.contains("V") || pic.contains(".")) {
            // Contains decimal separator - always BigDecimal
            return "java.math.BigDecimal";
        }

        // Numeric without decimals
        if (pic.matches(".*S?9.*") && !pic.contains("X")) {
            // Determine size - count 9s and expand 9(n) notation
            int size = calculateNumericSize(pic);
            if (size <= 4) {
                return "Integer";
            } else if (size <= 9) {
                return "Long";
            } else {
                return "java.math.BigDecimal";
            }
        }

        // Alphabetic or alphanumeric
        return "String";
    }

    /**
     * Calculate total numeric size from PICTURE clause
     * Handles both 999 and 9(n) notation
     */
    private int calculateNumericSize(String pic) {
        int total = 0;

        // Pattern: 9(n) - extract repetition count
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile("9\\((\\d+)\\)");
        java.util.regex.Matcher matcher = pattern.matcher(pic);

        // Sum all 9(n) occurrences
        while (matcher.find()) {
            total += Integer.parseInt(matcher.group(1));
        }

        // Remove 9(n) patterns and count remaining explicit 9s
        String remaining = pic.replaceAll("9\\(\\d+\\)", "");
        total += remaining.chars().filter(ch -> ch == '9').count();

        return total;
    }

    /**
     * Convert COBOL field name to Java field name
     */
    private String toJavaFieldName(String cobolName) {
        // Remove leading/trailing spaces
        String name = cobolName.trim();

        // Convert to camelCase
        StringBuilder result = new StringBuilder();
        boolean nextUpper = false;

        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);

            if (c == '-' || c == '_') {
                nextUpper = true;
            } else if (nextUpper) {
                result.append(Character.toUpperCase(c));
                nextUpper = false;
            } else {
                result.append(Character.toLowerCase(c));
            }
        }

        return result.toString();
    }

    /**
     * Get initial value for a field based on its type
     */
    private String getInitialValue(DataItem item, String javaType) {
        // Check if item has explicit VALUE clause
        if (item.getValue() != null && !item.getValue().isEmpty()) {
            String value = item.getValue().trim();

            switch (javaType) {
                case "String":
                    // Remove quotes if present
                    value = value.replaceAll("^['\"]|['\"]$", "");
                    return "\"" + value + "\"";

                case "Integer":
                    return value.replaceAll("[^0-9-]", "");

                case "Long":
                    return value.replaceAll("[^0-9-]", "") + "L";

                case "java.math.BigDecimal":
                    if (value.equals("0") || value.equals("ZERO") || value.equals("ZEROS")) {
                        return "java.math.BigDecimal.ZERO";
                    }
                    // Remove non-numeric characters except dot and minus
                    value = value.replaceAll("[^0-9.-]", "");
                    if (value.isEmpty()) {
                        return "java.math.BigDecimal.ZERO";
                    }
                    return "new java.math.BigDecimal(\"" + value + "\")";
            }
        }

        // Default initial values
        switch (javaType) {
            case "String":
                return "\"\"";
            case "Integer":
                return "0";
            case "Long":
                return "0L";
            case "java.math.BigDecimal":
                return "java.math.BigDecimal.ZERO";
            default:
                return "null";
        }
    }

    /**
     * Generate getters and setters for WORKING-STORAGE fields
     */
    private String generateGettersSetters(List<DataItem> items) {
        StringBuilder code = new StringBuilder();

        code.append("    // ========================================\n");
        code.append("    // WORKING-STORAGE Getters/Setters\n");
        code.append("    // ========================================\n\n");

        for (DataItem item : items) {
            String javaType = mapCobolTypeToJava(item);
            String fieldName = toJavaFieldName(item.getName());
            String capitalizedName = capitalize(fieldName);

            // Getter
            code.append("    public ").append(javaType)
                .append(" get").append(capitalizedName).append("() {\n")
                .append("        return this.").append(fieldName).append(";\n")
                .append("    }\n\n");

            // Setter
            code.append("    public void set").append(capitalizedName)
                .append("(").append(javaType).append(" value) {\n")
                .append("        this.").append(fieldName).append(" = value;\n")
                .append("    }\n\n");
        }

        return code.toString();
    }

    /**
     * Capitalize first letter of a string
     */
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return Character.toUpperCase(str.charAt(0)) + str.substring(1);
    }
}
