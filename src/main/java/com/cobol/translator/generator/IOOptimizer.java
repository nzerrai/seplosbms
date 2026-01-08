package com.cobol.translator.generator;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Optimizes INPUT/OUTPUT section conversion for Spring Batch ItemReader/ItemWriter.
 *
 * Improvements:
 * - Auto-detect file format (delimited, fixed-length, positional)
 * - Generate optimal field mappings
 * - Support for COBOL PICTURE clauses (PIC X, PIC 9, etc.)
 * - Handle COMP-3, BINARY fields
 * - Generate custom line tokenizers for fixed-length records
 */
public class IOOptimizer {

    private static final Logger logger = LoggerFactory.getLogger(IOOptimizer.class);

    /**
     * File format types detected from COBOL FILE SECTION
     */
    public enum FileFormat {
        DELIMITED,           // CSV, pipe-delimited, etc.
        FIXED_LENGTH,        // Fixed-width positional records
        INDEXED_SEQUENTIAL,  // VSAM KSDS
        SEQUENTIAL,          // Plain sequential file
        BINARY               // Binary/COMP-3 fields present
    }

    /**
     * Metadata about a file's structure for optimal I/O generation
     */
    public static class FileIOMetadata {
        private FileFormat format;
        private String delimiter;
        private int recordLength;
        private List<FieldMapping> fieldMappings;
        private boolean hasHeaderRecord;
        private boolean hasBinaryFields;
        private String encoding;

        public FileIOMetadata() {
            this.fieldMappings = new ArrayList<>();
            this.delimiter = ",";  // Default
            this.encoding = "UTF-8";
        }

        // Getters and setters
        public FileFormat getFormat() { return format; }
        public void setFormat(FileFormat format) { this.format = format; }

        public String getDelimiter() { return delimiter; }
        public void setDelimiter(String delimiter) { this.delimiter = delimiter; }

        public int getRecordLength() { return recordLength; }
        public void setRecordLength(int recordLength) { this.recordLength = recordLength; }

        public List<FieldMapping> getFieldMappings() { return fieldMappings; }
        public void setFieldMappings(List<FieldMapping> fieldMappings) { this.fieldMappings = fieldMappings; }

        public boolean isHasHeaderRecord() { return hasHeaderRecord; }
        public void setHasHeaderRecord(boolean hasHeaderRecord) { this.hasHeaderRecord = hasHeaderRecord; }

        public boolean isHasBinaryFields() { return hasBinaryFields; }
        public void setHasBinaryFields(boolean hasBinaryFields) { this.hasBinaryFields = hasBinaryFields; }

        public String getEncoding() { return encoding; }
        public void setEncoding(String encoding) { this.encoding = encoding; }
    }

    /**
     * Mapping of a single field from COBOL to Java
     */
    public static class FieldMapping {
        private String cobolName;
        private String javaFieldName;
        private String pictureClause;
        private String usage;  // DISPLAY, COMP-3, COMP, BINARY
        private int startPosition;  // For fixed-length
        private int length;
        private String javaType;  // String, BigDecimal, Integer, LocalDate, etc.
        private boolean isNumeric;
        private boolean isSigned;
        private int decimalPlaces;

        // Getters and setters
        public String getCobolName() { return cobolName; }
        public void setCobolName(String cobolName) { this.cobolName = cobolName; }

        public String getJavaFieldName() { return javaFieldName; }
        public void setJavaFieldName(String javaFieldName) { this.javaFieldName = javaFieldName; }

        public String getPictureClause() { return pictureClause; }
        public void setPictureClause(String pictureClause) { this.pictureClause = pictureClause; }

        public String getUsage() { return usage; }
        public void setUsage(String usage) { this.usage = usage; }

        public int getStartPosition() { return startPosition; }
        public void setStartPosition(int startPosition) { this.startPosition = startPosition; }

        public int getLength() { return length; }
        public void setLength(int length) { this.length = length; }

        public String getJavaType() { return javaType; }
        public void setJavaType(String javaType) { this.javaType = javaType; }

        public boolean isNumeric() { return isNumeric; }
        public void setNumeric(boolean numeric) { isNumeric = numeric; }

        public boolean isSigned() { return isSigned; }
        public void setSigned(boolean signed) { isSigned = signed; }

        public int getDecimalPlaces() { return decimalPlaces; }
        public void setDecimalPlaces(int decimalPlaces) { this.decimalPlaces = decimalPlaces; }
    }

    /**
     * Analyzes a COBOL file definition and returns optimal I/O metadata
     */
    public FileIOMetadata analyzeFileDefinition(FileDefinition fileDef, CobolProgram program) {
        logger.info("Analyzing file definition: {}", fileDef.getFileName());

        FileIOMetadata metadata = new FileIOMetadata();

        // Determine file format based on COBOL clauses
        metadata.setFormat(detectFileFormat(fileDef));

        // Extract field mappings from record layout
        if (fileDef.getRecordLayout() != null) {
            List<FieldMapping> mappings = extractFieldMappings(
                fileDef.getRecordLayout(),
                program.getDataItems()
            );
            metadata.setFieldMappings(mappings);

            // Calculate total record length
            int totalLength = mappings.stream()
                .mapToInt(FieldMapping::getLength)
                .sum();
            metadata.setRecordLength(totalLength);

            // Check for binary fields
            boolean hasBinary = mappings.stream()
                .anyMatch(m -> "COMP-3".equals(m.getUsage()) ||
                              "COMP".equals(m.getUsage()) ||
                              "BINARY".equals(m.getUsage()));
            metadata.setHasBinaryFields(hasBinary);
        }

        // Set delimiter based on organization
        if (metadata.getFormat() == FileFormat.DELIMITED) {
            metadata.setDelimiter(detectDelimiter(fileDef));
        }

        logger.info("Detected format: {}, record length: {}, fields: {}",
            metadata.getFormat(),
            metadata.getRecordLength(),
            metadata.getFieldMappings().size());

        return metadata;
    }

    /**
     * Detects the file format from COBOL FILE SECTION clauses
     */
    private FileFormat detectFileFormat(FileDefinition fileDef) {
        String organization = fileDef.getOrganization();

        // Use explicit organization metadata if available
        if (organization != null) {
            switch (organization.toUpperCase()) {
                case "INDEXED":
                case "ORGANIZATION IS INDEXED":
                    logger.info("Detected INDEXED file: {}", fileDef.getFileName());
                    return FileFormat.INDEXED_SEQUENTIAL;

                case "SEQUENTIAL":
                case "ORGANIZATION IS SEQUENTIAL":
                    // Check if fixed or variable length
                    if (fileDef.isFixedLength() && fileDef.getMinRecordLength() != null) {
                        logger.info("Detected FIXED LENGTH sequential file: {} (length: {})", 
                                   fileDef.getFileName(), fileDef.getMinRecordLength());
                        return FileFormat.FIXED_LENGTH;
                    }
                    logger.info("Detected SEQUENTIAL file: {}", fileDef.getFileName());
                    return FileFormat.SEQUENTIAL;

                case "LINE SEQUENTIAL":
                case "ORGANIZATION IS LINE SEQUENTIAL":
                    // Line sequential files are typically delimited
                    logger.info("Detected LINE SEQUENTIAL (delimited) file: {}", fileDef.getFileName());
                    return FileFormat.DELIMITED;

                case "RELATIVE":
                case "ORGANIZATION IS RELATIVE":
                    logger.info("Detected RELATIVE file: {}", fileDef.getFileName());
                    return FileFormat.SEQUENTIAL;
            }
        }

        // Check for binary fields (COMP-3, COMP, BINARY)
        if (fileDef.getRecordLayout() != null) {
            boolean hasBinary = containsBinaryFields(fileDef.getRecordLayout());
            if (hasBinary) {
                logger.info("Detected BINARY fields in file: {}", fileDef.getFileName());
                return FileFormat.BINARY;
            }
        }

        // Use RECORD CONTAINS metadata to determine format
        if (fileDef.isFixedLength() && fileDef.getMinRecordLength() != null) {
            logger.info("Detected FIXED LENGTH file: {} (length: {})", 
                       fileDef.getFileName(), fileDef.getMinRecordLength());
            return FileFormat.FIXED_LENGTH;
        }

        // Default to fixed-length if record layout is defined
        if (fileDef.getRecordLayout() != null) {
            logger.info("Defaulting to FIXED LENGTH for: {}", fileDef.getFileName());
            return FileFormat.FIXED_LENGTH;
        }

        // Final fallback
        logger.warn("Could not determine format for {}, defaulting to SEQUENTIAL", fileDef.getFileName());
        return FileFormat.SEQUENTIAL;
    }

    /**
     * Check if a record contains binary fields (COMP-3, COMP, BINARY)
     */
    private boolean containsBinaryFields(DataItem record) {
        if (record.getUsage() != null) {
            String usage = record.getUsage().toUpperCase();
            if (usage.contains("COMP") || usage.contains("BINARY")) {
                return true;
            }
        }

        // Note: DataItem doesn't have a getChildren() method in the current model
        // This would require traversal through all data items separately
        // For now, we check only the top-level record

        return false;
    }

    /**
     * Detects the delimiter for delimited files (default: comma)
     */
    private String detectDelimiter(FileDefinition fileDef) {
        // Check for hints in file name or comments
        String fileName = fileDef.getFileName().toLowerCase();

        if (fileName.contains("csv")) return ",";
        if (fileName.contains("tab") || fileName.contains("tsv")) return "\\t";
        if (fileName.contains("pipe")) return "|";
        if (fileName.contains("semicolon")) return ";";

        // Default to comma
        return ",";
    }

    /**
     * Extracts field mappings from COBOL record layout
     */
    private List<FieldMapping> extractFieldMappings(DataItem recordLayout, List<DataItem> allItems) {
        List<FieldMapping> mappings = new ArrayList<>();

        int parentIdx = allItems.indexOf(recordLayout);
        if (parentIdx == -1) {
            logger.warn("Record layout not found in data items");
            return mappings;
        }

        int currentPosition = 0;

        // Traverse children of record layout
        for (int i = parentIdx + 1; i < allItems.size(); i++) {
            DataItem item = allItems.get(i);

            // Stop at next top-level item
            if (item.getLevel() <= recordLayout.getLevel()) {
                break;
            }

            // Only process elementary fields (skip groups)
            if (item.isElementary() && !item.isFiller()) {
                FieldMapping mapping = createFieldMapping(item, currentPosition);
                mappings.add(mapping);
                currentPosition += mapping.getLength();

                logger.debug("Mapped field: {} -> {} (pos: {}, len: {})",
                    mapping.getCobolName(),
                    mapping.getJavaFieldName(),
                    mapping.getStartPosition(),
                    mapping.getLength());
            } else if (item.isElementary() && item.isFiller()) {
                // Skip filler but advance position
                int fillerLength = calculateLength(item.getPictureClause());
                currentPosition += fillerLength;
            }
        }

        return mappings;
    }

    /**
     * Creates a field mapping from a COBOL DataItem
     */
    private FieldMapping createFieldMapping(DataItem item, int startPosition) {
        FieldMapping mapping = new FieldMapping();

        mapping.setCobolName(item.getName());
        mapping.setJavaFieldName(item.getJavaFieldName());
        mapping.setPictureClause(item.getPictureClause());
        mapping.setUsage(item.getUsage());
        mapping.setStartPosition(startPosition);

        // Calculate length from PICTURE clause
        int length = calculateLength(item.getPictureClause());
        mapping.setLength(length);

        // Determine Java type based on PICTURE and USAGE
        mapping.setJavaType(determineJavaType(item));

        // Parse PICTURE for numeric info
        if (item.getPictureClause() != null) {
            PictureAnalysis analysis = analyzePicture(item.getPictureClause());
            mapping.setNumeric(analysis.isNumeric);
            mapping.setSigned(analysis.isSigned);
            mapping.setDecimalPlaces(analysis.decimalPlaces);
        }

        return mapping;
    }

    /**
     * Determines the optimal Java type for a COBOL field
     */
    private String determineJavaType(DataItem item) {
        String picture = item.getPictureClause();
        String usage = item.getUsage();

        if (picture == null) {
            return "String";
        }

        // Check for date patterns
        if (picture.matches("9\\(8\\)") || picture.matches("PIC 9\\(8\\)")) {
            return "LocalDate";  // Likely YYYYMMDD date
        }

        // Numeric types
        if (picture.contains("9")) {
            // Has decimal places?
            if (picture.contains("V") || picture.contains(".")) {
                return "BigDecimal";
            }

            // Integer types based on size
            int digits = countDigits(picture);
            if (digits <= 4) {
                return "Short";
            } else if (digits <= 9) {
                return "Integer";
            } else if (digits <= 18) {
                return "Long";
            } else {
                return "BigDecimal";
            }
        }

        // Alphanumeric - default to String
        return "String";
    }

    /**
     * Analyzes a PICTURE clause for numeric properties
     */
    private PictureAnalysis analyzePicture(String picture) {
        PictureAnalysis analysis = new PictureAnalysis();

        // Check if numeric
        analysis.isNumeric = picture.contains("9");

        // Check if signed
        analysis.isSigned = picture.contains("S");

        // Count decimal places (after V)
        if (picture.contains("V")) {
            String afterV = picture.substring(picture.indexOf('V') + 1);
            analysis.decimalPlaces = countDigits(afterV);
        }

        return analysis;
    }

    /**
     * Counts the number of digits in a PICTURE clause
     */
    private int countDigits(String picture) {
        int count = 0;

        // Handle 9(n) format
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile("9\\((\\d+)\\)");
        java.util.regex.Matcher matcher = pattern.matcher(picture);

        while (matcher.find()) {
            count += Integer.parseInt(matcher.group(1));
        }

        // Handle individual 9s
        for (char c : picture.toCharArray()) {
            if (c == '9') count++;
        }

        return count;
    }

    /**
     * Calculates the total length of a field from its PICTURE clause
     */
    private int calculateLength(String pictureClause) {
        if (pictureClause == null || pictureClause.isEmpty()) {
            return 0;
        }

        int length = 0;

        // Remove "PIC " or "PICTURE " prefix if present
        String pic = pictureClause.toUpperCase()
            .replaceFirst("^PIC(TURE)?\\s+", "")
            .trim();

        // Handle X(n), 9(n), A(n) format
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile("([X9AS])\\((\\d+)\\)");
        java.util.regex.Matcher matcher = pattern.matcher(pic);

        while (matcher.find()) {
            length += Integer.parseInt(matcher.group(2));
        }

        // Handle individual characters (X, 9, A, V, S, etc.)
        for (char c : pic.toCharArray()) {
            if (c == 'X' || c == '9' || c == 'A') {
                length++;
            }
            // V (decimal point) and S (sign) don't add to length in DISPLAY format
        }

        return length;
    }

    /**
     * Helper class for PICTURE analysis results
     */
    private static class PictureAnalysis {
        boolean isNumeric;
        boolean isSigned;
        int decimalPlaces;
    }

    /**
     * Generates optimal ItemReader configuration code
     */
    public String generateOptimizedReader(FileIOMetadata metadata, String entityType, String beanName) {
        StringBuilder code = new StringBuilder();

        code.append("    @Bean\n");
        code.append("    public FlatFileItemReader<").append(entityType).append("> ")
            .append(beanName).append("() {\n");

        switch (metadata.getFormat()) {
            case DELIMITED:
                generateDelimitedReader(code, metadata, entityType);
                break;

            case FIXED_LENGTH:
                generateFixedLengthReader(code, metadata, entityType);
                break;

            case INDEXED_SEQUENTIAL:
                // For VSAM KSDS, generate JPA/JDBC reader instead
                return generateIndexedFileReader(metadata, entityType, beanName);

            case SEQUENTIAL:
            default:
                // Auto-detect: use delimited if delimiter detected, else fixed-length
                if (metadata.getFieldMappings().stream()
                        .anyMatch(m -> m.getStartPosition() > 0)) {
                    generateFixedLengthReader(code, metadata, entityType);
                } else {
                    generateDelimitedReader(code, metadata, entityType);
                }
                break;
        }

        code.append("    }\n\n");

        return code.toString();
    }

    /**
     * Generates JdbcCursorItemReader for INDEXED files (VSAM KSDS)
     */
    private String generateIndexedFileReader(FileIOMetadata metadata, String entityType, String beanName) {
        StringBuilder code = new StringBuilder();

        code.append("    @Bean\n");
        code.append("    public JdbcCursorItemReader<").append(entityType).append("> ")
            .append(beanName).append("() {\n");
        code.append("        // INDEXED file (VSAM KSDS) - using JdbcCursorItemReader\n");
        code.append("        return new JdbcCursorItemReaderBuilder<").append(entityType).append(">()\n");
        code.append("            .name(\"").append(entityType.toLowerCase()).append("Reader\")\n");
        code.append("            .dataSource(dataSource)  // Inject DataSource\n");
        code.append("            .sql(\"SELECT * FROM ").append(entityType.toUpperCase()).append(" ORDER BY ");

        // Use first field as key if no explicit key specified
        if (metadata.getFieldMappings().isEmpty()) {
            code.append("ID");
        } else {
            code.append(metadata.getFieldMappings().get(0).getJavaFieldName().toUpperCase());
        }

        code.append("\")\n");
        code.append("            .rowMapper(new BeanPropertyRowMapper<>(" + entityType + ".class))\n");
        code.append("            .build();\n");
        code.append("    }\n\n");

        // Add comment about database migration
        code.append("    // NOTE: VSAM KSDS files should be migrated to relational database\n");
        code.append("    // Use db2move or similar tools to export VSAM to DB2/PostgreSQL\n\n");

        return code.toString();
    }

    /**
     * Generates delimited file reader configuration
     */
    private void generateDelimitedReader(StringBuilder code, FileIOMetadata metadata, String entityType) {
        code.append("        return new FlatFileItemReaderBuilder<").append(entityType).append(">()\n");
        code.append("            .name(\"").append(entityType.toLowerCase()).append("Reader\")\n");
        code.append("            .resource(new FileSystemResource(\"data/input/")
            .append(entityType.toLowerCase()).append(".csv\"))\n");

        if (metadata.isHasHeaderRecord()) {
            code.append("            .linesToSkip(1)  // Skip header\n");
        }

        code.append("            .delimited()\n");
        code.append("            .delimiter(\"").append(metadata.getDelimiter()).append("\")\n");
        code.append("            .names(new String[] {\n");

        // Generate field names array
        List<FieldMapping> mappings = metadata.getFieldMappings();
        for (int i = 0; i < mappings.size(); i++) {
            code.append("                \"").append(mappings.get(i).getJavaFieldName()).append("\"");
            if (i < mappings.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }

        code.append("            })\n");
        code.append("            .fieldSetMapper(new BeanWrapperFieldSetMapper<").append(entityType).append(">() {{\n");
        code.append("                setTargetType(").append(entityType).append(".class);\n");
        code.append("            }})\n");
        code.append("            .build();\n");
    }

    /**
     * Generates fixed-length file reader configuration
     */
    private void generateFixedLengthReader(StringBuilder code, FileIOMetadata metadata, String entityType) {
        code.append("        return new FlatFileItemReaderBuilder<").append(entityType).append(">()\n");
        code.append("            .name(\"").append(entityType.toLowerCase()).append("Reader\")\n");
        code.append("            .resource(new FileSystemResource(\"data/input/")
            .append(entityType.toLowerCase()).append(".dat\"))\n");

        code.append("            .fixedLength()\n");
        code.append("            .columns(new Range[] {\n");

        // Generate column ranges for fixed-length
        List<FieldMapping> mappings = metadata.getFieldMappings();
        for (int i = 0; i < mappings.size(); i++) {
            FieldMapping m = mappings.get(i);
            int start = m.getStartPosition() + 1;  // 1-based
            int end = start + m.getLength() - 1;

            code.append("                new Range(").append(start).append(", ").append(end).append(")");
            if (i < mappings.size() - 1) {
                code.append(",");
            }
            code.append("  // ").append(m.getCobolName()).append(" (").append(m.getPictureClause()).append(")\n");
        }

        code.append("            })\n");
        code.append("            .names(new String[] {\n");

        for (int i = 0; i < mappings.size(); i++) {
            code.append("                \"").append(mappings.get(i).getJavaFieldName()).append("\"");
            if (i < mappings.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }

        code.append("            })\n");
        code.append("            .fieldSetMapper(new BeanWrapperFieldSetMapper<").append(entityType).append(">() {{\n");
        code.append("                setTargetType(").append(entityType).append(".class);\n");
        code.append("            }})\n");
        code.append("            .build();\n");
    }

    /**
     * Generates optimal ItemWriter configuration code
     */
    public String generateOptimizedWriter(FileIOMetadata metadata, String entityType, String beanName) {
        StringBuilder code = new StringBuilder();

        code.append("    @Bean\n");
        code.append("    public FlatFileItemWriter<").append(entityType).append("> ")
            .append(beanName).append("() {\n");

        switch (metadata.getFormat()) {
            case DELIMITED:
                generateDelimitedWriter(code, metadata, entityType);
                break;

            case FIXED_LENGTH:
                generateFormattedWriter(code, metadata, entityType);
                break;

            case SEQUENTIAL:
            default:
                generateDelimitedWriter(code, metadata, entityType);
                break;
        }

        code.append("    }\n\n");

        return code.toString();
    }

    /**
     * Generates delimited file writer configuration
     */
    private void generateDelimitedWriter(StringBuilder code, FileIOMetadata metadata, String entityType) {
        code.append("        return new FlatFileItemWriterBuilder<").append(entityType).append(">()\n");
        code.append("            .name(\"").append(entityType.toLowerCase()).append("Writer\")\n");
        code.append("            .resource(new FileSystemResource(\"data/output/")
            .append(entityType.toLowerCase()).append(".csv\"))\n");
        code.append("            .delimited()\n");
        code.append("            .delimiter(\"").append(metadata.getDelimiter()).append("\")\n");
        code.append("            .names(new String[] {\n");

        List<FieldMapping> mappings = metadata.getFieldMappings();
        for (int i = 0; i < mappings.size(); i++) {
            code.append("                \"").append(mappings.get(i).getJavaFieldName()).append("\"");
            if (i < mappings.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }

        code.append("            })\n");
        code.append("            .build();\n");
    }

    /**
     * Generates formatted (fixed-length) file writer configuration
     */
    private void generateFormattedWriter(StringBuilder code, FileIOMetadata metadata, String entityType) {
        code.append("        return new FlatFileItemWriterBuilder<").append(entityType).append(">()\n");
        code.append("            .name(\"").append(entityType.toLowerCase()).append("Writer\")\n");
        code.append("            .resource(new FileSystemResource(\"data/output/")
            .append(entityType.toLowerCase()).append(".dat\"))\n");
        code.append("            .formatted()\n");
        code.append("            .format(\"");

        // Generate format string for fixed-length output
        List<FieldMapping> mappings = metadata.getFieldMappings();
        for (FieldMapping m : mappings) {
            if (m.isNumeric()) {
                code.append("%").append(m.getLength()).append("d");
            } else {
                code.append("%-").append(m.getLength()).append("s");
            }
        }

        code.append("\")\n");
        code.append("            .names(new String[] {\n");

        for (int i = 0; i < mappings.size(); i++) {
            code.append("                \"").append(mappings.get(i).getJavaFieldName()).append("\"");
            if (i < mappings.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }

        code.append("            })\n");
        code.append("            .build();\n");
    }
}
