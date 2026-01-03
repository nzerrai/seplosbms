package com.cobol.translator.parser;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import com.cobol.translator.model.Statement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Simplified COBOL parser.
 *
 * This is a basic implementation that demonstrates the concept.
 * A production parser would use ANTLR4 with a complete COBOL grammar.
 */
public class CobolParser {

    private static final Logger logger = LoggerFactory.getLogger(CobolParser.class);

    // Regular expressions for COBOL constructs
    private static final Pattern PROGRAM_ID_PATTERN =
        Pattern.compile("PROGRAM-ID\\.\\s+(\\w+)", Pattern.CASE_INSENSITIVE);

    private static final Pattern AUTHOR_PATTERN =
        Pattern.compile("AUTHOR\\.\\s+(.+)", Pattern.CASE_INSENSITIVE);

    // Pattern for regular data items and FILLER
    // Group 1: Level number (01-49, 66, 77, 88)
    // Group 2: Field name or FILLER keyword
    // Group 3: PICTURE clause (optional)
    // Group 4: USAGE clause (optional)
    private static final Pattern DATA_ITEM_PATTERN =
        Pattern.compile("^\\s*(\\d{2})\\s+((?:FILLER|\\w+(?:-\\w+)*))(?:\\s+PIC(?:TURE)?\\s+([^.\\s]+))?(?:\\s+(COMP-3|COMP|BINARY))?",
                Pattern.CASE_INSENSITIVE);

    // Pattern for Level-88 condition names with VALUE
    private static final Pattern LEVEL_88_PATTERN =
        Pattern.compile("^\\s*88\\s+(\\w+(?:-\\w+)*)\\s+VALUE\\s+(?:IS\\s+)?['\"]([^'\"]+)['\"]",
                Pattern.CASE_INSENSITIVE);

    private static final Pattern FD_PATTERN =
        Pattern.compile("^\\s*FD\\s+(\\w+(?:-\\w+)*)", Pattern.CASE_INSENSITIVE);

    public CobolProgram parse(String cobolSource) {
        logger.info("Parsing COBOL source...");

        CobolProgram program = new CobolProgram();
        String[] lines = cobolSource.split("\\r?\\n");
        program.setTotalLines(lines.length);

        boolean inDataDivision = false;
        boolean inProcedureDivision = false;
        boolean inFileSection = false;
        boolean inWorkingStorage = false;
        FileDefinition currentFile = null;
        int fillerCounter = 1;  // Counter for generating unique FILLER names
        DataItem lastDataItem = null;  // Track last data item for Level-88 parent linking

        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];

            // Skip comments (asterisk in column 7)
            if (line.length() > 6 && line.charAt(6) == '*') {
                continue;
            }

            // Extract program name
            Matcher programIdMatcher = PROGRAM_ID_PATTERN.matcher(line);
            if (programIdMatcher.find()) {
                String programName = programIdMatcher.group(1);
                program.setProgramName(programName);
                program.setProgramId(programName);
                logger.debug("Found program: {}", programName);
            }

            // Extract author
            Matcher authorMatcher = AUTHOR_PATTERN.matcher(line);
            if (authorMatcher.find()) {
                program.setAuthor(authorMatcher.group(1).trim());
            }

            // Detect divisions
            if (line.toUpperCase().contains("DATA DIVISION")) {
                inDataDivision = true;
                inProcedureDivision = false;
                logger.debug("Entered DATA DIVISION");
            } else if (line.toUpperCase().contains("PROCEDURE DIVISION")) {
                inDataDivision = false;
                inProcedureDivision = true;
                logger.debug("Entered PROCEDURE DIVISION");
            }

            // Detect sections
            if (inDataDivision) {
                if (line.toUpperCase().contains("FILE SECTION")) {
                    inFileSection = true;
                    inWorkingStorage = false;
                    logger.debug("Entered FILE SECTION");
                } else if (line.toUpperCase().contains("WORKING-STORAGE SECTION")) {
                    inFileSection = false;
                    inWorkingStorage = true;
                    logger.debug("Entered WORKING-STORAGE SECTION");
                }
            }

            // Parse file definitions
            Matcher fdMatcher = FD_PATTERN.matcher(line);
            if (fdMatcher.find() && inFileSection) {
                String fileName = fdMatcher.group(1);
                currentFile = new FileDefinition(fileName);
                program.addFile(currentFile);
                program.getFileDefinitions().add(currentFile);
                logger.debug("Found file: {}", fileName);
            }

            // Parse Level-88 condition names first
            Matcher level88Matcher = LEVEL_88_PATTERN.matcher(line);
            if (level88Matcher.find() && (inFileSection || inWorkingStorage)) {
                String conditionName = level88Matcher.group(1);
                String conditionValue = level88Matcher.group(2);

                DataItem item = new DataItem(88, conditionName);
                item.setConditionName(true);
                item.setConditionValue(conditionValue);
                item.setConditionParent(lastDataItem);
                item.setJavaFieldName(toJavaFieldName(conditionName));

                program.addDataItem(item);

                if (inWorkingStorage) {
                    program.getWorkingStorageItems().add(item);
                }

                logger.debug("Found Level-88 condition: {} VALUE '{}'", conditionName, conditionValue);
            }

            // Parse data items
            Matcher dataItemMatcher = DATA_ITEM_PATTERN.matcher(line);
            if (dataItemMatcher.find() && (inFileSection || inWorkingStorage)) {
                int level = Integer.parseInt(dataItemMatcher.group(1));
                String name = dataItemMatcher.group(2);
                String picture = dataItemMatcher.group(3);
                String usage = dataItemMatcher.group(4);

                // Check if this is a FILLER field
                boolean isFiller = name.equalsIgnoreCase("FILLER");
                String actualName = name;

                // Generate unique name for FILLER fields
                if (isFiller) {
                    actualName = "FILLER-" + fillerCounter++;
                }

                DataItem item = new DataItem(level, actualName);
                item.setPictureClause(picture);
                item.setUsage(usage);
                item.setGroup(picture == null);
                item.setFiller(isFiller);

                // Compute Java type
                item.setJavaType(computeJavaType(item));
                item.setJavaFieldName(toJavaFieldName(actualName));

                program.addDataItem(item);

                if (inFileSection && currentFile != null && level == 1) {
                    currentFile.setRecordLayout(item);
                }

                if (inWorkingStorage) {
                    program.getWorkingStorageItems().add(item);
                }

                // Track this as the last data item for Level-88 parent linking
                lastDataItem = item;

                logger.debug("Found data item: {} (level {}, type {}, filler: {})",
                            actualName, level, item.getJavaType(), isFiller);
            }

            // Parse procedure division statements (simplified)
            if (inProcedureDivision) {
                Statement statement = parseStatement(line, i + 1);
                if (statement != null) {
                    program.addStatement(statement);
                }
            }
        }

        logger.info("Parsed {} data items and {} statements",
                   program.getDataItems().size(),
                   program.getStatements().size());

        return program;
    }

    /**
     * Parses a single COBOL statement (simplified).
     */
    private Statement parseStatement(String line, int lineNumber) {
        String trimmed = line.trim().toUpperCase();

        if (trimmed.isEmpty() || trimmed.endsWith(".")) {
            // Skip empty lines and paragraph headers
            return null;
        }

        Statement stmt = new Statement();
        stmt.setLineNumber(lineNumber);
        stmt.setOriginalCobol(line.trim());

        // Detect statement type
        if (trimmed.startsWith("MOVE ")) {
            stmt.setType(Statement.StatementType.MOVE);
        } else if (trimmed.startsWith("COMPUTE ")) {
            stmt.setType(Statement.StatementType.COMPUTE);
        } else if (trimmed.startsWith("IF ")) {
            stmt.setType(Statement.StatementType.IF);
        } else if (trimmed.startsWith("READ ")) {
            stmt.setType(Statement.StatementType.READ);
        } else if (trimmed.startsWith("WRITE ")) {
            stmt.setType(Statement.StatementType.WRITE);
        } else if (trimmed.startsWith("PERFORM ")) {
            if (trimmed.contains("UNTIL")) {
                stmt.setType(Statement.StatementType.PERFORM_UNTIL);
            } else if (trimmed.contains("TIMES")) {
                stmt.setType(Statement.StatementType.PERFORM_TIMES);
            } else {
                stmt.setType(Statement.StatementType.PERFORM);
            }
        } else if (trimmed.startsWith("ADD ")) {
            stmt.setType(Statement.StatementType.ADD);
        } else if (trimmed.startsWith("DISPLAY ")) {
            stmt.setType(Statement.StatementType.DISPLAY);
        } else {
            // Unknown or unsupported statement
            return null;
        }

        return stmt;
    }

    /**
     * Computes Java type from COBOL data item.
     */
    private String computeJavaType(DataItem item) {
        if (item.isGroup()) {
            return null; // Group items don't have a direct Java type
        }

        String pic = item.getPictureClause();
        if (pic == null) {
            return "String";
        }

        pic = pic.toUpperCase();

        // Numeric with decimals or COMP-3 -> BigDecimal
        if (item.hasDecimals() || item.isComp3()) {
            return "BigDecimal";
        }

        // Date field -> LocalDate
        if (item.isPotentialDateField()) {
            return "LocalDate";
        }

        // Numeric without decimals
        if (item.isNumeric()) {
            // Determine Integer vs Long based on size
            int size = extractNumericSize(pic);
            return size <= 9 ? "Integer" : "Long";
        }

        // Alphanumeric
        return "String";
    }

    /**
     * Extracts numeric size from PICTURE clause.
     */
    private int extractNumericSize(String picture) {
        Pattern pattern = Pattern.compile("9\\((\\d+)\\)");
        Matcher matcher = pattern.matcher(picture);
        if (matcher.find()) {
            return Integer.parseInt(matcher.group(1));
        }
        // Count 9s
        return (int) picture.chars().filter(ch -> ch == '9').count();
    }

    /**
     * Converts COBOL name to Java field name (camelCase).
     */
    private String toJavaFieldName(String cobolName) {
        String[] parts = cobolName.toLowerCase().split("-");
        StringBuilder result = new StringBuilder(parts[0]);
        for (int i = 1; i < parts.length; i++) {
            result.append(Character.toUpperCase(parts[i].charAt(0)));
            result.append(parts[i].substring(1));
        }
        return result.toString();
    }
}
