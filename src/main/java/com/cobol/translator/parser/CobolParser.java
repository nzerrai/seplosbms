package com.cobol.translator.parser;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import com.cobol.translator.model.Paragraph;
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
        Paragraph currentParagraph = null;
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
                // Check if this is a paragraph name (ends with . and no statement keyword)
                String trimmed = line.trim();
                if (!trimmed.isEmpty() && trimmed.endsWith(".") &&
                    !trimmed.toUpperCase().startsWith("MOVE") &&
                    !trimmed.toUpperCase().startsWith("IF") &&
                    !trimmed.toUpperCase().startsWith("PERFORM") &&
                    !trimmed.toUpperCase().contains("DIVISION")) {

                    // This is likely a paragraph name
                    String paragraphName = trimmed.substring(0, trimmed.length() - 1).trim();
                    if (paragraphName.matches("[A-Z0-9-]+")) {
                        // Save previous paragraph if exists
                        if (currentParagraph != null) {
                            currentParagraph.setEndLine(i);
                            program.addParagraph(currentParagraph);
                        }

                        // Start new paragraph
                        currentParagraph = new Paragraph(paragraphName);
                        currentParagraph.setStartLine(i + 1);
                        logger.debug("Found paragraph: {}", paragraphName);
                    }
                }

                // For IF statements, check if condition spans multiple lines
                String statementLine = line;
                int currentLineIndex = i;
                if (trimmed.toUpperCase().startsWith("IF ")) {
                    // Collect multi-line IF condition
                    StringBuilder multiLineCondition = new StringBuilder(line.trim());

                    // Check if condition continues on next lines
                    while (currentLineIndex + 1 < lines.length &&
                           isConditionContinuation(multiLineCondition.toString())) {
                        currentLineIndex++;
                        String nextLine = lines[currentLineIndex].trim();

                        // Stop if we hit a statement keyword or END-IF
                        String nextUpper = nextLine.toUpperCase();
                        if (nextUpper.startsWith("MOVE ") ||
                            nextUpper.startsWith("DISPLAY ") ||
                            nextUpper.startsWith("END-IF") ||
                            nextUpper.startsWith("ELSE") ||
                            nextUpper.startsWith("PERFORM ") ||
                            nextUpper.startsWith("ADD ") ||
                            nextUpper.startsWith("COMPUTE ") ||
                            nextLine.isEmpty()) {
                            break;
                        }

                        // Append continuation line
                        multiLineCondition.append(" ").append(nextLine);
                    }

                    statementLine = multiLineCondition.toString();
                    i = currentLineIndex; // Skip processed continuation lines
                }

                Statement statement = parseStatement(statementLine, i + 1);
                if (statement != null) {
                    program.addStatement(statement);
                    if (currentParagraph != null) {
                        currentParagraph.addStatement(statement);
                    }
                }
            }
        }

        // Save last paragraph
        if (currentParagraph != null) {
            currentParagraph.setEndLine(lines.length);
            program.addParagraph(currentParagraph);
        }

        logger.info("Parsed {} data items, {} statements, and {} paragraphs",
                   program.getDataItems().size(),
                   program.getStatements().size(),
                   program.getParagraphs().size());

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
            // Parse IF statement: "IF condition"
            parseIfStatement(trimmed, stmt);
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
            // Parse ADD statement: "ADD value TO variable"
            parseAddStatement(trimmed, stmt);
        } else if (trimmed.startsWith("DISPLAY ")) {
            stmt.setType(Statement.StatementType.DISPLAY);
            // Parse DISPLAY statement: "DISPLAY 'text' variable"
            parseDisplayStatement(trimmed, stmt);
        } else {
            // Unknown or unsupported statement
            return null;
        }

        return stmt;
    }

    /**
     * Parses ADD statement to extract source and target.
     * Format: "ADD value TO variable" or "ADD value1 value2 TO variable"
     */
    private void parseAddStatement(String trimmed, Statement stmt) {
        // Pattern: ADD <source> TO <target>
        int toIndex = trimmed.indexOf(" TO ");
        if (toIndex > 0) {
            String sourcePart = trimmed.substring(4, toIndex).trim(); // After "ADD "
            String targetPart = trimmed.substring(toIndex + 4).trim(); // After " TO "

            stmt.setSource(sourcePart);
            stmt.setTarget(targetPart);
        }
    }

    /**
     * Parses IF statement to extract condition.
     * Format: "IF condition" or "IF variable = value" etc.
     */
    private void parseIfStatement(String trimmed, Statement stmt) {
        // Extract condition (everything after "IF ")
        String condition = trimmed.substring(3).trim(); // After "IF "

        // Remove trailing "THEN" if present
        if (condition.endsWith(" THEN")) {
            condition = condition.substring(0, condition.length() - 5).trim();
        }

        stmt.setCondition(condition);

        // Parse comparison operators: =, >, <, >=, <=, NOT
        parseCondition(condition, stmt);
    }

    /**
     * Parses a condition to extract left operand, operator, and right operand.
     * Format: "variable operator value" (e.g., "CUST-AMOUNT > 1000")
     */
    private void parseCondition(String condition, Statement stmt) {
        // Try to find comparison operators
        String[] operators = {" >= ", " <= ", " = ", " > ", " < ", " NOT "};

        for (String op : operators) {
            int index = condition.indexOf(op);
            if (index > 0) {
                String left = condition.substring(0, index).trim();
                String right = condition.substring(index + op.length()).trim();

                stmt.setLeftOperand(left);
                stmt.setOperator(op.trim());
                stmt.setRightOperand(right);
                break;
            }
        }
    }

    /**
     * Checks if an IF condition continues on the next line.
     * A condition continues if it ends with a logical operator (OR, AND)
     * or a comparison operator without a closing END-IF.
     */
    private boolean isConditionContinuation(String currentCondition) {
        String trimmed = currentCondition.trim().toUpperCase();

        // Remove comments (starting with *)
        int commentIndex = trimmed.indexOf('*');
        if (commentIndex >= 0) {
            trimmed = trimmed.substring(0, commentIndex).trim();
        }

        // Condition continues if it ends with logical operators
        if (trimmed.endsWith(" OR") || trimmed.endsWith(" AND")) {
            return true;
        }

        // Condition continues if it ends with NOT (as in "NOT =")
        if (trimmed.endsWith(" NOT")) {
            return true;
        }

        // Check if we have an unbalanced parenthesis
        int openParens = 0;
        for (char c : trimmed.toCharArray()) {
            if (c == '(') openParens++;
            if (c == ')') openParens--;
        }
        if (openParens > 0) {
            return true; // Unclosed parenthesis means continuation
        }

        return false;
    }

    /**
     * Parses DISPLAY statement to extract what is being displayed.
     * Format: "DISPLAY 'text' variable" or "DISPLAY variable"
     */
    private void parseDisplayStatement(String trimmed, Statement stmt) {
        // Extract everything after "DISPLAY "
        String displayContent = trimmed.substring(8).trim(); // After "DISPLAY "

        // For now, store in source field
        stmt.setSource(displayContent);
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
        String name = item.getName() != null ? item.getName().toUpperCase() : "";

        // EXCLUDE counter/accumulator fields from BigDecimal
        // Even if COMP-3, counters are fundamentally numeric (Integer/Long)
        if (isCounterFieldName(name)) {
            int size = extractNumericSize(pic);
            return size <= 9 ? "Integer" : "Long";
        }

        // Edit patterns (Z, comma, period) -> BigDecimal
        // These are used for output formatting and must support numeric operations
        if (pic.contains("Z") || (pic.contains(",") && pic.contains("9"))) {
            return "BigDecimal";
        }

        // Numeric with decimals or COMP-3 -> BigDecimal (unless already handled as counter)
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
     * Identifies counter/accumulator field names.
     * Conservative: matches fields that are clearly accumulators or counts, not totals of monetary amounts.
     */
    private boolean isCounterFieldName(String fieldName) {
        if (fieldName == null || fieldName.isEmpty()) {
            return false;
        }

        // Explicit counter/accumulator patterns
        if (fieldName.endsWith("-COUNT") || fieldName.endsWith("-READ") || 
            fieldName.endsWith("-PROCESSED") || fieldName.endsWith("-INDEX") || 
            fieldName.endsWith("-COUNTER") || fieldName.endsWith("-WRITTEN") || 
            fieldName.endsWith("-SKIPPED") || fieldName.endsWith("-UPDATED")) {
            return true;
        }
        
        // WS- fields with counter suffixes (more permissive for working storage)
        if (fieldName.startsWith("WS-")) {
            if (fieldName.contains("-ERROR") || fieldName.contains("-READ") || 
                fieldName.contains("-PROCESSED") || fieldName.contains("-COUNT") ||
                fieldName.contains("-NUM-") || fieldName.contains("-TOTAL-ITEMS") ||
                fieldName.contains("-TOTAL-RECORDS")) {
                // But exclude if it's clearly a message or description field
                if (!fieldName.contains("MESSAGE") && !fieldName.contains("DESC")) {
                    return true;
                }
            }
        }
        
        return false;
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
