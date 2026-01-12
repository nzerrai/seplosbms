package com.cobol.translator.generator;

import com.cobol.translator.model.Paragraph;
import com.cobol.translator.model.Statement;
import com.cobol.translator.model.Statement.StatementType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Translates COBOL business logic (paragraphs and statements) into Java code.
 * Analyzes PROCEDURE DIVISION statements and generates equivalent Java implementations.
 * 
 * Phase 3 Improvements:
 * - Cached regex patterns for better performance
 * - Improved null/empty handling
 * - Better expression parsing with validation
 * - Consolidated arithmetic operation code
 */
public class BusinessLogicTranslator {

    private static final Logger logger = LoggerFactory.getLogger(BusinessLogicTranslator.class);
    
    // Cached regex patterns for performance
    private static final Pattern COBOL_LITERAL_QUOTED = Pattern.compile("^'([^']*)'$");
    private static final Pattern COBOL_LITERAL_NUMBER = Pattern.compile("^-?\\d+(\\.\\d+)?$");
    private static final Pattern COBOL_FIELD_NAME = Pattern.compile("^[A-Z][A-Z0-9-]*$");
    
    // Set of 88-level condition names (populated by ProcessorGenerator)
    private java.util.Set<String> conditionNames = new java.util.HashSet<>();

    /**
     * Sets the condition names (88-level) for proper is*() method generation.
     */
    public void setConditionNames(java.util.Set<String> conditionNames) {
        if (conditionNames != null) {
            this.conditionNames = new java.util.HashSet<>(conditionNames);
        }
    }

    /**
     * Translates a COBOL paragraph into Java method code.
     */
    public String translateParagraph(Paragraph paragraph, String recordType) {
        if (paragraph == null || paragraph.getStatements().isEmpty()) {
            return generateTodoComment(paragraph);
        }

        StringBuilder code = new StringBuilder();
        code.append("        // Translated from COBOL paragraph: ").append(paragraph.getName()).append("\n");
        code.append("        logger.debug(\"Executing business logic from paragraph: ")
            .append(paragraph.getName()).append("\");\n\n");

        for (Statement stmt : paragraph.getStatements()) {
            String translatedStmt = translateStatement(stmt, recordType, "        ");
            if (translatedStmt != null && !translatedStmt.isEmpty()) {
                code.append(translatedStmt);
            }
        }

        return code.toString();
    }

    /**
     * Translates a single COBOL statement into Java code.
     */
    private String translateStatement(Statement stmt, String recordType, String indent) {
        if (stmt == null || stmt.getType() == null) {
            return "";
        }

        switch (stmt.getType()) {
            case IF:
                return translateIf(stmt, recordType, indent);
            
            case EVALUATE:
                return translateEvaluate(stmt, recordType, indent);
            
            case MOVE:
                return translateMove(stmt, recordType, indent);
            
            case COMPUTE:
                return translateCompute(stmt, recordType, indent);
            
            case PERFORM:
                return translatePerform(stmt, recordType, indent);
            
            case ADD:
                return translateAdd(stmt, recordType, indent);
            
            case SUBTRACT:
                return translateSubtract(stmt, recordType, indent);
            
            case MULTIPLY:
                return translateMultiply(stmt, recordType, indent);
            
            case DIVIDE:
                return translateDivide(stmt, recordType, indent);
            
            case DISPLAY:
                return translateDisplay(stmt, recordType, indent);
            
            // Phase 4: Advanced Statements
            case GO_TO:
                return translateGoTo(stmt, recordType, indent);
            
            case INSPECT:
                return translateInspect(stmt, recordType, indent);
            
            case STRING:
                return translateString(stmt, recordType, indent);
            
            case UNSTRING:
                return translateUnstring(stmt, recordType, indent);
            
            case SEARCH:
                return translateSearch(stmt, recordType, indent);
            
            case CALL:
                return translateCall(stmt, recordType, indent);
            
            default:
                return translateGeneric(stmt, indent);
        }
    }

    /**
     * Translates COBOL IF statement to Java with improved validation.
     */
    private String translateIf(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String condition = stmt.getCondition();
        if (condition == null || condition.trim().isEmpty()) {
            // Try to reconstruct from operands
            if (stmt.getLeftOperand() != null && stmt.getOperator() != null && stmt.getRightOperand() != null) {
                condition = translateCondition(stmt);
            } else {
                condition = "/* TODO: add condition */";
            }
        } else {
            condition = translateCobolCondition(condition);
        }

        code.append(indent).append("// COBOL: IF ").append(stmt.getOriginalCobol() != null ? stmt.getOriginalCobol() : "").append("\n");
        code.append(indent).append("if (").append(condition).append(") {\n");

        // Translate THEN part (children statements)
        if (stmt.getChildren() != null && !stmt.getChildren().isEmpty()) {
            for (Statement child : stmt.getChildren()) {
                String childCode = translateStatement(child, recordType, indent + "    ");
                if (childCode != null && !childCode.isEmpty()) {
                    code.append(childCode);
                }
            }
        } else {
            // Add COBOL code as comment before TODO
            if (stmt.getOriginalCobol() != null && !stmt.getOriginalCobol().trim().isEmpty()) {
                code.append(indent).append("    // COBOL original: ").append(stmt.getOriginalCobol()).append("\n");
            }
            code.append(indent).append("    // TODO: add statement\n");
        }

        code.append(indent).append("}\n\n");
        return code.toString();
    }

    /**
     * Translates COBOL EVALUATE statement to Java switch or if-else.
     * Supports EVALUATE TRUE, regular EVALUATE, and EVALUATE ALSO patterns.
     */
    private String translateEvaluate(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        code.append(indent).append("// COBOL: EVALUATE ").append(stmt.getOriginalCobol() != null ? stmt.getOriginalCobol() : "").append("\n");
        
        String evaluateExpr = stmt.getExpression();
        
        // Check for EVALUATE ALSO (multi-expression)
        if (evaluateExpr != null && evaluateExpr.toUpperCase().contains("ALSO")) {
            code.append(translateEvaluateAlso(stmt, recordType, indent));
        } else if (evaluateExpr != null && evaluateExpr.trim().toUpperCase().equals("TRUE")) {
            // EVALUATE TRUE pattern - translate to if-else chain
            code.append(translateEvaluateTrue(stmt, recordType, indent));
        } else if (evaluateExpr != null && !evaluateExpr.trim().isEmpty()) {
            // Regular EVALUATE - translate to switch
            code.append(translateEvaluateSwitch(stmt, recordType, indent));
        } else {
            code.append(indent).append("// TODO: Invalid EVALUATE statement\n");
        }
        
        return code.toString();
    }

    /**
     * Translates EVALUATE ALSO (multi-expression EVALUATE) to nested if-else.
     * Example: EVALUATE STATUS ALSO ERROR-CODE
     *          WHEN 'A' ALSO '01' ...
     */
    private String translateEvaluateAlso(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        if (stmt.getChildren() == null || stmt.getChildren().isEmpty()) {
            code.append(indent).append("// TODO: EVALUATE ALSO with no WHEN clauses\n");
            return code.toString();
        }
        
        code.append(indent).append("// Multi-expression evaluation with ALSO\n");
        
        boolean first = true;
        for (Statement whenClause : stmt.getChildren()) {
            if (whenClause == null || whenClause.getCondition() == null) continue;
            
            // EVALUATE ALSO uses multiple conditions separated by ALSO
            String conditions = whenClause.getCondition();
            String javaCondition = translateCobolCondition(conditions);
            
            if (first) {
                code.append(indent).append("if (").append(javaCondition).append(") {\n");
                first = false;
            } else {
                code.append(indent).append("} else if (").append(javaCondition).append(") {\n");
            }
            
            // Translate statements in WHEN clause
            if (whenClause.getChildren() != null) {
                for (Statement action : whenClause.getChildren()) {
                    String actionCode = translateStatement(action, recordType, indent + "    ");
                    if (actionCode != null && !actionCode.isEmpty()) {
                        code.append(actionCode);
                    }
                }
            }
        }
        
        if (!first) {
            code.append(indent).append("}\n\n");
        }
        
        return code.toString();
    }

    /**
     * Translates EVALUATE TRUE into if-else chain with better error handling.
     */
    private String translateEvaluateTrue(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        if (stmt.getChildren() == null || stmt.getChildren().isEmpty()) {
            code.append(indent).append("// TODO: EVALUATE TRUE with no WHEN clauses\n");
            return code.toString();
        }
        
        boolean first = true;
        for (Statement whenClause : stmt.getChildren()) {
            if (whenClause == null) continue;
            
            String condition = whenClause.getCondition();
            if (condition == null || condition.trim().isEmpty()) {
                continue; // Skip empty when clauses
            }
            
            String javaCondition = translateCobolCondition(condition);
            
            if (first) {
                code.append(indent).append("if (").append(javaCondition).append(") {\n");
                first = false;
            } else {
                code.append(indent).append("} else if (").append(javaCondition).append(") {\n");
            }
            
            // Translate statements in this WHEN clause
            if (whenClause.getChildren() != null) {
                for (Statement action : whenClause.getChildren()) {
                    String actionCode = translateStatement(action, recordType, indent + "    ");
                    if (actionCode != null && !actionCode.isEmpty()) {
                        code.append(actionCode);
                    }
                }
            } else {
                // Add COBOL code as comment before TODO
                if (whenClause.getOriginalCobol() != null && !whenClause.getOriginalCobol().trim().isEmpty()) {
                    code.append(indent).append("    // COBOL original: ").append(whenClause.getOriginalCobol()).append("\n");
                }
                code.append(indent).append("    // TODO: add action\n");
            }
        }
        
        if (!first) {
            code.append(indent).append("}\n\n");
        }
        
        return code.toString();
    }

    /**
     * Translates regular EVALUATE into switch statement with validation.
     */
    private String translateEvaluateSwitch(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String variable = toJavaGetter(stmt.getExpression(), recordType);
        code.append(indent).append("switch (").append(variable).append(") {\n");
        
        if (stmt.getChildren() != null) {
            for (Statement whenClause : stmt.getChildren()) {
                if (whenClause == null || whenClause.getCondition() == null) continue;
                
                code.append(indent).append("    case ").append(whenClause.getCondition()).append(":\n");
                
                if (whenClause.getChildren() != null) {
                    for (Statement action : whenClause.getChildren()) {
                        String actionCode = translateStatement(action, recordType, indent + "        ");
                        if (actionCode != null && !actionCode.isEmpty()) {
                            code.append(actionCode);
                        }
                    }
                }
                
                code.append(indent).append("        break;\n");
            }
        }
        
        code.append(indent).append("}\n\n");
        return code.toString();
    }

    /**
     * Translates COBOL MOVE statement with validation and type conversion.
     * Fixed to detect and correct inverted assignments and handle type conversions.
     * Supports bidirectional LocalDate ↔ Integer conversions.
     */
    private String translateMove(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        if (!isValidStatement(stmt, stmt.getSource(), stmt.getTarget())) {
            return indent + "// TODO: Invalid MOVE - missing source or target\n";
        }
        
        String source = stmt.getSource();
        String target = stmt.getTarget();
        
        // Convert source to Java expression
        String javaSource = toJavaExpression(source, recordType);
        String javaSetter = toJavaSetter(target, recordType);
        String javaGetter = toJavaGetter(target, recordType);

        // Check if target is BigDecimal and coerce source accordingly
        if (isBigDecimalExpression(javaGetter, target)) {
            if (javaSource.matches("^\\d+$")) {
                // Integer literal moving to BigDecimal field
                if (javaSource.equals("0")) {
                    javaSource = "BigDecimal.ZERO";
                } else if (javaSource.equals("1")) {
                    javaSource = "BigDecimal.ONE";
                } else {
                    javaSource = "BigDecimal.valueOf(" + javaSource + ")";
                }
            } else if (!isBigDecimalExpression(javaSource, source)) {
                // Non-BigDecimal expression (including getters): wrap safely
                javaSource = "BigDecimal.valueOf(" + javaSource + ")";
            }
        }
        
        // Special case: FUNCTION CURRENT-DATE to String field
        // Convert LocalDate.now() to LocalDate.now().toString() for String targets
        if (javaSource.contains("LocalDate.now()")) {
            code.append(indent).append("// COBOL: MOVE ").append(source).append(" TO ").append(target).append("\n");
            code.append(indent).append("// Converting LocalDate to String for WORKING STORAGE field\n");
            code.append(indent).append(javaSetter).append("(LocalDate.now().toString());");
            code.append("\n");
            return code.toString();
        }
        
        // Detect LocalDate → Integer conversion (source is DATE field, target is not)
        // Use token-aware matching to avoid false positives like "updated"
        String sourceUpper = source != null ? source.toUpperCase() : "";
        String targetUpper = target != null ? target.toUpperCase() : "";
        // Match DATE as its own token (e.g., -DATE or _DATE or exact DATE)
        boolean sourceIsDate = (javaSource.contains("get") && javaSource.toLowerCase().matches(".*\\bdate\\b.*")) ||
                      sourceUpper.matches(".*(^|[-_])DATE($|[-_]).*");
        boolean targetIsDate = targetUpper.matches(".*(^|[-_])DATE($|[-_]).*") || targetUpper.contains("UPDATE");
        
        boolean needsDateToIntConversion = sourceIsDate && !targetIsDate;
        
        // Detect Integer → LocalDate conversion (source is numeric, target is DATE field)
        boolean needsIntToDateConversion = (javaSource.matches("\\d+") || 
                                           (javaSource.contains("get") && !sourceIsDate)) &&
                                          targetIsDate;
        
        if (needsDateToIntConversion) {
            // Convert LocalDate to Integer (YYYYMMDD format)
            code.append(indent).append("// COBOL: MOVE ").append(source).append(" TO ").append(target).append("\n");
            code.append(indent).append("// Converting LocalDate to Integer (YYYYMMDD format)\n");
            code.append(indent).append(javaSetter).append("(\n");
            code.append(indent).append("    Integer.parseInt(").append(javaSource);
            code.append(".format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE))\n");
            code.append(indent).append(");");
        } else if (needsIntToDateConversion) {
            // Convert Integer to LocalDate (parse YYYYMMDD format)
            code.append(indent).append("// COBOL: MOVE ").append(source).append(" TO ").append(target).append("\n");
            code.append(indent).append("// Converting Integer to LocalDate (YYYYMMDD format)\n");
            code.append(indent).append(javaSetter).append("(\n");
            code.append(indent).append("    java.time.LocalDate.parse(String.valueOf(").append(javaSource);
            code.append("), java.time.format.DateTimeFormatter.BASIC_ISO_DATE)\n");
            code.append(indent).append(");");
        } else {
            // Regular MOVE without conversion
            code.append(indent).append("// COBOL: MOVE ").append(source).append(" TO ").append(target).append("\n");
            code.append(indent).append(javaSetter).append("(").append(javaSource).append(");");
        }
        code.append("\n");
        
        return code.toString();
    }

    /**
     * Translates COBOL COMPUTE statement with validation.
     */
    private String translateCompute(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        if (!isValidStatement(stmt, stmt.getTarget(), stmt.getExpression())) {
            return indent + "// TODO: Invalid COMPUTE - missing target or expression\n";
        }
        
        String target = stmt.getTarget();
        String expression = stmt.getExpression();
        String setter = toJavaSetter(target, recordType);
        String javaExpr = translateArithmeticExpression(expression, recordType);
        
        code.append(indent).append("// COBOL: COMPUTE ").append(target).append(" = ").append(expression).append("\n");
        code.append(indent).append(setter).append("(").append(javaExpr).append(");\n");
        
        return code.toString();
    }

    /**
     * Translates COBOL PERFORM statement with improved validation.
     * Enhanced to detect file processing loops (PERFORM UNTIL EOF).
     */
    private String translatePerform(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();

        String paragraphName = stmt.getParagraphName();
        if (paragraphName == null || paragraphName.trim().isEmpty()) {
            return indent + "// TODO: PERFORM statement without paragraph name\n";
        }

        String methodName = toJavaMethodName(paragraphName);

        if (stmt.getPerformTimes() != null && stmt.getPerformTimes() > 0) {
            // PERFORM n TIMES
            code.append(indent).append("// COBOL: PERFORM ").append(paragraphName).append(" ")
                .append(stmt.getPerformTimes()).append(" TIMES\n");
            code.append(indent).append("for (int i = 0; i < ").append(stmt.getPerformTimes()).append("; i++) {\n");
            code.append(indent).append("    ").append(methodName).append("(record);\n");
            code.append(indent).append("}\n");
        } else if (stmt.getUntilCondition() != null && !stmt.getUntilCondition().trim().isEmpty()) {
            // PERFORM UNTIL - Check if it's a file processing loop
            String condition = stmt.getUntilCondition().trim();

            // Detect EOF patterns: WS-EOF = 'Y', EOF-FLAG = 'Y', etc.
            // Only match when there's an explicit comparison with 'Y' or when it's a flag assignment pattern
            boolean isFileProcessingLoop = condition.matches(".*EOF.*=.*['\"]Y['\"].*") ||
                                          condition.matches(".*EOF-FLAG.*=.*['\"]Y['\"].*") ||
                                          condition.matches(".*WS-EOF.*=.*['\"]Y['\"].*");

            if (isFileProcessingLoop) {
                // This is a file processing loop - handled by Spring Batch framework
                code.append(indent).append("/* COBOL: PERFORM UNTIL ").append(condition).append("\n");
                code.append(indent).append(" * \n");
                code.append(indent).append(" * This PERFORM UNTIL loop pattern is automatically handled by Spring Batch:\n");
                code.append(indent).append(" * - The READ statement is replaced by ItemReader.read()\n");
                code.append(indent).append(" * - The EOF condition (").append(condition).append(") is detected when reader returns null\n");
                code.append(indent).append(" * - The framework loops automatically calling this process() method for each record\n");
                code.append(indent).append(" * - No explicit loop code is needed in Spring Batch ItemProcessor\n");
                code.append(indent).append(" *\n");
                code.append(indent).append(" * Original COBOL logic:\n");
                code.append(indent).append(" *   PERFORM UNTIL ").append(condition).append("\n");
                code.append(indent).append(" *       READ FILE AT END MOVE 'Y' TO EOF-FLAG\n");
                code.append(indent).append(" *       PERFORM PROCESS-RECORD\n");
                code.append(indent).append(" *   END-PERFORM\n");
                code.append(indent).append(" *\n");
                code.append(indent).append(" * Spring Batch equivalent:\n");
                code.append(indent).append(" *   - ItemReader reads one record at a time\n");
                code.append(indent).append(" *   - This process() method is called for each record\n");
                code.append(indent).append(" *   - Loop ends when reader returns null (EOF)\n");
                code.append(indent).append(" */\n");
                code.append(indent).append("// Processing logic for paragraph: ").append(paragraphName).append("\n");
                code.append(indent).append(methodName).append("(record);\n");
            } else {
                // Regular PERFORM UNTIL
                String javaCondition = translateCobolCondition(condition);
                code.append(indent).append("// COBOL: PERFORM ").append(paragraphName).append(" UNTIL ").append(condition).append("\n");
                code.append(indent).append("while (!(").append(javaCondition).append(")) {\n");
                code.append(indent).append("    ").append(methodName).append("(record);\n");
                code.append(indent).append("}\n");
            }
        } else {
            // Simple PERFORM
            code.append(indent).append("// COBOL: PERFORM ").append(paragraphName).append("\n");
            code.append(indent).append(methodName).append("(record);\n");
        }

        return code.toString();
    }

    /**
     * Translates COBOL ADD statement.
     * Enhanced to detect counter patterns and suggest Spring Batch metrics.
     */
    private String translateAdd(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();

        // Check if this is a counter increment pattern (ADD 1 TO counter)
        boolean isCounterIncrement = stmt.getSource() != null &&
                                    stmt.getSource().trim().equals("1") &&
                                    stmt.getTarget() != null &&
                                    stmt.getTarget().matches(".*(-COUNT|-READ|-PROCESSED|-ERROR|-UPDATED)");

        if (isCounterIncrement) {
            // Suggest using Spring Batch metrics for counters
            code.append(indent).append("// COBOL: ADD 1 TO ").append(stmt.getTarget()).append("\n");
            code.append(indent).append("// NOTE: Consider using Spring Batch StepExecution for counter tracking\n");
        }

        // Use consolidated arithmetic operation translator with BigDecimal support
        String arithmeticCode = translateArithmeticOperation(stmt, recordType, indent, "ADD", "add");
        code.append(arithmeticCode);

        return code.toString();
    }

    /**
     * Translates COBOL SUBTRACT statement.
     */
    private String translateSubtract(Statement stmt, String recordType, String indent) {
        return translateArithmeticOperation(stmt, recordType, indent, "SUBTRACT", "subtract");
    }

    /**
     * Translates COBOL MULTIPLY statement.
     */
    private String translateMultiply(Statement stmt, String recordType, String indent) {
        return translateArithmeticOperation(stmt, recordType, indent, "MULTIPLY", "multiply");
    }

    /**
     * Translates COBOL DIVIDE statement.
     * Improved to handle BigDecimal operations properly.
     */
    private String translateDivide(Statement stmt, String recordType, String indent) {
        // Use the consolidated arithmetic translator which handles BigDecimal correctly
        return translateArithmeticOperation(stmt, recordType, indent, "DIVIDE", "divide");
    }

    /**
     * Consolidated arithmetic operation translator.
     * Handles ADD, SUBTRACT, MULTIPLY with consistent pattern.
     * Fixed to properly handle BigDecimal vs Integer operations.
     */
    private String translateArithmeticOperation(Statement stmt, String recordType, String indent,
                                                String cobolOp, String javaMethod) {
        StringBuilder code = new StringBuilder();

        // Validate inputs
        if (!isValidStatement(stmt, stmt.getSource(), stmt.getTarget())) {
            return indent + "// TODO: Invalid arithmetic operation - missing source or target\n";
        }

        String target = stmt.getTarget();
        String getter = toJavaGetter(target, recordType);

        // Check if target is BigDecimal type
        boolean targetIsBigDecimal = isBigDecimalExpression(getter, target);

        // Convert source to Java expression
        String source = toJavaExpression(stmt.getSource(), recordType);

        // If target is BigDecimal, ensure source is also BigDecimal
        // This applies even for getters that are not monetary types
        if (targetIsBigDecimal) {
            logger.debug("Target is BigDecimal: {}, source before: '{}'", target, source);

            boolean sourceIsBigDecimal = isBigDecimalExpression(source, stmt.getSource());

            if (!sourceIsBigDecimal && source != null) {
                // Numeric literal fast-path
                if (source.matches("^\\d+$")) {
                    if (source.equals("1")) {
                        source = "BigDecimal.ONE";
                    } else if (source.equals("0")) {
                        source = "BigDecimal.ZERO";
                    } else {
                        source = "BigDecimal.valueOf(" + source + ")";
                    }
                } else {
                    // Wrap any non-BigDecimal expression (including getters) safely
                    source = "BigDecimal.valueOf(" + source + ")";
                }
                logger.debug("Source coerced to BigDecimal: '{}'", source);
            }
        }

        String setter = toJavaSetter(target, recordType);

        code.append(indent).append("// COBOL: ").append(cobolOp).append(" ").append(stmt.getSource())
            .append(" TO ").append(target).append("\n");
        
        // Generate the correct code based on whether target is BigDecimal or Integer
        if (targetIsBigDecimal) {
            // BigDecimal: use .add(), .subtract(), .multiply(), .divide()
            if (javaMethod.equals("divide")) {
                // For divide, add rounding mode
                code.append(indent).append(setter).append("(").append(getter).append(".")
                    .append(javaMethod).append("(").append(source)
                    .append(", 2, java.math.RoundingMode.HALF_UP));");
            } else {
                code.append(indent).append(setter).append("(").append(getter).append(".")
                    .append(javaMethod).append("(").append(source).append("));");
            }
        } else {
            // Check if source is also a BigDecimal getter - if so, must use BigDecimal methods
            boolean sourceIsBigDecimal = isBigDecimalExpression(source, stmt.getSource());
            
            if (sourceIsBigDecimal) {
                // Target is NOT BigDecimal, but source is BigDecimal.
                // Convert source to int for arithmetic with integer targets.
                String srcAsInt = "(" + source + ").intValue()";
                String operator = javaMethod.equals("add") ? "+" : 
                                javaMethod.equals("subtract") ? "-" : 
                                javaMethod.equals("multiply") ? "*" : 
                                javaMethod.equals("divide") ? "/" : "+" ;
                code.append(indent).append(setter).append("(").append(getter)
                    .append(" ").append(operator).append(" ").append(srcAsInt).append(");");
            } else {
                // Integer/primitive: use + operator (not +=, which can't be in method argument)
                String operator = javaMethod.equals("add") ? "+" : 
                                javaMethod.equals("subtract") ? "-" : 
                                javaMethod.equals("multiply") ? "*" :
                                javaMethod.equals("divide") ? "/" : "+" ;
                code.append(indent).append(setter).append("(").append(getter)
                    .append(" ").append(operator).append(" ").append(source).append(");");
            }
        }
        code.append("\n");

        return code.toString();
    }

    /**
     * Validates if a statement has required fields.
     */
    private boolean isValidStatement(Statement stmt, String... requiredFields) {
        if (stmt == null) return false;
        for (String field : requiredFields) {
            if (field == null || field.trim().isEmpty()) {
                return false;
            }
        }
        return true;
    }

    /**
     * Translates COBOL DISPLAY statement with validation.
     */
    private String translateDisplay(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();

        code.append(indent).append("// COBOL: DISPLAY ").append(stmt.getSource() != null ? stmt.getSource() : "").append("\n");

        // DISPLAY can have multiple arguments: "DISPLAY 'text' FIELD1 'more' FIELD2"
        // Parse them and generate appropriate logger.info with placeholders
        String source = stmt.getSource() != null ? stmt.getSource().trim() : "";

        if (!source.isEmpty()) {
            java.util.List<String> parts = parseDisplayArguments(source);

            if (parts.size() == 1) {
                // Single argument
                String message = toJavaExpression(parts.get(0), recordType);
                code.append(indent).append("logger.info(").append(message).append(");\n");
            } else {
                // Multiple arguments - build format string and args
                StringBuilder format = new StringBuilder();
                java.util.List<String> args = new java.util.ArrayList<>();

                for (String part : parts) {
                    if (part.startsWith("'") && part.endsWith("'")) {
                        // String literal
                        format.append(part.substring(1, part.length() - 1));
                    } else {
                        // Field reference - add placeholder
                        format.append("{}");
                        args.add(toJavaGetter(part, recordType));
                    }
                }

                code.append(indent).append("logger.info(\"").append(format.toString()).append("\"");
                for (String arg : args) {
                    code.append(", ").append(arg);
                }
                code.append(");\n");
            }
        } else {
            code.append(indent).append("logger.info(\"\");\n");
        }

        return code.toString();
    }

    /**
     * Parses DISPLAY statement arguments into separate parts.
     * Example: "'HIGH VALUE: ' CUST-NAME" -> ["'HIGH VALUE: '", "CUST-NAME"]
     */
    private java.util.List<String> parseDisplayArguments(String source) {
        java.util.List<String> parts = new java.util.ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;

        for (int i = 0; i < source.length(); i++) {
            char c = source.charAt(i);

            if (c == '\'' || c == '"') {
                inQuotes = !inQuotes;
                current.append(c);
            } else if (c == ' ' && !inQuotes) {
                // Space outside quotes - potential separator
                if (current.length() > 0) {
                    parts.add(current.toString().trim());
                    current = new StringBuilder();
                }
            } else {
                current.append(c);
            }
        }

        // Add last part
        if (current.length() > 0) {
            parts.add(current.toString().trim());
        }

        return parts;
    }

    /**
     * Generic translation for unsupported statement types.
     */
    private String translateGeneric(Statement stmt, String indent) {
        return indent + "// TODO: Translate " + stmt.getType() + " statement\n" +
               indent + "// COBOL: " + (stmt.getOriginalCobol() != null ? stmt.getOriginalCobol() : stmt.getType()) + "\n\n";
    }

    /**
     * Translates COBOL condition to Java boolean expression.
     * Improved to handle more complex conditions and edge cases.
     */
    private String translateCobolCondition(String condition) {
        if (condition == null || condition.trim().isEmpty()) {
            return "true";
        }

        String result = condition.trim();

        // Translate COBOL class conditions FIRST (before NOT gets translated to !)
        // NOTE: isNumeric() will be post-processed later to handle LocalDate fields
        result = result.replaceAll("(?i)\\bIS\\s+NOT\\s+NUMERIC\\b", ".isNumeric() == false");
        result = result.replaceAll("(?i)\\bNOT\\s+NUMERIC\\b", ".isNumeric() == false");
        result = result.replaceAll("(?i)\\bIS\\s+NUMERIC\\b", ".isNumeric()");
        result = result.replaceAll("(?i)\\bNUMERIC\\b", ".isNumeric()");
        result = result.replaceAll("(?i)\\bIS\\s+NOT\\s+ALPHABETIC\\b", ".isAlphabetic() == false");
        result = result.replaceAll("(?i)\\bNOT\\s+ALPHABETIC\\b", ".isAlphabetic() == false");
        result = result.replaceAll("(?i)\\bIS\\s+ALPHABETIC\\b", ".isAlphabetic()");

        // Translate COBOL comparison operators (temporarily, will be post-processed)
        result = result.replaceAll("(?i)\\bNOT\\s*=", " NOT_EQUALS ");
        result = result.replaceAll("(?i)\\bIS\\s+NOT\\s+EQUAL\\s+TO\\b", " NOT_EQUALS ");
        result = result.replaceAll("(?i)\\bIS\\s+NOT\\s+EQUAL", " NOT_EQUALS ");
        result = result.replaceAll("(?i)\\bIS\\s+EQUAL\\s+TO\\b", " EQUALS ");
        result = result.replaceAll("(?i)\\bIS\\s+EQUAL", " EQUALS ");
        // Match = operator with whitespace on both sides (no word boundary required)
        // This handles: "field = value", "getter() = value", etc.
        result = result.replaceAll("\\s+=\\s+", " EQUALS ");

        // NOW translate COBOL logical operators (after NOT= and class conditions have been handled)
        result = result.replaceAll("(?i)\\bAND\\b", "&&");
        result = result.replaceAll("(?i)\\bOR\\b", "||");
        result = result.replaceAll("(?i)\\bNOT\\b", "!");

        // Convert COBOL single-quoted literals to Java double-quoted String literals
        // This fixes char literal errors like '00' -> "00"
        result = result.replaceAll("'([^']*)'", "\"$1\"");

        // Translate COBOL special values
        result = result.replaceAll("(?i)\\bZERO(?!S)", "0");
        result = result.replaceAll("(?i)\\bZEROS\\b", "0");
        result = result.replaceAll("(?i)\\bSPACES?\\b", "\" \"");
        result = result.replaceAll("(?i)\\bHIGH\\s+VALUE", "Integer.MAX_VALUE");
        result = result.replaceAll("(?i)\\bLOW\\s+VALUE", "Integer.MIN_VALUE");

        // Convert COBOL field names to Java getters
        // Look for COBOL identifiers (words with hyphens like CUST-AMOUNT, WS-COUNT)
        // and convert them to getter calls like record.getCustAmount()
        result = convertFieldNamesToGetters(result);

        // FIX: Remove .isNumeric() calls on date fields (LocalDate doesn't have isNumeric())
        // Pattern: getTrTransactionDate() .isNumeric() -> true (dates are always "numeric" in COBOL terms)
        result = fixIsNumericOnDateFields(result);

        // Convert BigDecimal arithmetic operations in conditions
        // Example: getMaOverdraftLimit() * -1 -> getMaOverdraftLimit().multiply(new BigDecimal(-1))
        result = convertBigDecimalArithmeticInConditions(result);

        // Post-process comparisons to use type-safe methods
        result = postProcessComparisons(result);

        return result;
    }

    /**
     * Fixes .isNumeric() calls on date fields.
     * LocalDate fields don't have isNumeric() method.
     * In COBOL, date fields (PIC 9(8)) are always numeric, so we replace with appropriate validation.
     */
    private String fixIsNumericOnDateFields(String expression) {
        // Pattern to match: getXxxDate() .isNumeric() or getXxxDate().isNumeric()
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(
            "(\\w+\\.)?get\\w*[Dd]ate\\(\\)\\s*\\.isNumeric\\(\\)(\\s*==\\s*false|\\s*==\\s*true)?"
        );
        
        java.util.regex.Matcher matcher = pattern.matcher(expression);
        StringBuffer result = new StringBuffer();
        
        while (matcher.find()) {
            String fullMatch = matcher.group(0);
            String comparison = matcher.group(2); // == false or == true
            
            // Determine replacement based on comparison
            String replacement;
            if (comparison != null && comparison.contains("false")) {
                // .isNumeric() == false -> field is null or invalid
                // For dates, check if null
                replacement = matcher.group(1) != null ?
                    matcher.group(1) + "get" + fullMatch.substring(fullMatch.indexOf("get") + 3, fullMatch.indexOf("(")) + "() == null" :
                    "get" + fullMatch.substring(fullMatch.indexOf("get") + 3, fullMatch.indexOf("(")) + "() == null";
            } else {
                // .isNumeric() or .isNumeric() == true -> field is valid
                // For dates, check if not null
                replacement = matcher.group(1) != null ?
                    matcher.group(1) + "get" + fullMatch.substring(fullMatch.indexOf("get") + 3, fullMatch.indexOf("(")) + "() != null" :
                    "get" + fullMatch.substring(fullMatch.indexOf("get") + 3, fullMatch.indexOf("(")) + "() != null";
            }
            
            matcher.appendReplacement(result, java.util.regex.Matcher.quoteReplacement(replacement));
        }
        
        matcher.appendTail(result);

        // Final pass: handle BigDecimal and Integer/Long relational comparisons
        // Use manual parsing instead of regex to handle complex parenthesized expressions
        String workingResult = result.toString();
        StringBuilder finalResult = new StringBuilder();
        int pos = 0;

        // Pattern to find getter() followed by comparison operator
        java.util.regex.Pattern getterPattern = java.util.regex.Pattern.compile(
            "([\\w.]+get[A-Z]\\w+\\(\\))\\s*(<=|>=|<|>|==|!=)\\s*"
        );

        matcher = getterPattern.matcher(workingResult);

        while (matcher.find(pos)) {
            String left = matcher.group(1).trim();
            String operator = matcher.group(2);
            int rightStart = matcher.end();

            // Skip if already using compareTo or equals
            if (left.contains("compareTo(") || left.contains(".equals(")) {
                finalResult.append(workingResult, pos, matcher.end());
                pos = matcher.end();
                continue;
            }

            // Extract right operand - handle parentheses properly
            String rightOperand = extractRightOperand(workingResult, rightStart);
            int rightEnd = rightStart + rightOperand.length();

            // Append everything before this comparison
            finalResult.append(workingResult, pos, matcher.start());

            // Process the comparison
            String processedRight = rightOperand.trim();

            // Strip and process parenthesized expressions
            if (processedRight.startsWith("(") && processedRight.endsWith(")")) {
                String inner = processedRight.substring(1, processedRight.length() - 1).trim();
                inner = convertBigDecimalArithmeticInConditions(inner);
                processedRight = inner;
            } else {
                processedRight = convertBigDecimalArithmeticInConditions(processedRight);
            }

            // Check if this is a BigDecimal comparison
            if (isBigDecimalOperand(left) || isBigDecimalOperand(processedRight)) {
                // If right operand is a simple literal
                if (processedRight.matches("-?\\d+")) {
                    processedRight = "0".equals(processedRight) ? "BigDecimal.ZERO" : "new BigDecimal(" + processedRight + ")";
                }
                finalResult.append(left).append(".compareTo(").append(processedRight).append(") ").append(operator).append(" 0");
            }
            // Check if this is an Integer/Long comparison
            else if (rightOperand.trim().matches("-?\\d+") && isIntegerOrLongOperand(left)) {
                finalResult.append(left).append(" ").append(operator).append(" ").append(rightOperand.trim());
            } else {
                // Keep as is
                finalResult.append(left).append(" ").append(operator).append(" ").append(rightOperand.trim());
            }

            pos = rightEnd;
        }

        // Append remaining text
        finalResult.append(workingResult.substring(pos));
        return finalResult.toString();
    }

    /**
     * Extracts the right operand from an expression starting at a given position.
     * Handles parenthesized expressions correctly by counting parentheses.
     * Stops at logical operators (&&, ||) or end of string.
     */
    private String extractRightOperand(String expression, int startPos) {
        if (startPos >= expression.length()) {
            return "";
        }

        int pos = startPos;
        int parenCount = 0;
        boolean inParens = false;

        // Skip leading whitespace
        while (pos < expression.length() && Character.isWhitespace(expression.charAt(pos))) {
            pos++;
        }

        int start = pos;

        // Scan until we hit a logical operator or end
        while (pos < expression.length()) {
            char c = expression.charAt(pos);

            if (c == '(') {
                parenCount++;
                inParens = true;
            } else if (c == ')') {
                parenCount--;
                if (parenCount == 0 && inParens) {
                    pos++; // Include the closing paren
                    break;
                }
            } else if (parenCount == 0) {
                // Check for logical operators at top level
                if (pos + 1 < expression.length()) {
                    String twoChar = expression.substring(pos, pos + 2);
                    if (twoChar.equals("&&") || twoChar.equals("||")) {
                        break;
                    }
                }
                // Check for single ) at top level (end of enclosing condition)
                if (c == ')') {
                    break;
                }
            }

            pos++;
        }

        return expression.substring(start, pos);
    }

    /**
     * Converts arithmetic operations on BigDecimal fields to proper BigDecimal method calls.
     * Handles operations like: getter() * -1, getter() * 2, etc.
     */
    private String convertBigDecimalArithmeticInConditions(String expression) {
        // Pattern to match: <getter_call>() <operator> <literal>
        // Example: getMaOverdraftLimit() * -1 or (getMaOverdraftLimit() * -1)
        // Handle both with and without parentheses
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(
            "([\\w.]+get[A-Z]\\w+\\(\\))\\s*([*+/\\-])\\s*(-?\\d+)"
        );
        
        java.util.regex.Matcher matcher = pattern.matcher(expression);
        StringBuffer result = new StringBuffer();
        
        boolean hasMatch = false;
        while (matcher.find()) {
            hasMatch = true;
            String getterCall = matcher.group(1);
            String operator = matcher.group(2);
            String literal = matcher.group(3);
            
            // Determine if this is likely a BigDecimal field (contains amount, balance, etc.)
            if (isBigDecimalExpression(getterCall, "")) {
                String methodName;
                switch (operator) {
                    case "*": methodName = "multiply"; break;
                    case "+": methodName = "add"; break;
                    case "-": methodName = "subtract"; break;
                    case "/": methodName = "divide"; break;
                    default: methodName = "add"; break;
                }
                
                String replacement;
                if ("/".equals(operator)) {
                    // Division needs rounding mode
                    replacement = getterCall + "." + methodName + "(new BigDecimal(" + literal + "), 2, java.math.RoundingMode.HALF_UP)";
                } else {
                    replacement = getterCall + "." + methodName + "(new BigDecimal(" + literal + "))";
                }
                matcher.appendReplacement(result, java.util.regex.Matcher.quoteReplacement(replacement));
            }
        }

        matcher.appendTail(result);
        return result.toString();
    }

    /**
     * Normalizes simple BigDecimal arithmetic operands so they are safe for compareTo()
     */
    private String normalizeBigDecimalArithmeticOperand(String operand) {
        if (operand == null) {
            return operand;
        }

        String trimmed = operand.trim();
        if (trimmed.startsWith("(") && trimmed.endsWith(")")) {
            trimmed = trimmed.substring(1, trimmed.length() - 1).trim();
        }

        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(
            "([\\w.]+get[A-Z]\\w+\\(\\))\\s*([*+/\\-])\\s*(-?\\d+)"
        );
        java.util.regex.Matcher matcher = pattern.matcher(trimmed);

        if (!matcher.matches()) {
            return operand;
        }

        String getterCall = matcher.group(1);
        String operator = matcher.group(2);
        String literal = matcher.group(3);

        if (!isBigDecimalExpression(getterCall, "")) {
            return operand;
        }

        String methodName;
        switch (operator) {
            case "*": methodName = "multiply"; break;
            case "+": methodName = "add"; break;
            case "-": methodName = "subtract"; break;
            case "/": methodName = "divide"; break;
            default: methodName = "add"; break;
        }

        if ("/".equals(operator)) {
            return getterCall + "." + methodName + "(new BigDecimal(" + literal + "), 2, java.math.RoundingMode.HALF_UP)";
        }
        return getterCall + "." + methodName + "(new BigDecimal(" + literal + "))";
    }

    /**
     * Post-processes comparisons to use type-safe comparison methods based on detected types
     */
    private String postProcessComparisons(String expression) {
        // Pattern to match: <left operand> (EQUALS|NOT_EQUALS|<=|>=|<|>) <right operand>
        // Process EQUALS/NOT_EQUALS first
        java.util.regex.Pattern equalsPattern = java.util.regex.Pattern.compile(
            "([^&|!]+?)\\s+(EQUALS|NOT_EQUALS)\\s+([^&|]+?)(?=\\s*(?:&&|\\|\\||\\)|$))"
        );

        java.util.regex.Matcher matcher = equalsPattern.matcher(expression);
        StringBuffer result = new StringBuffer();

        while (matcher.find()) {
            String left = matcher.group(1).trim();
            String operator = matcher.group(2);
            String right = matcher.group(3).trim();

            String comparison = generateTypeSafeComparison(left, right, operator);
            matcher.appendReplacement(result, java.util.regex.Matcher.quoteReplacement(comparison));
        }

        matcher.appendTail(result);
        
        // Now process relational operators (<=, >=, <, >)
        java.util.regex.Pattern relationalPattern = java.util.regex.Pattern.compile(
            "([^<>=&|!]+?)\\s*(<=|>=|<|>)\\s*(.+?)(?=\\s*(?:&&|\\|\\||\\)|$))"
        );

        matcher = relationalPattern.matcher(result.toString());
        StringBuffer finalResult = new StringBuffer();

        while (matcher.find()) {
            String left = matcher.group(1).trim();
            String operator = matcher.group(2);
            String right = matcher.group(3).trim();

            // Normalize parentheses
            if (left.startsWith("(") && left.endsWith(")")) {
                left = left.substring(1, left.length() - 1).trim();
            }
            if (right.startsWith("(") && right.endsWith(")")) {
                right = right.substring(1, right.length() - 1).trim();
            }

            // Ensure arithmetic BigDecimal operands are converted to method calls before compareTo
            right = normalizeBigDecimalArithmeticOperand(right);
            left = normalizeBigDecimalArithmeticOperand(left);

            // Skip if operands already use compareTo
            if (left.contains("compareTo(") || right.contains("compareTo(")) {
                matcher.appendReplacement(finalResult, matcher.group(0));
                continue;
            }

            if (isBigDecimalOperand(left) || isBigDecimalOperand(right)) {
                if (right.matches("-?\\d+")) {
                    right = "0".equals(right) ? "BigDecimal.ZERO" : "new BigDecimal(" + right + ")";
                }
                String comparison = left + ".compareTo(" + right + ") " + operator + " 0";
                matcher.appendReplacement(finalResult, java.util.regex.Matcher.quoteReplacement(comparison));
            }
        }

        matcher.appendTail(finalResult);
        return finalResult.toString();
    }

    /**
     * Generates type-safe comparison based on operand types
     */
    private String generateTypeSafeComparison(String left, String right, String operator) {
        // Detect if operands are strings or BigDecimal
        boolean leftIsString = isStringOperand(left);
        boolean rightIsString = isStringOperand(right);
        boolean leftIsBigDecimal = isBigDecimalOperand(left);
        boolean rightIsBigDecimal = isBigDecimalOperand(right);

        if (leftIsString || rightIsString) {
            // Use .equals() for strings
            if ("EQUALS".equals(operator)) {
                if (right.startsWith("\"")) {
                    return right + ".equals(" + left + ")";
                } else {
                    return left + ".equals(" + right + ")";
                }
            } else { // NOT_EQUALS
                if (right.startsWith("\"")) {
                    return "!" + right + ".equals(" + left + ")";
                } else {
                    return "!" + left + ".equals(" + right + ")";
                }
            }
        } else if (leftIsBigDecimal || rightIsBigDecimal) {
            // Convert right operand to BigDecimal if it's a literal
            if (right.matches("-?\\d+") && !right.contains("BigDecimal")) {
                right = "0".equals(right) ? "BigDecimal.ZERO" : "new BigDecimal(" + right + ")";
            }
            
            // Use .compareTo() for BigDecimal
            if ("EQUALS".equals(operator)) {
                return left + ".compareTo(" + right + ") == 0";
            } else { // NOT_EQUALS
                return left + ".compareTo(" + right + ") != 0";
            }
        } else {
            // Use == or != for primitives
            if ("EQUALS".equals(operator)) {
                return left + " == " + right;
            } else { // NOT_EQUALS
                return left + " != " + right;
            }
        }
    }

    /**
     * Checks if an operand is a string type
     */
    private boolean isStringOperand(String operand) {
        // Quoted string
        if (operand.startsWith("\"") && operand.endsWith("\"")) {
            return true;
        }
        // String getter patterns
        return operand.contains("get") &&
               (operand.toLowerCase().contains("status") ||
                operand.toLowerCase().contains("code") ||
                operand.toLowerCase().contains("type") ||
                operand.toLowerCase().contains("name") ||
                operand.toLowerCase().contains("flag") ||
                (operand.toLowerCase().contains("id") && !operand.toLowerCase().contains("number")));
    }

    /**
     * Checks if an operand is a BigDecimal type
     */
    private boolean isBigDecimalOperand(String operand) {
        // BigDecimal constant
        if (operand.contains("BigDecimal") || operand.contains("ZERO")) {
            return true;
        }
        // BigDecimal getter patterns
        String lower = operand.toLowerCase();
        return operand.contains("get") &&
               (lower.contains("amount") ||
            lower.contains("balance") ||
            lower.contains("total") ||
            lower.contains("price") ||
            lower.contains("quantity") ||
            lower.contains("hours") ||
            lower.contains("salary") ||
            lower.contains("pay") ||
            lower.contains("rate") ||
            lower.contains("gross") ||
            lower.contains("limit") ||
            lower.contains("overdraft"));
    }

    /**
     * Checks if an operand is an Integer or Long type getter.
     * Used to detect numeric getters that return boxed types (Integer, Long)
     * which need special handling in comparisons.
     */
    private boolean isIntegerOrLongOperand(String operand) {
        // If it doesn't contain "get", it's not a getter
        if (!operand.contains("get")) {
            return false;
        }

        // Exclude BigDecimal getters
        if (isBigDecimalOperand(operand)) {
            return false;
        }

        // Check for common Integer/Long counter patterns
        // NOTE: Be conservative here - only include patterns that are NEVER BigDecimal
        String lower = operand.toLowerCase();
        return lower.contains("counter") ||
               lower.contains("count") ||
               lower.contains("id") ||
               lower.contains("number") ||
               lower.contains("size") ||
               lower.contains("length");
    }

    /**
     * Converts COBOL field names in an expression to Java getter calls.
     * Example: "CUST-AMOUNT > 1000" -> "record.getCustAmount() > 1000"
     * Working Storage fields (WS-*) are accessed directly as class fields.
     */
    private String convertFieldNamesToGetters(String expression) {
        // Match COBOL identifiers: alphanumeric with hyphens, not starting with a digit
        // Exclude numbers and string literals
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(
            "\\b([A-Z][A-Z0-9-]*[A-Z0-9])\\b"
        );

        java.util.regex.Matcher matcher = pattern.matcher(expression);
        StringBuffer result = new StringBuffer();

        while (matcher.find()) {
            String identifier = matcher.group(1);

            // Skip if it's a Java keyword or operator that we already translated
            if (isJavaKeywordOrOperator(identifier)) {
                matcher.appendReplacement(result, identifier);
                continue;
            }

            // Check if it's a WORKING STORAGE field (WS- prefix) or a flag/switch
            String javaFieldName = toJavaFieldName(identifier);
            
            // Check if this is an 88-level condition - use is*() instead of get*()
            boolean isCondition = conditionNames.contains(identifier) || 
                                  conditionNames.contains(identifier.replace("-", "_")) ||
                                  conditionNames.contains(identifier.replace("_", "-")) ||
                                  conditionNames.contains(javaFieldName) ||
                                  conditionNames.contains(javaFieldName.toUpperCase());
            
            String methodPrefix = isCondition ? "is" : "get";
            String accessCode;
            
            // Check if it's a secondary file field (like MA-CLOSED from MASTER-ACCOUNT-FILE)
            String filePrefix = detectFilePrefix(identifier);
            
            if (filePrefix != null) {
                // Field belongs to a secondary file - access via that file's variable
                String varName = getSecondaryFileVariableName(filePrefix);
                accessCode = varName + "." + methodPrefix + capitalize(javaFieldName) + "()";
            } else if (identifier.startsWith("WS-") || isWorkingStorageFlag(identifier) || 
                       (!identifier.startsWith("TR-") && !identifier.startsWith("MA-") && !identifier.startsWith("UA-") && 
                        !identifier.startsWith("ER-") && !identifier.startsWith("AT-") && isCondition)) {
                // WORKING STORAGE field/flag OR 88-level condition without file prefix (is WORKING STORAGE)
                // These are always accessed via this.
                accessCode = "this." + methodPrefix + capitalize(javaFieldName) + "()";
            } else if (identifier.startsWith("TR-")) {
                // Primary file record field (TR- prefix)
                accessCode = "record." + methodPrefix + capitalize(javaFieldName) + "()";
            } else {
                // Default to record for unqualified fields
                accessCode = "record." + methodPrefix + capitalize(javaFieldName) + "()";
            }

            // Surround replacement with spaces to avoid accidental token merging
            matcher.appendReplacement(result, " " + java.util.regex.Matcher.quoteReplacement(accessCode) + " ");
        }

        matcher.appendTail(result);
        return result.toString();
    }

    /**
     * Checks if a string is a Java keyword or operator we've already translated.
     */
    private boolean isJavaKeywordOrOperator(String word) {
        if (word == null) return false;
        String upper = word.toUpperCase();
        return upper.equals("MAX") || upper.equals("MIN") ||
               upper.equals("VALUE") || upper.equals("INTEGER") ||
               upper.equals("EQUALS") || upper.equals("NOT") || upper.equals("NOT_EQUALS");
    }

    /**
     * Detects if a COBOL field is a WORKING STORAGE flag/switch based on naming patterns.
     * These fields should be treated as processor-level state variables, not record fields.
     * 
     * Common patterns for flags/switches in COBOL:
     * - Contains "FLAG" or "SWITCH"
     * - Starts with "END-OF-"
     * - Ends with "-VALID", "-EXISTS", "-OK", "-CLOSED", "-FROZEN"
     * - Special function fields like "FUNCTION-CURRENT-DATE"
     */
    private boolean isWorkingStorageFlag(String fieldName) {
        if (fieldName == null || fieldName.trim().isEmpty()) {
            return false;
        }
        
        String upper = fieldName.toUpperCase().trim();
        
        // Pattern 1: Contains FLAG or SWITCH
        if (upper.contains("FLAG") || upper.contains("SWITCH")) {
            return true;
        }
        
        // Pattern 2: Starts with END-OF- (e.g., END-OF-TRANSACTIONS)
        if (upper.startsWith("END-OF-")) {
            return true;
        }
        
        // Pattern 3: Ends with common flag/status suffixes
        if (upper.endsWith("-VALID") || upper.endsWith("-EXISTS") || 
            upper.endsWith("-OK") || upper.endsWith("-CLOSED") || 
            upper.endsWith("-FROZEN") || upper.endsWith("-FOUND") ||
            upper.endsWith("-STATUS")) {
            return true;
        }
        
        // Pattern 4: Special function fields
        if (upper.startsWith("FUNCTION-")) {
            return true;
        }
        
        // Pattern 5: Common boolean-like fields
        if (upper.equals("MORE-RECORDS") || upper.equals("NO-MORE-RECORDS") ||
            upper.equals("END-OF-FILE") || upper.equals("EOF")) {
            return true;
        }
        
        return false;
    }

    /**
     * Converts COBOL field name to Java getter call with improved validation.
     * Handles 88-level conditions by using is*() instead of get*().
     */
    private String toJavaGetter(String cobolField, String recordType) {
        if (cobolField == null || cobolField.trim().isEmpty()) {
            return "null";
        }
        
        cobolField = cobolField.trim();
        
        // Handle literals - always use double quotes for String literals
        if (cobolField.startsWith("'") && cobolField.endsWith("'")) {
            String literal = cobolField.substring(1, cobolField.length() - 1);
            return "\"" + literal + "\"";
        }
        if (cobolField.startsWith("\"") && cobolField.endsWith("\"")) {
            // Already double quoted, keep as is
            return cobolField;
        }
        if (COBOL_LITERAL_NUMBER.matcher(cobolField).matches()) {
            return cobolField.contains(".") ? 
                "new BigDecimal(\"" + cobolField + "\")" : cobolField;
        }
        
        String javaField = toJavaFieldName(cobolField);
        
        // Check if this is an 88-level condition - use is*() instead of get*()
        // Check against original COBOL name, Java name, and all variations
        boolean isCondition = conditionNames.contains(cobolField) || 
                              conditionNames.contains(cobolField.replace("-", "_")) ||
                              conditionNames.contains(cobolField.replace("_", "-")) ||
                              conditionNames.contains(javaField) ||
                              conditionNames.contains(javaField.toUpperCase());
        
        String methodPrefix = isCondition ? "is" : "get";
        
        // Check if it's a WORKING STORAGE field or flag
        if (cobolField.startsWith("WS-") || isWorkingStorageFlag(cobolField)) {
            return "this." + methodPrefix + capitalize(javaField) + "()";
        }
        
        // Check if it's a secondary file field (MA-, UA-, etc.)
        String filePrefix = detectFilePrefix(cobolField);
        if (filePrefix != null) {
            String varName = getSecondaryFileVariableName(filePrefix);
            return varName + "." + methodPrefix + capitalize(javaField) + "()";
        }
        
        return "record." + methodPrefix + capitalize(javaField) + "()";
    }

    /**
     * Converts COBOL field name to Java setter call with improved validation.
     */
    private String toJavaSetter(String cobolField, String recordType) {
        if (cobolField == null || cobolField.trim().isEmpty()) {
            return "/* invalid setter */";
        }
        
        String javaField = toJavaFieldName(cobolField);
        
        // Check if it's a WORKING STORAGE field or flag
        if (cobolField.startsWith("WS-") || isWorkingStorageFlag(cobolField)) {
            return "this.set" + capitalize(javaField);
        }
        
        // Check if it's a secondary file field (MA-, UA-, etc.)
        String filePrefix = detectFilePrefix(cobolField);
        if (filePrefix != null) {
            String varName = getSecondaryFileVariableName(filePrefix);
            return varName + ".set" + capitalize(javaField);
        }
        
        return "record.set" + capitalize(javaField);
    }

    /**
     * Converts COBOL expression to Java expression with improved handling.
     */
    private String toJavaExpression(String expr, String recordType) {
        if (expr == null || expr.trim().isEmpty()) {
            return "null";
        }
        
        expr = expr.trim();
        
        // Literals with quotes
        if (expr.startsWith("'") && expr.endsWith("'")) {
            return "\"" + expr.substring(1, expr.length() - 1) + "\"";
        }
        
        // Numeric literals
        if (COBOL_LITERAL_NUMBER.matcher(expr).matches()) {
            if (expr.contains(".")) {
                return "new BigDecimal(\"" + expr + "\")";
            }
            return expr;
        }
        
        // Special COBOL values
        if (expr.equalsIgnoreCase("ZERO") || expr.equalsIgnoreCase("ZEROS")) {
            return "BigDecimal.ZERO";
        }
        if (expr.equalsIgnoreCase("SPACE") || expr.equalsIgnoreCase("SPACES")) {
            return "\" \"";
        }
        
        // COBOL FUNCTION calls
        if (expr.toUpperCase().startsWith("FUNCTION ")) {
            return translateCobolFunction(expr);
        }
        
        // Field reference
        return toJavaGetter(expr, recordType);
    }

    /**
     * Translates COBOL FUNCTION calls to Java equivalents.
     * Examples:
     *   FUNCTION CURRENT-DATE -> LocalDate.now()
     *   FUNCTION LENGTH(field) -> field.length()
     */
    private String translateCobolFunction(String functionCall) {
        String upperFunc = functionCall.toUpperCase().trim();
        
        if (upperFunc.contains("CURRENT-DATE")) {
            return "LocalDate.now()";
        }
        
        if (upperFunc.contains("LENGTH")) {
            // Extract argument from FUNCTION LENGTH(arg)
            int startParen = functionCall.indexOf('(');
            int endParen = functionCall.lastIndexOf(')');
            if (startParen > 0 && endParen > startParen) {
                String arg = functionCall.substring(startParen + 1, endParen).trim();
                return toJavaGetter(arg, "") + ".length()";
            }
        }
        
        // Default: generate TODO
        return "/* TODO: Translate " + functionCall + " */ null";
    }

    /**
     * Extracts a string literal (quoted value) from a COBOL expression.
     * Returns the literal with Java quotes if found, otherwise returns quoted expression.
     */
    private String extractStringLiteral(String text) {
        if (text == null || text.isEmpty()) {
            return "\"\"";
        }
        
        text = text.trim();
        
        // Look for single-quoted string
        int singleStart = text.indexOf("'");
        if (singleStart >= 0) {
            int singleEnd = text.indexOf("'", singleStart + 1);
            if (singleEnd > singleStart) {
                String literal = text.substring(singleStart + 1, singleEnd);
                return "\"" + literal + "\"";
            }
        }
        
        // Look for double-quoted string
        int doubleStart = text.indexOf("\"");
        if (doubleStart >= 0) {
            int doubleEnd = text.indexOf("\"", doubleStart + 1);
            if (doubleEnd > doubleStart) {
                String literal = text.substring(doubleStart + 1, doubleEnd);
                return "\"" + literal + "\"";
            }
        }
        
        // No quoted string found, return as is with quotes
        return "\"" + text + "\"";
    }

    /**
     * Translates arithmetic expression with better variable handling.
     */
    private String translateArithmeticExpression(String expr, String recordType) {
        if (expr == null || expr.trim().isEmpty()) {
            return "BigDecimal.ZERO";
        }
        
        // Handle simple variables in expressions using regex replacement
        // This is a simple implementation - can be enhanced with proper expression parsing
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile("\\b([A-Z][A-Z0-9-]+)\\b");
        java.util.regex.Matcher matcher = pattern.matcher(expr);
        StringBuilder result = new StringBuilder();
        
        while (matcher.find()) {
            String field = matcher.group(1);
            // Check if it's a COBOL keyword or literal, not a field name
            if (!field.matches("^(ZERO|ZEROS|SPACE|SPACES|AND|OR|NOT)$")) {
                matcher.appendReplacement(result, toJavaGetter(field, recordType));
            }
        }
        matcher.appendTail(result);
        
        return result.length() > 0 ? result.toString() : expr;
    }

    /**
     * Reconstructs condition from statement operands with validation.
     * Generates type-safe comparisons for String, BigDecimal, and primitive types.
     */
    private String translateCondition(Statement stmt) {
        if (!isValidStatement(stmt, stmt.getLeftOperand(), stmt.getOperator(), stmt.getRightOperand())) {
            return "true /* invalid condition */";
        }

        String leftExpr = stmt.getLeftOperand();
        String rightExpr = stmt.getRightOperand();
        String op = stmt.getOperator();

        String left = toJavaExpression(leftExpr, "");
        String right = toJavaExpression(rightExpr, "");

        // Detect if operands are strings (quoted literals or string getters)
        boolean leftIsString = isStringExpression(left, leftExpr);
        boolean rightIsString = isStringExpression(right, rightExpr);

        // Detect if operands are BigDecimal
        boolean leftIsBigDecimal = isBigDecimalExpression(left, leftExpr);
        boolean rightIsBigDecimal = isBigDecimalExpression(right, rightExpr);

        // Generate appropriate comparison based on types
        if (leftIsString || rightIsString) {
            return generateStringComparison(left, right, op);
        } else if (leftIsBigDecimal || rightIsBigDecimal) {
            return generateBigDecimalComparison(left, right, op);
        } else {
            // Primitive types (int, long) - use direct operators
            String javaOp = translateOperator(op);
            return left + " " + javaOp + " " + right;
        }
    }

    /**
     * Checks if an expression represents a String type
     */
    private boolean isStringExpression(String javaExpr, String cobolExpr) {
        // Check if it's a quoted string literal
        if (javaExpr.startsWith("\"") && javaExpr.endsWith("\"")) {
            return true;
        }
        // Check if it's a getter that likely returns String (contains "get" and common string field patterns)
        if (javaExpr.contains("get") &&
            (javaExpr.toLowerCase().contains("status") ||
             javaExpr.toLowerCase().contains("code") ||
             javaExpr.toLowerCase().contains("type") ||
             javaExpr.toLowerCase().contains("name") ||
             javaExpr.toLowerCase().contains("id") && !javaExpr.toLowerCase().contains("number"))) {
            return true;
        }
        // Check COBOL expression for string patterns
        if (cobolExpr != null && cobolExpr.matches(".*-STATUS|.*-CODE|.*-TYPE|.*-NAME|.*-FLAG")) {
            return true;
        }
        return false;
    }

    /**
     * Checks if an expression represents a BigDecimal type
     */
    private boolean isBigDecimalExpression(String javaExpr, String cobolExpr) {
        // Check if expression contains BigDecimal
        if (javaExpr.contains("BigDecimal") || javaExpr.contains("ZERO")) {
            return true;
        }
        
        String lowerExpr = javaExpr.toLowerCase();
        String lowerCobol = (cobolExpr != null) ? cobolExpr.toLowerCase() : "";
        
        // EXCLUDE simple numeric literals
        if (lowerExpr.matches("^\\d+$") || lowerExpr.equals("1") || lowerExpr.equals("0")) {
            return false;
        }
        
        // EXCLUDE ONLY very specific counter patterns - be conservative
        // Only exclude system-level counters used by Spring Batch
        if (lowerCobol.matches(".*(record-count|line-count|page-count|items-processed).*")) {
            return false;
        }

        // Monetary patterns (getter names) - broad but safe
        if (javaExpr.contains("get") &&
            (lowerExpr.contains("amount") ||
             lowerExpr.contains("balance") ||
             lowerExpr.contains("price") ||
             lowerExpr.contains("limit") ||
             lowerExpr.contains("overdraft") ||
             lowerExpr.contains("salary") ||
             lowerExpr.contains("wage") ||
             lowerExpr.contains("pay") ||
             lowerExpr.contains("rate") ||
             lowerExpr.contains("gross") ||
             lowerExpr.contains("debit") ||
             lowerExpr.contains("credit"))) {
            return true;
        }
        
        // DEFAULT: Treat all working storage numeric fields as BigDecimal for safety
        // This ensures precision is maintained for all business calculations
        if (javaExpr.contains("get") || javaExpr.contains("this.")) {
            return true;
        }

        // TOTAL fields: use COBOL tokens to decide
        // TOTAL-amount/balance/debit(s)/credit(s) -> BigDecimal; TOTAL-items/records handled above as counters
        if (!lowerCobol.isEmpty() && lowerCobol.contains("total")) {
            if (lowerCobol.matches(".*total-(amount|balance|debit|debits|credit|credits).*")) {
                return true;
            }
            return false;
        }

        // Check COBOL expression for EXPLICIT monetary patterns (strict match)
        if (cobolExpr != null && 
            cobolExpr.matches(".*(-AMOUNT|-BALANCE|-PRICE|-LIMIT|-OVERDRAFT|-SALARY|-WAGE|-PAY|-RATE|-GROSS|-DEBIT|-DEBITS|-CREDIT|-CREDITS).*")) {
            return true;
        }
        
        return false;
    }

    /**
     * Generates type-safe String comparison using .equals()
     */
    private String generateStringComparison(String left, String right, String op) {
        String normalizedOp = op.toUpperCase().trim();

        switch (normalizedOp) {
            case "=":
            case "EQUAL":
            case "IS EQUAL":
                // Use .equals() for string equality
                if (right.startsWith("\"")) {
                    return right + ".equals(" + left + ")";
                } else {
                    return left + ".equals(" + right + ")";
                }

            case "NOT":
            case "!=":
            case "NOT EQUAL":
            case "IS NOT EQUAL":
                // Use !.equals() for string inequality
                if (right.startsWith("\"")) {
                    return "!" + right + ".equals(" + left + ")";
                } else {
                    return "!" + left + ".equals(" + right + ")";
                }

            default:
                // For other operators (>, <, etc.), fall back to default
                String javaOp = translateOperator(op);
                return left + " " + javaOp + " " + right;
        }
    }

    /**
     * Generates type-safe BigDecimal comparison using .compareTo()
     * Fixed to properly convert int literals to BigDecimal
     */
    private String generateBigDecimalComparison(String left, String right, String op) {
        String normalizedOp = op.toUpperCase().trim();

        // Ensure both sides are BigDecimal
        if (!left.contains("BigDecimal") && !left.contains("get")) {
            if (left.matches("-?\\d+(\\.\\d+)?")) {
                if (left.equals("0")) {
                    left = "BigDecimal.ZERO";
                } else if (left.equals("1")) {
                    left = "BigDecimal.ONE";
                } else {
                    left = "new BigDecimal(\"" + left + "\")";
                }
            }
        }
        if (!right.contains("BigDecimal") && !right.contains("get")) {
            if (right.matches("-?\\d+(\\.\\d+)?")) {
                if (right.equals("0")) {
                    right = "BigDecimal.ZERO";
                } else if (right.equals("1")) {
                    right = "BigDecimal.ONE";
                } else {
                    right = "new BigDecimal(\"" + right + "\")";
                }
            }
        }

        switch (normalizedOp) {
            case "=":
            case "EQUAL":
            case "IS EQUAL":
                return left + ".compareTo(" + right + ") == 0";

            case "NOT":
            case "!=":
            case "NOT EQUAL":
            case "IS NOT EQUAL":
                return left + ".compareTo(" + right + ") != 0";

            case "GREATER":
            case ">":
            case "IS GREATER":
                return left + ".compareTo(" + right + ") > 0";

            case "LESS":
            case "<":
            case "IS LESS":
                return left + ".compareTo(" + right + ") < 0";

            case ">=":
            case "NOT LESS":
            case "IS NOT LESS":
                return left + ".compareTo(" + right + ") >= 0";

            case "<=":
            case "NOT GREATER":
            case "IS NOT GREATER":
                return left + ".compareTo(" + right + ") <= 0";

            default:
                return left + ".compareTo(" + right + ") == 0";
        }
    }

    /**
     * Translates COBOL operator to Java with extended operator support.
     */
    private String translateOperator(String op) {
        if (op == null || op.trim().isEmpty()) return "==";
        
        op = op.trim().toUpperCase();
        
        switch (op) {
            case "=":
            case "EQUAL":
            case "IS EQUAL": return "==";
            
            case "NOT":
            case "!=":
            case "NOT EQUAL":
            case "IS NOT EQUAL": return "!=";
            
            case "GREATER":
            case ">":
            case "IS GREATER": return ">";
            
            case "LESS":
            case "<":
            case "IS LESS": return "<";
            
            case ">=":
            case "NOT LESS":
            case "IS NOT LESS": return ">=";
            
            case "<=":
            case "NOT GREATER":
            case "IS NOT GREATER": return "<=";
            
            default: return op;
        }
    }

    /**
     * Converts COBOL field name to Java field name (camelCase) with improved validation.
     */
    private String toJavaFieldName(String cobolName) {
        if (cobolName == null || cobolName.trim().isEmpty()) {
            return "field";
        }
        
        cobolName = cobolName.trim().toLowerCase();
        String[] parts = cobolName.split("-");
        StringBuilder result = new StringBuilder();
        
        for (int i = 0; i < parts.length; i++) {
            String part = parts[i].replaceAll("^[0-9]+", "").replaceAll("[^a-z0-9]", "");
            
            if (!part.isEmpty()) {
                if (i == 0) {
                    result.append(part);
                } else {
                    result.append(Character.toUpperCase(part.charAt(0)));
                    if (part.length() > 1) {
                        result.append(part.substring(1));
                    }
                }
            }
        }
        
        // Fallback if result is empty
        return result.length() > 0 ? result.toString() : "field";
    }

    /**
     * Converts COBOL paragraph name to Java method name.
     */
    private String toJavaMethodName(String paragraphName) {
        if (paragraphName == null || paragraphName.trim().isEmpty()) {
            return "executeLogic";
        }
        return toJavaFieldName(paragraphName);
    }

    /**
     * Capitalizes first letter with null safety.
     */
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) return str;
        return Character.toUpperCase(str.charAt(0)) + (str.length() > 1 ? str.substring(1) : "");
    }

    /**
     * Generates a TODO comment for empty or unimplemented paragraphs.
     */
    private String generateTodoComment(Paragraph paragraph) {
        StringBuilder todo = new StringBuilder();
        String paragraphName = (paragraph != null ? paragraph.getName() : "unknown");
        todo.append("        // TODO: Implement logic from COBOL paragraph: ").append(paragraphName).append("\n");
        todo.append("        // COBOL: PERFORM ").append(paragraphName).append("\n");
        return todo.toString();
    }

    // ==================== Phase 4: Advanced Statements ====================

    /**
     * Translates COBOL GO TO statement to Java.
     * GO TO is discouraged in modern code, so we generate a comment with suggestion.
     */
    /**
     * Translates COBOL GO TO statement.
     * Supports both simple GO TO and GO TO DEPENDING ON variants.
     */
    private String translateGoTo(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String paragraphName = stmt.getParagraphName();
        if (paragraphName == null || paragraphName.trim().isEmpty()) {
            return indent + "// TODO: GO TO statement without target paragraph\n";
        }
        
        code.append(indent).append("// COBOL: GO TO ").append(paragraphName).append("\n");
        code.append(indent).append("// WARNING: GO TO is discouraged - Consider refactoring to method calls\n");
        
        // For GO TO DEPENDING ON
        if (stmt.getExpression() != null && stmt.getExpression().toUpperCase().contains("DEPENDING")) {
            code.append(translateGotoDependingOn(stmt, recordType, indent));
        } else {
            // Simple GO TO - translate to method call with return
            String methodName = toJavaMethodName(paragraphName);
            code.append(indent).append(methodName).append("(record);\n");
            code.append(indent).append("return; // Unconditional jump to ").append(paragraphName).append("\n");
        }
        
        return code.toString();
    }

    /**
     * Translates GO TO DEPENDING ON statement.
     * Example: GO TO PARA-100 PARA-200 PARA-300 DEPENDING ON WS-INDEX
     */
    private String translateGotoDependingOn(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String indexVar = stmt.getExpression();
        if (indexVar == null) {
            code.append(indent).append("// TODO: GO TO DEPENDING ON requires index variable\n");
            return code.toString();
        }
        
        // Extract "DEPENDING ON variable"
        String depVar = indexVar.replaceAll("(?i).*DEPENDING\\s+ON\\s+(\\S+).*", "$1");
        String javaIndex = toJavaGetter(depVar, recordType);
        
        code.append(indent).append("switch (").append(javaIndex).append(") {\n");
        
        // Multiple paragraphs to jump to
        // Children contain the alternative paragraphs
        if (stmt.getChildren() != null) {
            int caseNum = 1;
            for (Statement para : stmt.getChildren()) {
                if (para.getParagraphName() != null) {
                    String methodName = toJavaMethodName(para.getParagraphName());
                    code.append(indent).append("    case ").append(caseNum).append(":\n");
                    code.append(indent).append("        ").append(methodName).append("(record);\n");
                    code.append(indent).append("        return;\n");
                    caseNum++;
                }
            }
        } else {
            code.append(indent).append("    // TODO: Add multiple paragraph destinations\n");
        }
        
        code.append(indent).append("    default:\n");
        code.append(indent).append("        // TODO: Handle out-of-range case\n");
        code.append(indent).append("}\n");
        
        return code.toString();
    }

    /**
     * Translates COBOL INSPECT statement to Java string operations.
     * Supports INSPECT TALLYING (count characters) and INSPECT REPLACING (replace characters).
     */
    private String translateInspect(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String target = stmt.getTarget();
        if (target == null || target.trim().isEmpty()) {
            return indent + "// TODO: INSPECT statement without target field\n";
        }
        
        code.append(indent).append("// COBOL: INSPECT ").append(target).append("\n");
        
        String operation = stmt.getExpression();
        if (operation == null || operation.trim().isEmpty()) {
            return indent + "// TODO: INSPECT without operation\n";
        }
        
        operation = operation.toUpperCase();
        
        if (operation.contains("TALLYING")) {
            // INSPECT field TALLYING counter FOR [ALL|LEADING|FIRST] 'char'
            // Extract counter variable (e.g., WS-COUNT)
            String[] parts = operation.split("FOR");
            if (parts.length >= 2) {
                String counterPart = parts[0].replace("TALLYING", "").trim();
                String forPart = parts[1].trim();
                
                // Extract the character to count (e.g., 'A' from FOR ALL 'A')
                String charToCount = extractStringLiteral(forPart);
                String countMode = "ALL"; // Default mode
                
                if (forPart.contains("LEADING")) {
                    countMode = "LEADING";
                } else if (forPart.contains("FIRST")) {
                    countMode = "FIRST";
                } else if (forPart.contains("ALL")) {
                    countMode = "ALL";
                }
                
                String targetGetter = toJavaGetter(target, recordType);
                String counterSetter = toJavaSetter(counterPart, recordType);
                
                code.append(indent).append("String inspectStr = ").append(targetGetter).append(";\n");
                code.append(indent).append("int tallyCount = 0;\n");
                
                if (countMode.equals("ALL")) {
                    // Count all occurrences
                    code.append(indent).append("for (int i = 0; i < inspectStr.length(); i++) {\n");
                    code.append(indent).append("    if (String.valueOf(inspectStr.charAt(i)).equals(").append(charToCount).append(")) {\n");
                    code.append(indent).append("        tallyCount++;\n");
                    code.append(indent).append("    }\n");
                    code.append(indent).append("}\n");
                } else if (countMode.equals("LEADING")) {
                    // Count leading occurrences only
                    code.append(indent).append("for (int i = 0; i < inspectStr.length(); i++) {\n");
                    code.append(indent).append("    if (String.valueOf(inspectStr.charAt(i)).equals(").append(charToCount).append(")) {\n");
                    code.append(indent).append("        tallyCount++;\n");
                    code.append(indent).append("    } else {\n");
                    code.append(indent).append("        break;\n");
                    code.append(indent).append("    }\n");
                    code.append(indent).append("}\n");
                }
                
                code.append(indent).append(counterSetter).append("(String.valueOf(tallyCount));\n");
            } else {
                code.append(indent).append("// TODO: Parse TALLYING clause properly\n");
            }
            
        } else if (operation.contains("REPLACING")) {
            // INSPECT field REPLACING [ALL|LEADING|FIRST] 'old' BY 'new'
            String[] parts = operation.split("BY");
            if (parts.length >= 2) {
                String beforeBy = parts[0]; // Contains REPLACING ... 'old'
                String newChar = extractStringLiteral(parts[1]);
                String oldChar = extractStringLiteral(beforeBy);
                
                String replaceMode = "ALL"; // Default
                if (beforeBy.contains("LEADING")) {
                    replaceMode = "LEADING";
                } else if (beforeBy.contains("FIRST")) {
                    replaceMode = "FIRST";
                }
                
                String targetGetter = toJavaGetter(target, recordType);
                String targetSetter = toJavaSetter(target, recordType);
                
                code.append(indent).append("String replaceStr = ").append(targetGetter).append(";\n");
                
                if (replaceMode.equals("ALL")) {
                    // Replace all occurrences
                    code.append(indent).append("replaceStr = replaceStr.replace(").append(oldChar).append(", ").append(newChar).append(");\n");
                } else if (replaceMode.equals("LEADING")) {
                    // Replace leading occurrences only
                    code.append(indent).append("StringBuilder sb = new StringBuilder(replaceStr);\n");
                    code.append(indent).append("for (int i = 0; i < sb.length(); i++) {\n");
                    code.append(indent).append("    if (String.valueOf(sb.charAt(i)).equals(").append(oldChar).append(")) {\n");
                    code.append(indent).append("        sb.setCharAt(i, ").append(newChar).append(".charAt(0));\n");
                    code.append(indent).append("    } else {\n");
                    code.append(indent).append("        break;\n");
                    code.append(indent).append("    }\n");
                    code.append(indent).append("}\n");
                    code.append(indent).append("replaceStr = sb.toString();\n");
                } else if (replaceMode.equals("FIRST")) {
                    // Replace only first occurrence
                    code.append(indent).append("int idx = replaceStr.indexOf(").append(oldChar).append(");\n");
                    code.append(indent).append("if (idx >= 0) {\n");
                    code.append(indent).append("    replaceStr = replaceStr.substring(0, idx) + ").append(newChar).append(" + replaceStr.substring(idx + 1);\n");
                    code.append(indent).append("}\n");
                }
                
                code.append(indent).append(targetSetter).append("(replaceStr);\n");
            } else {
                code.append(indent).append("// TODO: Parse REPLACING clause properly\n");
            }
            
        } else {
            code.append(indent).append("// TODO: Unsupported INSPECT operation\n");
        }
        
        return code.toString();
    }

    /**
     * Translates COBOL STRING statement to Java StringBuilder operations.
     */
    private String translateString(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String target = stmt.getTarget();
        if (target == null || target.trim().isEmpty()) {
            return indent + "// TODO: STRING statement without INTO target\n";
        }
        
        code.append(indent).append("// COBOL: STRING ... INTO ").append(target).append("\n");
        code.append(indent).append("StringBuilder stringBuilder = new StringBuilder();\n");
        
        // Source fields from statement
        String source = stmt.getSource();
        if (source != null && !source.trim().isEmpty()) {
            String[] sources = source.split(",");
            for (String src : sources) {
                src = src.trim();
                if (!src.isEmpty()) {
                    code.append(indent).append("stringBuilder.append(")
                        .append(toJavaExpression(src, recordType)).append(");\n");
                }
            }
        } else {
            code.append(indent).append("// TODO: Add source fields to STRING\n");
        }
        
        String setter = toJavaSetter(target, recordType);
        code.append(indent).append(setter).append("(stringBuilder.toString());\n");
        
        return code.toString();
    }

    /**
     * Translates COBOL UNSTRING statement to Java String split operations.
     * Supports DELIMITED BY clause with customizable delimiters.
     */
    private String translateUnstring(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String source = stmt.getSource();
        if (source == null || source.trim().isEmpty()) {
            return indent + "// TODO: UNSTRING statement without source field\n";
        }
        
        code.append(indent).append("// COBOL: UNSTRING ").append(source).append("\n");
        
        // Get delimiter from expression - defaults to space or comma
        String delimiter = " "; // Default to space
        String delimiterStr = "\" \"";
        
        if (stmt.getExpression() != null) {
            String expr = stmt.getExpression().toUpperCase();
            if (expr.contains("DELIMITED")) {
                // Extract the delimiter: DELIMITED BY 'char' or DELIMITED BY ','
                String delim = extractStringLiteral(expr.substring(expr.indexOf("BY") + 2));
                if (!delim.equals("\"\"")) {
                    delimiterStr = delim;
                    // Extract just the character for Java regex
                    delimiter = delim.replace("\"", "");
                }
            }
        }
        
        // Escape special regex characters
        String escapedDelim = delimiter
            .replace("\\", "\\\\")
            .replace(".", "\\.")
            .replace("*", "\\*")
            .replace("+", "\\+")
            .replace("?", "\\?")
            .replace("[", "\\[")
            .replace("]", "\\]")
            .replace("(", "\\(")
            .replace(")", "\\)")
            .replace("|", "\\|")
            .replace("^", "\\^")
            .replace("$", "\\$");
        
        String getter = toJavaGetter(source, recordType);
        code.append(indent).append("String sourceStr = ").append(getter).append(";\n");
        code.append(indent).append("String[] parts = sourceStr.split(\"").append(escapedDelim).append("\");\n");
        
        // Target fields
        String target = stmt.getTarget();
        if (target != null && !target.trim().isEmpty()) {
            String[] targets = target.split(",");
            for (int i = 0; i < targets.length; i++) {
                String tgt = targets[i].trim();
                if (!tgt.isEmpty()) {
                    String setter = toJavaSetter(tgt, recordType);
                    code.append(indent).append("if (parts.length > ").append(i).append(") {\n");
                    code.append(indent).append("    ").append(setter).append("(parts[").append(i).append("].trim());\n");
                    code.append(indent).append("} else {\n");
                    code.append(indent).append("    ").append(setter).append("(\"\");\n");
                    code.append(indent).append("}\n");
                }
            }
        } else {
            code.append(indent).append("// TODO: Add target fields for UNSTRING\n");
        }
        
        return code.toString();
    }

    /**
     * Translates COBOL SEARCH statement to Java linear or binary search.
     * Supports SEARCH (linear) and SEARCH ALL (binary search on sorted table).
     */
    private String translateSearch(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String tableName = stmt.getSource(); // Array/table name
        if (tableName == null || tableName.trim().isEmpty()) {
            return indent + "// TODO: SEARCH statement without table name\n";
        }
        
        code.append(indent).append("// COBOL: SEARCH ").append(tableName).append("\n");
        
        // Check if it's SEARCH ALL (binary search) or simple SEARCH (linear)
        boolean isSearchAll = stmt.getExpression() != null && 
                              stmt.getExpression().toUpperCase().contains("ALL");
        
        if (isSearchAll) {
            // Binary search on sorted table
            code.append(indent).append("// SEARCH ALL (binary search on sorted table)\n");
            
            String varName = tableName.toLowerCase();
            if (varName.endsWith("s")) {
                varName = varName.substring(0, varName.length() - 1); // Remove 's' for singular
            }
            
            code.append(indent).append("int searchIndex = java.util.Arrays.binarySearch(").append(tableName)
                .append(", 0, ").append(tableName).append(".length, null"); // TODO: add search key
            code.append(");\n");
            code.append(indent).append("if (searchIndex >= 0) {\n");
            
            // WHEN condition/actions when found
            if (stmt.getChildren() != null && !stmt.getChildren().isEmpty()) {
                for (Statement action : stmt.getChildren()) {
                    String actionCode = translateStatement(action, recordType, indent + "    ");
                    if (actionCode != null && !actionCode.isEmpty()) {
                        code.append(actionCode);
                    }
                }
            } else {
                code.append(indent).append("    // TODO: Add action when found\n");
            }
            
            code.append(indent).append("} else {\n");
            code.append(indent).append("    // TODO: Add action when NOT found\n");
            code.append(indent).append("}\n");
            
        } else {
            // Linear search
            code.append(indent).append("// SEARCH (linear search)\n");
            code.append(indent).append("boolean found = false;\n");
            code.append(indent).append("int searchIndex = -1;\n");
            code.append(indent).append("for (int idx = 0; idx < ").append(tableName).append(".length; idx++) {\n");
            
            // WHEN condition
            if (stmt.getCondition() != null) {
                String javaCondition = translateCobolCondition(stmt.getCondition());
                code.append(indent).append("    if (").append(javaCondition).append(") {\n");
                code.append(indent).append("        found = true;\n");
                code.append(indent).append("        searchIndex = idx;\n");
                
                // Execute child statements if found
                if (stmt.getChildren() != null && !stmt.getChildren().isEmpty()) {
                    for (Statement action : stmt.getChildren()) {
                        String actionCode = translateStatement(action, recordType, indent + "        ");
                        if (actionCode != null && !actionCode.isEmpty()) {
                            code.append(actionCode);
                        }
                    }
                }
                
                code.append(indent).append("        break;\n");
                code.append(indent).append("    }\n");
            } else {
                code.append(indent).append("    // TODO: Add WHEN condition\n");
            }
            
            code.append(indent).append("}\n");
            code.append(indent).append("// found = false if no match, searchIndex = -1\n");
        }
        
        return code.toString();
    }

    /**
     * Translates COBOL CALL statement to Java method invocation.
     */
    private String translateCall(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String programName = stmt.getSource(); // Called program name
        if (programName == null || programName.trim().isEmpty()) {
            return indent + "// TODO: CALL statement without program name\n";
        }
        
        // Clean up program name (remove quotes if present)
        programName = programName.replace("'", "").replace("\"", "");
        
        code.append(indent).append("// COBOL: CALL '").append(programName).append("'\n");
        
        // Convert program name to Java method name
        String methodName = toJavaMethodName(programName);
        
        // Parameters from USING clause
        String params = stmt.getExpression(); // USING parameters
        if (params != null && params.toUpperCase().contains("USING")) {
            code.append(indent).append("// USING parameters specified\n");
            // Extract parameter list
            String[] paramList = params.replace("USING", "").trim().split("\\s+");
            StringBuilder paramStr = new StringBuilder();
            for (int i = 0; i < paramList.length; i++) {
                if (i > 0) paramStr.append(", ");
                paramStr.append(toJavaGetter(paramList[i], recordType));
            }
            code.append(indent).append(methodName).append("(").append(paramStr).append(");\n");
        } else {
            code.append(indent).append(methodName).append("();\n");
        }
        
        // ON EXCEPTION handling
        if (stmt.getCondition() != null && stmt.getCondition().toUpperCase().contains("EXCEPTION")) {
            code.append(indent).append("// TODO: Add ON EXCEPTION handling\n");
        }
        
        return code.toString();
    }

    /**
     * Detects if a COBOL field belongs to a secondary file based on its prefix.
     * Returns the prefix if it's a known file prefix, null otherwise.
     * 
     * Note: TR- is excluded because it typically represents the primary input file (TRANSACTION),
     * not a secondary file accessed separately.
     */
    private String detectFilePrefix(String cobolField) {
        if (cobolField == null || cobolField.length() < 3) {
            return null;
        }
        
        // Common file prefixes in COBOL (excluding TR- which is the primary input file)
        String[] knownPrefixes = {"MA-", "UA-", "ER-", "AT-", "MS-", "UP-", "ED-", "ERR-", "AUD-"};
        
        for (String prefix : knownPrefixes) {
            if (cobolField.startsWith(prefix)) {
                // Normalize to two-letter code where applicable
                if (prefix.startsWith("ERR-")) return "ER"; // error
                if (prefix.startsWith("AUD-")) return "AT"; // audit trail
                return prefix.substring(0, 2); // Return prefix without dash (e.g., "MA")
            }
        }
        
        return null;
    }

    /**
     * Gets the Java variable name for a secondary file based on its prefix.
     */
    private String getSecondaryFileVariableName(String prefix) {
        // Map common COBOL prefixes to variable names
        return switch (prefix) {
            case "MA" -> "masterAccountRecord";
            case "UA" -> "updatedAccountRecord";
            case "ER" -> "errorReportRecord";
            case "AT" -> "auditTrailRecord";
            case "MS" -> "masterRecord";
            case "UP" -> "updateRecord";
            case "ED" -> "editRecord";
            default -> prefix.toLowerCase() + "Record";
        };
    }
}
