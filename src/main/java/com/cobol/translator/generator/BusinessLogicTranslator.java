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
     * Translates COBOL MOVE statement with validation.
     */
    private String translateMove(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        if (!isValidStatement(stmt, stmt.getSource(), stmt.getTarget())) {
            return indent + "// TODO: Invalid MOVE - missing source or target\n";
        }
        
        String source = toJavaExpression(stmt.getSource(), recordType);
        String setter = toJavaSetter(stmt.getTarget(), recordType);
        
        code.append(indent).append("// COBOL: MOVE ").append(stmt.getSource()).append(" TO ").append(stmt.getTarget()).append("\n");
        code.append(indent).append(setter).append("(").append(source).append(");\n");
        
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
            // PERFORM UNTIL
            String condition = translateCobolCondition(stmt.getUntilCondition());
            code.append(indent).append("// COBOL: PERFORM ").append(paragraphName).append(" UNTIL ").append(stmt.getUntilCondition()).append("\n");
            code.append(indent).append("while (!(").append(condition).append(")) {\n");
            code.append(indent).append("    ").append(methodName).append("(record);\n");
            code.append(indent).append("}\n");
        } else {
            // Simple PERFORM
            code.append(indent).append("// COBOL: PERFORM ").append(paragraphName).append("\n");
            code.append(indent).append(methodName).append("(record);\n");
        }
        
        return code.toString();
    }

    /**
     * Translates COBOL ADD statement.
     */
    private String translateAdd(Statement stmt, String recordType, String indent) {
        return translateArithmeticOperation(stmt, recordType, indent, "ADD", "add");
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
     */
    private String translateDivide(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String source = toJavaExpression(stmt.getSource(), recordType);
        String target = stmt.getTarget();
        String getter = toJavaGetter(target, recordType);
        String setter = toJavaSetter(target, recordType);
        
        code.append(indent).append("// COBOL: DIVIDE ").append(target).append(" BY ").append(stmt.getSource()).append("\n");
        code.append(indent).append(setter).append("(").append(getter).append(".divide(").append(source)
            .append(", 2, java.math.RoundingMode.HALF_UP));\n");
        
        return code.toString();
    }

    /**
     * Consolidated arithmetic operation translator.
     * Handles ADD, SUBTRACT, MULTIPLY with consistent pattern.
     */
    private String translateArithmeticOperation(Statement stmt, String recordType, String indent, 
                                                String cobolOp, String javaMethod) {
        StringBuilder code = new StringBuilder();
        
        // Validate inputs
        if (!isValidStatement(stmt, stmt.getSource(), stmt.getTarget())) {
            return indent + "// TODO: Invalid arithmetic operation - missing source or target\n";
        }
        
        String source = toJavaExpression(stmt.getSource(), recordType);
        String target = stmt.getTarget();
        String getter = toJavaGetter(target, recordType);
        String setter = toJavaSetter(target, recordType);
        
        code.append(indent).append("// COBOL: ").append(cobolOp).append(" ").append(stmt.getSource())
            .append(" TO ").append(target).append("\n");
        code.append(indent).append(setter).append("(").append(getter).append(".")
            .append(javaMethod).append("(").append(source).append("));\n");
        
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
        
        String message = stmt.getSource() != null && !stmt.getSource().trim().isEmpty() ? 
            toJavaExpression(stmt.getSource(), recordType) : "\"\"";
        
        code.append(indent).append("// COBOL: DISPLAY ").append(stmt.getSource() != null ? stmt.getSource() : "").append("\n");
        code.append(indent).append("logger.info(").append(message).append(");\n");
        
        return code.toString();
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
        
        // Translate COBOL logical operators (case-insensitive)
        result = result.replaceAll("(?i)\\bAND\\b", "&&");
        result = result.replaceAll("(?i)\\bOR\\b", "||");
        result = result.replaceAll("(?i)\\bNOT\\b", "!");
        
        // Translate COBOL comparison operators
        result = result.replaceAll("(?i)\\b=\\b", "==");
        result = result.replaceAll("(?i)\\bNOT\\s*=", "!=");
        result = result.replaceAll("(?i)\\bIS\\s+EQUAL", "==");
        result = result.replaceAll("(?i)\\bIS\\s+NOT\\s+EQUAL", "!=");
        
        // Translate COBOL special values
        result = result.replaceAll("(?i)\\bZERO(?!S)", "0");
        result = result.replaceAll("(?i)\\bZEROS\\b", "0");
        result = result.replaceAll("(?i)\\bSPACES?\\b", "\" \"");
        result = result.replaceAll("(?i)\\bHIGH\\s+VALUE", "Integer.MAX_VALUE");
        result = result.replaceAll("(?i)\\bLOW\\s+VALUE", "Integer.MIN_VALUE");
        
        return result;
    }

    /**
     * Converts COBOL field name to Java getter call with improved validation.
     */
    private String toJavaGetter(String cobolField, String recordType) {
        if (cobolField == null || cobolField.trim().isEmpty()) {
            return "null";
        }
        
        cobolField = cobolField.trim();
        
        // Handle literals
        if (cobolField.startsWith("'") && cobolField.endsWith("'")) {
            return "\"" + cobolField.substring(1, cobolField.length() - 1) + "\"";
        }
        if (COBOL_LITERAL_NUMBER.matcher(cobolField).matches()) {
            return cobolField.contains(".") ? 
                "new BigDecimal(\"" + cobolField + "\")" : cobolField;
        }
        
        String javaField = toJavaFieldName(cobolField);
        return "record.get" + capitalize(javaField) + "()";
    }

    /**
     * Converts COBOL field name to Java setter call with improved validation.
     */
    private String toJavaSetter(String cobolField, String recordType) {
        if (cobolField == null || cobolField.trim().isEmpty()) {
            return "/* invalid setter */";
        }
        
        String javaField = toJavaFieldName(cobolField);
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
        
        // Field reference
        return toJavaGetter(expr, recordType);
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
     */
    private String translateCondition(Statement stmt) {
        if (!isValidStatement(stmt, stmt.getLeftOperand(), stmt.getOperator(), stmt.getRightOperand())) {
            return "true /* invalid condition */";
        }
        
        String left = toJavaExpression(stmt.getLeftOperand(), "");
        String op = translateOperator(stmt.getOperator());
        String right = toJavaExpression(stmt.getRightOperand(), "");
        
        return left + " " + op + " " + right;
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
        return "        // TODO: Implement logic from COBOL paragraph: " + 
               (paragraph != null ? paragraph.getName() : "unknown") + "\n";
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
}
