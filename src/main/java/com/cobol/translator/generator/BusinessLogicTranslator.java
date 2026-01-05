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
     */
    private String translateEvaluate(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        code.append(indent).append("// COBOL: EVALUATE ").append(stmt.getOriginalCobol() != null ? stmt.getOriginalCobol() : "").append("\n");
        
        String evaluateExpr = stmt.getExpression();
        if (evaluateExpr != null && evaluateExpr.trim().toUpperCase().equals("TRUE")) {
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
}
