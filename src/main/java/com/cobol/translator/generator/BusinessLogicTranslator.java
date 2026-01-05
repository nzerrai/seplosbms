package com.cobol.translator.generator;

import com.cobol.translator.model.Paragraph;
import com.cobol.translator.model.Statement;
import com.cobol.translator.model.Statement.StatementType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Translates COBOL business logic (paragraphs and statements) into Java code.
 * Analyzes PROCEDURE DIVISION statements and generates equivalent Java implementations.
 */
public class BusinessLogicTranslator {

    private static final Logger logger = LoggerFactory.getLogger(BusinessLogicTranslator.class);

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
     * Translates COBOL IF statement to Java.
     */
    private String translateIf(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String condition = stmt.getCondition();
        if (condition == null || condition.isEmpty()) {
            // Try to reconstruct from operands
            if (stmt.getLeftOperand() != null && stmt.getOperator() != null) {
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
        for (Statement child : stmt.getChildren()) {
            code.append(translateStatement(child, recordType, indent + "    "));
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
        if ("TRUE".equalsIgnoreCase(evaluateExpr)) {
            // EVALUATE TRUE pattern - translate to if-else chain
            code.append(translateEvaluateTrue(stmt, recordType, indent));
        } else {
            // Regular EVALUATE - translate to switch
            code.append(translateEvaluateSwitch(stmt, recordType, indent));
        }
        
        return code.toString();
    }

    /**
     * Translates EVALUATE TRUE into if-else chain.
     */
    private String translateEvaluateTrue(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        boolean first = true;
        for (Statement whenClause : stmt.getChildren()) {
            String condition = whenClause.getCondition();
            if (condition != null) {
                String javaCondition = translateCobolCondition(condition);
                
                if (first) {
                    code.append(indent).append("if (").append(javaCondition).append(") {\n");
                    first = false;
                } else {
                    code.append(indent).append("} else if (").append(javaCondition).append(") {\n");
                }
                
                // Translate statements in this WHEN clause
                for (Statement action : whenClause.getChildren()) {
                    code.append(translateStatement(action, recordType, indent + "    "));
                }
            }
        }
        
        if (!first) {
            code.append(indent).append("}\n\n");
        }
        
        return code.toString();
    }

    /**
     * Translates regular EVALUATE into switch statement.
     */
    private String translateEvaluateSwitch(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String variable = toJavaGetter(stmt.getExpression(), recordType);
        code.append(indent).append("switch (").append(variable).append(") {\n");
        
        for (Statement whenClause : stmt.getChildren()) {
            if (whenClause.getCondition() != null) {
                code.append(indent).append("    case ").append(whenClause.getCondition()).append(":\n");
                
                for (Statement action : whenClause.getChildren()) {
                    code.append(translateStatement(action, recordType, indent + "        "));
                }
                
                code.append(indent).append("        break;\n");
            }
        }
        
        code.append(indent).append("}\n\n");
        return code.toString();
    }

    /**
     * Translates COBOL MOVE statement.
     */
    private String translateMove(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String source = toJavaExpression(stmt.getSource(), recordType);
        String target = toJavaGetter(stmt.getTarget(), recordType);
        String setter = toJavaSetter(stmt.getTarget(), recordType);
        
        code.append(indent).append("// COBOL: MOVE ").append(stmt.getSource()).append(" TO ").append(stmt.getTarget()).append("\n");
        code.append(indent).append(setter).append("(").append(source).append(");\n");
        
        return code.toString();
    }

    /**
     * Translates COBOL COMPUTE statement.
     */
    private String translateCompute(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String target = stmt.getTarget();
        String expression = stmt.getExpression();
        
        if (target != null && expression != null) {
            String setter = toJavaSetter(target, recordType);
            String javaExpr = translateArithmeticExpression(expression, recordType);
            
            code.append(indent).append("// COBOL: COMPUTE ").append(target).append(" = ").append(expression).append("\n");
            code.append(indent).append(setter).append("(").append(javaExpr).append(");\n");
        }
        
        return code.toString();
    }

    /**
     * Translates COBOL PERFORM statement.
     */
    private String translatePerform(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String paragraphName = stmt.getParagraphName();
        if (paragraphName != null) {
            String methodName = toJavaMethodName(paragraphName);
            
            if (stmt.getPerformTimes() != null) {
                // PERFORM n TIMES
                code.append(indent).append("// COBOL: PERFORM ").append(paragraphName).append(" ")
                    .append(stmt.getPerformTimes()).append(" TIMES\n");
                code.append(indent).append("for (int i = 0; i < ").append(stmt.getPerformTimes()).append("; i++) {\n");
                code.append(indent).append("    ").append(methodName).append("(record);\n");
                code.append(indent).append("}\n");
            } else if (stmt.getUntilCondition() != null) {
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
        }
        
        return code.toString();
    }

    /**
     * Translates COBOL ADD statement.
     */
    private String translateAdd(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String source = toJavaExpression(stmt.getSource(), recordType);
        String target = stmt.getTarget();
        String getter = toJavaGetter(target, recordType);
        String setter = toJavaSetter(target, recordType);
        
        code.append(indent).append("// COBOL: ADD ").append(stmt.getSource()).append(" TO ").append(target).append("\n");
        code.append(indent).append(setter).append("(").append(getter).append(".add(").append(source).append("));\n");
        
        return code.toString();
    }

    /**
     * Translates COBOL SUBTRACT statement.
     */
    private String translateSubtract(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String source = toJavaExpression(stmt.getSource(), recordType);
        String target = stmt.getTarget();
        String getter = toJavaGetter(target, recordType);
        String setter = toJavaSetter(target, recordType);
        
        code.append(indent).append("// COBOL: SUBTRACT ").append(stmt.getSource()).append(" FROM ").append(target).append("\n");
        code.append(indent).append(setter).append("(").append(getter).append(".subtract(").append(source).append("));\n");
        
        return code.toString();
    }

    /**
     * Translates COBOL MULTIPLY statement.
     */
    private String translateMultiply(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String source = toJavaExpression(stmt.getSource(), recordType);
        String target = stmt.getTarget();
        String getter = toJavaGetter(target, recordType);
        String setter = toJavaSetter(target, recordType);
        
        code.append(indent).append("// COBOL: MULTIPLY ").append(target).append(" BY ").append(stmt.getSource()).append("\n");
        code.append(indent).append(setter).append("(").append(getter).append(".multiply(").append(source).append("));\n");
        
        return code.toString();
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
     * Translates COBOL DISPLAY statement.
     */
    private String translateDisplay(Statement stmt, String recordType, String indent) {
        StringBuilder code = new StringBuilder();
        
        String message = stmt.getSource() != null ? toJavaExpression(stmt.getSource(), recordType) : "\"\"";
        
        code.append(indent).append("// COBOL: DISPLAY ").append(stmt.getSource()).append("\n");
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
     */
    private String translateCobolCondition(String condition) {
        if (condition == null) return "true";
        
        // Translate COBOL operators to Java
        String result = condition
            .replaceAll("\\bAND\\b", "&&")
            .replaceAll("\\bOR\\b", "||")
            .replaceAll("\\bNOT\\b", "!")
            .replaceAll("\\b=\\b", "==")
            .replaceAll("\\bZERO\\b", "0")
            .replaceAll("\\bSPACE\\b", "\" \"")
            .replaceAll("\\bSPACES\\b", "\" \"");
        
        return result;
    }

    /**
     * Reconstructs condition from statement operands.
     */
    private String translateCondition(Statement stmt) {
        String left = toJavaExpression(stmt.getLeftOperand(), "");
        String op = translateOperator(stmt.getOperator());
        String right = toJavaExpression(stmt.getRightOperand(), "");
        
        return left + " " + op + " " + right;
    }

    /**
     * Translates COBOL operator to Java.
     */
    private String translateOperator(String op) {
        if (op == null) return "==";
        switch (op.toUpperCase()) {
            case "=": return "==";
            case "NOT": return "!=";
            case "GREATER": case ">": return ">";
            case "LESS": case "<": return "<";
            case ">=": case "NOT LESS": return ">=";
            case "<=": case "NOT GREATER": return "<=";
            default: return op;
        }
    }

    /**
     * Converts COBOL field name to Java getter call.
     */
    private String toJavaGetter(String cobolField, String recordType) {
        if (cobolField == null) return "null";
        
        // Handle literals
        if (cobolField.startsWith("'") || cobolField.matches("\\d+")) {
            return cobolField;
        }
        
        String javaField = toJavaFieldName(cobolField);
        return "record.get" + capitalize(javaField) + "()";
    }

    /**
     * Converts COBOL field name to Java setter call.
     */
    private String toJavaSetter(String cobolField, String recordType) {
        if (cobolField == null) return "/* null */";
        
        String javaField = toJavaFieldName(cobolField);
        return "record.set" + capitalize(javaField);
    }

    /**
     * Converts COBOL expression to Java expression.
     */
    private String toJavaExpression(String expr, String recordType) {
        if (expr == null) return "null";
        
        // Literals
        if (expr.startsWith("'") && expr.endsWith("'")) {
            return "\"" + expr.substring(1, expr.length() - 1) + "\"";
        }
        if (expr.matches("-?\\d+(\\.\\d+)?")) {
            return expr.contains(".") ? "new BigDecimal(\"" + expr + "\")" : expr;
        }
        if (expr.equalsIgnoreCase("ZERO") || expr.equalsIgnoreCase("ZEROS")) {
            return "BigDecimal.ZERO";
        }
        
        // Field reference
        return toJavaGetter(expr, recordType);
    }

    /**
     * Translates arithmetic expression.
     */
    private String translateArithmeticExpression(String expr, String recordType) {
        // Simple implementation - can be enhanced
        return expr.replaceAll("\\b([A-Z][A-Z0-9-]+)\\b", "record.get$1()");
    }

    /**
     * Converts COBOL field name to Java field name (camelCase).
     */
    private String toJavaFieldName(String cobolName) {
        String[] parts = cobolName.toLowerCase().split("-");
        StringBuilder result = new StringBuilder(parts[0].replaceAll("^[0-9]+", ""));
        for (int i = 1; i < parts.length; i++) {
            String part = parts[i].replaceAll("^[0-9]+", "");
            if (!part.isEmpty()) {
                result.append(Character.toUpperCase(part.charAt(0)));
                result.append(part.substring(1));
            }
        }
        return result.toString();
    }

    /**
     * Converts COBOL paragraph name to Java method name.
     */
    private String toJavaMethodName(String paragraphName) {
        String fieldName = toJavaFieldName(paragraphName);
        return fieldName;
    }

    /**
     * Capitalizes first letter.
     */
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) return str;
        return Character.toUpperCase(str.charAt(0)) + str.substring(1);
    }

    /**
     * Generates a TODO comment for empty or unimplemented paragraphs.
     */
    private String generateTodoComment(Paragraph paragraph) {
        return "        // TODO: Implement logic from COBOL paragraph: " + 
               (paragraph != null ? paragraph.getName() : "unknown") + "\n";
    }
}
