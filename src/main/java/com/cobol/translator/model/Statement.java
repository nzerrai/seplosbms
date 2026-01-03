package com.cobol.translator.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a COBOL statement in the PROCEDURE DIVISION.
 */
public class Statement {

    private StatementType type;
    private String originalCobol;
    private int lineNumber;

    // For structured statements
    private List<Statement> children = new ArrayList<>();

    // Statement-specific fields
    private String condition;        // For IF, EVALUATE
    private String target;           // For MOVE, COMPUTE, etc.
    private String source;           // For MOVE, COMPUTE, etc.
    private String expression;       // For COMPUTE
    private String fileName;         // For READ, WRITE, OPEN, CLOSE
    private String paragraphName;    // For PERFORM

    // PERFORM variants
    private String untilCondition;
    private Integer performTimes;

    // Enhanced fields for validation logic
    private String leftOperand;      // For comparisons (IF A = B)
    private String operator;         // For comparisons (=, <, >, NOT, OR, AND)
    private String rightOperand;     // For comparisons
    private String errorCode;        // For error handling (MOVE 'E001' TO WS-ERR-CODE)
    private String errorMessage;     // For error messages

    public Statement() {
    }

    public Statement(StatementType type) {
        this.type = type;
    }

    public void addChild(Statement child) {
        this.children.add(child);
    }

    // Getters and Setters

    public StatementType getType() { return type; }
    public void setType(StatementType type) { this.type = type; }

    public String getOriginalCobol() { return originalCobol; }
    public void setOriginalCobol(String originalCobol) { this.originalCobol = originalCobol; }

    public int getLineNumber() { return lineNumber; }
    public void setLineNumber(int lineNumber) { this.lineNumber = lineNumber; }

    public List<Statement> getChildren() { return children; }
    public void setChildren(List<Statement> children) { this.children = children; }

    public String getCondition() { return condition; }
    public void setCondition(String condition) { this.condition = condition; }

    public String getTarget() { return target; }
    public void setTarget(String target) { this.target = target; }

    public String getSource() { return source; }
    public void setSource(String source) { this.source = source; }

    public String getExpression() { return expression; }
    public void setExpression(String expression) { this.expression = expression; }

    public String getFileName() { return fileName; }
    public void setFileName(String fileName) { this.fileName = fileName; }

    public String getParagraphName() { return paragraphName; }
    public void setParagraphName(String paragraphName) { this.paragraphName = paragraphName; }

    public String getUntilCondition() { return untilCondition; }
    public void setUntilCondition(String untilCondition) { this.untilCondition = untilCondition; }

    public Integer getPerformTimes() { return performTimes; }
    public void setPerformTimes(Integer performTimes) { this.performTimes = performTimes; }

    public String getLeftOperand() { return leftOperand; }
    public void setLeftOperand(String leftOperand) { this.leftOperand = leftOperand; }

    public String getOperator() { return operator; }
    public void setOperator(String operator) { this.operator = operator; }

    public String getRightOperand() { return rightOperand; }
    public void setRightOperand(String rightOperand) { this.rightOperand = rightOperand; }

    public String getErrorCode() { return errorCode; }
    public void setErrorCode(String errorCode) { this.errorCode = errorCode; }

    public String getErrorMessage() { return errorMessage; }
    public void setErrorMessage(String errorMessage) { this.errorMessage = errorMessage; }

    @Override
    public String toString() {
        return "Statement{" +
                "type=" + type +
                ", line=" + lineNumber +
                (condition != null ? ", condition='" + condition + '\'' : "") +
                '}';
    }

    /**
     * COBOL statement types.
     */
    public enum StatementType {
        // File operations
        OPEN,
        CLOSE,
        READ,
        WRITE,

        // Data movement
        MOVE,
        INITIALIZE,

        // Arithmetic
        ADD,
        SUBTRACT,
        MULTIPLY,
        DIVIDE,
        COMPUTE,

        // Control flow
        IF,
        ELSE,
        EVALUATE,
        WHEN,
        PERFORM,
        PERFORM_UNTIL,
        PERFORM_TIMES,
        GO_TO,
        STOP_RUN,
        EXIT,

        // String operations
        STRING,
        UNSTRING,
        INSPECT,

        // Display
        DISPLAY,
        ACCEPT,

        // Other
        SEARCH,
        SET,
        CALL,

        // Compound
        BLOCK,
        PARAGRAPH
    }
}
