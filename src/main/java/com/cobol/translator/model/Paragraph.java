package com.cobol.translator.model;

/**
 * Represents a COBOL paragraph in the PROCEDURE DIVISION.
 * A paragraph is a named block of statements that can be PERFORMed.
 */
public class Paragraph {

    private String name;
    private int startLine;
    private int endLine;
    private java.util.List<Statement> statements = new java.util.ArrayList<>();

    public Paragraph() {
    }

    public Paragraph(String name) {
        this.name = name;
    }

    public void addStatement(Statement statement) {
        this.statements.add(statement);
    }

    // Getters and Setters

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getStartLine() {
        return startLine;
    }

    public void setStartLine(int startLine) {
        this.startLine = startLine;
    }

    public int getEndLine() {
        return endLine;
    }

    public void setEndLine(int endLine) {
        this.endLine = endLine;
    }

    public java.util.List<Statement> getStatements() {
        return statements;
    }

    public void setStatements(java.util.List<Statement> statements) {
        this.statements = statements;
    }

    /**
     * Determines if this is a major paragraph (typically 3-digit numbers like 100-, 200-, etc.).
     * Major paragraphs often represent main processing steps.
     */
    public boolean isMajorParagraph() {
        if (name == null) return false;
        // Pattern: starts with digit followed by 00- (e.g., 100-INIT, 200-PROCESS)
        return name.matches("\\d00-.*");
    }

    @Override
    public String toString() {
        return "Paragraph{" +
                "name='" + name + '\'' +
                ", statements=" + statements.size() +
                '}';
    }
}
