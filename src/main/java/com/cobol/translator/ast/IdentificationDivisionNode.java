package com.cobol.translator.ast;

/**
 * AST node representing the IDENTIFICATION DIVISION.
 */
public class IdentificationDivisionNode extends ASTNode {

    private String programId;
    private String author;
    private String dateWritten;
    private String security;

    public IdentificationDivisionNode(String programId) {
        super();
        this.programId = programId;
    }

    @Override
    public String getNodeType() {
        return "IdentificationDivision";
    }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitIdentificationDivisionNode(this);
    }

    // Getters and setters

    public String getProgramId() {
        return programId;
    }

    public void setProgramId(String programId) {
        this.programId = programId;
    }

    public String getAuthor() {
        return author;
    }

    public void setAuthor(String author) {
        this.author = author;
    }

    public String getDateWritten() {
        return dateWritten;
    }

    public void setDateWritten(String dateWritten) {
        this.dateWritten = dateWritten;
    }

    public String getSecurity() {
        return security;
    }

    public void setSecurity(String security) {
        this.security = security;
    }

    @Override
    public String toString() {
        return String.format("IdentificationDivision: %s", programId);
    }
}
