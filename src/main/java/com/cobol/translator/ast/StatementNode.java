package com.cobol.translator.ast;

public abstract class StatementNode extends ASTNode {
    private String originalCobol;

    public String getOriginalCobol() { return originalCobol; }
    public void setOriginalCobol(String originalCobol) { this.originalCobol = originalCobol; }
}
