package com.cobol.translator.ast;

public class IdentifierNode extends ASTNode {
    private String name;

    public IdentifierNode(String name) {
        this.name = name;
    }

    @Override
    public String getNodeType() { return "Identifier"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitIdentifierNode(this);
    }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    @Override
    public String toString() {
        return String.format("Identifier: %s", name);
    }
}
