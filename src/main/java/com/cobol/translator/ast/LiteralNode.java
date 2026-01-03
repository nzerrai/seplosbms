package com.cobol.translator.ast;

public class LiteralNode extends ASTNode {
    private String value;
    private String type;

    public LiteralNode(String value, String type) {
        this.value = value;
        this.type = type;
    }

    @Override
    public String getNodeType() { return "Literal"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitLiteralNode(this);
    }

    public String getValue() { return value; }
    public void setValue(String value) { this.value = value; }
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }

    @Override
    public String toString() {
        return String.format("Literal: %s (%s)", value, type);
    }
}
