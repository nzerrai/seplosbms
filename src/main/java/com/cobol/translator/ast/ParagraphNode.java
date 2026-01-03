package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.List;

public class ParagraphNode extends ASTNode {
    private String name;
    private final List<StatementNode> statements = new ArrayList<>();

    public ParagraphNode(String name) {
        this.name = name;
    }

    @Override
    public String getNodeType() { return "Paragraph"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitParagraphNode(this);
    }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public void addStatement(StatementNode statement) {
        statements.add(statement);
        addChild(statement);
    }

    public List<StatementNode> getStatements() {
        return new ArrayList<>(statements);
    }
}
