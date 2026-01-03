package com.cobol.translator.ast;

public class GobackStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "GobackStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitGobackStatementNode(this);
    }
}
