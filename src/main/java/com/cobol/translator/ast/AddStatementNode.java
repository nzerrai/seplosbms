package com.cobol.translator.ast;

public class AddStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "AddStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitAddStatementNode(this);
    }
}
