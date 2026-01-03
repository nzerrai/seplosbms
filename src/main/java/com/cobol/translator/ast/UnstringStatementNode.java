package com.cobol.translator.ast;

public class UnstringStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "UnstringStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitUnstringStatementNode(this);
    }
}
