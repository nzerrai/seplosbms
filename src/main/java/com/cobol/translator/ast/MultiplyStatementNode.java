package com.cobol.translator.ast;

public class MultiplyStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "MultiplyStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitMultiplyStatementNode(this);
    }
}
