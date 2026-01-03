package com.cobol.translator.ast;

public class ComputeStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "ComputeStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitComputeStatementNode(this);
    }
}
