package com.cobol.translator.ast;

public class PerformStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "PerformStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitPerformStatementNode(this);
    }
}
