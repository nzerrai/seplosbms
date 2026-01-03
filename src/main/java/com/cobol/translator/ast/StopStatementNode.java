package com.cobol.translator.ast;

public class StopStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "StopStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitStopStatementNode(this);
    }
}
