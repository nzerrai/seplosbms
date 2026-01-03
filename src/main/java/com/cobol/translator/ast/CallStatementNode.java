package com.cobol.translator.ast;

public class CallStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "CallStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitCallStatementNode(this);
    }
}
