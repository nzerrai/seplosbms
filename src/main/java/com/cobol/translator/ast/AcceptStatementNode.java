package com.cobol.translator.ast;

public class AcceptStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "AcceptStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitAcceptStatementNode(this);
    }
}
