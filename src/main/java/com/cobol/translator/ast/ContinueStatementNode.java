package com.cobol.translator.ast;

public class ContinueStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "ContinueStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitContinueStatementNode(this);
    }
}
