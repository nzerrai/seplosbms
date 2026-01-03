package com.cobol.translator.ast;

public class ExitStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "ExitStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitExitStatementNode(this);
    }
}
