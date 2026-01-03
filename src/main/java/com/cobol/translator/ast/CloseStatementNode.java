package com.cobol.translator.ast;

public class CloseStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "CloseStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitCloseStatementNode(this);
    }
}
