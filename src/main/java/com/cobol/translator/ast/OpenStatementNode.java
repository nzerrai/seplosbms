package com.cobol.translator.ast;

public class OpenStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "OpenStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitOpenStatementNode(this);
    }
}
