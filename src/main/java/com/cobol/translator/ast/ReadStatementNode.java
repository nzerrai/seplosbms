package com.cobol.translator.ast;

public class ReadStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "ReadStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitReadStatementNode(this);
    }
}
