package com.cobol.translator.ast;

public class StringStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "StringStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitStringStatementNode(this);
    }
}
