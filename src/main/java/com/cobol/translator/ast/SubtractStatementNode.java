package com.cobol.translator.ast;

public class SubtractStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "SubtractStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitSubtractStatementNode(this);
    }
}
