package com.cobol.translator.ast;

public class IfStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "IfStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitIfStatementNode(this);
    }
}
