package com.cobol.translator.ast;

public class SetStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "SetStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitSetStatementNode(this);
    }
}
