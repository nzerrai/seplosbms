package com.cobol.translator.ast;

public class MoveStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "MoveStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitMoveStatementNode(this);
    }
}
