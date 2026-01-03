package com.cobol.translator.ast;

public class SortStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "SortStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitSortStatementNode(this);
    }
}
