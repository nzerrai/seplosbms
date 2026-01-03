package com.cobol.translator.ast;

public class SearchStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "SearchStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitSearchStatementNode(this);
    }
}
