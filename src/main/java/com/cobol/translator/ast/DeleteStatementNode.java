package com.cobol.translator.ast;

public class DeleteStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "DeleteStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDeleteStatementNode(this);
    }
}
