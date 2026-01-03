package com.cobol.translator.ast;

public class RewriteStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "RewriteStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitRewriteStatementNode(this);
    }
}
