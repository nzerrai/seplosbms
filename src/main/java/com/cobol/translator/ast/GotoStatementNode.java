package com.cobol.translator.ast;

public class GotoStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "GotoStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitGotoStatementNode(this);
    }
}
