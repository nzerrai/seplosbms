package com.cobol.translator.ast;

public class DisplayStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "DisplayStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDisplayStatementNode(this);
    }
}
