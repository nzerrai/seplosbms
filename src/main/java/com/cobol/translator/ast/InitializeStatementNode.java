package com.cobol.translator.ast;

public class InitializeStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "InitializeStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitInitializeStatementNode(this);
    }
}
