package com.cobol.translator.ast;

public class DivideStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "DivideStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDivideStatementNode(this);
    }
}
