package com.cobol.translator.ast;

public class InspectStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "InspectStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitInspectStatementNode(this);
    }
}
