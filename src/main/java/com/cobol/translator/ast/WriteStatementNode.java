package com.cobol.translator.ast;

public class WriteStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "WriteStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitWriteStatementNode(this);
    }
}
