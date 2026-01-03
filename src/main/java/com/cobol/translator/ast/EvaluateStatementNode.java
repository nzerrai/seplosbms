package com.cobol.translator.ast;

public class EvaluateStatementNode extends StatementNode {
    @Override
    public String getNodeType() { return "EvaluateStatement"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitEvaluateStatementNode(this);
    }
}
