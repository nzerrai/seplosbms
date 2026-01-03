package com.cobol.translator.ast;

public class ArithmeticExpressionNode extends ASTNode {
    private String operator;
    private ASTNode left;
    private ASTNode right;

    @Override
    public String getNodeType() { return "ArithmeticExpression"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitArithmeticExpressionNode(this);
    }

    public String getOperator() { return operator; }
    public void setOperator(String operator) { this.operator = operator; }
    public ASTNode getLeft() { return left; }
    public void setLeft(ASTNode left) { this.left = left; }
    public ASTNode getRight() { return right; }
    public void setRight(ASTNode right) { this.right = right; }
}
