package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.List;

/**
 * AST node representing the ENVIRONMENT DIVISION.
 */
public class EnvironmentDivisionNode extends ASTNode {

    private final List<FileDescriptionNode> fileDescriptions = new ArrayList<>();

    public EnvironmentDivisionNode() {
        super();
    }

    @Override
    public String getNodeType() {
        return "EnvironmentDivision";
    }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitEnvironmentDivisionNode(this);
    }

    public void addFileDescription(FileDescriptionNode fileDesc) {
        fileDescriptions.add(fileDesc);
        addChild(fileDesc);
    }

    public List<FileDescriptionNode> getFileDescriptions() {
        return new ArrayList<>(fileDescriptions);
    }

    @Override
    public String toString() {
        return "EnvironmentDivision";
    }
}
