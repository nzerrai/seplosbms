package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.List;

public class FileSectionNode extends ASTNode {
    private final List<FileDescriptionNode> fileDescriptions = new ArrayList<>();

    @Override
    public String getNodeType() { return "FileSection"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitFileSectionNode(this);
    }

    public void addFileDescription(FileDescriptionNode fd) {
        fileDescriptions.add(fd);
        addChild(fd);
    }

    public List<FileDescriptionNode> getFileDescriptions() {
        return new ArrayList<>(fileDescriptions);
    }
}
