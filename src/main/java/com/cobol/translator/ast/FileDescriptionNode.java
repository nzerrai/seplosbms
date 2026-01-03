package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.List;

public class FileDescriptionNode extends ASTNode {
    private String fileName;
    private final List<DataItemNode> records = new ArrayList<>();

    public FileDescriptionNode(String fileName) {
        this.fileName = fileName;
    }

    @Override
    public String getNodeType() { return "FileDescription"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitFileDescriptionNode(this);
    }

    public String getFileName() { return fileName; }
    public void setFileName(String fileName) { this.fileName = fileName; }

    public void addRecord(DataItemNode record) {
        records.add(record);
        addChild(record);
    }

    public List<DataItemNode> getRecords() {
        return new ArrayList<>(records);
    }
}
