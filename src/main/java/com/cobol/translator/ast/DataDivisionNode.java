package com.cobol.translator.ast;

/**
 * AST node representing the DATA DIVISION.
 */
public class DataDivisionNode extends ASTNode {

    private FileSectionNode fileSection;
    private WorkingStorageSectionNode workingStorageSection;
    private LinkageSectionNode linkageSection;

    public DataDivisionNode() {
        super();
    }

    @Override
    public String getNodeType() {
        return "DataDivision";
    }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDataDivisionNode(this);
    }

    // Getters and setters

    public FileSectionNode getFileSection() {
        return fileSection;
    }

    public void setFileSection(FileSectionNode fileSection) {
        this.fileSection = fileSection;
        addChild(fileSection);
    }

    public WorkingStorageSectionNode getWorkingStorageSection() {
        return workingStorageSection;
    }

    public void setWorkingStorageSection(WorkingStorageSectionNode workingStorageSection) {
        this.workingStorageSection = workingStorageSection;
        addChild(workingStorageSection);
    }

    public LinkageSectionNode getLinkageSection() {
        return linkageSection;
    }

    public void setLinkageSection(LinkageSectionNode linkageSection) {
        this.linkageSection = linkageSection;
        addChild(linkageSection);
    }

    @Override
    public String toString() {
        return "DataDivision";
    }
}
