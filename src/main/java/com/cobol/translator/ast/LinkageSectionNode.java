package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.List;

public class LinkageSectionNode extends ASTNode {
    private final List<DataItemNode> dataItems = new ArrayList<>();

    @Override
    public String getNodeType() { return "LinkageSection"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitLinkageSectionNode(this);
    }

    public void addDataItem(DataItemNode item) {
        dataItems.add(item);
        addChild(item);
    }

    public List<DataItemNode> getDataItems() {
        return new ArrayList<>(dataItems);
    }
}
