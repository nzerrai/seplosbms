package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.List;

public class SectionNode extends ASTNode {
    private String name;
    private final List<ParagraphNode> paragraphs = new ArrayList<>();

    public SectionNode(String name) {
        this.name = name;
    }

    @Override
    public String getNodeType() { return "Section"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitSectionNode(this);
    }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public void addParagraph(ParagraphNode paragraph) {
        paragraphs.add(paragraph);
        addChild(paragraph);
    }

    public List<ParagraphNode> getParagraphs() {
        return new ArrayList<>(paragraphs);
    }
}
