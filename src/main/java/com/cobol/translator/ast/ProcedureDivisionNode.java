package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.List;

public class ProcedureDivisionNode extends ASTNode {
    private final List<SectionNode> sections = new ArrayList<>();
    private final List<ParagraphNode> paragraphs = new ArrayList<>();

    @Override
    public String getNodeType() { return "ProcedureDivision"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitProcedureDivisionNode(this);
    }

    public void addSection(SectionNode section) {
        sections.add(section);
        addChild(section);
    }

    public void addParagraph(ParagraphNode paragraph) {
        paragraphs.add(paragraph);
        addChild(paragraph);
    }

    public List<SectionNode> getSections() { return new ArrayList<>(sections); }
    public List<ParagraphNode> getParagraphs() { return new ArrayList<>(paragraphs); }
}
