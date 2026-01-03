package com.cobol.translator.ast;

public class DataItemNode extends ASTNode {
    private int level;
    private String name;
    private String picture;
    private String value;
    private String redefines;

    public DataItemNode(int level, String name) {
        this.level = level;
        this.name = name;
    }

    @Override
    public String getNodeType() { return "DataItem"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDataItemNode(this);
    }

    public int getLevel() { return level; }
    public void setLevel(int level) { this.level = level; }
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public String getPicture() { return picture; }
    public void setPicture(String picture) { this.picture = picture; }
    public String getValue() { return value; }
    public void setValue(String value) { this.value = value; }
    public String getRedefines() { return redefines; }
    public void setRedefines(String redefines) { this.redefines = redefines; }

    @Override
    public String toString() {
        return String.format("DataItem: %02d %s PIC %s", level, name, picture);
    }
}
