package com.cobol.translator.ast;

import java.util.ArrayList;
import java.util.List;

/**
 * Base class for all AST nodes in the COBOL Abstract Syntax Tree.
 * Provides common functionality for tree navigation, metadata, and visitor pattern support.
 */
public abstract class ASTNode {

    private int lineNumber;
    private int columnNumber;
    private ASTNode parent;
    private final List<ASTNode> children = new ArrayList<>();

    /**
     * Constructor with source location
     */
    public ASTNode(int lineNumber, int columnNumber) {
        this.lineNumber = lineNumber;
        this.columnNumber = columnNumber;
    }

    /**
     * Default constructor
     */
    public ASTNode() {
        this(0, 0);
    }

    /**
     * Accept method for visitor pattern
     */
    public abstract <T> T accept(ASTVisitor<T> visitor);

    /**
     * Get the node type as a string
     */
    public abstract String getNodeType();

    /**
     * Add a child node
     */
    public void addChild(ASTNode child) {
        if (child != null) {
            children.add(child);
            child.setParent(this);
        }
    }

    /**
     * Remove a child node
     */
    public void removeChild(ASTNode child) {
        children.remove(child);
        if (child != null) {
            child.setParent(null);
        }
    }

    /**
     * Get all children
     */
    public List<ASTNode> getChildren() {
        return new ArrayList<>(children);
    }

    /**
     * Check if this node has children
     */
    public boolean hasChildren() {
        return !children.isEmpty();
    }

    /**
     * Get the number of children
     */
    public int getChildCount() {
        return children.size();
    }

    /**
     * Get a child by index
     */
    public ASTNode getChild(int index) {
        if (index >= 0 && index < children.size()) {
            return children.get(index);
        }
        return null;
    }

    // Getters and setters

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public int getColumnNumber() {
        return columnNumber;
    }

    public void setColumnNumber(int columnNumber) {
        this.columnNumber = columnNumber;
    }

    public ASTNode getParent() {
        return parent;
    }

    public void setParent(ASTNode parent) {
        this.parent = parent;
    }

    /**
     * Get the source location as a string
     */
    public String getLocation() {
        return String.format("Line %d, Column %d", lineNumber, columnNumber);
    }

    @Override
    public String toString() {
        return String.format("%s [%s]", getNodeType(), getLocation());
    }
}
