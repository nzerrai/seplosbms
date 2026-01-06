package com.cobol.translator.semantic;

/**
 * Représente une portée dans le programme COBOL
 * Peut être GLOBAL, SECTION, ou DIVISION
 */
public class Scope {
    private final String name;
    private final ScopeLevel level;
    private final Scope parent;

    public enum ScopeLevel {
        GLOBAL("GLOBAL"),
        DIVISION("DIVISION"),
        SECTION("SECTION"),
        PARAGRAPH("PARAGRAPH");

        private final String displayName;

        ScopeLevel(String displayName) {
            this.displayName = displayName;
        }

        public String getDisplayName() {
            return displayName;
        }
    }

    public Scope(String name, ScopeLevel level, Scope parent) {
        this.name = name;
        this.level = level;
        this.parent = parent;
    }

    public String getName() {
        return name;
    }

    public ScopeLevel getLevel() {
        return level;
    }

    public Scope getParent() {
        return parent;
    }

    public String getFullPath() {
        if (parent == null) {
            return name;
        }
        return parent.getFullPath() + " > " + name;
    }

    @Override
    public String toString() {
        return "Scope{" +
                "name='" + name + '\'' +
                ", level=" + level +
                ", fullPath='" + getFullPath() + '\'' +
                '}';
    }
}
