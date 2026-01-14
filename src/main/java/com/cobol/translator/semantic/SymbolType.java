package com.cobol.translator.semantic;

/**
 * Énumération des types de symboles dans un programme COBOL
 */
public enum SymbolType {
    VARIABLE("Variable"),
    PARAGRAPH("Paragraph"),
    FILE("File"),
    FILE_RECORD("FileRecord"),
    SECTION("Section"),
    DIVISION("Division"),
    PROGRAM("Program"),
    UNKNOWN("Unknown");

    private final String displayName;

    SymbolType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
