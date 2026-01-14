package com.cobol.translator.semantic;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Représente un symbole COBOL (variable, paragraphe, fichier, etc.)
 */
public class Symbol {
    private final String name;
    private final SymbolType type;
    private final Scope scope;
    private String javaType;
    private String picClause;
    private int lineNumber;
    private Map<String, Object> attributes;

    public Symbol(String name, SymbolType type, Scope scope) {
        this.name = name;
        this.type = type;
        this.scope = scope;
        this.attributes = new HashMap<>();
    }

    public String getName() {
        return name;
    }

    public SymbolType getType() {
        return type;
    }

    public Scope getScope() {
        return scope;
    }

    public String getJavaType() {
        return javaType;
    }

    public void setJavaType(String javaType) {
        this.javaType = javaType;
    }

    public String getPicClause() {
        return picClause;
    }

    public void setPicClause(String picClause) {
        this.picClause = picClause;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public void setAttribute(String key, Object value) {
        attributes.put(key, value);
    }

    public Object getAttribute(String key) {
        return attributes.get(key);
    }

    public String getFullyQualifiedName() {
        return scope.getFullPath() + "." + name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Symbol symbol = (Symbol) o;
        return Objects.equals(name, symbol.name) &&
               type == symbol.type &&
               Objects.equals(scope, symbol.scope);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, type, scope);
    }

    @Override
    public String toString() {
        return "Symbol{" +
                "name='" + name + '\'' +
                ", type=" + type +
                ", scope=" + scope.getName() +
                ", javaType='" + javaType + '\'' +
                ", picClause='" + picClause + '\'' +
                ", line=" + lineNumber +
                '}';
    }
}
