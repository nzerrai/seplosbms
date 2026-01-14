package com.cobol.translator.semantic;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Table des symboles pour un programme COBOL
 * Gère l'enregistrement et la résolution des variables, paragraphes, fichiers
 */
public class SymbolTable {
    private final Map<String, Symbol> symbols;
    private final Stack<Scope> scopeStack;
    private Scope globalScope;

    public SymbolTable() {
        this.symbols = new HashMap<>();
        this.scopeStack = new Stack<>();
        this.globalScope = new Scope("GLOBAL", Scope.ScopeLevel.GLOBAL, null);
        this.scopeStack.push(globalScope);
    }

    /**
     * Enregistre un nouveau symbole dans la table
     */
    public void registerSymbol(String name, SymbolType type) {
        Scope currentScope = getCurrentScope();
        Symbol symbol = new Symbol(name, type, currentScope);
        String key = getSymbolKey(name, currentScope);
        symbols.put(key, symbol);
    }

    /**
     * Enregistre un symbole avec des détails PIC
     */
    public void registerVariable(String name, String picClause) {
        registerSymbol(name, SymbolType.VARIABLE);
        Symbol symbol = lookupSymbol(name);
        if (symbol != null) {
            symbol.setPicClause(picClause);
            symbol.setJavaType(convertPicToJavaType(picClause));
        }
    }

    /**
     * Enregistre un paragraphe
     */
    public void registerParagraph(String name) {
        registerSymbol(name, SymbolType.PARAGRAPH);
    }

    /**
     * Enregistre un fichier
     */
    public void registerFile(String name) {
        registerSymbol(name, SymbolType.FILE);
    }

    /**
     * Cherche un symbole dans la portée courante et les portées parent
     */
    public Symbol lookupSymbol(String name) {
        Scope scope = getCurrentScope();
        while (scope != null) {
            String key = getSymbolKey(name, scope);
            if (symbols.containsKey(key)) {
                return symbols.get(key);
            }
            scope = scope.getParent();
        }
        return null;
    }

    /**
     * Cherche un symbole au niveau GLOBAL uniquement
     */
    public Symbol lookupGlobalSymbol(String name) {
        String key = getSymbolKey(name, globalScope);
        return symbols.get(key);
    }

    /**
     * Vérifie si un symbole existe
     */
    public boolean symbolExists(String name) {
        return lookupSymbol(name) != null;
    }

    /**
     * Rentre dans une nouvelle portée (SECTION, DIVISION, PARAGRAPH)
     */
    public void enterScope(String name, Scope.ScopeLevel level) {
        Scope newScope = new Scope(name, level, getCurrentScope());
        scopeStack.push(newScope);
    }

    /**
     * Sort de la portée courante
     */
    public void exitScope() {
        if (scopeStack.size() > 1) {
            scopeStack.pop();
        }
    }

    /**
     * Retourne la portée courante
     */
    public Scope getCurrentScope() {
        return scopeStack.peek();
    }

    /**
     * Liste tous les symboles
     */
    public List<Symbol> listAllSymbols() {
        return new ArrayList<>(symbols.values());
    }

    /**
     * Liste les symboles d'un type spécifique
     */
    public List<Symbol> listSymbolsByType(SymbolType type) {
        return symbols.values().stream()
            .filter(s -> s.getType() == type)
            .collect(Collectors.toList());
    }

    /**
     * Obtient le nombre de symboles
     */
    public int size() {
        return symbols.size();
    }

    /**
     * Vide la table des symboles
     */
    public void clear() {
        symbols.clear();
        scopeStack.clear();
        scopeStack.push(globalScope);
    }

    /**
     * Convertit une clause PIC COBOL en type Java
     */
    private String convertPicToJavaType(String picClause) {
        if (picClause == null || picClause.isEmpty()) {
            return "String";
        }

        picClause = picClause.trim().toUpperCase();
        
        // Remove the "PIC" keyword if present
        if (picClause.startsWith("PIC ")) {
            picClause = picClause.substring(4).trim();
        }

        // Numériques avec virgule (DECIMAL)
        if (picClause.contains("V")) {
            return "BigDecimal";
        }

        // Numériques signés
        if (picClause.startsWith("S")) {
            if (picClause.length() > 1 && picClause.charAt(1) == '9') {
                return "Integer";
            }
            return "Integer";
        }

        // Numériques non signés
        if (picClause.matches("^9.*")) {
            int length = countDigits(picClause);
            if (length <= 4) {
                return "Integer";
            } else if (length <= 9) {
                return "Long";
            } else {
                return "BigDecimal";
            }
        }

        // Alphanumériques
        if (picClause.matches("^X.*")) {
            return "String";
        }

        // Par défaut
        return "String";
    }

    /**
     * Compte les chiffres dans une clause PIC
     */
    private int countDigits(String picClause) {
        int count = 0;
        for (char c : picClause.toCharArray()) {
            if (c == '9') {
                count++;
            } else if (c == '(') {
                // Traiter les répétitions comme 9(5)
                int closeIdx = picClause.indexOf(')', picClause.indexOf(c));
                if (closeIdx > 0) {
                    String repeat = picClause.substring(picClause.indexOf(c) + 1, closeIdx);
                    try {
                        count += Integer.parseInt(repeat);
                    } catch (NumberFormatException ignored) {
                    }
                }
            }
        }
        return Math.max(count, 1);
    }

    /**
     * Génère une clé unique pour un symbole dans sa portée
     */
    private String getSymbolKey(String name, Scope scope) {
        return scope.getFullPath() + "#" + name;
    }

    @Override
    public String toString() {
        return "SymbolTable{" +
                "symbols=" + symbols.size() +
                ", currentScope=" + getCurrentScope().getName() +
                '}';
    }
}
