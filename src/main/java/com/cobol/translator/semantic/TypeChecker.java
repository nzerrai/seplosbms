package com.cobol.translator.semantic;

import java.math.BigDecimal;
import java.util.*;

/**
 * Validateur de types COBOL
 * Vérifie la compatibilité des opérations (MOVE, COMPUTE, IF, etc.)
 */
public class TypeChecker {
    private final SymbolTable symbolTable;
    private final SemanticErrorCollector errorCollector;

    public TypeChecker(SymbolTable symbolTable) {
        this.symbolTable = symbolTable;
        this.errorCollector = new SemanticErrorCollector();
    }

    /**
     * Valide un MOVE statement
     * MOVE source TO target
     */
    public boolean validateMove(String source, String target, int lineNumber) {
        Symbol sourceSymbol = symbolTable.lookupSymbol(source);
        Symbol targetSymbol = symbolTable.lookupSymbol(target);

        if (sourceSymbol == null) {
            errorCollector.addError("E001", "Undefined source variable: " + source, lineNumber);
            return false;
        }

        if (targetSymbol == null) {
            errorCollector.addError("E002", "Undefined target variable: " + target, lineNumber);
            return false;
        }

        return validateTypeCompatibility(sourceSymbol, targetSymbol, "MOVE", lineNumber);
    }

    /**
     * Valide une expression COMPUTE
     */
    public boolean validateCompute(String target, String expression, int lineNumber) {
        Symbol targetSymbol = symbolTable.lookupSymbol(target);

        if (targetSymbol == null) {
            errorCollector.addError("E002", "Undefined target variable: " + target, lineNumber);
            return false;
        }

        // Vérifier que toutes les variables dans l'expression existent
        List<String> variablesInExpression = extractVariablesFromExpression(expression);
        for (String var : variablesInExpression) {
            if (!symbolTable.symbolExists(var)) {
                errorCollector.addError("E001", "Undefined variable in expression: " + var, lineNumber);
                return false;
            }
        }

        return true;
    }

    /**
     * Valide une condition IF
     */
    public boolean validateIfCondition(String condition, int lineNumber) {
        List<String> variables = extractVariablesFromExpression(condition);

        for (String var : variables) {
            if (!symbolTable.symbolExists(var)) {
                errorCollector.addError("E001", "Undefined variable in IF condition: " + var, lineNumber);
                return false;
            }
        }

        return true;
    }

    /**
     * Valide un PERFORM paragraph
     */
    public boolean validatePerform(String paragraphName, int lineNumber) {
        Symbol paragraph = symbolTable.lookupSymbol(paragraphName);

        if (paragraph == null || paragraph.getType() != SymbolType.PARAGRAPH) {
            errorCollector.addError("E003", "Undefined paragraph: " + paragraphName, lineNumber);
            return false;
        }

        return true;
    }

    /**
     * Valide un OPEN/READ/WRITE sur fichier
     */
    public boolean validateFileOperation(String fileName, String operation, int lineNumber) {
        Symbol fileSymbol = symbolTable.lookupSymbol(fileName);

        if (fileSymbol == null || fileSymbol.getType() != SymbolType.FILE) {
            errorCollector.addError("E004", "Undefined file: " + fileName, lineNumber);
            return false;
        }

        return true;
    }

    /**
     * Vérifie la compatibilité de type entre deux symboles
     */
    public boolean validateTypeCompatibility(Symbol source, Symbol target, String operation, int lineNumber) {
        String sourceType = getTypeCategory(source);
        String targetType = getTypeCategory(target);

        // Même type = toujours compatible
        if (sourceType.equals(targetType)) {
            return true;
        }

        // NUMERIC vers NUMERIC = compatible (avec perte possible de précision)
        if (sourceType.equals("NUMERIC") && targetType.equals("NUMERIC")) {
            return true;
        }

        // NUMERIC vers ALPHA = NON compatible
        if (sourceType.equals("NUMERIC") && targetType.equals("ALPHA")) {
            errorCollector.addWarning("W001", 
                "Type mismatch in " + operation + ": " + source.getName() + 
                " (" + sourceType + ") -> " + target.getName() + " (" + targetType + ")", 
                lineNumber);
            return false;
        }

        // ALPHA vers NUMERIC = conversion possible avec attention
        if (sourceType.equals("ALPHA") && targetType.equals("NUMERIC")) {
            errorCollector.addWarning("W002", 
                "Lossy conversion in " + operation + ": " + source.getName() + 
                " (" + sourceType + ") -> " + target.getName() + " (" + targetType + ")", 
                lineNumber);
            return true; // Acceptable mais attention
        }

        return true;
    }

    /**
     * Détermine la catégorie de type
     */
    private String getTypeCategory(Symbol symbol) {
        String javaType = symbol.getJavaType();
        if (javaType == null) {
            javaType = "String";
        }

        if (javaType.contains("Integer") || javaType.contains("Long") || javaType.contains("BigDecimal")) {
            return "NUMERIC";
        }

        return "ALPHA";
    }

    /**
     * Extrait les variables d'une expression
     */
    private List<String> extractVariablesFromExpression(String expression) {
        List<String> variables = new ArrayList<>();

        if (expression == null || expression.isEmpty()) {
            return variables;
        }

        // Regex simple pour extraire les identifiants (mots composés de lettres, chiffres, tirets)
        String[] tokens = expression.split("[\\s+\\-*/()=<>!]+");

        for (String token : tokens) {
            token = token.trim();
            if (!token.isEmpty() && !isNumericLiteral(token)) {
                variables.add(token);
            }
        }

        return variables;
    }

    /**
     * Vérifie si une chaîne est un littéral numérique
     */
    private boolean isNumericLiteral(String token) {
        try {
            Double.parseDouble(token);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * Obtient le collecteur d'erreurs
     */
    public SemanticErrorCollector getErrorCollector() {
        return errorCollector;
    }

    /**
     * Affiche un rapport de validation
     */
    public void printReport() {
        errorCollector.printAll();
    }

    /**
     * Vérifie s'il y a des erreurs bloquantes
     */
    public boolean hasErrors() {
        return errorCollector.hasErrors();
    }

    @Override
    public String toString() {
        return "TypeChecker{" +
                "errors=" + errorCollector.getErrorCount() +
                ", warnings=" + errorCollector.getWarningCount() +
                '}';
    }
}
