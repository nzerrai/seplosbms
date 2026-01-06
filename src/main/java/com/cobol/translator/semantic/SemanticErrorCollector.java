package com.cobol.translator.semantic;

import java.util.*;

/**
 * Collecteur des erreurs sémantiques détectées
 */
public class SemanticErrorCollector {
    private final List<SemanticError> errors;
    private final List<SemanticError> warnings;

    public static class SemanticError {
        private final String code;
        private final String message;
        private final int lineNumber;
        private final ErrorLevel level;

        public enum ErrorLevel {
            ERROR, WARNING, INFO
        }

        public SemanticError(String code, String message, int lineNumber, ErrorLevel level) {
            this.code = code;
            this.message = message;
            this.lineNumber = lineNumber;
            this.level = level;
        }

        public String getCode() {
            return code;
        }

        public String getMessage() {
            return message;
        }

        public int getLineNumber() {
            return lineNumber;
        }

        public ErrorLevel getLevel() {
            return level;
        }

        @Override
        public String toString() {
            return "[" + level + "] Line " + lineNumber + ": " + code + " - " + message;
        }
    }

    public SemanticErrorCollector() {
        this.errors = new ArrayList<>();
        this.warnings = new ArrayList<>();
    }

    /**
     * Ajoute une erreur sémantique
     */
    public void addError(String code, String message, int lineNumber) {
        errors.add(new SemanticError(code, message, lineNumber, SemanticError.ErrorLevel.ERROR));
    }

    /**
     * Ajoute un avertissement
     */
    public void addWarning(String code, String message, int lineNumber) {
        warnings.add(new SemanticError(code, message, lineNumber, SemanticError.ErrorLevel.WARNING));
    }

    /**
     * Ajoute un message informatif
     */
    public void addInfo(String code, String message, int lineNumber) {
        warnings.add(new SemanticError(code, message, lineNumber, SemanticError.ErrorLevel.INFO));
    }

    /**
     * Retourne toutes les erreurs
     */
    public List<SemanticError> getErrors() {
        return new ArrayList<>(errors);
    }

    /**
     * Retourne tous les avertissements
     */
    public List<SemanticError> getWarnings() {
        return new ArrayList<>(warnings);
    }

    /**
     * Retourne tous les messages (erreurs + avertissements)
     */
    public List<SemanticError> getAllMessages() {
        List<SemanticError> all = new ArrayList<>(errors);
        all.addAll(warnings);
        return all;
    }

    /**
     * Vérifie s'il y a des erreurs
     */
    public boolean hasErrors() {
        return !errors.isEmpty();
    }

    /**
     * Vérifie s'il y a des avertissements
     */
    public boolean hasWarnings() {
        return !warnings.isEmpty();
    }

    /**
     * Compte les erreurs
     */
    public int getErrorCount() {
        return errors.size();
    }

    /**
     * Compte les avertissements
     */
    public int getWarningCount() {
        return warnings.size();
    }

    /**
     * Vide le collecteur
     */
    public void clear() {
        errors.clear();
        warnings.clear();
    }

    /**
     * Affiche tous les messages
     */
    public void printAll() {
        System.out.println("=== Semantic Analysis Report ===");
        if (errors.isEmpty() && warnings.isEmpty()) {
            System.out.println("✓ No errors or warnings found");
        } else {
            if (!errors.isEmpty()) {
                System.out.println("\nErrors (" + errors.size() + "):");
                errors.forEach(System.out::println);
            }
            if (!warnings.isEmpty()) {
                System.out.println("\nWarnings (" + warnings.size() + "):");
                warnings.forEach(System.out::println);
            }
        }
    }

    @Override
    public String toString() {
        return "SemanticErrorCollector{" +
                "errors=" + errors.size() +
                ", warnings=" + warnings.size() +
                '}';
    }
}
