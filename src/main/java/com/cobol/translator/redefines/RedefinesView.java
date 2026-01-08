package com.cobol.translator.redefines;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente une vue (redéfinition) d'un champ COBOL
 */
public class RedefinesView {
    
    private final String name;
    private final String type;
    private final int size;
    private final ViewType viewType;
    private final List<SubField> subFields;
    
    public RedefinesView(String name, String type, int size, ViewType viewType, List<SubField> subFields) {
        this.name = name;
        this.type = type;
        this.size = size;
        this.viewType = viewType;
        this.subFields = subFields != null ? new ArrayList<>(subFields) : new ArrayList<>();
    }
    
    /**
     * Vérifie si cette vue est structurée (contient des sous-champs)
     */
    public boolean isStructured() {
        return !subFields.isEmpty();
    }
    
    /**
     * Retourne le type Java correspondant
     */
    public String getJavaType() {
        return switch (viewType) {
            case ALPHANUMERIC, ORIGINAL -> "String";
            case NUMERIC -> "BigDecimal";
            case STRUCTURED -> toJavaClassName(name);
            case BINARY -> "byte[]";
        };
    }
    
    /**
     * Retourne le nom de la méthode getter pour cette vue
     */
    public String getGetterName() {
        if (viewType == ViewType.ORIGINAL) {
            return "asOriginal";
        }
        return "as" + capitalize(toCamelCase(name));
    }
    
    /**
     * Retourne le nom de la méthode setter pour cette vue
     */
    public String getSetterName() {
        if (viewType == ViewType.ORIGINAL) {
            return "setAsOriginal";
        }
        return "setAs" + capitalize(toCamelCase(name));
    }
    
    // Getters
    public String getName() {
        return name;
    }
    
    public String getType() {
        return type;
    }
    
    public int getSize() {
        return size;
    }
    
    public ViewType getViewType() {
        return viewType;
    }
    
    public List<SubField> getSubFields() {
        return new ArrayList<>(subFields);
    }
    
    // Utilitaires de conversion de noms
    private String toJavaClassName(String cobolName) {
        return capitalize(toCamelCase(cobolName)) + "View";
    }
    
    private String toCamelCase(String name) {
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = false;
        
        for (char c : name.toCharArray()) {
            if (c == '-' || c == '_') {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                result.append(Character.toLowerCase(c));
            }
        }
        
        return result.toString();
    }
    
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return Character.toUpperCase(str.charAt(0)) + str.substring(1);
    }
    
    /**
     * Classe interne pour représenter un sous-champ d'une vue structurée
     */
    public static class SubField {
        private final String name;
        private final String type;
        private final int size;
        private final int offset;
        
        public SubField(String name, String type, int size, int offset) {
            this.name = name;
            this.type = type;
            this.size = size;
            this.offset = offset;
        }
        
        public String getName() {
            return name;
        }
        
        public String getType() {
            return type;
        }
        
        public int getSize() {
            return size;
        }
        
        public int getOffset() {
            return offset;
        }
        
        public String getJavaType() {
            if (type.startsWith("9")) {
                return "BigDecimal";
            }
            return "String";
        }
        
        public String getFieldName() {
            return toCamelCase(name);
        }
        
        private String toCamelCase(String name) {
            StringBuilder result = new StringBuilder();
            boolean capitalizeNext = false;
            
            for (char c : name.toCharArray()) {
                if (c == '-' || c == '_') {
                    capitalizeNext = true;
                } else if (capitalizeNext) {
                    result.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    result.append(Character.toLowerCase(c));
                }
            }
            
            return result.toString();
        }
    }
}
