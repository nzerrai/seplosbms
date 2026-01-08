package com.cobol.translator.redefines;

import java.util.List;

/**
 * Générateur de classes wrapper Java pour les unions COBOL (REDEFINES)
 * Crée des classes avec vues multiples sur les mêmes données
 */
public class UnionTypeGenerator {
    
    private static final String INDENT = "    ";
    private static final String DOUBLE_INDENT = "        ";
    
    /**
     * Génère une classe wrapper Java pour un champ avec redéfinitions
     */
    public String generateWrapperClass(RedefinesInfo info, String packageName) {
        StringBuilder code = new StringBuilder();
        
        // Package et imports
        code.append("package ").append(packageName).append(";\n\n");
        code.append("import java.math.BigDecimal;\n");
        code.append("import java.nio.charset.StandardCharsets;\n");
        code.append("import java.util.Arrays;\n\n");
        
        // Documentation de la classe
        String className = toClassName(info.getOriginalFieldName());
        code.append("/**\n");
        code.append(" * Wrapper for COBOL REDEFINES: ").append(info.getOriginalFieldName()).append("\n");
        code.append(" *\n");
        code.append(" * Available views:\n");
        
        for (RedefinesView view : info.getAllViews()) {
            code.append(" * - ").append(view.getGetterName()).append("(): View as ")
                .append(view.getName()).append(" (").append(view.getType()).append(")\n");
        }
        
        code.append(" */\n");
        code.append("public class ").append(className).append(" {\n\n");
        
        // Champ de stockage brut
        code.append(INDENT).append("// Raw storage (").append(info.getFieldSize()).append(" bytes)\n");
        code.append(INDENT).append("private byte[] rawData = new byte[").append(info.getFieldSize()).append("];\n\n");
        
        // Constructeur par défaut
        code.append(INDENT).append("public ").append(className).append("() {\n");
        code.append(DOUBLE_INDENT).append("// Initialize with spaces\n");
        code.append(DOUBLE_INDENT).append("Arrays.fill(rawData, (byte) ' ');\n");
        code.append(INDENT).append("}\n\n");
        
        // Constructeur avec données initiales
        code.append(INDENT).append("public ").append(className).append("(byte[] initialData) {\n");
        code.append(DOUBLE_INDENT).append("if (initialData != null && initialData.length > 0) {\n");
        code.append(DOUBLE_INDENT).append(INDENT)
            .append("System.arraycopy(initialData, 0, rawData, 0, ")
            .append("Math.min(initialData.length, ").append(info.getFieldSize()).append("));\n");
        code.append(DOUBLE_INDENT).append("}\n");
        code.append(INDENT).append("}\n\n");
        
        // Générer les getters/setters pour chaque vue
        for (RedefinesView view : info.getAllViews()) {
            code.append(generateViewGetterSetter(view, info.getFieldSize()));
        }
        
        // Méthode pour accéder aux données brutes
        code.append(INDENT).append("/**\n");
        code.append(INDENT).append(" * Get raw byte data\n");
        code.append(INDENT).append(" */\n");
        code.append(INDENT).append("public byte[] getRawData() {\n");
        code.append(DOUBLE_INDENT).append("return Arrays.copyOf(rawData, rawData.length);\n");
        code.append(INDENT).append("}\n\n");
        
        code.append(INDENT).append("/**\n");
        code.append(INDENT).append(" * Set raw byte data\n");
        code.append(INDENT).append(" */\n");
        code.append(INDENT).append("public void setRawData(byte[] data) {\n");
        code.append(DOUBLE_INDENT).append("if (data != null) {\n");
        code.append(DOUBLE_INDENT).append(INDENT)
            .append("System.arraycopy(data, 0, rawData, 0, Math.min(data.length, rawData.length));\n");
        code.append(DOUBLE_INDENT).append("}\n");
        code.append(INDENT).append("}\n\n");
        
        // Méthode toString
        code.append(INDENT).append("@Override\n");
        code.append(INDENT).append("public String toString() {\n");
        code.append(DOUBLE_INDENT).append("return \"").append(className).append("{\" +\n");
        code.append(DOUBLE_INDENT).append(INDENT).append("\"rawData=\" + Arrays.toString(rawData) +\n");
        code.append(DOUBLE_INDENT).append(INDENT).append("\"}\";\n");
        code.append(INDENT).append("}\n");
        
        code.append("}\n");
        
        // Générer les classes pour les vues structurées
        for (RedefinesView view : info.getRedefinitions()) {
            if (view.isStructured()) {
                code.append("\n");
                code.append(generateStructuredViewClass(view, packageName));
            }
        }
        
        return code.toString();
    }
    
    /**
     * Génère les méthodes getter/setter pour une vue
     */
    private String generateViewGetterSetter(RedefinesView view, int totalSize) {
        StringBuilder code = new StringBuilder();
        
        // Documentation
        code.append(INDENT).append("/**\n");
        code.append(INDENT).append(" * View as ").append(view.getName());
        code.append(" (").append(view.getType()).append(", ").append(view.getSize()).append(" bytes)\n");
        if (view.isStructured()) {
            code.append(INDENT).append(" * Structured view with ").append(view.getSubFields().size())
                .append(" sub-fields\n");
        }
        code.append(INDENT).append(" */\n");
        
        // Getter
        String getterName = view.getGetterName();
        String javaType = view.getJavaType();
        
        code.append(INDENT).append("public ").append(javaType).append(" ").append(getterName).append("() {\n");
        
        switch (view.getViewType()) {
            case ALPHANUMERIC, ORIGINAL -> {
                code.append(DOUBLE_INDENT).append("return new String(rawData, StandardCharsets.UTF_8).trim();\n");
            }
            case NUMERIC -> {
                code.append(DOUBLE_INDENT).append("String numStr = new String(rawData, StandardCharsets.UTF_8).trim();\n");
                code.append(DOUBLE_INDENT).append("if (numStr.isEmpty()) {\n");
                code.append(DOUBLE_INDENT).append(INDENT).append("return BigDecimal.ZERO;\n");
                code.append(DOUBLE_INDENT).append("}\n");
                code.append(DOUBLE_INDENT).append("try {\n");
                code.append(DOUBLE_INDENT).append(INDENT).append("return new BigDecimal(numStr);\n");
                code.append(DOUBLE_INDENT).append("} catch (NumberFormatException e) {\n");
                code.append(DOUBLE_INDENT).append(INDENT).append("return BigDecimal.ZERO;\n");
                code.append(DOUBLE_INDENT).append("}\n");
            }
            case STRUCTURED -> {
                code.append(DOUBLE_INDENT).append("return new ").append(javaType).append("(rawData);\n");
            }
            case BINARY -> {
                code.append(DOUBLE_INDENT).append("return Arrays.copyOf(rawData, rawData.length);\n");
            }
        }
        
        code.append(INDENT).append("}\n\n");
        
        // Setter
        String setterName = view.getSetterName();
        code.append(INDENT).append("/**\n");
        code.append(INDENT).append(" * Set as ").append(view.getName()).append("\n");
        code.append(INDENT).append(" */\n");
        code.append(INDENT).append("public void ").append(setterName).append("(").append(javaType).append(" value) {\n");
        
        switch (view.getViewType()) {
            case ALPHANUMERIC, ORIGINAL -> {
                code.append(DOUBLE_INDENT).append("if (value == null) {\n");
                code.append(DOUBLE_INDENT).append(INDENT).append("Arrays.fill(rawData, (byte) ' ');\n");
                code.append(DOUBLE_INDENT).append(INDENT).append("return;\n");
                code.append(DOUBLE_INDENT).append("}\n");
                code.append(DOUBLE_INDENT).append("byte[] bytes = value.getBytes(StandardCharsets.UTF_8);\n");
                code.append(DOUBLE_INDENT).append("Arrays.fill(rawData, (byte) ' ');\n");
                code.append(DOUBLE_INDENT).append("System.arraycopy(bytes, 0, rawData, 0, Math.min(bytes.length, ")
                    .append(totalSize).append("));\n");
            }
            case NUMERIC -> {
                code.append(DOUBLE_INDENT).append("if (value == null) {\n");
                code.append(DOUBLE_INDENT).append(INDENT).append("value = BigDecimal.ZERO;\n");
                code.append(DOUBLE_INDENT).append("}\n");
                code.append(DOUBLE_INDENT).append("String numStr = String.format(\"%0")
                    .append(totalSize).append("d\", value.longValue());\n");
                code.append(DOUBLE_INDENT).append("byte[] bytes = numStr.getBytes(StandardCharsets.UTF_8);\n");
                code.append(DOUBLE_INDENT).append("System.arraycopy(bytes, 0, rawData, 0, Math.min(bytes.length, ")
                    .append(totalSize).append("));\n");
            }
            case STRUCTURED -> {
                code.append(DOUBLE_INDENT).append("if (value != null) {\n");
                code.append(DOUBLE_INDENT).append(INDENT).append("byte[] bytes = value.toByteArray();\n");
                code.append(DOUBLE_INDENT).append(INDENT)
                    .append("System.arraycopy(bytes, 0, rawData, 0, Math.min(bytes.length, ")
                    .append(totalSize).append("));\n");
                code.append(DOUBLE_INDENT).append("}\n");
            }
            case BINARY -> {
                code.append(DOUBLE_INDENT).append("if (value != null) {\n");
                code.append(DOUBLE_INDENT).append(INDENT)
                    .append("System.arraycopy(value, 0, rawData, 0, Math.min(value.length, ")
                    .append(totalSize).append("));\n");
                code.append(DOUBLE_INDENT).append("}\n");
            }
        }
        
        code.append(INDENT).append("}\n\n");
        
        return code.toString();
    }
    
    /**
     * Génère une classe pour une vue structurée
     */
    private String generateStructuredViewClass(RedefinesView view, String packageName) {
        StringBuilder code = new StringBuilder();
        
        String className = view.getJavaType();
        
        code.append("/**\n");
        code.append(" * Structured view: ").append(view.getName()).append("\n");
        code.append(" */\n");
        code.append("class ").append(className).append(" {\n\n");
        
        // Champs
        for (RedefinesView.SubField subField : view.getSubFields()) {
            code.append(INDENT).append("private ").append(subField.getJavaType()).append(" ")
                .append(subField.getFieldName()).append(";\n");
        }
        code.append("\n");
        
        // Constructeur à partir de byte[]
        code.append(INDENT).append("public ").append(className).append("(byte[] data) {\n");
        for (RedefinesView.SubField subField : view.getSubFields()) {
            int offset = subField.getOffset();
            int size = subField.getSize();
            
            if (subField.getJavaType().equals("String")) {
                code.append(DOUBLE_INDENT).append("this.").append(subField.getFieldName())
                    .append(" = new String(data, ").append(offset).append(", ")
                    .append(size).append(", StandardCharsets.UTF_8).trim();\n");
            } else {
                code.append(DOUBLE_INDENT).append("String numStr").append(offset)
                    .append(" = new String(data, ").append(offset).append(", ")
                    .append(size).append(", StandardCharsets.UTF_8).trim();\n");
                code.append(DOUBLE_INDENT).append("this.").append(subField.getFieldName())
                    .append(" = numStr").append(offset).append(".isEmpty() ? BigDecimal.ZERO : new BigDecimal(numStr")
                    .append(offset).append(");\n");
            }
        }
        code.append(INDENT).append("}\n\n");
        
        // Méthode toByteArray
        code.append(INDENT).append("public byte[] toByteArray() {\n");
        code.append(DOUBLE_INDENT).append("byte[] result = new byte[").append(view.getSize()).append("];\n");
        code.append(DOUBLE_INDENT).append("Arrays.fill(result, (byte) ' ');\n");
        
        for (RedefinesView.SubField subField : view.getSubFields()) {
            int offset = subField.getOffset();
            int size = subField.getSize();
            
            if (subField.getJavaType().equals("String")) {
                code.append(DOUBLE_INDENT).append("byte[] bytes").append(offset)
                    .append(" = ").append(subField.getFieldName())
                    .append(".getBytes(StandardCharsets.UTF_8);\n");
            } else {
                code.append(DOUBLE_INDENT).append("String numStr").append(offset)
                    .append(" = String.format(\"%0").append(size).append("d\", ")
                    .append(subField.getFieldName()).append(".longValue());\n");
                code.append(DOUBLE_INDENT).append("byte[] bytes").append(offset)
                    .append(" = numStr").append(offset).append(".getBytes(StandardCharsets.UTF_8);\n");
            }
            
            code.append(DOUBLE_INDENT).append("System.arraycopy(bytes").append(offset)
                .append(", 0, result, ").append(offset).append(", Math.min(bytes")
                .append(offset).append(".length, ").append(size).append("));\n");
        }
        
        code.append(DOUBLE_INDENT).append("return result;\n");
        code.append(INDENT).append("}\n\n");
        
        // Getters et setters
        for (RedefinesView.SubField subField : view.getSubFields()) {
            String fieldName = subField.getFieldName();
            String capitalizedName = Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
            
            code.append(INDENT).append("public ").append(subField.getJavaType())
                .append(" get").append(capitalizedName).append("() {\n");
            code.append(DOUBLE_INDENT).append("return ").append(fieldName).append(";\n");
            code.append(INDENT).append("}\n\n");
            
            code.append(INDENT).append("public void set").append(capitalizedName)
                .append("(").append(subField.getJavaType()).append(" value) {\n");
            code.append(DOUBLE_INDENT).append("this.").append(fieldName).append(" = value;\n");
            code.append(INDENT).append("}\n\n");
        }
        
        code.append("}\n");
        
        return code.toString();
    }
    
    /**
     * Convertit un nom de champ COBOL en nom de classe Java
     */
    private String toClassName(String cobolName) {
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;
        
        for (char c : cobolName.toCharArray()) {
            if (c == '-' || c == '_') {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                result.append(Character.toLowerCase(c));
            }
        }
        
        return result.toString() + "Wrapper";
    }
}
