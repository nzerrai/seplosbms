package com.cobol.translator.vsam;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.DataItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

/**
 * Maps VSAM file definitions to JDBC/JPA entity structures with appropriate indexes.
 */
public class VsamToJdbcMapper {
    
    private static final Logger logger = LoggerFactory.getLogger(VsamToJdbcMapper.class);
    
    private final TranslationConfig config;
    
    public VsamToJdbcMapper(TranslationConfig config) {
        this.config = config;
    }
    
    /**
     * Generate JPA entity with indexes for VSAM file
     */
    public String generateJpaEntity(VsamFileInfo vsamInfo, DataItem recordLayout) throws IOException {
        StringBuilder java = new StringBuilder();
        
        String className = toClassName(vsamInfo.getFileName());
        String packageName = config.getOutputPackage() + ".model";
        
        // Package and imports
        java.append("package ").append(packageName).append(";\n\n");
        java.append("import jakarta.persistence.*;\n");
        java.append("import java.math.BigDecimal;\n");
        java.append("import java.time.LocalDate;\n");
        java.append("import java.io.Serializable;\n\n");
        
        // Class-level annotations
        java.append("/**\n");
        java.append(" * JPA Entity generated from VSAM file: ").append(vsamInfo.getFileName()).append("\n");
        java.append(" * VSAM Type: ").append(vsamInfo.getVsamType().getDescription()).append("\n");
        java.append(" * Access Mode: ").append(vsamInfo.getAccessMode()).append("\n");
        if (vsamInfo.getPrimaryKey() != null) {
            java.append(" * Primary Key: ").append(vsamInfo.getPrimaryKey()).append("\n");
        }
        java.append(" * \n");
        java.append(" * @generated from COBOL VSAM definition\n");
        java.append(" */\n");
        java.append("@Entity\n");
        java.append("@Table(name = \"").append(toTableName(vsamInfo.getFileName())).append("\"");
        
        // Add indexes for KSDS (indexed) files
        if (vsamInfo.isIndexed() && vsamInfo.hasAlternateKeys()) {
            java.append(",\n    indexes = {\n");
            
            // Alternate key indexes
            for (int i = 0; i < vsamInfo.getAlternateKeys().size(); i++) {
                AlternateKeyInfo altKey = vsamInfo.getAlternateKeys().get(i);
                java.append("        @Index(name = \"idx_")
                    .append(toTableName(vsamInfo.getFileName())).append("_")
                    .append(toJavaFieldName(altKey.getKeyName())).append("\", ");
                java.append("columnList = \"").append(toJavaFieldName(altKey.getKeyName())).append("\", ");
                java.append("unique = ").append(!altKey.isAllowDuplicates()).append(")");
                
                if (i < vsamInfo.getAlternateKeys().size() - 1) {
                    java.append(",");
                }
                java.append("\n");
            }
            
            java.append("    }");
        }
        
        java.append(")\n");
        java.append("public class ").append(className).append(" implements Serializable {\n\n");
        java.append("    private static final long serialVersionUID = 1L;\n\n");
        
        // Generate ID field based on primary key
        if (vsamInfo.getPrimaryKey() != null) {
            String keyFieldName = toJavaFieldName(vsamInfo.getPrimaryKey());
            java.append("    @Id\n");
            
            DataItem keyField = recordLayout;
            if (keyField != null && isNumeric(keyField)) {
                java.append("    @Column(name = \"").append(keyFieldName).append("\", nullable = false)\n");
                java.append("    private Long ").append(keyFieldName).append(";\n\n");
            } else {
                java.append("    @Column(name = \"").append(keyFieldName)
                    .append("\", nullable = false, length = 50)\n");
                java.append("    private String ").append(keyFieldName).append(";\n\n");
            }
        } else {
            // No primary key defined, generate auto-increment ID
            java.append("    @Id\n");
            java.append("    @GeneratedValue(strategy = GenerationType.IDENTITY)\n");
            java.append("    private Long id;\n\n");
        }
        
        // Generate getters and setters
        java.append("    // Getters and Setters\n\n");
        
        if (vsamInfo.getPrimaryKey() != null) {
            String keyFieldName = toJavaFieldName(vsamInfo.getPrimaryKey());
            DataItem keyField = recordLayout;
            String keyType = (keyField != null && isNumeric(keyField)) ? "Long" : "String";
            
            java.append("    public ").append(keyType).append(" get")
                .append(capitalize(keyFieldName)).append("() {\n");
            java.append("        return ").append(keyFieldName).append(";\n");
            java.append("    }\n\n");
            
            java.append("    public void set").append(capitalize(keyFieldName))
                .append("(").append(keyType).append(" ").append(keyFieldName).append(") {\n");
            java.append("        this.").append(keyFieldName).append(" = ").append(keyFieldName).append(";\n");
            java.append("    }\n\n");
        } else {
            java.append("    public Long getId() {\n");
            java.append("        return id;\n");
            java.append("    }\n\n");
            
            java.append("    public void setId(Long id) {\n");
            java.append("        this.id = id;\n");
            java.append("    }\n\n");
        }
        
        java.append("}\n");
        
        logger.info("Generated JPA entity for VSAM file: {}", className);
        return java.toString();
    }
    
    private boolean isNumeric(DataItem item) {
        String picture = item.getPictureClause();
        return picture != null && picture.contains("9");
    }
    
    private String toClassName(String fileName) {
        return toPascalCase(fileName) + "Entity";
    }
    
    private String toTableName(String fileName) {
        return fileName.toLowerCase().replaceAll("[^a-z0-9]", "_");
    }
    
    private String toJavaFieldName(String cobolName) {
        return toCamelCase(cobolName);
    }
    
    private String toPascalCase(String input) {
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;
        for (char c : input.toCharArray()) {
            if (Character.isLetterOrDigit(c)) {
                result.append(capitalizeNext ? Character.toUpperCase(c) : Character.toLowerCase(c));
                capitalizeNext = false;
            } else {
                capitalizeNext = true;
            }
        }
        return result.toString();
    }
    
    private String toCamelCase(String input) {
        String pascal = toPascalCase(input);
        return pascal.isEmpty() ? pascal : Character.toLowerCase(pascal.charAt(0)) + pascal.substring(1);
    }
    
    private String capitalize(String input) {
        return input.isEmpty() ? input : Character.toUpperCase(input.charAt(0)) + input.substring(1);
    }
}
