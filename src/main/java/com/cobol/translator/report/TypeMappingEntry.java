package com.cobol.translator.report;

/**
 * Représente une entrée dans la table de correspondance COBOL/JCL → Java.
 * Documente la conversion d'une variable ou attribut avec ses types et commentaires.
 */
public class TypeMappingEntry {

    private String cobolName;           // Nom COBOL original (ex: WS-CUSTOMER-NAME)
    private String cobolType;           // Type COBOL (ex: PIC X(30))
    private String cobolSection;        // Section COBOL (FILE, WORKING-STORAGE, LINKAGE)
    private int cobolLevel;             // Niveau COBOL (01, 05, 77, etc.)

    private String javaName;            // Nom Java (ex: wsCustomerName)
    private String javaType;            // Type Java (ex: String)
    private String javaClass;           // Classe Java contenant le champ

    private String conversionComment;   // Commentaire sur la conversion
    private boolean isRedefines;        // Si c'est un REDEFINES
    private boolean isOccurs;           // Si c'est un OCCURS (tableau)
    private String occursInfo;          // Info sur OCCURS (ex: "OCCURS 10")

    public TypeMappingEntry(String cobolName, String cobolType, String javaName, String javaType) {
        this.cobolName = cobolName;
        this.cobolType = cobolType;
        this.javaName = javaName;
        this.javaType = javaType;
    }

    // Getters et Setters

    public String getCobolName() { return cobolName; }
    public void setCobolName(String cobolName) { this.cobolName = cobolName; }

    public String getCobolType() { return cobolType; }
    public void setCobolType(String cobolType) { this.cobolType = cobolType; }

    public String getCobolSection() { return cobolSection; }
    public void setCobolSection(String cobolSection) { this.cobolSection = cobolSection; }

    public int getCobolLevel() { return cobolLevel; }
    public void setCobolLevel(int cobolLevel) { this.cobolLevel = cobolLevel; }

    public String getJavaName() { return javaName; }
    public void setJavaName(String javaName) { this.javaName = javaName; }

    public String getJavaType() { return javaType; }
    public void setJavaType(String javaType) { this.javaType = javaType; }

    public String getJavaClass() { return javaClass; }
    public void setJavaClass(String javaClass) { this.javaClass = javaClass; }

    public String getConversionComment() { return conversionComment; }
    public void setConversionComment(String conversionComment) { this.conversionComment = conversionComment; }

    public boolean isRedefines() { return isRedefines; }
    public void setRedefines(boolean redefines) { isRedefines = redefines; }

    public boolean isOccurs() { return isOccurs; }
    public void setOccurs(boolean occurs) { isOccurs = occurs; }

    public String getOccursInfo() { return occursInfo; }
    public void setOccursInfo(String occursInfo) { this.occursInfo = occursInfo; }

    /**
     * Génère une ligne formatée pour le rapport texte
     */
    public String toReportLine() {
        StringBuilder sb = new StringBuilder();

        // Format: COBOL-NAME (PIC) → javaName (Type) [Section] - Comment
        sb.append(String.format("%-30s", cobolName));
        sb.append(String.format("%-20s", cobolType != null ? cobolType : "N/A"));
        sb.append(" → ");
        sb.append(String.format("%-25s", javaName));
        sb.append(String.format("%-20s", javaType));

        if (cobolSection != null) {
            sb.append(String.format("[%-15s]", cobolSection));
        }

        if (conversionComment != null && !conversionComment.isEmpty()) {
            sb.append(" - ").append(conversionComment);
        }

        return sb.toString();
    }

    /**
     * Génère une ligne CSV pour export
     */
    public String toCsvLine() {
        return String.format("%s,%s,%s,%d,%s,%s,%s,%s,%s,%s",
            escapeCSV(cobolName),
            escapeCSV(cobolType),
            escapeCSV(cobolSection),
            cobolLevel,
            escapeCSV(javaName),
            escapeCSV(javaType),
            escapeCSV(javaClass),
            escapeCSV(conversionComment),
            isRedefines ? "YES" : "NO",
            isOccurs ? occursInfo != null ? occursInfo : "YES" : "NO"
        );
    }

    private String escapeCSV(String value) {
        if (value == null) return "";
        if (value.contains(",") || value.contains("\"") || value.contains("\n")) {
            return "\"" + value.replace("\"", "\"\"") + "\"";
        }
        return value;
    }

    @Override
    public String toString() {
        return String.format("TypeMapping{%s (%s) → %s (%s)}",
            cobolName, cobolType, javaName, javaType);
    }
}
