package com.cobol.translator.report;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.*;

/**
 * Représente un champ COBOL qui a été automatiquement inféré et typé.
 * Chaque champ contient les métadonnées d'inférence: contextes d'utilisation,
 * confiance, reasoning, et suggestions d'annotations Java.
 *
 * @since 1.0
 */
public class InferredField {

    /** Nom du champ en Java (camelCase) */
    @JsonProperty("fieldName")
    private String fieldName;

    /** Type Java inféré (String, Long, BigDecimal, LocalDate, etc.) */
    @JsonProperty("javaType")
    private String javaType;

    /** Score de confiance de 0.0 à 1.0 */
    @JsonProperty("confidenceScore")
    private Double confidenceScore;

    /** Niveau de confiance (VERY_HIGH, HIGH, MEDIUM, LOW) */
    @JsonProperty("confidenceLevel")
    private ConfidenceLevel confidenceLevel;

    /** Icône correspondante au niveau de confiance */
    @JsonProperty("confidenceIcon")
    private String confidenceIcon;

    /** Liste des contextes d'utilisation détectés (ARITHMETIC, STRING_OPS, DATE_OPS, etc.) */
    @JsonProperty("usageContexts")
    private List<String> usageContexts;

    /** Explication du reasoning derrière l'inférence */
    @JsonProperty("reasoning")
    private String reasoning;

    /** Vrai si le champ venait déjà du layout COBOL, faux si inféré */
    @JsonProperty("isFromLayout")
    private Boolean isFromLayout;

    /** Annotations Java suggérées (@Digits, @Pattern, etc.) */
    @JsonProperty("suggestedAnnotations")
    private List<String> suggestedAnnotations;

    /** Nombre de fois où ce champ a été référencé */
    @JsonProperty("referenceCount")
    private Integer referenceCount;

    /**
     * Énumération des niveaux de confiance
     */
    public enum ConfidenceLevel {
        VERY_HIGH("🟢", 0.9),
        HIGH("🟢", 0.7),
        MEDIUM("🟡", 0.5),
        LOW("🔴", 0.3);

        private final String icon;
        private final Double threshold;

        ConfidenceLevel(String icon, Double threshold) {
            this.icon = icon;
            this.threshold = threshold;
        }

        public String getIcon() {
            return icon;
        }

        public Double getThreshold() {
            return threshold;
        }

        /**
         * Détermine le niveau approprié basé sur le score
         */
        public static ConfidenceLevel fromScore(Double score) {
            if (score >= 0.9) return VERY_HIGH;
            if (score >= 0.7) return HIGH;
            if (score >= 0.5) return MEDIUM;
            return LOW;
        }
    }

    // ==================== Constructeurs ====================

    public InferredField() {
    }

    public InferredField(String fieldName, String javaType, Double confidenceScore) {
        this.fieldName = fieldName;
        this.javaType = javaType;
        this.confidenceScore = confidenceScore;
        this.confidenceLevel = ConfidenceLevel.fromScore(confidenceScore);
        this.confidenceIcon = this.confidenceLevel.getIcon();
        this.usageContexts = new ArrayList<>();
        this.suggestedAnnotations = new ArrayList<>();
        this.isFromLayout = false;
        this.referenceCount = 0;
    }

    // ==================== Getters & Setters ====================

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getJavaType() {
        return javaType;
    }

    public void setJavaType(String javaType) {
        this.javaType = javaType;
    }

    public Double getConfidenceScore() {
        return confidenceScore;
    }

    public void setConfidenceScore(Double confidenceScore) {
        this.confidenceScore = confidenceScore;
        if (confidenceScore != null) {
            this.confidenceLevel = ConfidenceLevel.fromScore(confidenceScore);
            this.confidenceIcon = this.confidenceLevel.getIcon();
        }
    }

    public ConfidenceLevel getConfidenceLevel() {
        return confidenceLevel;
    }

    public void setConfidenceLevel(ConfidenceLevel confidenceLevel) {
        this.confidenceLevel = confidenceLevel;
    }

    public String getConfidenceIcon() {
        return confidenceIcon;
    }

    public void setConfidenceIcon(String confidenceIcon) {
        this.confidenceIcon = confidenceIcon;
    }

    public List<String> getUsageContexts() {
        return usageContexts;
    }

    public void setUsageContexts(List<String> usageContexts) {
        this.usageContexts = usageContexts;
    }

    public String getReasoning() {
        return reasoning;
    }

    public void setReasoning(String reasoning) {
        this.reasoning = reasoning;
    }

    public Boolean getIsFromLayout() {
        return isFromLayout;
    }

    public void setIsFromLayout(Boolean isFromLayout) {
        this.isFromLayout = isFromLayout;
    }

    public List<String> getSuggestedAnnotations() {
        return suggestedAnnotations;
    }

    public void setSuggestedAnnotations(List<String> suggestedAnnotations) {
        this.suggestedAnnotations = suggestedAnnotations;
    }

    public Integer getReferenceCount() {
        return referenceCount;
    }

    public void setReferenceCount(Integer referenceCount) {
        this.referenceCount = referenceCount;
    }

    // ==================== Helper Methods ====================

    /**
     * Ajoute un contexte d'utilisation s'il n'existe pas déjà
     */
    public void addUsageContext(String context) {
        if (usageContexts == null) {
            usageContexts = new ArrayList<>();
        }
        if (!usageContexts.contains(context)) {
            usageContexts.add(context);
        }
    }

    /**
     * Ajoute une annotation suggérée s'il n'existe pas déjà
     */
    public void addSuggestedAnnotation(String annotation) {
        if (suggestedAnnotations == null) {
            suggestedAnnotations = new ArrayList<>();
        }
        if (!suggestedAnnotations.contains(annotation)) {
            suggestedAnnotations.add(annotation);
        }
    }

    // ==================== equals, hashCode, toString ====================

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        InferredField that = (InferredField) o;
        return Objects.equals(fieldName, that.fieldName) &&
               Objects.equals(javaType, that.javaType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fieldName, javaType);
    }

    @Override
    public String toString() {
        return "InferredField{" +
                "fieldName='" + fieldName + '\'' +
                ", javaType='" + javaType + '\'' +
                ", confidenceScore=" + confidenceScore +
                ", confidenceIcon='" + confidenceIcon + '\'' +
                ", usageContexts=" + usageContexts +
                ", referenceCount=" + referenceCount +
                '}';
    }
}
