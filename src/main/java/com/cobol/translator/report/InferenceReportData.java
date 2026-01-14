package com.cobol.translator.report;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Rapport complet d'inférence algorithmique des champs.
 * Contient les statistiques globales, le détail par champ, la distribution des types,
 * les contextes d'utilisation, et les recommandations générées.
 *
 * @since 1.0
 */
public class InferenceReportData {

    /** Nombre total de champs inférés */
    @JsonProperty("totalFieldsInferred")
    private Integer totalFieldsInferred;

    /** Nombre de champs au total (layout + inférés) */
    @JsonProperty("totalFieldsIncludingLayout")
    private Integer totalFieldsIncludingLayout;

    /** Distribution des types Java inférés (String: 5, BigDecimal: 3, Long: 2) */
    @JsonProperty("typeDistribution")
    private Map<String, Integer> typeDistribution;

    /** Statistiques par contexte d'utilisation */
    @JsonProperty("contextStats")
    private Map<String, Integer> contextStats;

    /** Score global de qualité de 0 à 100 */
    @JsonProperty("overallQualityScore")
    private Integer overallQualityScore;

    /** Niveau de qualité (EXCELLENT, GOOD, FAIR, POOR) */
    @JsonProperty("qualityLevel")
    private QualityLevel qualityLevel;

    /** Liste des recommandations pour l'utilisateur */
    @JsonProperty("recommendations")
    private List<String> recommendations;

    /** Map des champs inférés avec détails */
    @JsonProperty("inferredFieldsMap")
    private Map<String, InferredField> inferredFieldsMap;

    /** Métrique de conversion: pourcentage de champs automatiquement typés */
    @JsonProperty("conversionMetrics")
    private ConversionMetrics conversionMetrics;

    /** Breakdown: combien de champs du layout vs inférés */
    @JsonProperty("layerBreakdown")
    private LayerBreakdown layerBreakdown;

    /** Timestamp de génération du rapport */
    @JsonProperty("generatedAt")
    private LocalDateTime generatedAt;

    /**
     * Énumération des niveaux de qualité
     */
    public enum QualityLevel {
        EXCELLENT("⭐⭐⭐⭐⭐", 80),
        GOOD("⭐⭐⭐⭐", 60),
        FAIR("⭐⭐⭐", 40),
        POOR("⭐⭐", 0);

        private final String display;
        private final Integer threshold;

        QualityLevel(String display, Integer threshold) {
            this.display = display;
            this.threshold = threshold;
        }

        public String getDisplay() {
            return display;
        }

        public static QualityLevel fromScore(Integer score) {
            if (score >= 80) return EXCELLENT;
            if (score >= 60) return GOOD;
            if (score >= 40) return FAIR;
            return POOR;
        }
    }

    /**
     * Métrique de conversion
     */
    public static class ConversionMetrics {
        @JsonProperty("automatedTypeCount")
        public Integer automatedTypeCount;

        @JsonProperty("manualTypeCount")
        public Integer manualTypeCount;

        @JsonProperty("automationPercentage")
        public Double automationPercentage;

        @JsonProperty("averageConfidence")
        public Double averageConfidence;

        public ConversionMetrics(Integer automatedTypeCount, Integer manualTypeCount) {
            this.automatedTypeCount = automatedTypeCount;
            this.manualTypeCount = manualTypeCount;
            this.automationPercentage = manualTypeCount == 0 ? 100.0 :
                    (automatedTypeCount.doubleValue() / (automatedTypeCount + manualTypeCount)) * 100;
            this.averageConfidence = 0.0;
        }

        public ConversionMetrics() {}
    }

    /**
     * Breakdown par layer (layout vs inférence)
     */
    public static class LayerBreakdown {
        @JsonProperty("fromLayout")
        public Integer fromLayout;

        @JsonProperty("fromInference")
        public Integer fromInference;

        @JsonProperty("deduplicatedCount")
        public Integer deduplicatedCount;

        public LayerBreakdown(Integer fromLayout, Integer fromInference, Integer deduplicatedCount) {
            this.fromLayout = fromLayout;
            this.fromInference = fromInference;
            this.deduplicatedCount = deduplicatedCount;
        }

        public LayerBreakdown() {}
    }

    // ==================== Constructeurs ====================

    public InferenceReportData() {
        this.typeDistribution = new LinkedHashMap<>();
        this.contextStats = new LinkedHashMap<>();
        this.recommendations = new ArrayList<>();
        this.inferredFieldsMap = new LinkedHashMap<>();
        this.conversionMetrics = new ConversionMetrics();
        this.layerBreakdown = new LayerBreakdown();
        this.generatedAt = LocalDateTime.now();
        this.totalFieldsInferred = 0;
        this.totalFieldsIncludingLayout = 0;
        this.overallQualityScore = 0;
    }

    // ==================== Getters & Setters ====================

    public Integer getTotalFieldsInferred() {
        return totalFieldsInferred;
    }

    public void setTotalFieldsInferred(Integer totalFieldsInferred) {
        this.totalFieldsInferred = totalFieldsInferred;
    }

    public Integer getTotalFieldsIncludingLayout() {
        return totalFieldsIncludingLayout;
    }

    public void setTotalFieldsIncludingLayout(Integer totalFieldsIncludingLayout) {
        this.totalFieldsIncludingLayout = totalFieldsIncludingLayout;
    }

    public Map<String, Integer> getTypeDistribution() {
        return typeDistribution;
    }

    public void setTypeDistribution(Map<String, Integer> typeDistribution) {
        this.typeDistribution = typeDistribution;
    }

    public Map<String, Integer> getContextStats() {
        return contextStats;
    }

    public void setContextStats(Map<String, Integer> contextStats) {
        this.contextStats = contextStats;
    }

    public Integer getOverallQualityScore() {
        return overallQualityScore;
    }

    public void setOverallQualityScore(Integer overallQualityScore) {
        this.overallQualityScore = overallQualityScore;
        this.qualityLevel = QualityLevel.fromScore(overallQualityScore);
    }

    public QualityLevel getQualityLevel() {
        return qualityLevel;
    }

    public void setQualityLevel(QualityLevel qualityLevel) {
        this.qualityLevel = qualityLevel;
    }

    public List<String> getRecommendations() {
        return recommendations;
    }

    public void setRecommendations(List<String> recommendations) {
        this.recommendations = recommendations;
    }

    public Map<String, InferredField> getInferredFieldsMap() {
        return inferredFieldsMap;
    }

    public void setInferredFieldsMap(Map<String, InferredField> inferredFieldsMap) {
        this.inferredFieldsMap = inferredFieldsMap;
    }

    public ConversionMetrics getConversionMetrics() {
        return conversionMetrics;
    }

    public void setConversionMetrics(ConversionMetrics conversionMetrics) {
        this.conversionMetrics = conversionMetrics;
    }

    public LayerBreakdown getLayerBreakdown() {
        return layerBreakdown;
    }

    public void setLayerBreakdown(LayerBreakdown layerBreakdown) {
        this.layerBreakdown = layerBreakdown;
    }

    public LocalDateTime getGeneratedAt() {
        return generatedAt;
    }

    public void setGeneratedAt(LocalDateTime generatedAt) {
        this.generatedAt = generatedAt;
    }

    // ==================== Helper Methods ====================

    /**
     * Ajoute un champ inféré au rapport
     */
    public void addInferredField(InferredField field) {
        if (inferredFieldsMap == null) {
            inferredFieldsMap = new LinkedHashMap<>();
        }
        inferredFieldsMap.put(field.getFieldName(), field);

        // Met à jour la distribution des types
        String type = field.getJavaType();
        typeDistribution.put(type, typeDistribution.getOrDefault(type, 0) + 1);

        // Met à jour les statistiques des contextes
        if (field.getUsageContexts() != null) {
            for (String context : field.getUsageContexts()) {
                contextStats.put(context, contextStats.getOrDefault(context, 0) + 1);
            }
        }
    }

    /**
     * Ajoute une recommandation
     */
    public void addRecommendation(String recommendation) {
        if (recommendations == null) {
            recommendations = new ArrayList<>();
        }
        if (!recommendations.contains(recommendation)) {
            recommendations.add(recommendation);
        }
    }

    /**
     * Recalcule le score global de qualité basé sur les champs et confiance
     */
    public void recalculateQualityScore() {
        if (inferredFieldsMap == null || inferredFieldsMap.isEmpty()) {
            this.overallQualityScore = 50;
        } else {
            double avgConfidence = inferredFieldsMap.values().stream()
                    .mapToDouble(InferredField::getConfidenceScore)
                    .average()
                    .orElse(0.5);
            this.overallQualityScore = (int) (avgConfidence * 100);

            if (conversionMetrics != null) {
                conversionMetrics.averageConfidence = avgConfidence;
            }
        }
        this.qualityLevel = QualityLevel.fromScore(this.overallQualityScore);
    }

    // ==================== equals, hashCode, toString ====================

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        InferenceReportData that = (InferenceReportData) o;
        return Objects.equals(totalFieldsInferred, that.totalFieldsInferred) &&
               Objects.equals(overallQualityScore, that.overallQualityScore);
    }

    @Override
    public int hashCode() {
        return Objects.hash(totalFieldsInferred, overallQualityScore);
    }

    @Override
    public String toString() {
        return "InferenceReportData{" +
                "totalFieldsInferred=" + totalFieldsInferred +
                ", overallQualityScore=" + overallQualityScore +
                ", qualityLevel=" + qualityLevel +
                ", typeDistribution=" + typeDistribution +
                '}';
    }
}
