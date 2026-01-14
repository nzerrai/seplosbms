package com.cobol.translator.redefines;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Optimiseur pour les accès mémoire des redéfinitions COBOL
 * Implémente le caching et le lazy loading pour améliorer les performances
 */
public class RedefinesOptimizer {
    
    // Cache des conversions coûteuses
    private final Map<String, CachedConversion> conversionCache;
    
    // Statistiques d'utilisation des vues
    private final Map<String, ViewUsageStats> usageStats;
    
    // Configuration
    private final int maxCacheSize;
    private final long cacheTTL; // Time to live en millisecondes
    
    /**
     * Constructeur avec configuration par défaut
     */
    public RedefinesOptimizer() {
        this(1000, 60000); // 1000 entrées, 60 secondes TTL
    }
    
    /**
     * Constructeur avec configuration personnalisée
     */
    public RedefinesOptimizer(int maxCacheSize, long cacheTTL) {
        this.maxCacheSize = maxCacheSize;
        this.cacheTTL = cacheTTL;
        this.conversionCache = new ConcurrentHashMap<>();
        this.usageStats = new ConcurrentHashMap<>();
    }
    
    /**
     * Récupère une conversion depuis le cache ou l'exécute
     */
    public <T> T getCachedConversion(String key, Class<T> targetType, ConversionFunction<T> converter) {
        // Vérifier le cache
        CachedConversion cached = conversionCache.get(key);
        
        if (cached != null && !cached.isExpired()) {
            // Mettre à jour les statistiques
            incrementHitCount(key);
            
            @SuppressWarnings("unchecked")
            T result = (T) cached.getValue();
            return result;
        }
        
        // Conversion non cachée ou expirée
        T result = converter.convert();
        
        // Mettre en cache
        cacheConversion(key, result);
        
        // Mettre à jour les statistiques
        incrementMissCount(key);
        
        return result;
    }
    
    /**
     * Met en cache une conversion
     */
    private void cacheConversion(String key, Object value) {
        // Vérifier la taille du cache
        if (conversionCache.size() >= maxCacheSize) {
            evictLeastUsed();
        }
        
        conversionCache.put(key, new CachedConversion(value, System.currentTimeMillis() + cacheTTL));
    }
    
    /**
     * Éviction du cache : retire l'entrée la moins utilisée
     */
    private void evictLeastUsed() {
        String leastUsedKey = null;
        long minAccessCount = Long.MAX_VALUE;
        
        for (Map.Entry<String, ViewUsageStats> entry : usageStats.entrySet()) {
            long accessCount = entry.getValue().getTotalAccesses();
            if (accessCount < minAccessCount) {
                minAccessCount = accessCount;
                leastUsedKey = entry.getKey();
            }
        }
        
        if (leastUsedKey != null) {
            conversionCache.remove(leastUsedKey);
        }
    }
    
    /**
     * Invalide le cache pour une clé spécifique
     */
    public void invalidateCache(String key) {
        conversionCache.remove(key);
    }
    
    /**
     * Invalide tout le cache
     */
    public void invalidateAllCache() {
        conversionCache.clear();
    }
    
    /**
     * Enregistre l'accès à une vue
     */
    public void recordViewAccess(String viewName, ViewAccessType accessType) {
        usageStats.computeIfAbsent(viewName, k -> new ViewUsageStats(viewName))
                  .recordAccess(accessType);
    }
    
    /**
     * Analyse l'utilisation des vues et identifie celles qui sont inutilisées
     */
    public List<String> findUnusedViews(long threshold) {
        List<String> unusedViews = new ArrayList<>();
        
        for (ViewUsageStats stats : usageStats.values()) {
            if (stats.getTotalAccesses() < threshold) {
                unusedViews.add(stats.getViewName());
            }
        }
        
        return unusedViews;
    }
    
    /**
     * Génère un rapport de performance
     */
    public PerformanceReport generatePerformanceReport() {
        PerformanceReport report = new PerformanceReport();
        
        report.setCacheSize(conversionCache.size());
        report.setMaxCacheSize(maxCacheSize);
        
        long totalHits = 0;
        long totalMisses = 0;
        long totalReads = 0;
        long totalWrites = 0;
        
        for (ViewUsageStats stats : usageStats.values()) {
            totalHits += stats.getCacheHits();
            totalMisses += stats.getCacheMisses();
            totalReads += stats.getReadCount();
            totalWrites += stats.getWriteCount();
        }
        
        report.setTotalCacheHits(totalHits);
        report.setTotalCacheMisses(totalMisses);
        report.setTotalReads(totalReads);
        report.setTotalWrites(totalWrites);
        
        // Calculer le taux de hit du cache
        if (totalHits + totalMisses > 0) {
            double hitRate = (double) totalHits / (totalHits + totalMisses) * 100;
            report.setCacheHitRate(hitRate);
        }
        
        // Identifier les vues les plus utilisées
        List<ViewUsageStats> sortedStats = new ArrayList<>(usageStats.values());
        sortedStats.sort((a, b) -> Long.compare(b.getTotalAccesses(), a.getTotalAccesses()));
        
        report.setTopUsedViews(sortedStats.subList(0, Math.min(10, sortedStats.size())));
        
        // Identifier les vues inutilisées
        report.setUnusedViews(findUnusedViews(5));
        
        return report;
    }
    
    /**
     * Nettoie les entrées expirées du cache
     */
    public void cleanExpiredEntries() {
        List<String> expiredKeys = new ArrayList<>();
        
        for (Map.Entry<String, CachedConversion> entry : conversionCache.entrySet()) {
            if (entry.getValue().isExpired()) {
                expiredKeys.add(entry.getKey());
            }
        }
        
        for (String key : expiredKeys) {
            conversionCache.remove(key);
        }
    }
    
    /**
     * Réinitialise toutes les statistiques
     */
    public void resetStatistics() {
        usageStats.clear();
    }
    
    // Méthodes privées pour gérer les statistiques
    
    private void incrementHitCount(String key) {
        usageStats.computeIfAbsent(key, k -> new ViewUsageStats(key))
                  .incrementCacheHits();
    }
    
    private void incrementMissCount(String key) {
        usageStats.computeIfAbsent(key, k -> new ViewUsageStats(key))
                  .incrementCacheMisses();
    }
    
    // Classes internes
    
    /**
     * Interface fonctionnelle pour les conversions
     */
    @FunctionalInterface
    public interface ConversionFunction<T> {
        T convert();
    }
    
    /**
     * Représente une conversion en cache
     */
    private static class CachedConversion {
        private final Object value;
        private final long expirationTime;
        
        CachedConversion(Object value, long expirationTime) {
            this.value = value;
            this.expirationTime = expirationTime;
        }
        
        Object getValue() {
            return value;
        }
        
        boolean isExpired() {
            return System.currentTimeMillis() > expirationTime;
        }
    }
    
    /**
     * Statistiques d'utilisation d'une vue
     */
    public static class ViewUsageStats {
        private final String viewName;
        private long readCount;
        private long writeCount;
        private long cacheHits;
        private long cacheMisses;
        
        ViewUsageStats(String viewName) {
            this.viewName = viewName;
        }
        
        void recordAccess(ViewAccessType accessType) {
            switch (accessType) {
                case READ -> readCount++;
                case WRITE -> writeCount++;
            }
        }
        
        void incrementCacheHits() {
            cacheHits++;
        }
        
        void incrementCacheMisses() {
            cacheMisses++;
        }
        
        public String getViewName() {
            return viewName;
        }
        
        public long getReadCount() {
            return readCount;
        }
        
        public long getWriteCount() {
            return writeCount;
        }
        
        public long getCacheHits() {
            return cacheHits;
        }
        
        public long getCacheMisses() {
            return cacheMisses;
        }
        
        public long getTotalAccesses() {
            return readCount + writeCount;
        }
        
        public double getCacheHitRate() {
            long total = cacheHits + cacheMisses;
            return total > 0 ? (double) cacheHits / total * 100 : 0;
        }
        
        @Override
        public String toString() {
            return String.format("%s: reads=%d, writes=%d, hits=%d, misses=%d, hit_rate=%.2f%%",
                viewName, readCount, writeCount, cacheHits, cacheMisses, getCacheHitRate());
        }
    }
    
    /**
     * Type d'accès à une vue
     */
    public enum ViewAccessType {
        READ,
        WRITE
    }
    
    /**
     * Rapport de performance
     */
    public static class PerformanceReport {
        private int cacheSize;
        private int maxCacheSize;
        private long totalCacheHits;
        private long totalCacheMisses;
        private double cacheHitRate;
        private long totalReads;
        private long totalWrites;
        private List<ViewUsageStats> topUsedViews;
        private List<String> unusedViews;
        
        // Getters et setters
        
        public int getCacheSize() {
            return cacheSize;
        }
        
        public void setCacheSize(int cacheSize) {
            this.cacheSize = cacheSize;
        }
        
        public int getMaxCacheSize() {
            return maxCacheSize;
        }
        
        public void setMaxCacheSize(int maxCacheSize) {
            this.maxCacheSize = maxCacheSize;
        }
        
        public long getTotalCacheHits() {
            return totalCacheHits;
        }
        
        public void setTotalCacheHits(long totalCacheHits) {
            this.totalCacheHits = totalCacheHits;
        }
        
        public long getTotalCacheMisses() {
            return totalCacheMisses;
        }
        
        public void setTotalCacheMisses(long totalCacheMisses) {
            this.totalCacheMisses = totalCacheMisses;
        }
        
        public double getCacheHitRate() {
            return cacheHitRate;
        }
        
        public void setCacheHitRate(double cacheHitRate) {
            this.cacheHitRate = cacheHitRate;
        }
        
        public long getTotalReads() {
            return totalReads;
        }
        
        public void setTotalReads(long totalReads) {
            this.totalReads = totalReads;
        }
        
        public long getTotalWrites() {
            return totalWrites;
        }
        
        public void setTotalWrites(long totalWrites) {
            this.totalWrites = totalWrites;
        }
        
        public List<ViewUsageStats> getTopUsedViews() {
            return topUsedViews;
        }
        
        public void setTopUsedViews(List<ViewUsageStats> topUsedViews) {
            this.topUsedViews = topUsedViews;
        }
        
        public List<String> getUnusedViews() {
            return unusedViews;
        }
        
        public void setUnusedViews(List<String> unusedViews) {
            this.unusedViews = unusedViews;
        }
        
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("═══════════════════════════════════════════════\n");
            sb.append("  REDEFINES PERFORMANCE REPORT\n");
            sb.append("═══════════════════════════════════════════════\n\n");
            
            sb.append(String.format("Cache Usage: %d / %d (%.1f%%)\n",
                cacheSize, maxCacheSize, (double) cacheSize / maxCacheSize * 100));
            sb.append(String.format("Cache Hit Rate: %.2f%% (%d hits / %d total)\n",
                cacheHitRate, totalCacheHits, totalCacheHits + totalCacheMisses));
            sb.append(String.format("Total Accesses: %d reads, %d writes\n\n",
                totalReads, totalWrites));
            
            if (topUsedViews != null && !topUsedViews.isEmpty()) {
                sb.append("Top Used Views:\n");
                for (int i = 0; i < Math.min(5, topUsedViews.size()); i++) {
                    sb.append("  ").append(i + 1).append(". ").append(topUsedViews.get(i)).append("\n");
                }
                sb.append("\n");
            }
            
            if (unusedViews != null && !unusedViews.isEmpty()) {
                sb.append("Unused Views (< 5 accesses):\n");
                for (String view : unusedViews) {
                    sb.append("  - ").append(view).append("\n");
                }
            }
            
            return sb.toString();
        }
    }
}
