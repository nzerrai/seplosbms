package com.cobol.translator.redefines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests unitaires pour RedefinesOptimizer
 */
class RedefinesOptimizerTest {
    
    private RedefinesOptimizer optimizer;
    
    @BeforeEach
    void setUp() {
        optimizer = new RedefinesOptimizer(100, 1000); // 100 entries, 1 second TTL
    }
    
    @Test
    @DisplayName("Doit cacher une conversion")
    void testCacheConversion() {
        String result = optimizer.getCachedConversion("key1", String.class, () -> "value1");
        
        assertEquals("value1", result);
        
        // Vérifier que la valeur est récupérée du cache
        String cachedResult = optimizer.getCachedConversion("key1", String.class, () -> "value2");
        assertEquals("value1", cachedResult); // Doit retourner la valeur cachée, pas value2
    }
    
    @Test
    @DisplayName("Doit invalider le cache pour une clé spécifique")
    void testInvalidateCache() {
        optimizer.getCachedConversion("key1", String.class, () -> "value1");
        
        optimizer.invalidateCache("key1");
        
        String result = optimizer.getCachedConversion("key1", String.class, () -> "value2");
        assertEquals("value2", result);
    }
    
    @Test
    @DisplayName("Doit invalider tout le cache")
    void testInvalidateAllCache() {
        optimizer.getCachedConversion("key1", String.class, () -> "value1");
        optimizer.getCachedConversion("key2", String.class, () -> "value2");
        
        optimizer.invalidateAllCache();
        
        String result1 = optimizer.getCachedConversion("key1", String.class, () -> "newValue1");
        String result2 = optimizer.getCachedConversion("key2", String.class, () -> "newValue2");
        
        assertEquals("newValue1", result1);
        assertEquals("newValue2", result2);
    }
    
    @Test
    @DisplayName("Doit enregistrer les accès aux vues")
    void testRecordViewAccess() {
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.WRITE);
        
        RedefinesOptimizer.PerformanceReport report = optimizer.generatePerformanceReport();
        
        assertEquals(2, report.getTotalReads());
        assertEquals(1, report.getTotalWrites());
    }
    
    @Test
    @DisplayName("Doit identifier les vues inutilisées")
    void testFindUnusedViews() {
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        
        optimizer.recordViewAccess("view2", RedefinesOptimizer.ViewAccessType.READ);
        optimizer.recordViewAccess("view2", RedefinesOptimizer.ViewAccessType.READ);
        
        optimizer.recordViewAccess("view3", RedefinesOptimizer.ViewAccessType.READ);
        
        List<String> unusedViews = optimizer.findUnusedViews(3); // Threshold = 3
        
        assertTrue(unusedViews.contains("view2")); // 2 accesses < 3
        assertTrue(unusedViews.contains("view3")); // 1 access < 3
        assertFalse(unusedViews.contains("view1")); // 3 accesses >= 3
    }
    
    @Test
    @DisplayName("Doit générer un rapport de performance")
    void testGeneratePerformanceReport() {
        optimizer.getCachedConversion("key1", String.class, () -> "value1");
        optimizer.getCachedConversion("key1", String.class, () -> "value2"); // Hit
        optimizer.getCachedConversion("key2", String.class, () -> "value3");
        
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.WRITE);
        
        RedefinesOptimizer.PerformanceReport report = optimizer.generatePerformanceReport();
        
        assertNotNull(report);
        assertTrue(report.getCacheHitRate() > 0);
        assertEquals(1, report.getTotalReads());
        assertEquals(1, report.getTotalWrites());
    }
    
    @Test
    @DisplayName("Doit calculer le taux de hit du cache")
    void testCacheHitRate() {
        // 1 miss
        optimizer.getCachedConversion("key1", String.class, () -> "value1");
        
        // 2 hits
        optimizer.getCachedConversion("key1", String.class, () -> "value2");
        optimizer.getCachedConversion("key1", String.class, () -> "value3");
        
        RedefinesOptimizer.PerformanceReport report = optimizer.generatePerformanceReport();
        
        // Hit rate = 2/(1+2) = 66.67%
        assertTrue(report.getCacheHitRate() > 60);
        assertTrue(report.getCacheHitRate() < 70);
    }
    
    @Test
    @DisplayName("Doit nettoyer les entrées expirées")
    void testCleanExpiredEntries() throws InterruptedException {
        // Créer un optimizer avec un TTL très court
        RedefinesOptimizer shortTTLOptimizer = new RedefinesOptimizer(100, 100); // 100ms TTL
        
        shortTTLOptimizer.getCachedConversion("key1", String.class, () -> "value1");
        
        // Attendre que l'entrée expire
        Thread.sleep(150);
        
        shortTTLOptimizer.cleanExpiredEntries();
        
        // La valeur devrait être recalculée
        String result = shortTTLOptimizer.getCachedConversion("key1", String.class, () -> "newValue");
        assertEquals("newValue", result);
    }
    
    @Test
    @DisplayName("Doit éviter les débordements de cache")
    void testCacheEviction() {
        RedefinesOptimizer smallCacheOptimizer = new RedefinesOptimizer(3, 10000); // Max 3 entries
        
        smallCacheOptimizer.getCachedConversion("key1", String.class, () -> "value1");
        smallCacheOptimizer.getCachedConversion("key2", String.class, () -> "value2");
        smallCacheOptimizer.getCachedConversion("key3", String.class, () -> "value3");
        
        // Accéder à key1 plusieurs fois pour la rendre "plus utilisée"
        smallCacheOptimizer.getCachedConversion("key1", String.class, () -> "value1");
        smallCacheOptimizer.getCachedConversion("key1", String.class, () -> "value1");
        
        // Ajouter une 4ème clé devrait évincer une des clés moins utilisées
        smallCacheOptimizer.getCachedConversion("key4", String.class, () -> "value4");
        
        RedefinesOptimizer.PerformanceReport report = smallCacheOptimizer.generatePerformanceReport();
        
        // La taille du cache ne devrait pas dépasser 3
        assertTrue(report.getCacheSize() <= 3);
    }
    
    @Test
    @DisplayName("Doit réinitialiser les statistiques")
    void testResetStatistics() {
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.WRITE);
        
        optimizer.resetStatistics();
        
        RedefinesOptimizer.PerformanceReport report = optimizer.generatePerformanceReport();
        
        assertEquals(0, report.getTotalReads());
        assertEquals(0, report.getTotalWrites());
    }
    
    @Test
    @DisplayName("Doit gérer les conversions avec des types différents")
    void testDifferentTypes() {
        String stringResult = optimizer.getCachedConversion("key1", String.class, () -> "text");
        Integer intResult = optimizer.getCachedConversion("key2", Integer.class, () -> 42);
        BigDecimal decimalResult = optimizer.getCachedConversion("key3", BigDecimal.class, 
            () -> new BigDecimal("123.45"));
        
        assertEquals("text", stringResult);
        assertEquals(42, intResult);
        assertEquals(new BigDecimal("123.45"), decimalResult);
    }
    
    @Test
    @DisplayName("Le rapport de performance doit contenir les vues les plus utilisées")
    void testTopUsedViews() {
        // Créer des accès avec différentes fréquences
        for (int i = 0; i < 10; i++) {
            optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        }
        for (int i = 0; i < 5; i++) {
            optimizer.recordViewAccess("view2", RedefinesOptimizer.ViewAccessType.READ);
        }
        for (int i = 0; i < 2; i++) {
            optimizer.recordViewAccess("view3", RedefinesOptimizer.ViewAccessType.READ);
        }
        
        RedefinesOptimizer.PerformanceReport report = optimizer.generatePerformanceReport();
        List<RedefinesOptimizer.ViewUsageStats> topViews = report.getTopUsedViews();
        
        assertNotNull(topViews);
        assertFalse(topViews.isEmpty());
        
        // view1 devrait être en première position
        assertEquals("view1", topViews.get(0).getViewName());
        assertEquals(10, topViews.get(0).getTotalAccesses());
    }
    
    @Test
    @DisplayName("Le rapport doit avoir une représentation textuelle")
    void testPerformanceReportToString() {
        optimizer.getCachedConversion("key1", String.class, () -> "value1");
        optimizer.recordViewAccess("view1", RedefinesOptimizer.ViewAccessType.READ);
        
        RedefinesOptimizer.PerformanceReport report = optimizer.generatePerformanceReport();
        String reportString = report.toString();
        
        assertNotNull(reportString);
        assertTrue(reportString.contains("REDEFINES PERFORMANCE REPORT"));
        assertTrue(reportString.contains("Cache Usage"));
        assertTrue(reportString.contains("Cache Hit Rate"));
    }
}
