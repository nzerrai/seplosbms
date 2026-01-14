package com.cobol.translator.redefines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests unitaires pour RedefinesAnalyzer
 */
class RedefinesAnalyzerTest {
    
    private RedefinesAnalyzer analyzer;
    
    @BeforeEach
    void setUp() {
        analyzer = new RedefinesAnalyzer();
    }
    
    @Test
    @DisplayName("Doit détecter une redéfinition simple")
    void testSimpleRedefines() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> result = analyzer.analyze(cobolCode);
        
        assertNotNull(result);
        assertTrue(result.containsKey("WS-FIELD-1"));
        
        RedefinesInfo info = result.get("WS-FIELD-1");
        assertEquals(1, info.getRedefinitionCount());
        assertEquals(10, info.getFieldSize());
    }
    
    @Test
    @DisplayName("Doit détecter les redéfinitions multiples")
    void testMultipleRedefines() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
               05 WS-FIELD-3 REDEFINES WS-FIELD-1.
                  10 WS-SUB-1  PIC X(5).
                  10 WS-SUB-2  PIC X(5).
            """;
        
        Map<String, RedefinesInfo> result = analyzer.analyze(cobolCode);
        
        assertNotNull(result);
        assertTrue(result.containsKey("WS-FIELD-1"));
        
        RedefinesInfo info = result.get("WS-FIELD-1");
        assertEquals(2, info.getRedefinitionCount());
        
        // Vérifier les types de vues
        long numericViews = info.getRedefinitions().stream()
            .filter(v -> v.getViewType() == ViewType.NUMERIC)
            .count();
        assertEquals(1, numericViews);
        
        long structuredViews = info.getRedefinitions().stream()
            .filter(v -> v.getViewType() == ViewType.STRUCTURED)
            .count();
        assertEquals(1, structuredViews);
    }
    
    @Test
    @DisplayName("Doit détecter les sous-champs d'une vue structurée")
    void testStructuredView() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-3 REDEFINES WS-FIELD-1.
                  10 WS-SUB-1  PIC X(5).
                  10 WS-SUB-2  PIC X(5).
            """;
        
        Map<String, RedefinesInfo> result = analyzer.analyze(cobolCode);
        
        RedefinesInfo info = result.get("WS-FIELD-1");
        RedefinesView structuredView = info.getRedefinitions().get(0);
        
        assertTrue(structuredView.isStructured());
        assertEquals(2, structuredView.getSubFields().size());
        
        assertEquals("WS-SUB-1", structuredView.getSubFields().get(0).getName());
        assertEquals("WS-SUB-2", structuredView.getSubFields().get(1).getName());
    }
    
    @Test
    @DisplayName("Doit analyser la compatibilité des types")
    void testCompatibilityAnalysis() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(5).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> result = analyzer.analyze(cobolCode);
        
        RedefinesInfo info = result.get("WS-FIELD-1");
        var incompatibilities = info.analyzeCompatibility();
        
        assertFalse(incompatibilities.isEmpty());
        assertTrue(incompatibilities.get(0).contains("exceeds"));
    }
    
    @Test
    @DisplayName("Doit générer un rapport d'analyse")
    void testGenerateReport() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        analyzer.analyze(cobolCode);
        String report = analyzer.generateReport();
        
        assertNotNull(report);
        assertTrue(report.contains("REDEFINES ANALYSIS REPORT"));
        assertTrue(report.contains("WS-FIELD-1"));
    }
    
    @Test
    @DisplayName("Doit ignorer les commentaires COBOL")
    void testIgnoreComments() {
        String cobolCode = """
            01 WS-DATA.
            *  Ceci est un commentaire
               05 WS-FIELD-1   PIC X(10).
            *  Encore un commentaire
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> result = analyzer.analyze(cobolCode);
        
        assertNotNull(result);
        assertTrue(result.containsKey("WS-FIELD-1"));
    }
    
    @Test
    @DisplayName("Doit gérer le code sans redéfinitions")
    void testNoRedefines() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2   PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> result = analyzer.analyze(cobolCode);
        
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }
    
    @Test
    @DisplayName("Doit calculer correctement la taille des champs")
    void testFieldSizeCalculation() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1.
                  10 WS-SUB-1  PIC X(5).
                  10 WS-SUB-2  PIC X(5).
            """;
        
        Map<String, RedefinesInfo> result = analyzer.analyze(cobolCode);
        
        RedefinesInfo info = result.get("WS-FIELD-1");
        assertEquals(10, info.getFieldSize());
        
        RedefinesView structuredView = info.getRedefinitions().get(0);
        assertEquals(10, structuredView.getSize());
    }
}
