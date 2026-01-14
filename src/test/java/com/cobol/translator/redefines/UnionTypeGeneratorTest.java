package com.cobol.translator.redefines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests unitaires pour UnionTypeGenerator
 */
class UnionTypeGeneratorTest {
    
    private UnionTypeGenerator generator;
    private RedefinesAnalyzer analyzer;
    
    @BeforeEach
    void setUp() {
        generator = new UnionTypeGenerator();
        analyzer = new RedefinesAnalyzer();
    }
    
    @Test
    @DisplayName("Doit générer une classe wrapper simple")
    void testGenerateSimpleWrapper() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        assertNotNull(javaCode);
        assertTrue(javaCode.contains("package com.example.generated"));
        assertTrue(javaCode.contains("class WsField1Wrapper"));
        assertTrue(javaCode.contains("private byte[] rawData"));
    }
    
    @Test
    @DisplayName("Doit générer les getters pour toutes les vues")
    void testGenerateViewGetters() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        // Vérifier la présence des getters
        assertTrue(javaCode.contains("public String asOriginal()"));
        assertTrue(javaCode.contains("public BigDecimal asWsField2()"));
    }
    
    @Test
    @DisplayName("Doit générer les setters pour toutes les vues")
    void testGenerateViewSetters() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        // Vérifier la présence des setters
        assertTrue(javaCode.contains("public void setAsOriginal(String value)"));
        assertTrue(javaCode.contains("public void setAsWsField2(BigDecimal value)"));
    }
    
    @Test
    @DisplayName("Doit générer une classe pour une vue structurée")
    void testGenerateStructuredViewClass() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-3 REDEFINES WS-FIELD-1.
                  10 WS-SUB-1  PIC X(5).
                  10 WS-SUB-2  PIC X(5).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        // Vérifier la génération de la classe pour la vue structurée
        assertTrue(javaCode.contains("class WsField3View"));
        assertTrue(javaCode.contains("public byte[] toByteArray()"));
        assertTrue(javaCode.contains("private String wsSub1"));
        assertTrue(javaCode.contains("private String wsSub2"));
    }
    
    @Test
    @DisplayName("Doit générer la documentation JavaDoc")
    void testGenerateJavaDoc() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        assertTrue(javaCode.contains("/**"));
        assertTrue(javaCode.contains("* Wrapper for COBOL REDEFINES"));
        assertTrue(javaCode.contains("* Available views:"));
    }
    
    @Test
    @DisplayName("Doit générer les imports nécessaires")
    void testGenerateImports() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        assertTrue(javaCode.contains("import java.math.BigDecimal"));
        assertTrue(javaCode.contains("import java.nio.charset.StandardCharsets"));
        assertTrue(javaCode.contains("import java.util.Arrays"));
    }
    
    @Test
    @DisplayName("Doit générer des constructeurs appropriés")
    void testGenerateConstructors() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        assertTrue(javaCode.contains("public WsField1Wrapper()"));
        assertTrue(javaCode.contains("public WsField1Wrapper(byte[] initialData)"));
    }
    
    @Test
    @DisplayName("Doit générer les méthodes getRawData et setRawData")
    void testGenerateRawDataMethods() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        assertTrue(javaCode.contains("public byte[] getRawData()"));
        assertTrue(javaCode.contains("public void setRawData(byte[] data)"));
    }
    
    @Test
    @DisplayName("Doit générer une méthode toString")
    void testGenerateToString() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-1   PIC X(10).
               05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-1");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        assertTrue(javaCode.contains("@Override"));
        assertTrue(javaCode.contains("public String toString()"));
    }
    
    @Test
    @DisplayName("Doit gérer correctement les noms de champs avec tirets")
    void testHandleHyphenatedNames() {
        String cobolCode = """
            01 WS-DATA.
               05 WS-FIELD-ONE   PIC X(10).
               05 WS-FIELD-TWO REDEFINES WS-FIELD-ONE PIC 9(10).
            """;
        
        Map<String, RedefinesInfo> analysis = analyzer.analyze(cobolCode);
        RedefinesInfo info = analysis.get("WS-FIELD-ONE");
        
        String javaCode = generator.generateWrapperClass(info, "com.example.generated");
        
        assertTrue(javaCode.contains("class WsFieldOneWrapper"));
        assertTrue(javaCode.contains("asWsFieldTwo"));
    }
}
