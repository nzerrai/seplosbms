package com.cobol.translator.redefines;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

/**
 * Exemple d'utilisation complète du système REDEFINES
 * Démontre l'analyse, la génération et l'optimisation
 */
public class RedefinesExample {
    
    public static void main(String[] args) {
        try {
            // Exemple de code COBOL avec redéfinitions multiples
            String cobolCode = """
                IDENTIFICATION DIVISION.
                PROGRAM-ID. REDEFINES-DEMO.
                
                DATA DIVISION.
                WORKING-STORAGE SECTION.
                01 WS-DATA.
                   05 WS-FIELD-1   PIC X(10).
                   05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
                   05 WS-FIELD-3 REDEFINES WS-FIELD-1.
                      10 WS-SUB-1  PIC X(5).
                      10 WS-SUB-2  PIC X(5).
                
                PROCEDURE DIVISION.
                    MOVE "HELLO" TO WS-FIELD-1.
                    MOVE 1234567890 TO WS-FIELD-2.
                    MOVE "ABCDE" TO WS-SUB-1.
                    STOP RUN.
                """;
            
            System.out.println("═══════════════════════════════════════════════");
            System.out.println("  REDEFINES COMPLETE EXAMPLE");
            System.out.println("═══════════════════════════════════════════════\n");
            
            // ============================================
            // ÉTAPE 1: ANALYSE DES REDEFINES
            // ============================================
            System.out.println("STEP 1: Analyzing REDEFINES clauses...\n");
            
            RedefinesAnalyzer analyzer = new RedefinesAnalyzer();
            Map<String, RedefinesInfo> redefinesMap = analyzer.analyze(cobolCode);
            
            System.out.println(analyzer.generateReport());
            
            // ============================================
            // ÉTAPE 2: GÉNÉRATION DES CLASSES WRAPPER
            // ============================================
            System.out.println("\nSTEP 2: Generating Java wrapper classes...\n");
            
            UnionTypeGenerator generator = new UnionTypeGenerator();
            
            for (Map.Entry<String, RedefinesInfo> entry : redefinesMap.entrySet()) {
                String fieldName = entry.getKey();
                RedefinesInfo info = entry.getValue();
                
                System.out.println("Generating wrapper for: " + fieldName);
                String javaCode = generator.generateWrapperClass(info, "com.example.generated");
                
                // Sauvegarder dans un fichier
                String className = toClassName(fieldName);
                Path outputPath = Paths.get("target/generated-sources", className + ".java");
                Files.createDirectories(outputPath.getParent());
                Files.writeString(outputPath, javaCode);
                
                System.out.println("  ✓ Generated: " + outputPath);
                System.out.println("  ✓ Views: " + info.getRedefinitionCount() + " redefinitions");
                System.out.println();
            }
            
            // ============================================
            // ÉTAPE 3: DÉMONSTRATION DE L'OPTIMISATION
            // ============================================
            System.out.println("\nSTEP 3: Demonstrating optimization...\n");
            
            RedefinesOptimizer optimizer = new RedefinesOptimizer();
            
            // Simuler des accès aux vues
            System.out.println("Simulating view accesses...");
            
            for (int i = 0; i < 100; i++) {
                // Accès fréquent à la vue originale
                optimizer.recordViewAccess("WS-FIELD-1", RedefinesOptimizer.ViewAccessType.READ);
                
                // Accès modéré à la vue numérique
                if (i % 2 == 0) {
                    optimizer.recordViewAccess("WS-FIELD-2", RedefinesOptimizer.ViewAccessType.READ);
                }
                
                // Accès rare à la vue structurée
                if (i % 10 == 0) {
                    optimizer.recordViewAccess("WS-FIELD-3", RedefinesOptimizer.ViewAccessType.READ);
                }
            }
            
            // Simuler des conversions cachées
            for (int i = 0; i < 50; i++) {
                final int index = i;
                String key = "conversion_" + (i % 10);
                optimizer.getCachedConversion(key, String.class, () -> "Value " + index);
            }
            
            // Générer le rapport de performance
            RedefinesOptimizer.PerformanceReport perfReport = optimizer.generatePerformanceReport();
            System.out.println(perfReport);
            
            // ============================================
            // ÉTAPE 4: EXEMPLE D'UTILISATION DU CODE GÉNÉRÉ
            // ============================================
            System.out.println("\nSTEP 4: Usage example (pseudo-code)...\n");
            
            System.out.println("""
                // Créer une instance du wrapper
                WsField1Wrapper wrapper = new WsField1Wrapper();
                
                // Utiliser comme String
                wrapper.setAsOriginal("HELLO");
                System.out.println(wrapper.asOriginal()); // "HELLO"
                
                // Utiliser comme numérique
                wrapper.setAsWsField2(new BigDecimal("1234567890"));
                System.out.println(wrapper.asWsField2()); // 1234567890
                
                // Utiliser comme structure
                WsField3View structured = new WsField3View(wrapper.getRawData());
                structured.setWsSub1("ABCDE");
                structured.setWsSub2("12345");
                wrapper.setAsWsField3(structured);
                
                // Les trois vues partagent les mêmes données en mémoire!
                """);
            
            // ============================================
            // ÉTAPE 5: RECOMMANDATIONS
            // ============================================
            System.out.println("\nSTEP 5: Recommendations...\n");
            
            System.out.println("Based on analysis:");
            
            // Incompatibilités
            for (RedefinesInfo info : redefinesMap.values()) {
                var issues = info.analyzeCompatibility();
                if (!issues.isEmpty()) {
                    System.out.println("\n⚠️  Compatibility warnings for " + info.getOriginalFieldName() + ":");
                    for (String issue : issues) {
                        System.out.println("   - " + issue);
                    }
                }
            }
            
            // Vues inutilisées
            var unusedViews = optimizer.findUnusedViews(5);
            if (!unusedViews.isEmpty()) {
                System.out.println("\n💡 Consider removing unused views:");
                for (String view : unusedViews) {
                    System.out.println("   - " + view);
                }
            }
            
            // Métriques de cache
            if (perfReport.getCacheHitRate() < 50) {
                System.out.println("\n💡 Cache hit rate is low (" + 
                    String.format("%.1f%%", perfReport.getCacheHitRate()) + ")");
                System.out.println("   Consider increasing cache size or TTL");
            } else {
                System.out.println("\n✅ Cache is performing well (" + 
                    String.format("%.1f%%", perfReport.getCacheHitRate()) + " hit rate)");
            }
            
            System.out.println("\n═══════════════════════════════════════════════");
            System.out.println("  Example completed successfully!");
            System.out.println("═══════════════════════════════════════════════");
            
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    private static String toClassName(String cobolName) {
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
