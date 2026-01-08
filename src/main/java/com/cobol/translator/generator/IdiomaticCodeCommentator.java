package com.cobol.translator.generator;

import com.cobol.translator.analyzer.CobolPatternDetector;
import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.FileDefinition;
import com.cobol.translator.model.Paragraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * Génère des commentaires optimisés pour les patterns COBOL idiomatiques.
 * 
 * Au lieu de générer des warnings pour du code COBOL standard,
 * ce générateur produit des commentaires qui expliquent comment
 * les patterns COBOL sont correctement mappés vers Spring Batch.
 */
public class IdiomaticCodeCommentator {
    
    private static final Logger logger = LoggerFactory.getLogger(IdiomaticCodeCommentator.class);
    
    /**
     * Génère un commentaire pour un pattern de traitement de fichier.
     * 
     * @param pattern Le pattern détecté
     * @param fileDefinition Définition du fichier COBOL
     * @return Commentaire explicatif
     */
    public String generateFileProcessingComment(
            CobolPatternDetector.FileProcessingPattern pattern,
            FileDefinition fileDefinition) {
        
        StringBuilder comment = new StringBuilder();
        
        comment.append("/**\n");
        comment.append(" * ✅ COBOL Standard File Processing Pattern Detected\n");
        comment.append(" * Score: ").append(pattern.getScore()).append("/100\n");
        comment.append(" *\n");
        comment.append(" * COBOL Code:\n");
        comment.append(" * -----------\n");
        comment.append(" * OPEN INPUT ").append(fileDefinition.getFileName()).append("\n");
        comment.append(" * PERFORM UNTIL ").append(pattern.getEofVariable()).append(" = 'Y'\n");
        comment.append(" *   READ ").append(fileDefinition.getFileName()).append("\n");
        comment.append(" *     AT END MOVE 'Y' TO ").append(pattern.getEofVariable()).append("\n");
        comment.append(" *     NOT AT END PERFORM PROCESS-RECORD\n");
        comment.append(" *   END-READ\n");
        comment.append(" * END-PERFORM\n");
        comment.append(" * CLOSE ").append(fileDefinition.getFileName()).append("\n");
        
        if (pattern.getCounterVariable() != null) {
            comment.append(" * DISPLAY 'PROCESSED: ' ").append(pattern.getCounterVariable()).append("\n");
        }
        
        comment.append(" *\n");
        comment.append(" * Spring Batch Translation:\n");
        comment.append(" * -------------------------\n");
        comment.append(" * ✅ OPEN INPUT → FlatFileItemReader configuration\n");
        comment.append(" * ✅ PERFORM UNTIL → Step chunk processing (automatic)\n");
        comment.append(" * ✅ READ → reader.read() (managed by Spring Batch)\n");
        comment.append(" * ✅ AT END → null return handled automatically\n");
        comment.append(" * ✅ PROCESS-RECORD → ItemProcessor.process()\n");
        comment.append(" * ✅ CLOSE → Automatic resource cleanup\n");
        
        if (pattern.getCounterVariable() != null) {
            comment.append(" * ✅ DISPLAY counter → StepExecution.getReadCount()\n");
        }
        
        comment.append(" *\n");
        comment.append(" * Benefits:\n");
        comment.append(" * - Automatic error handling and retry\n");
        comment.append(" * - Transaction management\n");
        comment.append(" * - Progress tracking and restart capability\n");
        comment.append(" * - Memory-efficient chunk processing\n");
        comment.append(" */\n");
        
        return comment.toString();
    }
    
    /**
     * Génère un commentaire pour une structure batch.
     */
    public String generateBatchStructureComment(
            CobolPatternDetector.BatchStructurePattern pattern) {
        
        StringBuilder comment = new StringBuilder();
        
        comment.append("/**\n");
        comment.append(" * ✅ COBOL Standard Batch Structure Detected\n");
        comment.append(" * Score: ").append(pattern.getScore()).append("/100\n");
        comment.append(" *\n");
        comment.append(" * COBOL Structure:\n");
        comment.append(" * ---------------\n");
        
        if (pattern.getMainParagraph() != null) {
            comment.append(" * ").append(pattern.getMainParagraph().getName()).append(".\n");
        }
        
        if (pattern.getInitParagraph() != null) {
            comment.append(" *   PERFORM ").append(pattern.getInitParagraph().getName()).append("\n");
        }
        
        if (pattern.getProcessParagraph() != null) {
            comment.append(" *   PERFORM ").append(pattern.getProcessParagraph().getName())
                   .append(" UNTIL condition\n");
        }
        
        if (pattern.getFinalizeParagraph() != null) {
            comment.append(" *   PERFORM ").append(pattern.getFinalizeParagraph().getName()).append("\n");
        }
        
        comment.append(" *   STOP RUN.\n");
        comment.append(" *\n");
        comment.append(" * Spring Batch Translation:\n");
        comment.append(" * -------------------------\n");
        
        if (pattern.getInitParagraph() != null) {
            comment.append(" * ✅ INIT paragraph → Tasklet or @BeforeStep method\n");
        }
        
        if (pattern.getProcessParagraph() != null) {
            comment.append(" * ✅ PROCESS paragraph → Step with Reader-Processor-Writer\n");
        }
        
        if (pattern.getFinalizeParagraph() != null) {
            comment.append(" * ✅ FINALIZE paragraph → @AfterStep method\n");
        }
        
        comment.append(" * ✅ STOP RUN → Natural job completion\n");
        comment.append(" *\n");
        comment.append(" * This is a standard batch job structure - no warnings needed!\n");
        comment.append(" */\n");
        
        return comment.toString();
    }
    
    /**
     * Génère un rapport de score idiomatique.
     */
    public String generateScoreReport(Map<String, Object> patterns) {
        StringBuilder report = new StringBuilder();
        
        report.append("\n");
        report.append("╔═══════════════════════════════════════════════════════════════════╗\n");
        report.append("║         COBOL Code Quality Report                                ║\n");
        report.append("╚═══════════════════════════════════════════════════════════════════╝\n");
        report.append("\n");
        
        if (patterns.containsKey("IDIOMATIC_SCORE")) {
            int score = (Integer) patterns.get("IDIOMATIC_SCORE");
            
            report.append("Overall Idiomatic Score: ").append(score).append("/100\n");
            report.append("\n");
            
            if (score >= 80) {
                report.append("✅ EXCELLENT: This COBOL code follows standard patterns\n");
                report.append("   Translation to Spring Batch will be clean and idiomatic.\n");
            } else if (score >= 60) {
                report.append("✅ GOOD: Most COBOL patterns are standard\n");
                report.append("   Minor optimizations may be suggested.\n");
            } else if (score >= 40) {
                report.append("⚠️  FAIR: Some non-standard patterns detected\n");
                report.append("   Review generated code for optimization opportunities.\n");
            } else {
                report.append("⚠️  NEEDS REVIEW: Several non-standard patterns\n");
                report.append("   Manual code review recommended.\n");
            }
            
            report.append("\n");
        }
        
        report.append("Detected Patterns:\n");
        report.append("─────────────────\n");
        
        if (patterns.containsKey("FILE_PROCESSING")) {
            CobolPatternDetector.FileProcessingPattern fp = 
                (CobolPatternDetector.FileProcessingPattern) patterns.get("FILE_PROCESSING");
            report.append("✅ File Processing Pattern (OPEN-READ-PERFORM-CLOSE)\n");
            report.append("   Score: ").append(fp.getScore()).append("/100\n");
            report.append("   → Mapped to Spring Batch ItemReader\n");
            report.append("\n");
        }
        
        if (patterns.containsKey("BATCH_STRUCTURE")) {
            CobolPatternDetector.BatchStructurePattern bp = 
                (CobolPatternDetector.BatchStructurePattern) patterns.get("BATCH_STRUCTURE");
            report.append("✅ Batch Structure Pattern (INIT-PROCESS-FINALIZE)\n");
            report.append("   Score: ").append(bp.getScore()).append("/100\n");
            report.append("   → Mapped to Spring Batch Job Steps\n");
            report.append("\n");
        }
        
        if (patterns.containsKey("TABLE_SEARCH")) {
            report.append("✅ Table Search Pattern (SEARCH/SEARCH ALL)\n");
            report.append("   → Mapped to Java Stream API or Collections\n");
            report.append("\n");
        }
        
        if (patterns.isEmpty() || patterns.size() == 1) {
            report.append("⚠️  No standard patterns detected\n");
            report.append("   This may indicate custom or legacy code structure\n");
            report.append("\n");
        }
        
        report.append("╔═══════════════════════════════════════════════════════════════════╗\n");
        report.append("║  Translation Strategy                                             ║\n");
        report.append("╚═══════════════════════════════════════════════════════════════════╝\n");
        report.append("\n");
        
        if (patterns.containsKey("FILE_PROCESSING")) {
            report.append("1. File I/O → Spring Batch ItemReader/ItemWriter\n");
            report.append("   - Automatic resource management\n");
            report.append("   - Built-in error handling and retry\n");
            report.append("   - Transaction support\n");
            report.append("\n");
        }
        
        if (patterns.containsKey("BATCH_STRUCTURE")) {
            report.append("2. Batch Structure → Spring Batch Job with Steps\n");
            report.append("   - Initialization as Tasklet\n");
            report.append("   - Processing as Chunk-oriented Step\n");
            report.append("   - Finalization as @AfterStep callback\n");
            report.append("\n");
        }
        
        report.append("3. Business Logic → ItemProcessor implementations\n");
        report.append("   - PERFORM paragraphs → Java methods\n");
        report.append("   - COBOL conditions → Java if/switch\n");
        report.append("   - COMPUTE statements → BigDecimal operations\n");
        report.append("\n");
        
        report.append("For detailed implementation, see generated Spring Batch configuration.\n");
        report.append("\n");
        
        return report.toString();
    }
    
    /**
     * Génère un message pour les développeurs.
     */
    public String generateDeveloperNote(Map<String, Object> patterns) {
        StringBuilder note = new StringBuilder();
        
        note.append("/*\n");
        note.append(" * ═══════════════════════════════════════════════════════════════════\n");
        note.append(" * DEVELOPER NOTE: Code Quality Assessment\n");
        note.append(" * ═══════════════════════════════════════════════════════════════════\n");
        note.append(" *\n");
        
        if (patterns.containsKey("IDIOMATIC_SCORE")) {
            int score = (Integer) patterns.get("IDIOMATIC_SCORE");
            
            if (score >= 80) {
                note.append(" * ✅ This generated code comes from WELL-STRUCTURED COBOL source\n");
                note.append(" *\n");
                note.append(" * The original COBOL program follows standard batch processing patterns.\n");
                note.append(" * The translation to Spring Batch is straightforward and idiomatic.\n");
                note.append(" *\n");
                note.append(" * What you see here:\n");
                note.append(" * - Clean separation of concerns\n");
                note.append(" * - Standard file I/O patterns → Spring Batch readers/writers\n");
                note.append(" * - Proper error handling with AT END clauses\n");
                note.append(" * - Maintainable structure with named paragraphs\n");
                note.append(" *\n");
                note.append(" * This is production-ready code. Minor adjustments may be needed for:\n");
                note.append(" * - Specific business validation rules\n");
                note.append(" * - Integration with existing services\n");
                note.append(" * - Performance tuning (chunk size, thread pool)\n");
            } else {
                note.append(" * ⚠️  This code may require additional review\n");
                note.append(" *\n");
                note.append(" * The COBOL source uses some non-standard patterns.\n");
                note.append(" * Review the generated code for:\n");
                note.append(" * - Manual optimizations\n");
                note.append(" * - Business logic verification\n");
                note.append(" * - Error handling completeness\n");
            }
        }
        
        note.append(" *\n");
        note.append(" * ═══════════════════════════════════════════════════════════════════\n");
        note.append(" */\n");
        
        return note.toString();
    }
}
