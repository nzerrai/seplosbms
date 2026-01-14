package com.cobol.translator.diagram;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.Statement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Génère des diagrammes algorithmiques Mermaid.js pour les programmes COBOL traduits
 * Les diagrammes sont générés automatiquement et livrés avec le projet Spring Batch
 */
public class AlgorithmDiagramGenerator {

    private static final Logger logger = LoggerFactory.getLogger(AlgorithmDiagramGenerator.class);

    /**
     * Génère tous les diagrammes pour un programme COBOL
     * @param program Programme COBOL analysé
     * @param outputDir Répertoire de sortie (docs/)
     * @return Liste des fichiers générés
     */
    public List<Path> generateDiagrams(CobolProgram program, Path outputDir) throws IOException {
        List<Path> generatedFiles = new ArrayList<>();

        logger.info("Generating algorithm diagrams for program: {}", program.getProgramName());

        // 1. Diagramme de flux principal (flowchart)
        Path flowchartFile = outputDir.resolve("algorithm-flowchart.md");
        generateFlowchart(program, flowchartFile);
        generatedFiles.add(flowchartFile);

        // 2. Diagramme de flux de données
        Path dataFlowFile = outputDir.resolve("data-flow-diagram.md");
        generateDataFlowDiagram(program, dataFlowFile);
        generatedFiles.add(dataFlowFile);

        // 3. Diagramme de séquence Spring Batch
        Path sequenceFile = outputDir.resolve("spring-batch-sequence.md");
        generateSpringBatchSequence(program, sequenceFile);
        generatedFiles.add(sequenceFile);

        // 4. Page HTML de visualisation
        Path htmlFile = outputDir.resolve("diagrams.html");
        generateVisualizationPage(program, htmlFile);
        generatedFiles.add(htmlFile);

        logger.info("Generated {} diagram files in {}", generatedFiles.size(), outputDir);

        return generatedFiles;
    }

    /**
     * Génère le flowchart algorithmique principal
     */
    private void generateFlowchart(CobolProgram program, Path outputFile) throws IOException {
        StringBuilder mermaid = new StringBuilder();

        mermaid.append("# Flowchart Algorithmique - ").append(program.getProgramName()).append("\n\n");
        mermaid.append("```mermaid\n");
        mermaid.append("flowchart TD\n");
        mermaid.append("    Start([\"🚀 START<br/>").append(program.getProgramName()).append("\"])\n");
        mermaid.append("    Start --> Init\n\n");

        // Identifier les paragraphes principaux
        Map<String, List<Statement>> paragraphs = groupStatementsByParagraph(program);

        int nodeId = 1;
        String previousNode = "Init";

        for (Map.Entry<String, List<Statement>> entry : paragraphs.entrySet()) {
            String paragraphName = entry.getKey();
            List<Statement> statements = entry.getValue();

            if (paragraphName.equals("MAIN") || paragraphName.contains("INITIALIZE")) {
                // Nœud d'initialisation
                mermaid.append("    Init[\"📋 INITIALIZATION<br/>").append(paragraphName).append("\"]");
                mermaid.append("\n");
                mermaid.append("    Init --> Process").append(nodeId).append("\n\n");
                previousNode = "Process" + nodeId;
                nodeId++;
                continue;
            }

            if (paragraphName.contains("PROCESS") || paragraphName.contains("MAIN-LOGIC")) {
                // Boucle principale de traitement
                String loopNode = "Loop" + nodeId;
                mermaid.append("    ").append(previousNode).append(" --> ").append(loopNode).append("\n");
                mermaid.append("    ").append(loopNode).append("{\"🔄 PROCESS LOOP<br/>");
                mermaid.append(paragraphName).append("\"}\n");

                // Analyser les statements pour détecter les conditions
                for (Statement stmt : statements) {
                    if (stmt.getType() != null && stmt.getType().toString().equals("IF")) {
                        nodeId++;
                        String ifNode = "If" + nodeId;
                        mermaid.append("    ").append(loopNode).append(" --> ").append(ifNode).append("\n");
                        mermaid.append("    ").append(ifNode).append("{\"❓ ").append(cleanCondition(stmt.getCondition())).append("\"}\n");
                        mermaid.append("    ").append(ifNode).append(" -->|Yes| Action").append(nodeId).append("\n");
                        mermaid.append("    ").append(ifNode).append(" -->|No| ").append(loopNode).append("\n");
                        mermaid.append("    Action").append(nodeId).append("[\"⚙️ Process Valid Transaction\"]\n");
                        mermaid.append("    Action").append(nodeId).append(" --> ").append(loopNode).append("\n");
                    }
                }

                mermaid.append("    ").append(loopNode).append(" -->|EOF| Finalize\n\n");
                previousNode = "Finalize";
                nodeId++;
                continue;
            }

            if (paragraphName.contains("FINALIZE") || paragraphName.contains("TERMINATE")) {
                mermaid.append("    Finalize[\"🏁 FINALIZATION<br/>").append(paragraphName).append("\"]\n");
                mermaid.append("    Finalize --> End\n\n");
            }
        }

        mermaid.append("    End([\"✅ END\"])\n\n");

        // Styling
        mermaid.append("    style Start fill:#e1f5e1,stroke:#4caf50,stroke-width:3px\n");
        mermaid.append("    style Init fill:#e3f2fd,stroke:#2196f3,stroke-width:2px\n");
        mermaid.append("    style End fill:#e1f5e1,stroke:#4caf50,stroke-width:3px\n");
        mermaid.append("    style Finalize fill:#fff3e0,stroke:#ff9800,stroke-width:2px\n");

        mermaid.append("```\n");

        writeFile(outputFile, mermaid.toString());
    }

    /**
     * Génère le diagramme de flux de données
     */
    private void generateDataFlowDiagram(CobolProgram program, Path outputFile) throws IOException {
        StringBuilder mermaid = new StringBuilder();

        mermaid.append("# Diagramme de Flux de Données - ").append(program.getProgramName()).append("\n\n");
        mermaid.append("```mermaid\n");
        mermaid.append("flowchart LR\n");

        // Fichiers d'entrée
        mermaid.append("    Input[(\"📂 INPUT<br/>").append(getInputFileName(program)).append("\")]\n");

        // ItemReader
        mermaid.append("    Reader[\"📖 ItemReader<br/>FlatFileItemReader\"]\n");
        mermaid.append("    Input --> Reader\n\n");

        // Entity
        String entityName = toJavaClassName(program.getProgramName()) + "Record";
        mermaid.append("    Entity[\"📦 Entity<br/>").append(entityName).append("\"]\n");
        mermaid.append("    Reader --> Entity\n\n");

        // Processor
        String processorName = toJavaClassName(program.getProgramName()) + "Processor";
        mermaid.append("    Processor[\"⚙️ ItemProcessor<br/>").append(processorName).append("\"]\n");
        mermaid.append("    Entity --> Processor\n\n");

        // Business Logic
        mermaid.append("    Logic{\"🧠 Business Logic<br/>Validation<br/>Transformation<br/>Calculation\"}\n");
        mermaid.append("    Processor --> Logic\n\n");

        // Validated Entity
        mermaid.append("    ValidEntity[\"✅ Validated Entity<br/>").append(entityName).append("\"]\n");
        mermaid.append("    Logic --> ValidEntity\n\n");

        // Writer
        mermaid.append("    Writer[\"💾 ItemWriter<br/>FlatFileItemWriter\"]\n");
        mermaid.append("    ValidEntity --> Writer\n\n");

        // Output
        mermaid.append("    Output[(\"📂 OUTPUT<br/>").append(getOutputFileName(program)).append("\")]\n");
        mermaid.append("    Writer --> Output\n\n");

        // Error Handling
        mermaid.append("    Error[(\"⚠️ ERROR LOG<br/>error-report.txt\")]\n");
        mermaid.append("    Logic -->|Invalid| Error\n\n");

        // Styling
        mermaid.append("    style Input fill:#e3f2fd,stroke:#2196f3,stroke-width:2px\n");
        mermaid.append("    style Output fill:#e1f5e1,stroke:#4caf50,stroke-width:2px\n");
        mermaid.append("    style Error fill:#ffebee,stroke:#f44336,stroke-width:2px\n");
        mermaid.append("    style Logic fill:#fff3e0,stroke:#ff9800,stroke-width:3px\n");
        mermaid.append("    style Processor fill:#f3e5f5,stroke:#9c27b0,stroke-width:2px\n");

        mermaid.append("```\n");

        writeFile(outputFile, mermaid.toString());
    }

    /**
     * Génère le diagramme de séquence Spring Batch
     */
    private void generateSpringBatchSequence(CobolProgram program, Path outputFile) throws IOException {
        StringBuilder mermaid = new StringBuilder();

        mermaid.append("# Diagramme de Séquence Spring Batch - ").append(program.getProgramName()).append("\n\n");
        mermaid.append("```mermaid\n");
        mermaid.append("sequenceDiagram\n");
        mermaid.append("    participant User\n");
        mermaid.append("    participant Job\n");
        mermaid.append("    participant Step\n");
        mermaid.append("    participant Reader\n");
        mermaid.append("    participant Processor\n");
        mermaid.append("    participant Writer\n");
        mermaid.append("    participant DB\n\n");

        mermaid.append("    User->>Job: Launch Job\n");
        mermaid.append("    Job->>Step: Execute Step\n");
        mermaid.append("    activate Step\n\n");

        mermaid.append("    loop For each chunk\n");
        mermaid.append("        Step->>Reader: read()\n");
        mermaid.append("        Reader-->>Step: Record\n");
        mermaid.append("        Step->>Processor: process(record)\n");
        mermaid.append("        activate Processor\n");
        mermaid.append("        Processor->>Processor: Validate\n");
        mermaid.append("        Processor->>Processor: Transform\n");
        mermaid.append("        Processor->>Processor: Calculate\n");
        mermaid.append("        Processor-->>Step: Processed Record\n");
        mermaid.append("        deactivate Processor\n");
        mermaid.append("    end\n\n");

        mermaid.append("    Step->>Writer: write(records)\n");
        mermaid.append("    Writer->>DB: Save batch\n");
        mermaid.append("    DB-->>Writer: OK\n");
        mermaid.append("    Writer-->>Step: Success\n\n");

        mermaid.append("    deactivate Step\n");
        mermaid.append("    Step-->>Job: Step Complete\n");
        mermaid.append("    Job-->>User: Job Complete\n");

        mermaid.append("```\n");

        writeFile(outputFile, mermaid.toString());
    }

    /**
     * Génère la page HTML de visualisation avec Mermaid.js
     */
    private void generateVisualizationPage(CobolProgram program, Path outputFile) throws IOException {
        StringBuilder html = new StringBuilder();

        html.append("<!DOCTYPE html>\n");
        html.append("<html lang=\"fr\">\n");
        html.append("<head>\n");
        html.append("    <meta charset=\"UTF-8\">\n");
        html.append("    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n");
        html.append("    <title>Diagrammes Algorithmiques - ").append(program.getProgramName()).append("</title>\n");
        html.append("    <script src=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js\"></script>\n");
        html.append("    <style>\n");
        html.append("        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }\n");
        html.append("        h1 { color: #2196f3; }\n");
        html.append("        h2 { color: #424242; margin-top: 40px; }\n");
        html.append("        .diagram { background: white; padding: 20px; margin: 20px 0; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n");
        html.append("        .info { background: #e3f2fd; padding: 15px; border-left: 4px solid #2196f3; margin: 20px 0; }\n");
        html.append("        .mermaid { text-align: center; }\n");
        html.append("        .tabs { display: flex; gap: 10px; margin: 20px 0; }\n");
        html.append("        .tab { padding: 10px 20px; background: white; border: 1px solid #ddd; cursor: pointer; border-radius: 4px; }\n");
        html.append("        .tab.active { background: #2196f3; color: white; }\n");
        html.append("        .tab-content { display: none; }\n");
        html.append("        .tab-content.active { display: block; }\n");
        html.append("    </style>\n");
        html.append("</head>\n");
        html.append("<body>\n");
        html.append("    <h1>📊 Diagrammes Algorithmiques</h1>\n");
        html.append("    <h2>Programme COBOL: ").append(program.getProgramName()).append("</h2>\n");

        html.append("    <div class=\"info\">\n");
        html.append("        <strong>ℹ️ Information:</strong> Ces diagrammes ont été générés automatiquement lors de la conversion COBOL → Java Spring Batch.\n");
        html.append("        Ils illustrent le flux algorithmique, le flux de données et la séquence d'exécution Spring Batch.\n");
        html.append("    </div>\n");

        // Tabs
        html.append("    <div class=\"tabs\">\n");
        html.append("        <div class=\"tab active\" onclick=\"showTab('flowchart')\">🔄 Flowchart</div>\n");
        html.append("        <div class=\"tab\" onclick=\"showTab('dataflow')\">📊 Flux de Données</div>\n");
        html.append("        <div class=\"tab\" onclick=\"showTab('sequence')\">🔗 Séquence</div>\n");
        html.append("    </div>\n");

        // Flowchart Tab
        html.append("    <div id=\"flowchart\" class=\"tab-content active\">\n");
        html.append("        <div class=\"diagram\">\n");
        html.append("            <h2>🔄 Flowchart Algorithmique</h2>\n");
        html.append("            <div class=\"mermaid\">\n");
        html.append(generateInlineFlowchart(program));
        html.append("            </div>\n");
        html.append("        </div>\n");
        html.append("    </div>\n");

        // Data Flow Tab
        html.append("    <div id=\"dataflow\" class=\"tab-content\">\n");
        html.append("        <div class=\"diagram\">\n");
        html.append("            <h2>📊 Flux de Données</h2>\n");
        html.append("            <div class=\"mermaid\">\n");
        html.append(generateInlineDataFlow(program));
        html.append("            </div>\n");
        html.append("        </div>\n");
        html.append("    </div>\n");

        // Sequence Tab
        html.append("    <div id=\"sequence\" class=\"tab-content\">\n");
        html.append("        <div class=\"diagram\">\n");
        html.append("            <h2>🔗 Diagramme de Séquence Spring Batch</h2>\n");
        html.append("            <div class=\"mermaid\">\n");
        html.append(generateInlineSequence(program));
        html.append("            </div>\n");
        html.append("        </div>\n");
        html.append("    </div>\n");

        html.append("    <script>\n");
        html.append("        mermaid.initialize({ startOnLoad: true, theme: 'default' });\n");
        html.append("        function showTab(tabName) {\n");
        html.append("            document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));\n");
        html.append("            document.querySelectorAll('.tab-content').forEach(t => t.classList.remove('active'));\n");
        html.append("            event.target.classList.add('active');\n");
        html.append("            document.getElementById(tabName).classList.add('active');\n");
        html.append("        }\n");
        html.append("    </script>\n");
        html.append("</body>\n");
        html.append("</html>\n");

        writeFile(outputFile, html.toString());
    }

    // Helper methods

    private String generateInlineFlowchart(CobolProgram program) {
        StringBuilder mermaid = new StringBuilder();
        mermaid.append("flowchart TD\n");
        mermaid.append("    Start([\"🚀 START\"])\n");
        mermaid.append("    Start --> Init[\"📋 Initialize\"]\n");
        mermaid.append("    Init --> Loop{\"🔄 Process Loop\"}\n");
        mermaid.append("    Loop -->|Record| Validate{\"❓ Valid?\"}\n");
        mermaid.append("    Validate -->|Yes| Process[\"⚙️ Process\"]\n");
        mermaid.append("    Validate -->|No| Error[\"⚠️ Log Error\"]\n");
        mermaid.append("    Process --> Loop\n");
        mermaid.append("    Error --> Loop\n");
        mermaid.append("    Loop -->|EOF| End([\"✅ END\"])\n");
        mermaid.append("    style Start fill:#e1f5e1,stroke:#4caf50,stroke-width:3px\n");
        mermaid.append("    style End fill:#e1f5e1,stroke:#4caf50,stroke-width:3px\n");
        return mermaid.toString();
    }

    private String generateInlineDataFlow(CobolProgram program) {
        StringBuilder mermaid = new StringBuilder();
        mermaid.append("flowchart LR\n");
        mermaid.append("    Input[(\"📂 INPUT\")]\n");
        mermaid.append("    Reader[\"📖 Reader\"]\n");
        mermaid.append("    Processor[\"⚙️ Processor\"]\n");
        mermaid.append("    Writer[\"💾 Writer\"]\n");
        mermaid.append("    Output[(\"📂 OUTPUT\")]\n");
        mermaid.append("    Input --> Reader --> Processor --> Writer --> Output\n");
        mermaid.append("    style Input fill:#e3f2fd,stroke:#2196f3\n");
        mermaid.append("    style Output fill:#e1f5e1,stroke:#4caf50\n");
        return mermaid.toString();
    }

    private String generateInlineSequence(CobolProgram program) {
        StringBuilder mermaid = new StringBuilder();
        mermaid.append("sequenceDiagram\n");
        mermaid.append("    participant Job\n");
        mermaid.append("    participant Reader\n");
        mermaid.append("    participant Processor\n");
        mermaid.append("    participant Writer\n");
        mermaid.append("    Job->>Reader: read()\n");
        mermaid.append("    Reader-->>Job: record\n");
        mermaid.append("    Job->>Processor: process(record)\n");
        mermaid.append("    Processor-->>Job: processed\n");
        mermaid.append("    Job->>Writer: write(records)\n");
        mermaid.append("    Writer-->>Job: success\n");
        return mermaid.toString();
    }

    private Map<String, List<Statement>> groupStatementsByParagraph(CobolProgram program) {
        Map<String, List<Statement>> paragraphs = new LinkedHashMap<>();
        paragraphs.put("MAIN", program.getStatements());
        return paragraphs;
    }

    private String cleanCondition(String condition) {
        if (condition == null) return "condition";
        return condition.length() > 30 ? condition.substring(0, 30) + "..." : condition;
    }

    private String getInputFileName(CobolProgram program) {
        // Extraire du SELECT ... ASSIGN TO
        return program.getProgramName().toLowerCase() + "-input.txt";
    }

    private String getOutputFileName(CobolProgram program) {
        return program.getProgramName().toLowerCase() + "-output.txt";
    }

    private String toJavaClassName(String cobolName) {
        if (cobolName == null) return "Program";
        return Arrays.stream(cobolName.split("[-_]"))
                .map(s -> s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase())
                .collect(Collectors.joining(""));
    }

    private void writeFile(Path outputFile, String content) throws IOException {
        try (FileWriter writer = new FileWriter(outputFile.toFile())) {
            writer.write(content);
        }
        logger.info("Generated diagram file: {}", outputFile.getFileName());
    }
}
