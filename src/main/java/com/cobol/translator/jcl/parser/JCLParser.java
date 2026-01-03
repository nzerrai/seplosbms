package com.cobol.translator.jcl.parser;

import com.cobol.translator.grammar.JCLLexer;
import com.cobol.translator.jcl.model.JCLJob;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Parser for JCL (Job Control Language) files
 */
public class JCLParser {

    private static final Logger logger = LoggerFactory.getLogger(JCLParser.class);

    /**
     * Parse a JCL file from a Path
     *
     * @param jclFile Path to the JCL file
     * @return Parsed JCLJob object
     * @throws IOException if file cannot be read
     */
    public JCLJob parse(Path jclFile) throws IOException {
        String content = Files.readString(jclFile);
        return parse(content);
    }

    /**
     * Parse JCL content from a String
     *
     * @param jclContent JCL source code
     * @return Parsed JCLJob object
     */
    public JCLJob parse(String jclContent) {
        logger.debug("Parsing JCL content ({} characters)", jclContent.length());

        try {
            // Create lexer and parser
            JCLLexer lexer = new JCLLexer(CharStreams.fromString(jclContent));
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            com.cobol.translator.grammar.JCLParser parser =
                new com.cobol.translator.grammar.JCLParser(tokens);

            // Add error listener
            parser.removeErrorListeners();
            parser.addErrorListener(new JCLErrorListener());

            // Parse the JCL file
            ParseTree tree = parser.jclFile();

            // Build JCL model using visitor
            JCLASTBuilder visitor = new JCLASTBuilder();
            JCLJob job = (JCLJob) visitor.visit(tree);

            logger.info("JCL parsing completed: Job '{}' with {} steps",
                       job.getJobName(), job.getSteps().size());

            return job;

        } catch (Exception e) {
            logger.error("Error parsing JCL: {}", e.getMessage(), e);
            throw new JCLParseException("Failed to parse JCL: " + e.getMessage(), e);
        }
    }

    /**
     * Check if a file is likely a JCL file based on content
     *
     * @param content File content
     * @return true if content appears to be JCL
     */
    public static boolean isJCLFile(String content) {
        if (content == null || content.trim().isEmpty()) {
            return false;
        }

        // JCL files typically start with // and contain JOB statement
        String firstLine = content.trim().split("\\r?\\n")[0];
        return firstLine.startsWith("//") &&
               content.toUpperCase().contains(" JOB ");
    }

    /**
     * Check if a file extension indicates a JCL file
     *
     * @param filename File name
     * @return true if filename has JCL extension
     */
    public static boolean hasJCLExtension(String filename) {
        if (filename == null) {
            return false;
        }

        String lowerName = filename.toLowerCase();
        return lowerName.endsWith(".jcl") ||
               lowerName.endsWith(".jclproc") ||
               lowerName.endsWith(".proc");
    }
}
