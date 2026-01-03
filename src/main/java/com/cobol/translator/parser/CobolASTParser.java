package com.cobol.translator.parser;

import com.cobol.translator.ast.ProgramNode;
import com.cobol.translator.grammar.CobolLexer;
import com.cobol.translator.grammar.CobolParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Parser for COBOL programs using ANTLR4.
 * Provides a clean API for parsing COBOL source files into AST.
 */
public class CobolASTParser {

    private static final Logger logger = LoggerFactory.getLogger(CobolASTParser.class);

    /**
     * Parse a COBOL source file and return the AST
     *
     * @param sourceFile Path to the COBOL source file
     * @return ProgramNode representing the parsed AST
     * @throws IOException if file cannot be read
     */
    public ProgramNode parse(Path sourceFile) throws IOException {
        logger.info("Parsing COBOL file: {}", sourceFile);

        // Read source file
        CharStream input = CharStreams.fromPath(sourceFile);

        return parse(input, sourceFile.toString());
    }

    /**
     * Parse COBOL source code from a string
     *
     * @param sourceCode The COBOL source code
     * @param sourceName Name/identifier for the source (for error messages)
     * @return ProgramNode representing the parsed AST
     */
    public ProgramNode parseString(String sourceCode, String sourceName) {
        logger.info("Parsing COBOL source: {}", sourceName);

        CharStream input = CharStreams.fromString(sourceCode);
        return parse(input, sourceName);
    }

    /**
     * Internal parse method
     */
    private ProgramNode parse(CharStream input, String sourceName) {
        try {
            // Create lexer
            CobolLexer lexer = new CobolLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);

            // Create parser
            CobolParser parser = new CobolParser(tokens);

            // Add error listener for better error reporting
            parser.removeErrorListeners();
            parser.addErrorListener(new CobolErrorListener(sourceName));

            // Parse the compilation unit (program)
            ParseTree tree = parser.compilationUnit();

            // Build AST from parse tree
            CobolASTBuilder astBuilder = new CobolASTBuilder();
            ProgramNode programNode = (ProgramNode) astBuilder.visit(tree);

            if (programNode != null) {
                programNode.setSourceFile(sourceName);
                logger.info("Successfully parsed program: {}", programNode.getProgramName());
            } else {
                logger.error("Failed to build AST for: {}", sourceName);
            }

            return programNode;

        } catch (Exception e) {
            logger.error("Error parsing COBOL source {}: {}", sourceName, e.getMessage(), e);
            throw new RuntimeException("Failed to parse COBOL source: " + sourceName, e);
        }
    }

    /**
     * Check if a source file is valid COBOL syntax
     *
     * @param sourceFile Path to the COBOL source file
     * @return true if the file has valid COBOL syntax
     */
    public boolean isValidSyntax(Path sourceFile) {
        try {
            parse(sourceFile);
            return true;
        } catch (Exception e) {
            logger.warn("Invalid COBOL syntax in {}: {}", sourceFile, e.getMessage());
            return false;
        }
    }

    /**
     * Check if a source string is valid COBOL syntax
     *
     * @param sourceCode The COBOL source code
     * @return true if the code has valid COBOL syntax
     */
    public boolean isValidSyntax(String sourceCode) {
        try {
            parseString(sourceCode, "inline-source");
            return true;
        } catch (Exception e) {
            logger.warn("Invalid COBOL syntax: {}", e.getMessage());
            return false;
        }
    }
}
