package com.cobol.translator.parser;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Custom error listener for COBOL parsing.
 * Provides better error messages and logging for syntax errors.
 */
public class CobolErrorListener extends BaseErrorListener {

    private static final Logger logger = LoggerFactory.getLogger(CobolErrorListener.class);
    private final String sourceName;

    public CobolErrorListener(String sourceName) {
        this.sourceName = sourceName;
    }

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer,
                           Object offendingSymbol,
                           int line,
                           int charPositionInLine,
                           String msg,
                           RecognitionException e) {
        String errorMessage = String.format(
            "Syntax error in %s at line %d:%d - %s",
            sourceName,
            line,
            charPositionInLine,
            msg
        );

        logger.error(errorMessage);

        // You can also collect errors instead of just logging
        // For now, we'll throw an exception to stop parsing
        throw new RuntimeException(errorMessage, e);
    }
}
