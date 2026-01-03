package com.cobol.translator.jcl.parser;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Error listener for JCL parsing
 */
public class JCLErrorListener extends BaseErrorListener {

    private static final Logger logger = LoggerFactory.getLogger(JCLErrorListener.class);

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer,
                           Object offendingSymbol,
                           int line,
                           int charPositionInLine,
                           String msg,
                           RecognitionException e) {
        String error = String.format("JCL Syntax error at line %d:%d - %s",
                                    line, charPositionInLine, msg);
        logger.error(error);
        throw new JCLParseException(error);
    }
}
