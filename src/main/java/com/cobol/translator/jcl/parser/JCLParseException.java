package com.cobol.translator.jcl.parser;

/**
 * Exception thrown when JCL parsing fails
 */
public class JCLParseException extends RuntimeException {

    public JCLParseException(String message) {
        super(message);
    }

    public JCLParseException(String message, Throwable cause) {
        super(message, cause);
    }
}
