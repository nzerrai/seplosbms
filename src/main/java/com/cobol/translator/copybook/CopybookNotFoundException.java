package com.cobol.translator.copybook;

/**
 * Exception thrown when a copybook cannot be found in search paths
 */
public class CopybookNotFoundException extends Exception {
    
    public CopybookNotFoundException(String message) {
        super(message);
    }
    
    public CopybookNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}
