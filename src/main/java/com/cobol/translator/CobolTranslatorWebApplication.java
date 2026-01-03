package com.cobol.translator;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Main Spring Boot application class for COBOL to Java Translator Web Interface
 *
 * This class starts the web server with:
 * - REST API for file conversion
 * - Web interface for uploading COBOL files
 * - H2 console for database debugging
 *
 * To run:
 * - Using Maven: mvn spring-boot:run
 * - Using JAR: java -cp target/cobol-translator.jar com.cobol.translator.CobolTranslatorWebApplication
 *
 * Access the web interface at: http://localhost:9090/conversion
 */
@SpringBootApplication
public class CobolTranslatorWebApplication {

    public static void main(String[] args) {
        SpringApplication.run(CobolTranslatorWebApplication.class, args);
    }
}
