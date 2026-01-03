package com.cobol.translator.generator;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.CobolProgram;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Generates JUnit tests for translated Java code.
 */
public class TestGenerator {

    private static final Logger logger = LoggerFactory.getLogger(TestGenerator.class);

    public List<File> generate(CobolProgram program, TranslationConfig config, Path outputDir) {
        logger.info("Test generation not yet implemented");
        return new ArrayList<>();
    }
}
