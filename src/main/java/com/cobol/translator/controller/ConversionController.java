package com.cobol.translator.controller;

import com.cobol.translator.jcl.parser.JCLParseException;
import com.cobol.translator.service.CobolConversionService;
import com.cobol.translator.service.ConversionResult;
import org.antlr.v4.runtime.RecognitionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Base64;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Controller for handling COBOL to Java Spring Batch conversion
 */
@Controller
@RequestMapping("/conversion")
public class ConversionController {

    private static final Logger logger = LoggerFactory.getLogger(ConversionController.class);

    @Autowired
    private CobolConversionService conversionService;

    @Value("${cobol.translator.temp.upload-dir}")
    private String uploadTempDir;

    @Value("${cobol.translator.temp.output-dir}")
    private String outputTempDir;

    /**
     * Display the upload page
     */
    @GetMapping
    public String showUploadPage() {
        return "conversion";
    }

    /**
     * Handle file upload and conversion
     * Supports COBOL files (.cob, .cbl) and optionally JCL files (.jcl)
     */
    @PostMapping("/upload")
    @ResponseBody
    public ResponseEntity<?> uploadAndConvert(
            @RequestParam("files") MultipartFile[] files,
            @RequestParam("projectName") String projectName,
            @RequestParam("basePackage") String basePackage) {

        logger.info("Received conversion request - Project: {}, Package: {}, Files: {}",
                projectName, basePackage, files.length);

        if (files == null || files.length == 0) {
            return ResponseEntity.badRequest().body(new ErrorResponse("No files uploaded"));
        }

        if (projectName == null || projectName.trim().isEmpty()) {
            return ResponseEntity.badRequest().body(new ErrorResponse("Project name is required"));
        }

        if (basePackage == null || basePackage.trim().isEmpty()) {
            basePackage = "com.example.batch";
        }

        // File tracking variables
        List<Path> cobolFiles = new ArrayList<>();
        Path jclFile = null;
        String jclFileName = null;

        try {
            // Create temporary directory for uploaded files using configured path
            Path tempDir = createTempDirectory(uploadTempDir, "upload-");

            // Save uploaded files
            for (MultipartFile file : files) {
                if (file.isEmpty()) {
                    continue;
                }

                String filename = file.getOriginalFilename();
                if (filename != null) {
                    Path filePath = tempDir.resolve(filename);
                    file.transferTo(filePath.toFile());

                    // Classify file type
                    if (filename.endsWith(".cob") || filename.endsWith(".cbl")) {
                        cobolFiles.add(filePath);
                        logger.info("Saved COBOL file: {}", filename);
                    } else if (filename.endsWith(".jcl")) {
                        jclFile = filePath;
                        jclFileName = filename;
                        logger.info("Saved JCL file: {}", filename);
                    }
                }
            }

            if (cobolFiles.isEmpty()) {
                return ResponseEntity.badRequest()
                        .body(new ErrorResponse("No valid COBOL files (.cob or .cbl) found"));
            }

            // Convert COBOL files to Spring Batch project
            // If JCL file is provided, use enhanced conversion with JCL
            ConversionResult result;
            if (jclFile != null) {
                logger.info("Using JCL-enhanced conversion");
                result = conversionService.convertWithJCL(
                        cobolFiles, jclFile, projectName, basePackage);
            } else {
                logger.info("Using standard COBOL conversion");
                result = conversionService.convertToSpringBatchProject(
                        cobolFiles, projectName, basePackage);
            }

            Path outputDir = result.getProjectPath();
            logger.info("Conversion completed. Output directory: {}", outputDir);

            // Create ZIP file with the generated project
            byte[] zipBytes = createZipFromDirectory(outputDir);

            // Clean up temporary directories
            deleteDirectory(tempDir.toFile());
            deleteDirectory(outputDir.toFile());

            // Create response with conversion report
            ConversionResponse response = ConversionResponse.success(
                    "Conversion completed successfully",
                    projectName,
                    result.getReport()
            );
            
            // Encode ZIP file as base64 for JSON response
            response.setZipFileBase64(Base64.getEncoder().encodeToString(zipBytes));

            return ResponseEntity.ok()
                    .contentType(MediaType.APPLICATION_JSON)
                    .body(response);

        } catch (JCLParseException e) {
            logger.error("JCL parsing error", e);
            return handleJCLParseException(e, jclFileName);
        } catch (RecognitionException e) {
            logger.error("COBOL parsing error", e);
            return handleCobolParseException(e);
        } catch (IOException e) {
            logger.error("I/O error during conversion", e);
            DetailedErrorResponse error = new DetailedErrorResponse(
                    "IO_ERROR", null, "File I/O error during conversion");
            error.setDetailedMessage(e.getMessage());
            error.setSuggestion("Check file permissions and disk space");
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
        } catch (Exception e) {
            logger.error("Error during conversion", e);
            DetailedErrorResponse error = new DetailedErrorResponse(
                    "CONVERSION_ERROR", null, "Conversion failed");
            error.setDetailedMessage(e.getMessage());
            error.setSuggestion("Check the error log for details. Ensure your COBOL/JCL files are valid.");
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
        }
    }

    /**
     * Create a ZIP file from a directory
     */
    private byte[] createZipFromDirectory(Path sourceDir) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (ZipOutputStream zos = new ZipOutputStream(baos)) {
            Files.walk(sourceDir)
                    .filter(path -> !Files.isDirectory(path))
                    .forEach(path -> {
                        try {
                            String zipEntryName = sourceDir.relativize(path).toString();
                            zos.putNextEntry(new ZipEntry(zipEntryName));
                            Files.copy(path, zos);
                            zos.closeEntry();
                        } catch (IOException e) {
                            throw new RuntimeException("Error creating ZIP", e);
                        }
                    });
        }
        return baos.toByteArray();
    }

    /**
     * Create a temporary directory using the configured base path
     */
    private Path createTempDirectory(String baseDir, String prefix) throws IOException {
        // Create base directory if it doesn't exist
        Path basePath = Paths.get(baseDir);
        if (!Files.exists(basePath)) {
            Files.createDirectories(basePath);
            logger.info("Created temporary base directory: {}", basePath);
        }

        // Create unique temporary directory with timestamp and random suffix
        String timestamp = String.valueOf(System.currentTimeMillis());
        String dirName = prefix + timestamp + "-" + (int)(Math.random() * 10000);
        Path tempDir = basePath.resolve(dirName);
        Files.createDirectories(tempDir);

        logger.debug("Created temporary directory: {}", tempDir);
        return tempDir;
    }

    /**
     * Delete a directory recursively
     */
    private void deleteDirectory(File directory) {
        if (directory.exists()) {
            File[] files = directory.listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory()) {
                        deleteDirectory(file);
                    } else {
                        file.delete();
                    }
                }
            }
            directory.delete();
        }
    }

    /**
     * Handle JCL parsing exceptions with detailed error information
     */
    private ResponseEntity<?> handleJCLParseException(JCLParseException e, String jclFileName) {
        String errorMsg = e.getMessage();
        DetailedErrorResponse error = new DetailedErrorResponse(
                "JCL_PARSE_ERROR",
                jclFileName != null ? jclFileName : "JCL file",
                "JCL parsing failed"
        );

        // Extract line and column information from error message
        // Format: "JCL Syntax error at line X:Y - message"
        if (errorMsg != null && errorMsg.contains("line")) {
            try {
                int lineStart = errorMsg.indexOf("line ") + 5;
                int colonPos = errorMsg.indexOf(":", lineStart);
                if (colonPos > lineStart) {
                    String lineNum = errorMsg.substring(lineStart, colonPos);
                    error.setLineNumber(Integer.parseInt(lineNum));

                    int dashPos = errorMsg.indexOf(" -", colonPos);
                    if (dashPos > colonPos) {
                        String colNum = errorMsg.substring(colonPos + 1, dashPos);
                        error.setColumnNumber(Integer.parseInt(colNum));
                    }
                }
            } catch (Exception parseEx) {
                // If parsing fails, just use the original message
                logger.debug("Could not parse line/column from error message", parseEx);
            }
        }

        error.setDetailedMessage(errorMsg);

        // Provide helpful suggestions based on common JCL errors
        if (errorMsg != null) {
            if (errorMsg.contains("extraneous input '('")) {
                error.setSuggestion("Remove accounting information in parentheses from the JOB statement. " +
                        "Use format: //JOBNAME JOB 'description',CLASS=A instead of //JOBNAME JOB (ACCT),'description'");
            } else if (errorMsg.contains("MSGLEVEL")) {
                error.setSuggestion("Simplify MSGLEVEL parameter. Use MSGLEVEL=1 instead of MSGLEVEL=(1,1)");
            } else if (errorMsg.contains("&SYSUID") || errorMsg.contains("&LY")) {
                error.setSuggestion("Remove the '&' prefix from system variables. Use SYSUID instead of &SYSUID");
            } else if (errorMsg.contains("mismatched input")) {
                error.setSuggestion("Check JCL syntax. Ensure all statements follow valid JCL format: //NAME OPERATION PARAMETERS");
            } else {
                error.setSuggestion("Check JCL syntax and ensure compatibility with the parser. " +
                        "See JCL_SUPPORT.md for supported JCL features.");
            }
        }

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
    }

    /**
     * Handle COBOL parsing exceptions with detailed error information
     */
    private ResponseEntity<?> handleCobolParseException(RecognitionException e) {
        DetailedErrorResponse error = new DetailedErrorResponse(
                "COBOL_PARSE_ERROR",
                "COBOL file",
                "COBOL parsing failed"
        );

        String errorMsg = e.getMessage();
        error.setDetailedMessage(errorMsg);

        // Try to extract line information from RecognitionException
        if (e.getOffendingToken() != null) {
            error.setLineNumber(e.getOffendingToken().getLine());
            error.setColumnNumber(e.getOffendingToken().getCharPositionInLine());
        }

        error.setSuggestion("Check COBOL syntax. Ensure the file follows valid COBOL structure with " +
                "IDENTIFICATION DIVISION, ENVIRONMENT DIVISION, DATA DIVISION, and PROCEDURE DIVISION.");

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
    }

    /**
     * Simple error response class for basic errors
     */
    static class ErrorResponse {
        private String error;

        public ErrorResponse(String error) {
            this.error = error;
        }

        public String getError() {
            return error;
        }

        public void setError(String error) {
            this.error = error;
        }
    }

    /**
     * Detailed error response class for parsing errors
     */
    static class DetailedErrorResponse {
        private String errorType;
        private String fileName;
        private Integer lineNumber;
        private Integer columnNumber;
        private String errorMessage;
        private String suggestion;
        private String detailedMessage;

        public DetailedErrorResponse(String errorType, String fileName, String errorMessage) {
            this.errorType = errorType;
            this.fileName = fileName;
            this.errorMessage = errorMessage;
        }

        public String getErrorType() {
            return errorType;
        }

        public void setErrorType(String errorType) {
            this.errorType = errorType;
        }

        public String getFileName() {
            return fileName;
        }

        public void setFileName(String fileName) {
            this.fileName = fileName;
        }

        public Integer getLineNumber() {
            return lineNumber;
        }

        public void setLineNumber(Integer lineNumber) {
            this.lineNumber = lineNumber;
        }

        public Integer getColumnNumber() {
            return columnNumber;
        }

        public void setColumnNumber(Integer columnNumber) {
            this.columnNumber = columnNumber;
        }

        public String getErrorMessage() {
            return errorMessage;
        }

        public void setErrorMessage(String errorMessage) {
            this.errorMessage = errorMessage;
        }

        public String getSuggestion() {
            return suggestion;
        }

        public void setSuggestion(String suggestion) {
            this.suggestion = suggestion;
        }

        public String getDetailedMessage() {
            return detailedMessage;
        }

        public void setDetailedMessage(String detailedMessage) {
            this.detailedMessage = detailedMessage;
        }
    }
}
