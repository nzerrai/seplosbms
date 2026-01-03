package com.cobol.translator;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.result.TranslationResult;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.Callable;

/**
 * Command-line interface for COBOL to Java translator.
 */
@Command(name = "cobol-translator",
         mixinStandardHelpOptions = true,
         version = "1.0.0",
         description = "Translates COBOL programs to Java Spring Batch")
public class CobolTranslatorCli implements Callable<Integer> {

    @Command(name = "translate",
             description = "Translate a single COBOL file")
    static class TranslateCommand implements Callable<Integer> {

        @Parameters(index = "0", description = "COBOL source file")
        private File sourceFile;

        @Option(names = {"-o", "--output"},
                description = "Output directory (default: src/main/java)",
                defaultValue = "src/main/java")
        private String outputDir;

        @Option(names = {"-p", "--package"},
                description = "Java package name",
                defaultValue = "com.generated.batch")
        private String packageName;

        @Option(names = {"--no-tests"},
                description = "Skip test generation")
        private boolean skipTests;

        @Option(names = {"--no-report"},
                description = "Skip conversion report generation")
        private boolean skipReport;

        @Override
        public Integer call() {
            System.out.println("╔════════════════════════════════════════════════════════╗");
            System.out.println("║   COBOL to Java Spring Batch Translator v1.0.0        ║");
            System.out.println("╚════════════════════════════════════════════════════════╝");
            System.out.println();

            if (!sourceFile.exists()) {
                System.err.println("❌ Error: Source file not found: " + sourceFile);
                return 1;
            }

            System.out.println("📄 Source file: " + sourceFile.getAbsolutePath());
            System.out.println("📦 Package: " + packageName);
            System.out.println("📂 Output directory: " + outputDir);
            System.out.println();

            TranslationConfig config = TranslationConfig.builder()
                    .sourceFile(sourceFile.getAbsolutePath())
                    .outputPackage(packageName)
                    .targetDirectory(outputDir)
                    .generateTests(!skipTests)
                    .generateReport(!skipReport)
                    .build();

            CobolTranslator translator;
            try {
                translator = new CobolTranslator();
            } catch (IOException e) {
                System.err.println("❌ Error: Failed to initialize translator: " + e.getMessage());
                return 1;
            }

            System.out.println("🔄 Starting translation...");
            System.out.println();

            TranslationResult result = translator.translate(config);

            if (result.isSuccess()) {
                System.out.println("✅ Translation completed successfully!");
                System.out.println();
                System.out.println("📊 Metrics:");
                System.out.println("   " + result.getMetrics());
                System.out.println();

                if (result.getConversionReport() != null) {
                    System.out.println("📋 Conversion Report:");
                    System.out.printf("   Conversion rate    : %.1f%%\n",
                        result.getConversionReport().getConversionPercentage());
                    System.out.printf("   Partial conversion : %.1f%%\n",
                        result.getConversionReport().getPartialConversionPercentage());
                    System.out.printf("   Unconverted        : %.1f%%\n",
                        result.getConversionReport().getFailurePercentage());
                    System.out.println("   Confidence level   : " +
                        result.getConversionReport().getOverallConfidence().getLabel());
                    System.out.println();
                }

                System.out.println("📝 Generated files:");
                for (File file : result.getGeneratedFiles()) {
                    System.out.println("   ✓ " + file.getName());
                }
                return 0;
            } else {
                System.err.println("❌ Translation failed:");
                System.err.println("   " + result.getErrorMessage());
                return 1;
            }
        }
    }

    @Command(name = "translate-all",
             description = "Translate all COBOL files in a directory")
    static class TranslateAllCommand implements Callable<Integer> {

        @Parameters(index = "0", description = "Directory containing COBOL files")
        private File inputDir;

        @Option(names = {"-o", "--output"},
                description = "Output directory (default: src/main/java)",
                defaultValue = "src/main/java")
        private String outputDir;

        @Option(names = {"-p", "--package"},
                description = "Java package name",
                defaultValue = "com.generated.batch")
        private String packageName;

        @Override
        public Integer call() {
            System.out.println("╔════════════════════════════════════════════════════════╗");
            System.out.println("║   COBOL to Java Spring Batch Translator v1.0.0        ║");
            System.out.println("╚════════════════════════════════════════════════════════╝");
            System.out.println();

            if (!inputDir.exists() || !inputDir.isDirectory()) {
                System.err.println("❌ Error: Invalid input directory: " + inputDir);
                return 1;
            }

            System.out.println("📂 Input directory: " + inputDir.getAbsolutePath());
            System.out.println("📦 Package: " + packageName);
            System.out.println("📂 Output directory: " + outputDir);
            System.out.println();

            TranslationConfig config = TranslationConfig.builder()
                    .outputPackage(packageName)
                    .targetDirectory(outputDir)
                    .build();

            CobolTranslator translator;
            try {
                translator = new CobolTranslator();
            } catch (IOException e) {
                System.err.println("❌ Error: Failed to initialize translator: " + e.getMessage());
                return 1;
            }

            List<TranslationResult> results = translator.translateDirectory(
                    inputDir.getAbsolutePath(), config);

            int successCount = 0;
            int failureCount = 0;

            System.out.println("📊 Summary:");
            System.out.println();

            for (TranslationResult result : results) {
                if (result.isSuccess()) {
                    successCount++;
                    System.out.println("✅ " + new File(result.getSourceFile()).getName());
                } else {
                    failureCount++;
                    System.err.println("❌ " + new File(result.getSourceFile()).getName() +
                                     ": " + result.getErrorMessage());
                }
            }

            System.out.println();
            System.out.println("Total: " + results.size() +
                             " | Success: " + successCount +
                             " | Failed: " + failureCount);

            return failureCount == 0 ? 0 : 1;
        }
    }

    @Override
    public Integer call() {
        CommandLine.usage(this, System.out);
        return 0;
    }

    public static void main(String[] args) {
        CommandLine cmd = new CommandLine(new CobolTranslatorCli());
        cmd.addSubcommand("translate", new TranslateCommand());
        cmd.addSubcommand("translate-all", new TranslateAllCommand());

        int exitCode = cmd.execute(args);
        System.exit(exitCode);
    }
}
