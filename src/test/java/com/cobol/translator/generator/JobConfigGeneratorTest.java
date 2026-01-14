package com.cobol.translator.generator;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class JobConfigGeneratorTest {

    @TempDir
    Path tempDir;

    @Test
    void generatesChunkStepWithReaderProcessorWriter() throws IOException {
        CobolProgram program = new CobolProgram("PAYROLL");
        DataItem record = new DataItem(1, "PAYROLL-REC");
        record.setGroup(true);
        program.addDataItem(record);

        FileDefinition fd = new FileDefinition("PAYROLL-REC");
        fd.setRecordLayout(record);
        program.setFileDefinitions(List.of(fd));

        Path outputDir = tempDir.resolve("src/main/java/com/example/config");
        Files.createDirectories(outputDir);

        TranslationConfig config = TranslationConfig.builder()
                .sourceFile("PAYROLL.cob")
                .outputPackage("com.example")
                .build();

        JobConfigGenerator generator = new JobConfigGenerator();
        generator.generate(program, config, outputDir);

        Path generated = outputDir.resolve("PayrollJobConfiguration.java");
        String content = Files.readString(generated);

        assertThat(content).contains("FlatFileItemReaderBuilder");
        assertThat(content).contains("FlatFileItemWriterBuilder");
        assertThat(content).contains("chunk(100");
        assertThat(content).contains("ItemReader<PayrollRecRecord>");
        assertThat(content).contains("PayrollProcessor");
    }
}
