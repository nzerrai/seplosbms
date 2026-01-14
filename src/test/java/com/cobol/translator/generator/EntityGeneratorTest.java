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

class EntityGeneratorTest {

    @TempDir
    Path tempDir;

    @Test
    void generatesFieldsOnlyFromRecordLayoutScope() throws IOException {
        // Build a program with two 01-level records to ensure scoping stops at the next top-level item
        CobolProgram program = new CobolProgram("TESTPROG");

        DataItem record1 = new DataItem(1, "REC-1");
        record1.setGroup(true);
        program.addDataItem(record1);

        DataItem fieldA = new DataItem(5, "FIELD-A");
        fieldA.setPictureClause("X(10)");
        fieldA.setJavaType("String");
        fieldA.setJavaFieldName("fieldA");
        program.addDataItem(fieldA);

        DataItem filler = new DataItem(5, "FILLER-1");
        filler.setFiller(true);
        filler.setPictureClause("X(2)");
        filler.setJavaType("String");
        filler.setJavaFieldName("filler1");
        program.addDataItem(filler);

        DataItem record2 = new DataItem(1, "OTHER-REC");
        record2.setGroup(true);
        program.addDataItem(record2);

        DataItem fieldB = new DataItem(5, "FIELD-B");
        fieldB.setPictureClause("X(5)");
        fieldB.setJavaType("String");
        fieldB.setJavaFieldName("fieldB");
        program.addDataItem(fieldB);

        FileDefinition fd = new FileDefinition("REC-1");
        fd.setRecordLayout(record1);
        program.setFileDefinitions(List.of(fd));

        // Prepare output dir with src/main/java to let package derivation work
        Path outputDir = tempDir.resolve("src/main/java/com/example/model");
        Files.createDirectories(outputDir);

        TranslationConfig config = TranslationConfig.builder()
                .sourceFile("REC1.cob")
                .outputPackage("com.example")
                .build();

        EntityGenerator generator = new EntityGenerator();
        generator.generate(program, config, outputDir);

        Path generated = outputDir.resolve("Rec1Record.java");
        assertThat(Files.exists(generated)).isTrue();

        String content = Files.readString(generated);
        // Should contain fields from REC-1 only
        assertThat(content).contains("private String fieldA;");
        assertThat(content).contains("FILLER field");
        assertThat(content).doesNotContain("fieldB");
    }
}
