package com.cobol.translator.parser;

import com.cobol.translator.ast.*;
import com.cobol.translator.grammar.CobolBaseVisitor;
import com.cobol.translator.grammar.CobolParser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Builds an AST from ANTLR4 parse tree.
 * Transforms the ANTLR4 ParseTree into our custom AST representation.
 */
public class CobolASTBuilder extends CobolBaseVisitor<ASTNode> {

    private static final Logger logger = LoggerFactory.getLogger(CobolASTBuilder.class);

    /**
     * Build AST from compilation unit (program root)
     */
    @Override
    public ASTNode visitCompilationUnit(CobolParser.CompilationUnitContext ctx) {
        logger.debug("Building AST from compilation unit");

        // Visit identification division to get program name
        String programName = "UNKNOWN";
        if (ctx.identificationDivision() != null) {
            CobolParser.IdentificationDivisionContext idDiv = ctx.identificationDivision();
            if (idDiv.programId() != null && idDiv.programId().programName() != null) {
                programName = idDiv.programId().programName().getText();
            }
        }

        ProgramNode programNode = new ProgramNode(programName);
        programNode.setLineNumber(ctx.getStart().getLine());
        programNode.setColumnNumber(ctx.getStart().getCharPositionInLine());

        // Process each division
        if (ctx.identificationDivision() != null) {
            IdentificationDivisionNode idDivNode = (IdentificationDivisionNode) visit(ctx.identificationDivision());
            programNode.setIdentificationDivision(idDivNode);
        }

        if (ctx.environmentDivision() != null) {
            EnvironmentDivisionNode envDivNode = (EnvironmentDivisionNode) visit(ctx.environmentDivision());
            programNode.setEnvironmentDivision(envDivNode);
        }

        if (ctx.dataDivision() != null) {
            DataDivisionNode dataDivNode = (DataDivisionNode) visit(ctx.dataDivision());
            programNode.setDataDivision(dataDivNode);
        }

        if (ctx.procedureDivision() != null) {
            ProcedureDivisionNode procDivNode = (ProcedureDivisionNode) visit(ctx.procedureDivision());
            programNode.setProcedureDivision(procDivNode);
        }

        logger.info("Built AST for program: {}", programName);
        return programNode;
    }

    /**
     * Build Identification Division node
     */
    @Override
    public ASTNode visitIdentificationDivision(CobolParser.IdentificationDivisionContext ctx) {
        String programId = "UNKNOWN";
        if (ctx.programId() != null && ctx.programId().programName() != null) {
            programId = ctx.programId().programName().getText();
        }

        IdentificationDivisionNode node = new IdentificationDivisionNode(programId);
        node.setLineNumber(ctx.getStart().getLine());

        // Extract author, date, security if present
        if (ctx.authorParagraph() != null && !ctx.authorParagraph().isEmpty()) {
            node.setAuthor(ctx.authorParagraph(0).getText());
        }
        if (ctx.dateWrittenParagraph() != null && !ctx.dateWrittenParagraph().isEmpty()) {
            node.setDateWritten(ctx.dateWrittenParagraph(0).getText());
        }
        if (ctx.securityParagraph() != null && !ctx.securityParagraph().isEmpty()) {
            node.setSecurity(ctx.securityParagraph(0).getText());
        }

        return node;
    }

    /**
     * Build Environment Division node
     */
    @Override
    public ASTNode visitEnvironmentDivision(CobolParser.EnvironmentDivisionContext ctx) {
        EnvironmentDivisionNode node = new EnvironmentDivisionNode();
        node.setLineNumber(ctx.getStart().getLine());
        return node;
    }

    /**
     * Build Data Division node
     */
    @Override
    public ASTNode visitDataDivision(CobolParser.DataDivisionContext ctx) {
        DataDivisionNode node = new DataDivisionNode();
        node.setLineNumber(ctx.getStart().getLine());

        // Process File Section
        if (ctx.fileSection() != null && !ctx.fileSection().isEmpty()) {
            FileSectionNode fileSectionNode = (FileSectionNode) visit(ctx.fileSection(0));
            node.setFileSection(fileSectionNode);
        }

        // Process Working-Storage Section
        if (ctx.workingStorageSection() != null && !ctx.workingStorageSection().isEmpty()) {
            WorkingStorageSectionNode wssNode = (WorkingStorageSectionNode) visit(ctx.workingStorageSection(0));
            node.setWorkingStorageSection(wssNode);
        }

        // Process Linkage Section
        if (ctx.linkageSection() != null && !ctx.linkageSection().isEmpty()) {
            LinkageSectionNode linkageNode = (LinkageSectionNode) visit(ctx.linkageSection(0));
            node.setLinkageSection(linkageNode);
        }

        return node;
    }

    /**
     * Build File Section node
     */
    @Override
    public ASTNode visitFileSection(CobolParser.FileSectionContext ctx) {
        FileSectionNode node = new FileSectionNode();
        node.setLineNumber(ctx.getStart().getLine());

        // Process each file description
        if (ctx.fileDescriptionEntry() != null) {
            for (CobolParser.FileDescriptionEntryContext fdCtx : ctx.fileDescriptionEntry()) {
                FileDescriptionNode fdNode = (FileDescriptionNode) visit(fdCtx);
                if (fdNode != null) {
                    node.addFileDescription(fdNode);
                }
            }
        }

        return node;
    }

    /**
     * Build File Description node
     */
    @Override
    public ASTNode visitFileDescriptionEntry(CobolParser.FileDescriptionEntryContext ctx) {
        String fileName = ctx.fileName() != null ? ctx.fileName().getText() : "UNKNOWN";
        FileDescriptionNode node = new FileDescriptionNode(fileName);
        node.setLineNumber(ctx.getStart().getLine());

        // Process data description entries (record layouts)
        if (ctx.dataDescriptionEntry() != null) {
            for (CobolParser.DataDescriptionEntryContext ddCtx : ctx.dataDescriptionEntry()) {
                DataItemNode dataItem = (DataItemNode) visit(ddCtx);
                if (dataItem != null) {
                    node.addRecord(dataItem);
                }
            }
        }

        return node;
    }

    /**
     * Build Working-Storage Section node
     */
    @Override
    public ASTNode visitWorkingStorageSection(CobolParser.WorkingStorageSectionContext ctx) {
        WorkingStorageSectionNode node = new WorkingStorageSectionNode();
        node.setLineNumber(ctx.getStart().getLine());

        // Process data items
        if (ctx.dataDescriptionEntry() != null) {
            for (CobolParser.DataDescriptionEntryContext ddCtx : ctx.dataDescriptionEntry()) {
                DataItemNode dataItem = (DataItemNode) visit(ddCtx);
                if (dataItem != null) {
                    node.addDataItem(dataItem);
                }
            }
        }

        return node;
    }

    /**
     * Build Linkage Section node
     */
    @Override
    public ASTNode visitLinkageSection(CobolParser.LinkageSectionContext ctx) {
        LinkageSectionNode node = new LinkageSectionNode();
        node.setLineNumber(ctx.getStart().getLine());

        // Process data items
        if (ctx.dataDescriptionEntry() != null) {
            for (CobolParser.DataDescriptionEntryContext ddCtx : ctx.dataDescriptionEntry()) {
                DataItemNode dataItem = (DataItemNode) visit(ddCtx);
                if (dataItem != null) {
                    node.addDataItem(dataItem);
                }
            }
        }

        return node;
    }

    /**
     * Build Data Item node
     */
    @Override
    public ASTNode visitDataDescriptionEntry(CobolParser.DataDescriptionEntryContext ctx) {
        // Extract level number
        int level = parseLevelNumber(ctx.levelNumber());

        // Extract data name
        String name = "FILLER";
        if (ctx.dataName() != null) {
            name = ctx.dataName().getText();
        }

        DataItemNode node = new DataItemNode(level, name);
        node.setLineNumber(ctx.getStart().getLine());

        // Extract PICTURE clause
        if (ctx.pictureClause() != null && !ctx.pictureClause().isEmpty()) {
            String picture = ctx.pictureClause(0).pictureString().getText();
            node.setPicture(picture);
        }

        // Extract VALUE clause
        if (ctx.valueClause() != null && !ctx.valueClause().isEmpty()) {
            String value = ctx.valueClause(0).literal().getText();
            node.setValue(value);
        }

        // Extract REDEFINES clause
        if (ctx.redefinesClause() != null && !ctx.redefinesClause().isEmpty()) {
            String redefines = ctx.redefinesClause(0).dataName().getText();
            node.setRedefines(redefines);
        }

        return node;
    }

    /**
     * Build Procedure Division node
     */
    @Override
    public ASTNode visitProcedureDivision(CobolParser.ProcedureDivisionContext ctx) {
        ProcedureDivisionNode node = new ProcedureDivisionNode();
        node.setLineNumber(ctx.getStart().getLine());

        // Process sections
        if (ctx.procedureSection() != null) {
            for (CobolParser.ProcedureSectionContext sectionCtx : ctx.procedureSection()) {
                SectionNode sectionNode = (SectionNode) visit(sectionCtx);
                if (sectionNode != null) {
                    node.addSection(sectionNode);
                }
            }
        }

        // Process paragraphs
        if (ctx.paragraph() != null) {
            for (CobolParser.ParagraphContext paragraphCtx : ctx.paragraph()) {
                ParagraphNode paragraphNode = (ParagraphNode) visit(paragraphCtx);
                if (paragraphNode != null) {
                    node.addParagraph(paragraphNode);
                }
            }
        }

        return node;
    }

    /**
     * Build Section node
     */
    @Override
    public ASTNode visitProcedureSection(CobolParser.ProcedureSectionContext ctx) {
        String sectionName = ctx.sectionName() != null ? ctx.sectionName().getText() : "UNKNOWN";
        SectionNode node = new SectionNode(sectionName);
        node.setLineNumber(ctx.getStart().getLine());

        // Process paragraphs within section
        if (ctx.paragraph() != null) {
            for (CobolParser.ParagraphContext paragraphCtx : ctx.paragraph()) {
                ParagraphNode paragraphNode = (ParagraphNode) visit(paragraphCtx);
                if (paragraphNode != null) {
                    node.addParagraph(paragraphNode);
                }
            }
        }

        return node;
    }

    /**
     * Build Paragraph node
     */
    @Override
    public ASTNode visitParagraph(CobolParser.ParagraphContext ctx) {
        String paragraphName = ctx.paragraphName() != null ? ctx.paragraphName().getText() : "UNKNOWN";
        ParagraphNode node = new ParagraphNode(paragraphName);
        node.setLineNumber(ctx.getStart().getLine());

        // Process sentences (which contain statements)
        if (ctx.sentence() != null) {
            for (CobolParser.SentenceContext sentenceCtx : ctx.sentence()) {
                // Process each statement within the sentence
                if (sentenceCtx.statement() != null) {
                    for (CobolParser.StatementContext stmtCtx : sentenceCtx.statement()) {
                        StatementNode stmtNode = (StatementNode) visitStatement(stmtCtx);
                        if (stmtNode != null) {
                            node.addStatement(stmtNode);
                        }
                    }
                }
            }
        }

        return node;
    }

    /**
     * Build Statement node (delegates to specific statement types)
     */
    @Override
    public ASTNode visitStatement(CobolParser.StatementContext ctx) {
        StatementNode node = null;

        // Determine statement type and create appropriate node
        if (ctx.moveStatement() != null) {
            node = new MoveStatementNode();
        } else if (ctx.displayStatement() != null) {
            node = new DisplayStatementNode();
        } else if (ctx.addStatement() != null) {
            node = new AddStatementNode();
        } else if (ctx.subtractStatement() != null) {
            node = new SubtractStatementNode();
        } else if (ctx.multiplyStatement() != null) {
            node = new MultiplyStatementNode();
        } else if (ctx.divideStatement() != null) {
            node = new DivideStatementNode();
        } else if (ctx.computeStatement() != null) {
            node = new ComputeStatementNode();
        } else if (ctx.ifStatement() != null) {
            node = new IfStatementNode();
        } else if (ctx.performStatement() != null) {
            node = new PerformStatementNode();
        } else if (ctx.readStatement() != null) {
            node = new ReadStatementNode();
        } else if (ctx.writeStatement() != null) {
            node = new WriteStatementNode();
        } else if (ctx.openStatement() != null) {
            node = new OpenStatementNode();
        } else if (ctx.closeStatement() != null) {
            node = new CloseStatementNode();
        } else if (ctx.stopStatement() != null) {
            node = new StopStatementNode();
        } else if (ctx.exitStatement() != null) {
            node = new ExitStatementNode();
        } else if (ctx.gobackStatement() != null) {
            node = new GobackStatementNode();
        } else if (ctx.acceptStatement() != null) {
            node = new AcceptStatementNode();
        } else if (ctx.callStatement() != null) {
            node = new CallStatementNode();
        } else if (ctx.evaluateStatement() != null) {
            node = new EvaluateStatementNode();
        } else if (ctx.searchStatement() != null) {
            node = new SearchStatementNode();
        } else if (ctx.sortStatement() != null) {
            node = new SortStatementNode();
        } else if (ctx.continueStatement() != null) {
            node = new ContinueStatementNode();
        } else if (ctx.initializeStatement() != null) {
            node = new InitializeStatementNode();
        } else if (ctx.inspectStatement() != null) {
            node = new InspectStatementNode();
        } else if (ctx.stringStatement() != null) {
            node = new StringStatementNode();
        } else if (ctx.unstringStatement() != null) {
            node = new UnstringStatementNode();
        } else if (ctx.setStatement() != null) {
            node = new SetStatementNode();
        } else if (ctx.deleteStatement() != null) {
            node = new DeleteStatementNode();
        } else if (ctx.rewriteStatement() != null) {
            node = new RewriteStatementNode();
        } else if (ctx.gotoStatement() != null) {
            node = new GotoStatementNode();
        }

        if (node != null) {
            node.setLineNumber(ctx.getStart().getLine());
            node.setColumnNumber(ctx.getStart().getCharPositionInLine());
            node.setOriginalCobol(ctx.getText());
        }

        return node;
    }

    /**
     * Parse level number from context
     */
    private int parseLevelNumber(CobolParser.LevelNumberContext ctx) {
        if (ctx == null) return 1;

        String levelText = ctx.getText();
        try {
            return Integer.parseInt(levelText);
        } catch (NumberFormatException e) {
            logger.warn("Failed to parse level number: {}", levelText);
            return 1;
        }
    }
}
