package com.cobol.translator.ast;

/**
 * Visitor interface for traversing the AST.
 * Implements the Visitor pattern for processing different node types.
 *
 * @param <T> The return type of visit methods
 */
public interface ASTVisitor<T> {

    // Program structure
    T visitProgramNode(ProgramNode node);
    T visitIdentificationDivisionNode(IdentificationDivisionNode node);
    T visitEnvironmentDivisionNode(EnvironmentDivisionNode node);
    T visitDataDivisionNode(DataDivisionNode node);
    T visitProcedureDivisionNode(ProcedureDivisionNode node);

    // Data Division sections
    T visitFileSectionNode(FileSectionNode node);
    T visitWorkingStorageSectionNode(WorkingStorageSectionNode node);
    T visitLinkageSectionNode(LinkageSectionNode node);

    // Data items
    T visitDataItemNode(DataItemNode node);
    T visitFileDescriptionNode(FileDescriptionNode node);

    // Procedure Division
    T visitSectionNode(SectionNode node);
    T visitParagraphNode(ParagraphNode node);

    // Statements
    T visitAcceptStatementNode(AcceptStatementNode node);
    T visitAddStatementNode(AddStatementNode node);
    T visitCallStatementNode(CallStatementNode node);
    T visitCloseStatementNode(CloseStatementNode node);
    T visitComputeStatementNode(ComputeStatementNode node);
    T visitContinueStatementNode(ContinueStatementNode node);
    T visitDeleteStatementNode(DeleteStatementNode node);
    T visitDisplayStatementNode(DisplayStatementNode node);
    T visitDivideStatementNode(DivideStatementNode node);
    T visitEvaluateStatementNode(EvaluateStatementNode node);
    T visitExitStatementNode(ExitStatementNode node);
    T visitGobackStatementNode(GobackStatementNode node);
    T visitGotoStatementNode(GotoStatementNode node);
    T visitIfStatementNode(IfStatementNode node);
    T visitInitializeStatementNode(InitializeStatementNode node);
    T visitInspectStatementNode(InspectStatementNode node);
    T visitMoveStatementNode(MoveStatementNode node);
    T visitMultiplyStatementNode(MultiplyStatementNode node);
    T visitOpenStatementNode(OpenStatementNode node);
    T visitPerformStatementNode(PerformStatementNode node);
    T visitReadStatementNode(ReadStatementNode node);
    T visitRewriteStatementNode(RewriteStatementNode node);
    T visitSearchStatementNode(SearchStatementNode node);
    T visitSetStatementNode(SetStatementNode node);
    T visitSortStatementNode(SortStatementNode node);
    T visitStopStatementNode(StopStatementNode node);
    T visitStringStatementNode(StringStatementNode node);
    T visitSubtractStatementNode(SubtractStatementNode node);
    T visitUnstringStatementNode(UnstringStatementNode node);
    T visitWriteStatementNode(WriteStatementNode node);

    // Expressions
    T visitArithmeticExpressionNode(ArithmeticExpressionNode node);
    T visitConditionNode(ConditionNode node);
    T visitLiteralNode(LiteralNode node);
    T visitIdentifierNode(IdentifierNode node);

    // Default visit for unknown nodes
    default T visitDefault(ASTNode node) {
        return null;
    }
}
