// Generated from com/cobol/translator/grammar/Cobol.g4 by ANTLR 4.13.1
package com.cobol.translator.grammar;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link CobolParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface CobolVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link CobolParser#compilationUnit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCompilationUnit(CobolParser.CompilationUnitContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#identificationDivision}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdentificationDivision(CobolParser.IdentificationDivisionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#programId}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgramId(CobolParser.ProgramIdContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#programName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgramName(CobolParser.ProgramNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#authorParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAuthorParagraph(CobolParser.AuthorParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#installationParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInstallationParagraph(CobolParser.InstallationParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#dateWrittenParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDateWrittenParagraph(CobolParser.DateWrittenParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#dateCompiledParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDateCompiledParagraph(CobolParser.DateCompiledParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#securityParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSecurityParagraph(CobolParser.SecurityParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#commentEntry}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommentEntry(CobolParser.CommentEntryContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#environmentDivision}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnvironmentDivision(CobolParser.EnvironmentDivisionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#configurationSection}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConfigurationSection(CobolParser.ConfigurationSectionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#sourceComputerParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSourceComputerParagraph(CobolParser.SourceComputerParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#objectComputerParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitObjectComputerParagraph(CobolParser.ObjectComputerParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#computerName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComputerName(CobolParser.ComputerNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#programCollatingSequence}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgramCollatingSequence(CobolParser.ProgramCollatingSequenceContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#alphabetName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAlphabetName(CobolParser.AlphabetNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#segmentLimitClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSegmentLimitClause(CobolParser.SegmentLimitClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#specialNamesParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSpecialNamesParagraph(CobolParser.SpecialNamesParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#specialNameClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSpecialNameClause(CobolParser.SpecialNameClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#environmentName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnvironmentName(CobolParser.EnvironmentNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#mnemonicName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMnemonicName(CobolParser.MnemonicNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#alphabetClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAlphabetClause(CobolParser.AlphabetClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#classClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassClause(CobolParser.ClassClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#className}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassName(CobolParser.ClassNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#classValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassValue(CobolParser.ClassValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#currencySignClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCurrencySignClause(CobolParser.CurrencySignClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#decimalPointClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecimalPointClause(CobolParser.DecimalPointClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#inputOutputSection}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInputOutputSection(CobolParser.InputOutputSectionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#fileControlParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFileControlParagraph(CobolParser.FileControlParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#selectClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelectClause(CobolParser.SelectClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#fileName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFileName(CobolParser.FileNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#assignClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssignClause(CobolParser.AssignClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#organizationClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOrganizationClause(CobolParser.OrganizationClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#accessModeClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAccessModeClause(CobolParser.AccessModeClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#recordKeyClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRecordKeyClause(CobolParser.RecordKeyClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#alternateRecordKeyClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAlternateRecordKeyClause(CobolParser.AlternateRecordKeyClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#fileStatusClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFileStatusClause(CobolParser.FileStatusClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#ioControlParagraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIoControlParagraph(CobolParser.IoControlParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#rerunClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRerunClause(CobolParser.RerunClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#implementationName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplementationName(CobolParser.ImplementationNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#endOfReel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEndOfReel(CobolParser.EndOfReelContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#sameAreaClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSameAreaClause(CobolParser.SameAreaClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#multipleFileClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultipleFileClause(CobolParser.MultipleFileClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#multipleFilePosition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultipleFilePosition(CobolParser.MultipleFilePositionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#dataDivision}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataDivision(CobolParser.DataDivisionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#fileSection}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFileSection(CobolParser.FileSectionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#fileDescriptionEntry}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFileDescriptionEntry(CobolParser.FileDescriptionEntryContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#blockContainsClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockContainsClause(CobolParser.BlockContainsClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#recordContainsClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRecordContainsClause(CobolParser.RecordContainsClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#labelRecordsClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLabelRecordsClause(CobolParser.LabelRecordsClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#valueOfClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValueOfClause(CobolParser.ValueOfClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#dataRecordsClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataRecordsClause(CobolParser.DataRecordsClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#linageClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLinageClause(CobolParser.LinageClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#recordingModeClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRecordingModeClause(CobolParser.RecordingModeClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#codeSetClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCodeSetClause(CobolParser.CodeSetClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#workingStorageSection}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWorkingStorageSection(CobolParser.WorkingStorageSectionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#linkageSection}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLinkageSection(CobolParser.LinkageSectionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#screenSection}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitScreenSection(CobolParser.ScreenSectionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#dataDescriptionEntry}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataDescriptionEntry(CobolParser.DataDescriptionEntryContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#levelNumber}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLevelNumber(CobolParser.LevelNumberContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#dataName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataName(CobolParser.DataNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#redefinesClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRedefinesClause(CobolParser.RedefinesClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#blankWhenZeroClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlankWhenZeroClause(CobolParser.BlankWhenZeroClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#externalClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExternalClause(CobolParser.ExternalClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#globalClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGlobalClause(CobolParser.GlobalClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#justifiedClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJustifiedClause(CobolParser.JustifiedClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#occursClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOccursClause(CobolParser.OccursClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#occursDependingClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOccursDependingClause(CobolParser.OccursDependingClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#ascendingDescendingKeyClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAscendingDescendingKeyClause(CobolParser.AscendingDescendingKeyClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#indexedByClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndexedByClause(CobolParser.IndexedByClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#indexName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndexName(CobolParser.IndexNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#pictureClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPictureClause(CobolParser.PictureClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#pictureString}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPictureString(CobolParser.PictureStringContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#signClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSignClause(CobolParser.SignClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#synchronizedClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSynchronizedClause(CobolParser.SynchronizedClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#usageClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUsageClause(CobolParser.UsageClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#valueClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValueClause(CobolParser.ValueClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#procedureDivision}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcedureDivision(CobolParser.ProcedureDivisionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#usingClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUsingClause(CobolParser.UsingClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#returningClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReturningClause(CobolParser.ReturningClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#declaratives}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDeclaratives(CobolParser.DeclarativesContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#procedureSection}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcedureSection(CobolParser.ProcedureSectionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#sectionName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSectionName(CobolParser.SectionNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#paragraph}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParagraph(CobolParser.ParagraphContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#sentence}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSentence(CobolParser.SentenceContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#paragraphName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParagraphName(CobolParser.ParagraphNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatement(CobolParser.StatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#acceptStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAcceptStatement(CobolParser.AcceptStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#addStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAddStatement(CobolParser.AddStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#alterStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAlterStatement(CobolParser.AlterStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#procedureName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcedureName(CobolParser.ProcedureNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#callStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCallStatement(CobolParser.CallStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#cancelStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCancelStatement(CobolParser.CancelStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#closeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCloseStatement(CobolParser.CloseStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#computeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComputeStatement(CobolParser.ComputeStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#arithmeticExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithmeticExpression(CobolParser.ArithmeticExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#multDivExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultDivExpression(CobolParser.MultDivExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#powerExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPowerExpression(CobolParser.PowerExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#unaryExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryExpression(CobolParser.UnaryExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#primaryExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryExpression(CobolParser.PrimaryExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#continueStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitContinueStatement(CobolParser.ContinueStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#deleteStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDeleteStatement(CobolParser.DeleteStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#displayStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDisplayStatement(CobolParser.DisplayStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#divideStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDivideStatement(CobolParser.DivideStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#evaluateStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEvaluateStatement(CobolParser.EvaluateStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#selectionSubject}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelectionSubject(CobolParser.SelectionSubjectContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#whenPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhenPhrase(CobolParser.WhenPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#whenOtherPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhenOtherPhrase(CobolParser.WhenOtherPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#selectionObject}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelectionObject(CobolParser.SelectionObjectContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#exitStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExitStatement(CobolParser.ExitStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#gobackStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGobackStatement(CobolParser.GobackStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#gotoStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGotoStatement(CobolParser.GotoStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#ifStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfStatement(CobolParser.IfStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#elseClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElseClause(CobolParser.ElseClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#condition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCondition(CobolParser.ConditionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#combinableCondition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCombinableCondition(CobolParser.CombinableConditionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#simpleCondition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSimpleCondition(CobolParser.SimpleConditionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#relationCondition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRelationCondition(CobolParser.RelationConditionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#classCondition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassCondition(CobolParser.ClassConditionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#signCondition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSignCondition(CobolParser.SignConditionContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#initializeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInitializeStatement(CobolParser.InitializeStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#replacingClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReplacingClause(CobolParser.ReplacingClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#inspectStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInspectStatement(CobolParser.InspectStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#tallyingPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTallyingPhrase(CobolParser.TallyingPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#replacingPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReplacingPhrase(CobolParser.ReplacingPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#convertingPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConvertingPhrase(CobolParser.ConvertingPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#beforeAfterPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBeforeAfterPhrase(CobolParser.BeforeAfterPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#mergeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMergeStatement(CobolParser.MergeStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#moveStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMoveStatement(CobolParser.MoveStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#multiplyStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultiplyStatement(CobolParser.MultiplyStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#openStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOpenStatement(CobolParser.OpenStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#performStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPerformStatement(CobolParser.PerformStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#timesPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTimesPhrase(CobolParser.TimesPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#untilPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUntilPhrase(CobolParser.UntilPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#varyingPhrase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVaryingPhrase(CobolParser.VaryingPhraseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#readStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReadStatement(CobolParser.ReadStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#atEndClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAtEndClause(CobolParser.AtEndClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#notAtEndClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNotAtEndClause(CobolParser.NotAtEndClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#releaseStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReleaseStatement(CobolParser.ReleaseStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#recordName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRecordName(CobolParser.RecordNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#returnStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReturnStatement(CobolParser.ReturnStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#rewriteStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRewriteStatement(CobolParser.RewriteStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#searchStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSearchStatement(CobolParser.SearchStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#setStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSetStatement(CobolParser.SetStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#sortStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSortStatement(CobolParser.SortStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#startStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStartStatement(CobolParser.StartStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#stopStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStopStatement(CobolParser.StopStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#stringStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStringStatement(CobolParser.StringStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#subtractStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubtractStatement(CobolParser.SubtractStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#unstringStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnstringStatement(CobolParser.UnstringStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#useStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUseStatement(CobolParser.UseStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#writeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWriteStatement(CobolParser.WriteStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#atEndOfPageClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAtEndOfPageClause(CobolParser.AtEndOfPageClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#notAtEndOfPageClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNotAtEndOfPageClause(CobolParser.NotAtEndOfPageClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#onSizeErrorClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOnSizeErrorClause(CobolParser.OnSizeErrorClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#notOnSizeErrorClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNotOnSizeErrorClause(CobolParser.NotOnSizeErrorClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#onOverflowClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOnOverflowClause(CobolParser.OnOverflowClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#notOnOverflowClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNotOnOverflowClause(CobolParser.NotOnOverflowClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#onExceptionClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOnExceptionClause(CobolParser.OnExceptionClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#notOnExceptionClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNotOnExceptionClause(CobolParser.NotOnExceptionClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#identifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdentifier(CobolParser.IdentifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobolParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteral(CobolParser.LiteralContext ctx);
}