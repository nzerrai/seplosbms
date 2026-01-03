// Generated from com/cobol/translator/grammar/Cobol.g4 by ANTLR 4.13.1
package com.cobol.translator.grammar;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link CobolParser}.
 */
public interface CobolListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link CobolParser#compilationUnit}.
	 * @param ctx the parse tree
	 */
	void enterCompilationUnit(CobolParser.CompilationUnitContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#compilationUnit}.
	 * @param ctx the parse tree
	 */
	void exitCompilationUnit(CobolParser.CompilationUnitContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#identificationDivision}.
	 * @param ctx the parse tree
	 */
	void enterIdentificationDivision(CobolParser.IdentificationDivisionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#identificationDivision}.
	 * @param ctx the parse tree
	 */
	void exitIdentificationDivision(CobolParser.IdentificationDivisionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#programId}.
	 * @param ctx the parse tree
	 */
	void enterProgramId(CobolParser.ProgramIdContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#programId}.
	 * @param ctx the parse tree
	 */
	void exitProgramId(CobolParser.ProgramIdContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#programName}.
	 * @param ctx the parse tree
	 */
	void enterProgramName(CobolParser.ProgramNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#programName}.
	 * @param ctx the parse tree
	 */
	void exitProgramName(CobolParser.ProgramNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#authorParagraph}.
	 * @param ctx the parse tree
	 */
	void enterAuthorParagraph(CobolParser.AuthorParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#authorParagraph}.
	 * @param ctx the parse tree
	 */
	void exitAuthorParagraph(CobolParser.AuthorParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#installationParagraph}.
	 * @param ctx the parse tree
	 */
	void enterInstallationParagraph(CobolParser.InstallationParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#installationParagraph}.
	 * @param ctx the parse tree
	 */
	void exitInstallationParagraph(CobolParser.InstallationParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#dateWrittenParagraph}.
	 * @param ctx the parse tree
	 */
	void enterDateWrittenParagraph(CobolParser.DateWrittenParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#dateWrittenParagraph}.
	 * @param ctx the parse tree
	 */
	void exitDateWrittenParagraph(CobolParser.DateWrittenParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#dateCompiledParagraph}.
	 * @param ctx the parse tree
	 */
	void enterDateCompiledParagraph(CobolParser.DateCompiledParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#dateCompiledParagraph}.
	 * @param ctx the parse tree
	 */
	void exitDateCompiledParagraph(CobolParser.DateCompiledParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#securityParagraph}.
	 * @param ctx the parse tree
	 */
	void enterSecurityParagraph(CobolParser.SecurityParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#securityParagraph}.
	 * @param ctx the parse tree
	 */
	void exitSecurityParagraph(CobolParser.SecurityParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#commentEntry}.
	 * @param ctx the parse tree
	 */
	void enterCommentEntry(CobolParser.CommentEntryContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#commentEntry}.
	 * @param ctx the parse tree
	 */
	void exitCommentEntry(CobolParser.CommentEntryContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#environmentDivision}.
	 * @param ctx the parse tree
	 */
	void enterEnvironmentDivision(CobolParser.EnvironmentDivisionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#environmentDivision}.
	 * @param ctx the parse tree
	 */
	void exitEnvironmentDivision(CobolParser.EnvironmentDivisionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#configurationSection}.
	 * @param ctx the parse tree
	 */
	void enterConfigurationSection(CobolParser.ConfigurationSectionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#configurationSection}.
	 * @param ctx the parse tree
	 */
	void exitConfigurationSection(CobolParser.ConfigurationSectionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#sourceComputerParagraph}.
	 * @param ctx the parse tree
	 */
	void enterSourceComputerParagraph(CobolParser.SourceComputerParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#sourceComputerParagraph}.
	 * @param ctx the parse tree
	 */
	void exitSourceComputerParagraph(CobolParser.SourceComputerParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#objectComputerParagraph}.
	 * @param ctx the parse tree
	 */
	void enterObjectComputerParagraph(CobolParser.ObjectComputerParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#objectComputerParagraph}.
	 * @param ctx the parse tree
	 */
	void exitObjectComputerParagraph(CobolParser.ObjectComputerParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#computerName}.
	 * @param ctx the parse tree
	 */
	void enterComputerName(CobolParser.ComputerNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#computerName}.
	 * @param ctx the parse tree
	 */
	void exitComputerName(CobolParser.ComputerNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#programCollatingSequence}.
	 * @param ctx the parse tree
	 */
	void enterProgramCollatingSequence(CobolParser.ProgramCollatingSequenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#programCollatingSequence}.
	 * @param ctx the parse tree
	 */
	void exitProgramCollatingSequence(CobolParser.ProgramCollatingSequenceContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#alphabetName}.
	 * @param ctx the parse tree
	 */
	void enterAlphabetName(CobolParser.AlphabetNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#alphabetName}.
	 * @param ctx the parse tree
	 */
	void exitAlphabetName(CobolParser.AlphabetNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#segmentLimitClause}.
	 * @param ctx the parse tree
	 */
	void enterSegmentLimitClause(CobolParser.SegmentLimitClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#segmentLimitClause}.
	 * @param ctx the parse tree
	 */
	void exitSegmentLimitClause(CobolParser.SegmentLimitClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#specialNamesParagraph}.
	 * @param ctx the parse tree
	 */
	void enterSpecialNamesParagraph(CobolParser.SpecialNamesParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#specialNamesParagraph}.
	 * @param ctx the parse tree
	 */
	void exitSpecialNamesParagraph(CobolParser.SpecialNamesParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#specialNameClause}.
	 * @param ctx the parse tree
	 */
	void enterSpecialNameClause(CobolParser.SpecialNameClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#specialNameClause}.
	 * @param ctx the parse tree
	 */
	void exitSpecialNameClause(CobolParser.SpecialNameClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#environmentName}.
	 * @param ctx the parse tree
	 */
	void enterEnvironmentName(CobolParser.EnvironmentNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#environmentName}.
	 * @param ctx the parse tree
	 */
	void exitEnvironmentName(CobolParser.EnvironmentNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#mnemonicName}.
	 * @param ctx the parse tree
	 */
	void enterMnemonicName(CobolParser.MnemonicNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#mnemonicName}.
	 * @param ctx the parse tree
	 */
	void exitMnemonicName(CobolParser.MnemonicNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#alphabetClause}.
	 * @param ctx the parse tree
	 */
	void enterAlphabetClause(CobolParser.AlphabetClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#alphabetClause}.
	 * @param ctx the parse tree
	 */
	void exitAlphabetClause(CobolParser.AlphabetClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#classClause}.
	 * @param ctx the parse tree
	 */
	void enterClassClause(CobolParser.ClassClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#classClause}.
	 * @param ctx the parse tree
	 */
	void exitClassClause(CobolParser.ClassClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#className}.
	 * @param ctx the parse tree
	 */
	void enterClassName(CobolParser.ClassNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#className}.
	 * @param ctx the parse tree
	 */
	void exitClassName(CobolParser.ClassNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#classValue}.
	 * @param ctx the parse tree
	 */
	void enterClassValue(CobolParser.ClassValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#classValue}.
	 * @param ctx the parse tree
	 */
	void exitClassValue(CobolParser.ClassValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#currencySignClause}.
	 * @param ctx the parse tree
	 */
	void enterCurrencySignClause(CobolParser.CurrencySignClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#currencySignClause}.
	 * @param ctx the parse tree
	 */
	void exitCurrencySignClause(CobolParser.CurrencySignClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#decimalPointClause}.
	 * @param ctx the parse tree
	 */
	void enterDecimalPointClause(CobolParser.DecimalPointClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#decimalPointClause}.
	 * @param ctx the parse tree
	 */
	void exitDecimalPointClause(CobolParser.DecimalPointClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#inputOutputSection}.
	 * @param ctx the parse tree
	 */
	void enterInputOutputSection(CobolParser.InputOutputSectionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#inputOutputSection}.
	 * @param ctx the parse tree
	 */
	void exitInputOutputSection(CobolParser.InputOutputSectionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#fileControlParagraph}.
	 * @param ctx the parse tree
	 */
	void enterFileControlParagraph(CobolParser.FileControlParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#fileControlParagraph}.
	 * @param ctx the parse tree
	 */
	void exitFileControlParagraph(CobolParser.FileControlParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#selectClause}.
	 * @param ctx the parse tree
	 */
	void enterSelectClause(CobolParser.SelectClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#selectClause}.
	 * @param ctx the parse tree
	 */
	void exitSelectClause(CobolParser.SelectClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#fileName}.
	 * @param ctx the parse tree
	 */
	void enterFileName(CobolParser.FileNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#fileName}.
	 * @param ctx the parse tree
	 */
	void exitFileName(CobolParser.FileNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#assignClause}.
	 * @param ctx the parse tree
	 */
	void enterAssignClause(CobolParser.AssignClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#assignClause}.
	 * @param ctx the parse tree
	 */
	void exitAssignClause(CobolParser.AssignClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#organizationClause}.
	 * @param ctx the parse tree
	 */
	void enterOrganizationClause(CobolParser.OrganizationClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#organizationClause}.
	 * @param ctx the parse tree
	 */
	void exitOrganizationClause(CobolParser.OrganizationClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#accessModeClause}.
	 * @param ctx the parse tree
	 */
	void enterAccessModeClause(CobolParser.AccessModeClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#accessModeClause}.
	 * @param ctx the parse tree
	 */
	void exitAccessModeClause(CobolParser.AccessModeClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#recordKeyClause}.
	 * @param ctx the parse tree
	 */
	void enterRecordKeyClause(CobolParser.RecordKeyClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#recordKeyClause}.
	 * @param ctx the parse tree
	 */
	void exitRecordKeyClause(CobolParser.RecordKeyClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#alternateRecordKeyClause}.
	 * @param ctx the parse tree
	 */
	void enterAlternateRecordKeyClause(CobolParser.AlternateRecordKeyClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#alternateRecordKeyClause}.
	 * @param ctx the parse tree
	 */
	void exitAlternateRecordKeyClause(CobolParser.AlternateRecordKeyClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#fileStatusClause}.
	 * @param ctx the parse tree
	 */
	void enterFileStatusClause(CobolParser.FileStatusClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#fileStatusClause}.
	 * @param ctx the parse tree
	 */
	void exitFileStatusClause(CobolParser.FileStatusClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#ioControlParagraph}.
	 * @param ctx the parse tree
	 */
	void enterIoControlParagraph(CobolParser.IoControlParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#ioControlParagraph}.
	 * @param ctx the parse tree
	 */
	void exitIoControlParagraph(CobolParser.IoControlParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#rerunClause}.
	 * @param ctx the parse tree
	 */
	void enterRerunClause(CobolParser.RerunClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#rerunClause}.
	 * @param ctx the parse tree
	 */
	void exitRerunClause(CobolParser.RerunClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#implementationName}.
	 * @param ctx the parse tree
	 */
	void enterImplementationName(CobolParser.ImplementationNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#implementationName}.
	 * @param ctx the parse tree
	 */
	void exitImplementationName(CobolParser.ImplementationNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#endOfReel}.
	 * @param ctx the parse tree
	 */
	void enterEndOfReel(CobolParser.EndOfReelContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#endOfReel}.
	 * @param ctx the parse tree
	 */
	void exitEndOfReel(CobolParser.EndOfReelContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#sameAreaClause}.
	 * @param ctx the parse tree
	 */
	void enterSameAreaClause(CobolParser.SameAreaClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#sameAreaClause}.
	 * @param ctx the parse tree
	 */
	void exitSameAreaClause(CobolParser.SameAreaClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#multipleFileClause}.
	 * @param ctx the parse tree
	 */
	void enterMultipleFileClause(CobolParser.MultipleFileClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#multipleFileClause}.
	 * @param ctx the parse tree
	 */
	void exitMultipleFileClause(CobolParser.MultipleFileClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#multipleFilePosition}.
	 * @param ctx the parse tree
	 */
	void enterMultipleFilePosition(CobolParser.MultipleFilePositionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#multipleFilePosition}.
	 * @param ctx the parse tree
	 */
	void exitMultipleFilePosition(CobolParser.MultipleFilePositionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#dataDivision}.
	 * @param ctx the parse tree
	 */
	void enterDataDivision(CobolParser.DataDivisionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#dataDivision}.
	 * @param ctx the parse tree
	 */
	void exitDataDivision(CobolParser.DataDivisionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#fileSection}.
	 * @param ctx the parse tree
	 */
	void enterFileSection(CobolParser.FileSectionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#fileSection}.
	 * @param ctx the parse tree
	 */
	void exitFileSection(CobolParser.FileSectionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#fileDescriptionEntry}.
	 * @param ctx the parse tree
	 */
	void enterFileDescriptionEntry(CobolParser.FileDescriptionEntryContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#fileDescriptionEntry}.
	 * @param ctx the parse tree
	 */
	void exitFileDescriptionEntry(CobolParser.FileDescriptionEntryContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#blockContainsClause}.
	 * @param ctx the parse tree
	 */
	void enterBlockContainsClause(CobolParser.BlockContainsClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#blockContainsClause}.
	 * @param ctx the parse tree
	 */
	void exitBlockContainsClause(CobolParser.BlockContainsClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#recordContainsClause}.
	 * @param ctx the parse tree
	 */
	void enterRecordContainsClause(CobolParser.RecordContainsClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#recordContainsClause}.
	 * @param ctx the parse tree
	 */
	void exitRecordContainsClause(CobolParser.RecordContainsClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#labelRecordsClause}.
	 * @param ctx the parse tree
	 */
	void enterLabelRecordsClause(CobolParser.LabelRecordsClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#labelRecordsClause}.
	 * @param ctx the parse tree
	 */
	void exitLabelRecordsClause(CobolParser.LabelRecordsClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#valueOfClause}.
	 * @param ctx the parse tree
	 */
	void enterValueOfClause(CobolParser.ValueOfClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#valueOfClause}.
	 * @param ctx the parse tree
	 */
	void exitValueOfClause(CobolParser.ValueOfClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#dataRecordsClause}.
	 * @param ctx the parse tree
	 */
	void enterDataRecordsClause(CobolParser.DataRecordsClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#dataRecordsClause}.
	 * @param ctx the parse tree
	 */
	void exitDataRecordsClause(CobolParser.DataRecordsClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#linageClause}.
	 * @param ctx the parse tree
	 */
	void enterLinageClause(CobolParser.LinageClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#linageClause}.
	 * @param ctx the parse tree
	 */
	void exitLinageClause(CobolParser.LinageClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#recordingModeClause}.
	 * @param ctx the parse tree
	 */
	void enterRecordingModeClause(CobolParser.RecordingModeClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#recordingModeClause}.
	 * @param ctx the parse tree
	 */
	void exitRecordingModeClause(CobolParser.RecordingModeClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#codeSetClause}.
	 * @param ctx the parse tree
	 */
	void enterCodeSetClause(CobolParser.CodeSetClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#codeSetClause}.
	 * @param ctx the parse tree
	 */
	void exitCodeSetClause(CobolParser.CodeSetClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#workingStorageSection}.
	 * @param ctx the parse tree
	 */
	void enterWorkingStorageSection(CobolParser.WorkingStorageSectionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#workingStorageSection}.
	 * @param ctx the parse tree
	 */
	void exitWorkingStorageSection(CobolParser.WorkingStorageSectionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#linkageSection}.
	 * @param ctx the parse tree
	 */
	void enterLinkageSection(CobolParser.LinkageSectionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#linkageSection}.
	 * @param ctx the parse tree
	 */
	void exitLinkageSection(CobolParser.LinkageSectionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#screenSection}.
	 * @param ctx the parse tree
	 */
	void enterScreenSection(CobolParser.ScreenSectionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#screenSection}.
	 * @param ctx the parse tree
	 */
	void exitScreenSection(CobolParser.ScreenSectionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#dataDescriptionEntry}.
	 * @param ctx the parse tree
	 */
	void enterDataDescriptionEntry(CobolParser.DataDescriptionEntryContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#dataDescriptionEntry}.
	 * @param ctx the parse tree
	 */
	void exitDataDescriptionEntry(CobolParser.DataDescriptionEntryContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#levelNumber}.
	 * @param ctx the parse tree
	 */
	void enterLevelNumber(CobolParser.LevelNumberContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#levelNumber}.
	 * @param ctx the parse tree
	 */
	void exitLevelNumber(CobolParser.LevelNumberContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#dataName}.
	 * @param ctx the parse tree
	 */
	void enterDataName(CobolParser.DataNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#dataName}.
	 * @param ctx the parse tree
	 */
	void exitDataName(CobolParser.DataNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#redefinesClause}.
	 * @param ctx the parse tree
	 */
	void enterRedefinesClause(CobolParser.RedefinesClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#redefinesClause}.
	 * @param ctx the parse tree
	 */
	void exitRedefinesClause(CobolParser.RedefinesClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#blankWhenZeroClause}.
	 * @param ctx the parse tree
	 */
	void enterBlankWhenZeroClause(CobolParser.BlankWhenZeroClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#blankWhenZeroClause}.
	 * @param ctx the parse tree
	 */
	void exitBlankWhenZeroClause(CobolParser.BlankWhenZeroClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#externalClause}.
	 * @param ctx the parse tree
	 */
	void enterExternalClause(CobolParser.ExternalClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#externalClause}.
	 * @param ctx the parse tree
	 */
	void exitExternalClause(CobolParser.ExternalClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#globalClause}.
	 * @param ctx the parse tree
	 */
	void enterGlobalClause(CobolParser.GlobalClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#globalClause}.
	 * @param ctx the parse tree
	 */
	void exitGlobalClause(CobolParser.GlobalClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#justifiedClause}.
	 * @param ctx the parse tree
	 */
	void enterJustifiedClause(CobolParser.JustifiedClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#justifiedClause}.
	 * @param ctx the parse tree
	 */
	void exitJustifiedClause(CobolParser.JustifiedClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#occursClause}.
	 * @param ctx the parse tree
	 */
	void enterOccursClause(CobolParser.OccursClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#occursClause}.
	 * @param ctx the parse tree
	 */
	void exitOccursClause(CobolParser.OccursClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#occursDependingClause}.
	 * @param ctx the parse tree
	 */
	void enterOccursDependingClause(CobolParser.OccursDependingClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#occursDependingClause}.
	 * @param ctx the parse tree
	 */
	void exitOccursDependingClause(CobolParser.OccursDependingClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#ascendingDescendingKeyClause}.
	 * @param ctx the parse tree
	 */
	void enterAscendingDescendingKeyClause(CobolParser.AscendingDescendingKeyClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#ascendingDescendingKeyClause}.
	 * @param ctx the parse tree
	 */
	void exitAscendingDescendingKeyClause(CobolParser.AscendingDescendingKeyClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#indexedByClause}.
	 * @param ctx the parse tree
	 */
	void enterIndexedByClause(CobolParser.IndexedByClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#indexedByClause}.
	 * @param ctx the parse tree
	 */
	void exitIndexedByClause(CobolParser.IndexedByClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#indexName}.
	 * @param ctx the parse tree
	 */
	void enterIndexName(CobolParser.IndexNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#indexName}.
	 * @param ctx the parse tree
	 */
	void exitIndexName(CobolParser.IndexNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#pictureClause}.
	 * @param ctx the parse tree
	 */
	void enterPictureClause(CobolParser.PictureClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#pictureClause}.
	 * @param ctx the parse tree
	 */
	void exitPictureClause(CobolParser.PictureClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#pictureString}.
	 * @param ctx the parse tree
	 */
	void enterPictureString(CobolParser.PictureStringContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#pictureString}.
	 * @param ctx the parse tree
	 */
	void exitPictureString(CobolParser.PictureStringContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#signClause}.
	 * @param ctx the parse tree
	 */
	void enterSignClause(CobolParser.SignClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#signClause}.
	 * @param ctx the parse tree
	 */
	void exitSignClause(CobolParser.SignClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#synchronizedClause}.
	 * @param ctx the parse tree
	 */
	void enterSynchronizedClause(CobolParser.SynchronizedClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#synchronizedClause}.
	 * @param ctx the parse tree
	 */
	void exitSynchronizedClause(CobolParser.SynchronizedClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#usageClause}.
	 * @param ctx the parse tree
	 */
	void enterUsageClause(CobolParser.UsageClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#usageClause}.
	 * @param ctx the parse tree
	 */
	void exitUsageClause(CobolParser.UsageClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#valueClause}.
	 * @param ctx the parse tree
	 */
	void enterValueClause(CobolParser.ValueClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#valueClause}.
	 * @param ctx the parse tree
	 */
	void exitValueClause(CobolParser.ValueClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#procedureDivision}.
	 * @param ctx the parse tree
	 */
	void enterProcedureDivision(CobolParser.ProcedureDivisionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#procedureDivision}.
	 * @param ctx the parse tree
	 */
	void exitProcedureDivision(CobolParser.ProcedureDivisionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#usingClause}.
	 * @param ctx the parse tree
	 */
	void enterUsingClause(CobolParser.UsingClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#usingClause}.
	 * @param ctx the parse tree
	 */
	void exitUsingClause(CobolParser.UsingClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#returningClause}.
	 * @param ctx the parse tree
	 */
	void enterReturningClause(CobolParser.ReturningClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#returningClause}.
	 * @param ctx the parse tree
	 */
	void exitReturningClause(CobolParser.ReturningClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#declaratives}.
	 * @param ctx the parse tree
	 */
	void enterDeclaratives(CobolParser.DeclarativesContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#declaratives}.
	 * @param ctx the parse tree
	 */
	void exitDeclaratives(CobolParser.DeclarativesContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#procedureSection}.
	 * @param ctx the parse tree
	 */
	void enterProcedureSection(CobolParser.ProcedureSectionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#procedureSection}.
	 * @param ctx the parse tree
	 */
	void exitProcedureSection(CobolParser.ProcedureSectionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#sectionName}.
	 * @param ctx the parse tree
	 */
	void enterSectionName(CobolParser.SectionNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#sectionName}.
	 * @param ctx the parse tree
	 */
	void exitSectionName(CobolParser.SectionNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#paragraph}.
	 * @param ctx the parse tree
	 */
	void enterParagraph(CobolParser.ParagraphContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#paragraph}.
	 * @param ctx the parse tree
	 */
	void exitParagraph(CobolParser.ParagraphContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#sentence}.
	 * @param ctx the parse tree
	 */
	void enterSentence(CobolParser.SentenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#sentence}.
	 * @param ctx the parse tree
	 */
	void exitSentence(CobolParser.SentenceContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#paragraphName}.
	 * @param ctx the parse tree
	 */
	void enterParagraphName(CobolParser.ParagraphNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#paragraphName}.
	 * @param ctx the parse tree
	 */
	void exitParagraphName(CobolParser.ParagraphNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#statement}.
	 * @param ctx the parse tree
	 */
	void enterStatement(CobolParser.StatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#statement}.
	 * @param ctx the parse tree
	 */
	void exitStatement(CobolParser.StatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#acceptStatement}.
	 * @param ctx the parse tree
	 */
	void enterAcceptStatement(CobolParser.AcceptStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#acceptStatement}.
	 * @param ctx the parse tree
	 */
	void exitAcceptStatement(CobolParser.AcceptStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#addStatement}.
	 * @param ctx the parse tree
	 */
	void enterAddStatement(CobolParser.AddStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#addStatement}.
	 * @param ctx the parse tree
	 */
	void exitAddStatement(CobolParser.AddStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#alterStatement}.
	 * @param ctx the parse tree
	 */
	void enterAlterStatement(CobolParser.AlterStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#alterStatement}.
	 * @param ctx the parse tree
	 */
	void exitAlterStatement(CobolParser.AlterStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#procedureName}.
	 * @param ctx the parse tree
	 */
	void enterProcedureName(CobolParser.ProcedureNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#procedureName}.
	 * @param ctx the parse tree
	 */
	void exitProcedureName(CobolParser.ProcedureNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#callStatement}.
	 * @param ctx the parse tree
	 */
	void enterCallStatement(CobolParser.CallStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#callStatement}.
	 * @param ctx the parse tree
	 */
	void exitCallStatement(CobolParser.CallStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#cancelStatement}.
	 * @param ctx the parse tree
	 */
	void enterCancelStatement(CobolParser.CancelStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#cancelStatement}.
	 * @param ctx the parse tree
	 */
	void exitCancelStatement(CobolParser.CancelStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#closeStatement}.
	 * @param ctx the parse tree
	 */
	void enterCloseStatement(CobolParser.CloseStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#closeStatement}.
	 * @param ctx the parse tree
	 */
	void exitCloseStatement(CobolParser.CloseStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#computeStatement}.
	 * @param ctx the parse tree
	 */
	void enterComputeStatement(CobolParser.ComputeStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#computeStatement}.
	 * @param ctx the parse tree
	 */
	void exitComputeStatement(CobolParser.ComputeStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#arithmeticExpression}.
	 * @param ctx the parse tree
	 */
	void enterArithmeticExpression(CobolParser.ArithmeticExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#arithmeticExpression}.
	 * @param ctx the parse tree
	 */
	void exitArithmeticExpression(CobolParser.ArithmeticExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#multDivExpression}.
	 * @param ctx the parse tree
	 */
	void enterMultDivExpression(CobolParser.MultDivExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#multDivExpression}.
	 * @param ctx the parse tree
	 */
	void exitMultDivExpression(CobolParser.MultDivExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#powerExpression}.
	 * @param ctx the parse tree
	 */
	void enterPowerExpression(CobolParser.PowerExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#powerExpression}.
	 * @param ctx the parse tree
	 */
	void exitPowerExpression(CobolParser.PowerExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#unaryExpression}.
	 * @param ctx the parse tree
	 */
	void enterUnaryExpression(CobolParser.UnaryExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#unaryExpression}.
	 * @param ctx the parse tree
	 */
	void exitUnaryExpression(CobolParser.UnaryExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#primaryExpression}.
	 * @param ctx the parse tree
	 */
	void enterPrimaryExpression(CobolParser.PrimaryExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#primaryExpression}.
	 * @param ctx the parse tree
	 */
	void exitPrimaryExpression(CobolParser.PrimaryExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#continueStatement}.
	 * @param ctx the parse tree
	 */
	void enterContinueStatement(CobolParser.ContinueStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#continueStatement}.
	 * @param ctx the parse tree
	 */
	void exitContinueStatement(CobolParser.ContinueStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#deleteStatement}.
	 * @param ctx the parse tree
	 */
	void enterDeleteStatement(CobolParser.DeleteStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#deleteStatement}.
	 * @param ctx the parse tree
	 */
	void exitDeleteStatement(CobolParser.DeleteStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#displayStatement}.
	 * @param ctx the parse tree
	 */
	void enterDisplayStatement(CobolParser.DisplayStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#displayStatement}.
	 * @param ctx the parse tree
	 */
	void exitDisplayStatement(CobolParser.DisplayStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#divideStatement}.
	 * @param ctx the parse tree
	 */
	void enterDivideStatement(CobolParser.DivideStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#divideStatement}.
	 * @param ctx the parse tree
	 */
	void exitDivideStatement(CobolParser.DivideStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#evaluateStatement}.
	 * @param ctx the parse tree
	 */
	void enterEvaluateStatement(CobolParser.EvaluateStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#evaluateStatement}.
	 * @param ctx the parse tree
	 */
	void exitEvaluateStatement(CobolParser.EvaluateStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#selectionSubject}.
	 * @param ctx the parse tree
	 */
	void enterSelectionSubject(CobolParser.SelectionSubjectContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#selectionSubject}.
	 * @param ctx the parse tree
	 */
	void exitSelectionSubject(CobolParser.SelectionSubjectContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#whenPhrase}.
	 * @param ctx the parse tree
	 */
	void enterWhenPhrase(CobolParser.WhenPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#whenPhrase}.
	 * @param ctx the parse tree
	 */
	void exitWhenPhrase(CobolParser.WhenPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#whenOtherPhrase}.
	 * @param ctx the parse tree
	 */
	void enterWhenOtherPhrase(CobolParser.WhenOtherPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#whenOtherPhrase}.
	 * @param ctx the parse tree
	 */
	void exitWhenOtherPhrase(CobolParser.WhenOtherPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#selectionObject}.
	 * @param ctx the parse tree
	 */
	void enterSelectionObject(CobolParser.SelectionObjectContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#selectionObject}.
	 * @param ctx the parse tree
	 */
	void exitSelectionObject(CobolParser.SelectionObjectContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#exitStatement}.
	 * @param ctx the parse tree
	 */
	void enterExitStatement(CobolParser.ExitStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#exitStatement}.
	 * @param ctx the parse tree
	 */
	void exitExitStatement(CobolParser.ExitStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#gobackStatement}.
	 * @param ctx the parse tree
	 */
	void enterGobackStatement(CobolParser.GobackStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#gobackStatement}.
	 * @param ctx the parse tree
	 */
	void exitGobackStatement(CobolParser.GobackStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#gotoStatement}.
	 * @param ctx the parse tree
	 */
	void enterGotoStatement(CobolParser.GotoStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#gotoStatement}.
	 * @param ctx the parse tree
	 */
	void exitGotoStatement(CobolParser.GotoStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#ifStatement}.
	 * @param ctx the parse tree
	 */
	void enterIfStatement(CobolParser.IfStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#ifStatement}.
	 * @param ctx the parse tree
	 */
	void exitIfStatement(CobolParser.IfStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#elseClause}.
	 * @param ctx the parse tree
	 */
	void enterElseClause(CobolParser.ElseClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#elseClause}.
	 * @param ctx the parse tree
	 */
	void exitElseClause(CobolParser.ElseClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#condition}.
	 * @param ctx the parse tree
	 */
	void enterCondition(CobolParser.ConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#condition}.
	 * @param ctx the parse tree
	 */
	void exitCondition(CobolParser.ConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#combinableCondition}.
	 * @param ctx the parse tree
	 */
	void enterCombinableCondition(CobolParser.CombinableConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#combinableCondition}.
	 * @param ctx the parse tree
	 */
	void exitCombinableCondition(CobolParser.CombinableConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#simpleCondition}.
	 * @param ctx the parse tree
	 */
	void enterSimpleCondition(CobolParser.SimpleConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#simpleCondition}.
	 * @param ctx the parse tree
	 */
	void exitSimpleCondition(CobolParser.SimpleConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#relationCondition}.
	 * @param ctx the parse tree
	 */
	void enterRelationCondition(CobolParser.RelationConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#relationCondition}.
	 * @param ctx the parse tree
	 */
	void exitRelationCondition(CobolParser.RelationConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#classCondition}.
	 * @param ctx the parse tree
	 */
	void enterClassCondition(CobolParser.ClassConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#classCondition}.
	 * @param ctx the parse tree
	 */
	void exitClassCondition(CobolParser.ClassConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#signCondition}.
	 * @param ctx the parse tree
	 */
	void enterSignCondition(CobolParser.SignConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#signCondition}.
	 * @param ctx the parse tree
	 */
	void exitSignCondition(CobolParser.SignConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#initializeStatement}.
	 * @param ctx the parse tree
	 */
	void enterInitializeStatement(CobolParser.InitializeStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#initializeStatement}.
	 * @param ctx the parse tree
	 */
	void exitInitializeStatement(CobolParser.InitializeStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#replacingClause}.
	 * @param ctx the parse tree
	 */
	void enterReplacingClause(CobolParser.ReplacingClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#replacingClause}.
	 * @param ctx the parse tree
	 */
	void exitReplacingClause(CobolParser.ReplacingClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#inspectStatement}.
	 * @param ctx the parse tree
	 */
	void enterInspectStatement(CobolParser.InspectStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#inspectStatement}.
	 * @param ctx the parse tree
	 */
	void exitInspectStatement(CobolParser.InspectStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#tallyingPhrase}.
	 * @param ctx the parse tree
	 */
	void enterTallyingPhrase(CobolParser.TallyingPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#tallyingPhrase}.
	 * @param ctx the parse tree
	 */
	void exitTallyingPhrase(CobolParser.TallyingPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#replacingPhrase}.
	 * @param ctx the parse tree
	 */
	void enterReplacingPhrase(CobolParser.ReplacingPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#replacingPhrase}.
	 * @param ctx the parse tree
	 */
	void exitReplacingPhrase(CobolParser.ReplacingPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#convertingPhrase}.
	 * @param ctx the parse tree
	 */
	void enterConvertingPhrase(CobolParser.ConvertingPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#convertingPhrase}.
	 * @param ctx the parse tree
	 */
	void exitConvertingPhrase(CobolParser.ConvertingPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#beforeAfterPhrase}.
	 * @param ctx the parse tree
	 */
	void enterBeforeAfterPhrase(CobolParser.BeforeAfterPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#beforeAfterPhrase}.
	 * @param ctx the parse tree
	 */
	void exitBeforeAfterPhrase(CobolParser.BeforeAfterPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#mergeStatement}.
	 * @param ctx the parse tree
	 */
	void enterMergeStatement(CobolParser.MergeStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#mergeStatement}.
	 * @param ctx the parse tree
	 */
	void exitMergeStatement(CobolParser.MergeStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#moveStatement}.
	 * @param ctx the parse tree
	 */
	void enterMoveStatement(CobolParser.MoveStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#moveStatement}.
	 * @param ctx the parse tree
	 */
	void exitMoveStatement(CobolParser.MoveStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#multiplyStatement}.
	 * @param ctx the parse tree
	 */
	void enterMultiplyStatement(CobolParser.MultiplyStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#multiplyStatement}.
	 * @param ctx the parse tree
	 */
	void exitMultiplyStatement(CobolParser.MultiplyStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#openStatement}.
	 * @param ctx the parse tree
	 */
	void enterOpenStatement(CobolParser.OpenStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#openStatement}.
	 * @param ctx the parse tree
	 */
	void exitOpenStatement(CobolParser.OpenStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#performStatement}.
	 * @param ctx the parse tree
	 */
	void enterPerformStatement(CobolParser.PerformStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#performStatement}.
	 * @param ctx the parse tree
	 */
	void exitPerformStatement(CobolParser.PerformStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#timesPhrase}.
	 * @param ctx the parse tree
	 */
	void enterTimesPhrase(CobolParser.TimesPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#timesPhrase}.
	 * @param ctx the parse tree
	 */
	void exitTimesPhrase(CobolParser.TimesPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#untilPhrase}.
	 * @param ctx the parse tree
	 */
	void enterUntilPhrase(CobolParser.UntilPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#untilPhrase}.
	 * @param ctx the parse tree
	 */
	void exitUntilPhrase(CobolParser.UntilPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#varyingPhrase}.
	 * @param ctx the parse tree
	 */
	void enterVaryingPhrase(CobolParser.VaryingPhraseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#varyingPhrase}.
	 * @param ctx the parse tree
	 */
	void exitVaryingPhrase(CobolParser.VaryingPhraseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#readStatement}.
	 * @param ctx the parse tree
	 */
	void enterReadStatement(CobolParser.ReadStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#readStatement}.
	 * @param ctx the parse tree
	 */
	void exitReadStatement(CobolParser.ReadStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#atEndClause}.
	 * @param ctx the parse tree
	 */
	void enterAtEndClause(CobolParser.AtEndClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#atEndClause}.
	 * @param ctx the parse tree
	 */
	void exitAtEndClause(CobolParser.AtEndClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#notAtEndClause}.
	 * @param ctx the parse tree
	 */
	void enterNotAtEndClause(CobolParser.NotAtEndClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#notAtEndClause}.
	 * @param ctx the parse tree
	 */
	void exitNotAtEndClause(CobolParser.NotAtEndClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#releaseStatement}.
	 * @param ctx the parse tree
	 */
	void enterReleaseStatement(CobolParser.ReleaseStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#releaseStatement}.
	 * @param ctx the parse tree
	 */
	void exitReleaseStatement(CobolParser.ReleaseStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#recordName}.
	 * @param ctx the parse tree
	 */
	void enterRecordName(CobolParser.RecordNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#recordName}.
	 * @param ctx the parse tree
	 */
	void exitRecordName(CobolParser.RecordNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#returnStatement}.
	 * @param ctx the parse tree
	 */
	void enterReturnStatement(CobolParser.ReturnStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#returnStatement}.
	 * @param ctx the parse tree
	 */
	void exitReturnStatement(CobolParser.ReturnStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#rewriteStatement}.
	 * @param ctx the parse tree
	 */
	void enterRewriteStatement(CobolParser.RewriteStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#rewriteStatement}.
	 * @param ctx the parse tree
	 */
	void exitRewriteStatement(CobolParser.RewriteStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#searchStatement}.
	 * @param ctx the parse tree
	 */
	void enterSearchStatement(CobolParser.SearchStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#searchStatement}.
	 * @param ctx the parse tree
	 */
	void exitSearchStatement(CobolParser.SearchStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#setStatement}.
	 * @param ctx the parse tree
	 */
	void enterSetStatement(CobolParser.SetStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#setStatement}.
	 * @param ctx the parse tree
	 */
	void exitSetStatement(CobolParser.SetStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#sortStatement}.
	 * @param ctx the parse tree
	 */
	void enterSortStatement(CobolParser.SortStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#sortStatement}.
	 * @param ctx the parse tree
	 */
	void exitSortStatement(CobolParser.SortStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#startStatement}.
	 * @param ctx the parse tree
	 */
	void enterStartStatement(CobolParser.StartStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#startStatement}.
	 * @param ctx the parse tree
	 */
	void exitStartStatement(CobolParser.StartStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#stopStatement}.
	 * @param ctx the parse tree
	 */
	void enterStopStatement(CobolParser.StopStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#stopStatement}.
	 * @param ctx the parse tree
	 */
	void exitStopStatement(CobolParser.StopStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#stringStatement}.
	 * @param ctx the parse tree
	 */
	void enterStringStatement(CobolParser.StringStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#stringStatement}.
	 * @param ctx the parse tree
	 */
	void exitStringStatement(CobolParser.StringStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#subtractStatement}.
	 * @param ctx the parse tree
	 */
	void enterSubtractStatement(CobolParser.SubtractStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#subtractStatement}.
	 * @param ctx the parse tree
	 */
	void exitSubtractStatement(CobolParser.SubtractStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#unstringStatement}.
	 * @param ctx the parse tree
	 */
	void enterUnstringStatement(CobolParser.UnstringStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#unstringStatement}.
	 * @param ctx the parse tree
	 */
	void exitUnstringStatement(CobolParser.UnstringStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#useStatement}.
	 * @param ctx the parse tree
	 */
	void enterUseStatement(CobolParser.UseStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#useStatement}.
	 * @param ctx the parse tree
	 */
	void exitUseStatement(CobolParser.UseStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#writeStatement}.
	 * @param ctx the parse tree
	 */
	void enterWriteStatement(CobolParser.WriteStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#writeStatement}.
	 * @param ctx the parse tree
	 */
	void exitWriteStatement(CobolParser.WriteStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#atEndOfPageClause}.
	 * @param ctx the parse tree
	 */
	void enterAtEndOfPageClause(CobolParser.AtEndOfPageClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#atEndOfPageClause}.
	 * @param ctx the parse tree
	 */
	void exitAtEndOfPageClause(CobolParser.AtEndOfPageClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#notAtEndOfPageClause}.
	 * @param ctx the parse tree
	 */
	void enterNotAtEndOfPageClause(CobolParser.NotAtEndOfPageClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#notAtEndOfPageClause}.
	 * @param ctx the parse tree
	 */
	void exitNotAtEndOfPageClause(CobolParser.NotAtEndOfPageClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#onSizeErrorClause}.
	 * @param ctx the parse tree
	 */
	void enterOnSizeErrorClause(CobolParser.OnSizeErrorClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#onSizeErrorClause}.
	 * @param ctx the parse tree
	 */
	void exitOnSizeErrorClause(CobolParser.OnSizeErrorClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#notOnSizeErrorClause}.
	 * @param ctx the parse tree
	 */
	void enterNotOnSizeErrorClause(CobolParser.NotOnSizeErrorClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#notOnSizeErrorClause}.
	 * @param ctx the parse tree
	 */
	void exitNotOnSizeErrorClause(CobolParser.NotOnSizeErrorClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#onOverflowClause}.
	 * @param ctx the parse tree
	 */
	void enterOnOverflowClause(CobolParser.OnOverflowClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#onOverflowClause}.
	 * @param ctx the parse tree
	 */
	void exitOnOverflowClause(CobolParser.OnOverflowClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#notOnOverflowClause}.
	 * @param ctx the parse tree
	 */
	void enterNotOnOverflowClause(CobolParser.NotOnOverflowClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#notOnOverflowClause}.
	 * @param ctx the parse tree
	 */
	void exitNotOnOverflowClause(CobolParser.NotOnOverflowClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#onExceptionClause}.
	 * @param ctx the parse tree
	 */
	void enterOnExceptionClause(CobolParser.OnExceptionClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#onExceptionClause}.
	 * @param ctx the parse tree
	 */
	void exitOnExceptionClause(CobolParser.OnExceptionClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#notOnExceptionClause}.
	 * @param ctx the parse tree
	 */
	void enterNotOnExceptionClause(CobolParser.NotOnExceptionClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#notOnExceptionClause}.
	 * @param ctx the parse tree
	 */
	void exitNotOnExceptionClause(CobolParser.NotOnExceptionClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#identifier}.
	 * @param ctx the parse tree
	 */
	void enterIdentifier(CobolParser.IdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#identifier}.
	 * @param ctx the parse tree
	 */
	void exitIdentifier(CobolParser.IdentifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobolParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterLiteral(CobolParser.LiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobolParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitLiteral(CobolParser.LiteralContext ctx);
}