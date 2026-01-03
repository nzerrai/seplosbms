grammar Cobol;

// =====================================================
// PARSER RULES
// =====================================================

// Top-level program structure
compilationUnit
    : identificationDivision
      environmentDivision?
      dataDivision?
      procedureDivision?
      EOF
    ;

// ==================== IDENTIFICATION DIVISION ====================
identificationDivision
    : IDENTIFICATION DIVISION DOT
      programId
      (authorParagraph | installationParagraph | dateWrittenParagraph | dateCompiledParagraph | securityParagraph)*
    ;

programId
    : PROGRAM_ID DOT programName (IS? (COMMON | INITIAL | LIBRARY | DEFINITION | RECURSIVE))? DOT
    ;

programName
    : IDENTIFIER
    ;

authorParagraph
    : AUTHOR DOT commentEntry? DOT
    ;

installationParagraph
    : INSTALLATION DOT commentEntry? DOT
    ;

dateWrittenParagraph
    : DATE_WRITTEN DOT commentEntry? DOT
    ;

dateCompiledParagraph
    : DATE_COMPILED DOT commentEntry? DOT
    ;

securityParagraph
    : SECURITY DOT commentEntry? DOT
    ;

commentEntry
    : (IDENTIFIER | LITERAL_STRING | INTEGER_LITERAL)+
    ;

// ==================== ENVIRONMENT DIVISION ====================
environmentDivision
    : ENVIRONMENT DIVISION DOT
      configurationSection?
      inputOutputSection?
    ;

configurationSection
    : CONFIGURATION SECTION DOT
      (sourceComputerParagraph | objectComputerParagraph | specialNamesParagraph)*
    ;

sourceComputerParagraph
    : SOURCE_COMPUTER DOT computerName (WITH? DEBUGGING MODE)? DOT
    ;

objectComputerParagraph
    : OBJECT_COMPUTER DOT computerName programCollatingSequence? segmentLimitClause? DOT
    ;

computerName
    : IDENTIFIER
    ;

programCollatingSequence
    : PROGRAM? COLLATING? SEQUENCE (IS? alphabetName)+
    ;

alphabetName
    : IDENTIFIER
    ;

segmentLimitClause
    : SEGMENT_LIMIT IS? INTEGER_LITERAL
    ;

specialNamesParagraph
    : SPECIAL_NAMES DOT (specialNameClause)* DOT
    ;

specialNameClause
    : environmentName IS? mnemonicName
    | alphabetClause
    | classClause
    | currencySignClause
    | decimalPointClause
    ;

environmentName
    : IDENTIFIER
    ;

mnemonicName
    : IDENTIFIER
    ;

alphabetClause
    : ALPHABET alphabetName (IS? STANDARD_1 | IS? NATIVE | IS? EBCDIC)
    ;

classClause
    : CLASS className IS? (classValue)+
    ;

className
    : IDENTIFIER
    ;

classValue
    : LITERAL_STRING | INTEGER_LITERAL
    ;

currencySignClause
    : CURRENCY SIGN? IS? LITERAL_STRING
    ;

decimalPointClause
    : DECIMAL_POINT IS? COMMA
    ;

inputOutputSection
    : INPUT_OUTPUT SECTION DOT
      fileControlParagraph?
      ioControlParagraph?
    ;

fileControlParagraph
    : FILE_CONTROL DOT (selectClause)*
    ;

selectClause
    : SELECT OPTIONAL? fileName
      assignClause
      (organizationClause | accessModeClause | recordKeyClause | alternateRecordKeyClause | fileStatusClause)*
      DOT
    ;

fileName
    : IDENTIFIER
    ;

assignClause
    : ASSIGN TO? (EXTERNAL? (DISK | TAPE | CARD_READER | CARD_PUNCH | PRINTER)? fileName | LITERAL_STRING)
    ;

organizationClause
    : ORGANIZATION IS? (SEQUENTIAL | LINE SEQUENTIAL | INDEXED | RELATIVE)
    ;

accessModeClause
    : ACCESS MODE? IS? (SEQUENTIAL | RANDOM | DYNAMIC)
    ;

recordKeyClause
    : RECORD KEY? IS? dataName (WITH? DUPLICATES)?
    ;

alternateRecordKeyClause
    : ALTERNATE RECORD? KEY? IS? dataName (WITH? DUPLICATES)?
    ;

fileStatusClause
    : FILE? STATUS IS? dataName (dataName)?
    ;

ioControlParagraph
    : I_O_CONTROL DOT (rerunClause | sameAreaClause | multipleFileClause)* DOT?
    ;

rerunClause
    : RERUN (ON? (fileName | implementationName))? EVERY (endOfReel | INTEGER_LITERAL RECORDS) OF? fileName
    ;

implementationName
    : IDENTIFIER
    ;

endOfReel
    : END OF? (REEL | UNIT)
    ;

sameAreaClause
    : SAME (RECORD | SORT | SORT_MERGE)? AREA? FOR? (fileName)+
    ;

multipleFileClause
    : MULTIPLE FILE TAPE? CONTAINS? (multipleFilePosition)+
    ;

multipleFilePosition
    : fileName (POSITION INTEGER_LITERAL)?
    ;

// ==================== DATA DIVISION ====================
dataDivision
    : DATA DIVISION DOT
      (fileSection | workingStorageSection | linkageSection | screenSection)*
    ;

fileSection
    : FILE SECTION DOT (fileDescriptionEntry)*
    ;

fileDescriptionEntry
    : FD fileName
      (blockContainsClause | recordContainsClause | labelRecordsClause | valueOfClause | dataRecordsClause | linageClause | recordingModeClause | codeSetClause)*
      DOT
      (dataDescriptionEntry)*
    ;

blockContainsClause
    : BLOCK CONTAINS? INTEGER_LITERAL (TO INTEGER_LITERAL)? (RECORDS | CHARACTERS)
    ;

recordContainsClause
    : RECORD CONTAINS? INTEGER_LITERAL (TO INTEGER_LITERAL)? CHARACTERS?
    | RECORD IS? VARYING IN? SIZE? (FROM? INTEGER_LITERAL)? (TO INTEGER_LITERAL)? CHARACTERS? (DEPENDING ON? dataName)?
    ;

labelRecordsClause
    : LABEL (RECORD IS? | RECORDS ARE?) (STANDARD | OMITTED)
    ;

valueOfClause
    : VALUE OF (implementationName IS? (dataName | LITERAL_STRING))+
    ;

dataRecordsClause
    : DATA (RECORD IS? | RECORDS ARE?) (dataName)+
    ;

linageClause
    : LINAGE IS? (dataName | INTEGER_LITERAL) LINES?
      (WITH? FOOTING AT? (dataName | INTEGER_LITERAL))?
      (LINES? AT? TOP (dataName | INTEGER_LITERAL))?
      (LINES? AT? BOTTOM (dataName | INTEGER_LITERAL))?
    ;

recordingModeClause
    : RECORDING MODE? IS? (F | V | S | U)
    ;

codeSetClause
    : CODE_SET IS? alphabetName
    ;

workingStorageSection
    : WORKING_STORAGE SECTION DOT (dataDescriptionEntry)*
    ;

linkageSection
    : LINKAGE SECTION DOT (dataDescriptionEntry)*
    ;

screenSection
    : SCREEN SECTION DOT (dataDescriptionEntry)*
    ;

dataDescriptionEntry
    : levelNumber (dataName | FILLER)?
      (redefinesClause | blankWhenZeroClause | externalClause | globalClause | justifiedClause |
       occursDependingClause | occursClause | pictureClause | signClause | synchronizedClause |
       usageClause | valueClause)*
      DOT?
    ;

levelNumber
    : LEVEL_NUMBER_01
    | LEVEL_NUMBER_66
    | LEVEL_NUMBER_77
    | LEVEL_NUMBER_88
    | LEVEL_NUMBER
    ;

dataName
    : IDENTIFIER
    ;

redefinesClause
    : REDEFINES dataName
    ;

blankWhenZeroClause
    : BLANK WHEN? (ZERO | ZEROS | ZEROES)
    ;

externalClause
    : IS? EXTERNAL
    ;

globalClause
    : IS? GLOBAL
    ;

justifiedClause
    : (JUSTIFIED | JUST) RIGHT?
    ;

occursClause
    : OCCURS INTEGER_LITERAL (TO INTEGER_LITERAL)? TIMES?
      (DEPENDING ON? dataName)?
      (ascendingDescendingKeyClause)*
      (indexedByClause)?
    ;

occursDependingClause
    : OCCURS DEPENDING ON? dataName
    ;

ascendingDescendingKeyClause
    : (ASCENDING | DESCENDING) KEY? IS? (dataName)+
    ;

indexedByClause
    : INDEXED BY? (indexName)+
    ;

indexName
    : IDENTIFIER
    ;

pictureClause
    : (PICTURE | PIC) IS? pictureString
    ;

pictureString
    : PICTURE_STRING
    ;

signClause
    : SIGN IS? (LEADING | TRAILING) (SEPARATE CHARACTER?)?
    ;

synchronizedClause
    : (SYNCHRONIZED | SYNC) (LEFT | RIGHT)?
    ;

usageClause
    : (USAGE IS?)? (BINARY | COMP | COMP_1 | COMP_2 | COMP_3 | COMP_4 | COMP_5 | DISPLAY | INDEX | PACKED_DECIMAL | POINTER)
    ;

valueClause
    : VALUE IS? literal
    ;

// ==================== PROCEDURE DIVISION ====================
procedureDivision
    : PROCEDURE DIVISION (usingClause)? (returningClause)? DOT
      (declaratives)?
      (procedureSection | paragraph)*
    ;

usingClause
    : USING (BY? (REFERENCE | VALUE))? (dataName)+
    ;

returningClause
    : RETURNING dataName
    ;

declaratives
    : DECLARATIVES DOT (procedureSection)+ END DECLARATIVES DOT
    ;

procedureSection
    : sectionName SECTION (INTEGER_LITERAL)? DOT
      (useStatement)?
      (paragraph)*
    ;

sectionName
    : IDENTIFIER
    ;

paragraph
    : paragraphName DOT (sentence)*
    ;

sentence
    : statement+ DOT
    ;

paragraphName
    : IDENTIFIER
    ;

statement
    : acceptStatement
    | addStatement
    | alterStatement
    | callStatement
    | cancelStatement
    | closeStatement
    | computeStatement
    | continueStatement
    | deleteStatement
    | displayStatement
    | divideStatement
    | evaluateStatement
    | exitStatement
    | gobackStatement
    | gotoStatement
    | ifStatement
    | initializeStatement
    | inspectStatement
    | mergeStatement
    | moveStatement
    | multiplyStatement
    | openStatement
    | performStatement
    | readStatement
    | releaseStatement
    | returnStatement
    | rewriteStatement
    | searchStatement
    | setStatement
    | sortStatement
    | startStatement
    | stopStatement
    | stringStatement
    | subtractStatement
    | unstringStatement
    | writeStatement
    ;

// ==================== ACCEPT STATEMENT ====================
acceptStatement
    : ACCEPT identifier (FROM (DATE | DAY | DAY_OF_WEEK | TIME))? (onExceptionClause)? (notOnExceptionClause)? (END_ACCEPT)?
    ;

// ==================== ADD STATEMENT ====================
addStatement
    : ADD (identifier | literal)+ (TO identifier (ROUNDED)?)+ (onSizeErrorClause)? (notOnSizeErrorClause)? (END_ADD)?
    | ADD (CORRESPONDING | CORR) identifier TO identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_ADD)?
    | ADD (identifier | literal)+ (TO (identifier | literal))? GIVING identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_ADD)?
    ;

// ==================== ALTER STATEMENT ====================
alterStatement
    : ALTER (procedureName TO (PROCEED TO)? procedureName)+
    ;

procedureName
    : paragraphName
    | sectionName
    ;

// ==================== CALL STATEMENT ====================
callStatement
    : CALL (identifier | LITERAL_STRING)
      (USING (BY? (REFERENCE | CONTENT | VALUE))? (identifier)+)?
      (RETURNING identifier)?
      (onExceptionClause)?
      (notOnExceptionClause)?
      (END_CALL)?
    ;

// ==================== CANCEL STATEMENT ====================
cancelStatement
    : CANCEL (identifier | LITERAL_STRING)+
    ;

// ==================== CLOSE STATEMENT ====================
closeStatement
    : CLOSE (fileName ((REEL | UNIT) (FOR? REMOVAL)? | (WITH? (NO REWIND | LOCK))?)?)+
    ;

// ==================== COMPUTE STATEMENT ====================
computeStatement
    : COMPUTE identifier (ROUNDED)? (EQ | EQUAL) arithmeticExpression (onSizeErrorClause)? (notOnSizeErrorClause)? (END_COMPUTE)?
    ;

arithmeticExpression
    : multDivExpression ((PLUS | MINUS) multDivExpression)*
    ;

multDivExpression
    : powerExpression ((MULT | DIV) powerExpression)*
    ;

powerExpression
    : unaryExpression (POWER unaryExpression)*
    ;

unaryExpression
    : (PLUS | MINUS)? primaryExpression
    ;

primaryExpression
    : LPAREN arithmeticExpression RPAREN
    | identifier
    | literal
    ;

// ==================== CONTINUE STATEMENT ====================
continueStatement
    : CONTINUE
    ;

// ==================== DELETE STATEMENT ====================
deleteStatement
    : DELETE fileName RECORD? (INVALID KEY? statement+)? (NOT INVALID KEY? statement+)? (END_DELETE)?
    ;

// ==================== DISPLAY STATEMENT ====================
displayStatement
    : DISPLAY (identifier | literal)+ (UPON (environmentName | CONSOLE))? (WITH? NO ADVANCING)? (onExceptionClause)? (notOnExceptionClause)? (END_DISPLAY)?
    ;

// ==================== DIVIDE STATEMENT ====================
divideStatement
    : DIVIDE (identifier | literal) INTO identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_DIVIDE)?
    | DIVIDE (identifier | literal) INTO (identifier | literal) GIVING identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_DIVIDE)?
    | DIVIDE (identifier | literal) BY (identifier | literal) GIVING identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_DIVIDE)?
    | DIVIDE (identifier | literal) INTO (identifier | literal) GIVING identifier (ROUNDED)? REMAINDER identifier (onSizeErrorClause)? (notOnSizeErrorClause)? (END_DIVIDE)?
    ;

// ==================== EVALUATE STATEMENT ====================
evaluateStatement
    : EVALUATE selectionSubject (ALSO selectionSubject)*
      (whenPhrase)+
      (whenOtherPhrase)?
      END_EVALUATE
    ;

selectionSubject
    : TRUE
    | FALSE
    | identifier
    | literal
    | arithmeticExpression
    | condition
    ;

whenPhrase
    : WHEN selectionObject (ALSO selectionObject)* (statement)*
    ;

whenOtherPhrase
    : WHEN OTHER (statement)*
    ;

selectionObject
    : ANY
    | condition
    | TRUE
    | FALSE
    | (NOT)? (identifier | literal | arithmeticExpression) ((THROUGH | THRU) (identifier | literal | arithmeticExpression))?
    ;

// ==================== EXIT STATEMENT ====================
exitStatement
    : EXIT (PROGRAM | SECTION | PARAGRAPH | PERFORM (CYCLE)?)?
    ;

// ==================== GOBACK STATEMENT ====================
gobackStatement
    : GOBACK (RETURNING (identifier | literal))?
    ;

// ==================== GOTO STATEMENT ====================
gotoStatement
    : GO TO? procedureName
    | GO TO? procedureName+ DEPENDING ON? identifier
    ;

// ==================== IF STATEMENT ====================
ifStatement
    : IF condition (THEN)? (statement)* (elseClause)? (END_IF)?
    ;

elseClause
    : ELSE (statement)*
    ;

condition
    : combinableCondition ((AND | OR) combinableCondition)*
    ;

combinableCondition
    : NOT? simpleCondition
    ;

simpleCondition
    : LPAREN condition RPAREN
    | relationCondition
    | classCondition
    | signCondition
    ;

relationCondition
    : (identifier | literal | arithmeticExpression)
      (IS? (NOT)? (GREATER THAN? | GT | LESS THAN? | LT | EQUAL TO? | EQ |
                   GREATER_EQUAL | GREATER THAN? OR EQUAL TO? | GE |
                   LESS_EQUAL | LESS THAN? OR EQUAL TO? | LE))
      (identifier | literal | arithmeticExpression)
    ;

classCondition
    : identifier IS? (NOT)? (NUMERIC | ALPHABETIC | ALPHABETIC_LOWER | ALPHABETIC_UPPER | className)
    ;

signCondition
    : (identifier | arithmeticExpression) IS? (NOT)? (POSITIVE | NEGATIVE | ZERO)
    ;

// ==================== INITIALIZE STATEMENT ====================
initializeStatement
    : INITIALIZE (identifier)+ (replacingClause)?
    ;

replacingClause
    : REPLACING (ALPHABETIC | ALPHANUMERIC | NUMERIC | ALPHANUMERIC_EDITED | NUMERIC_EDITED) DATA? BY (identifier | literal)
    ;

// ==================== INSPECT STATEMENT ====================
inspectStatement
    : INSPECT identifier (tallyingPhrase | replacingPhrase | tallyingPhrase replacingPhrase | convertingPhrase)
    ;

tallyingPhrase
    : TALLYING identifier FOR (CHARACTERS | ALL | LEADING) (identifier | literal)+ (beforeAfterPhrase)*
    ;

replacingPhrase
    : REPLACING (CHARACTERS BY (identifier | literal) (beforeAfterPhrase)* | (ALL | LEADING | FIRST) (identifier | literal) BY (identifier | literal) (beforeAfterPhrase)*)+
    ;

convertingPhrase
    : CONVERTING (identifier | literal) TO (identifier | literal) (beforeAfterPhrase)*
    ;

beforeAfterPhrase
    : (BEFORE | AFTER) INITIAL? (identifier | literal)
    ;

// ==================== MERGE STATEMENT ====================
mergeStatement
    : MERGE fileName (ON? (ASCENDING | DESCENDING) KEY? (dataName)+)+
      (COLLATING? SEQUENCE IS? alphabetName)?
      USING fileName fileName+
      (OUTPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)? | GIVING fileName+)
    ;

// ==================== MOVE STATEMENT ====================
moveStatement
    : MOVE (identifier | literal) TO (identifier)+
    | MOVE (CORRESPONDING | CORR) identifier TO identifier
    ;

// ==================== MULTIPLY STATEMENT ====================
multiplyStatement
    : MULTIPLY (identifier | literal) BY identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_MULTIPLY)?
    | MULTIPLY (identifier | literal) BY (identifier | literal) GIVING identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_MULTIPLY)?
    ;

// ==================== OPEN STATEMENT ====================
openStatement
    : OPEN (INPUT (fileName)+)? (OUTPUT (fileName)+)? (I_O (fileName)+)? (EXTEND (fileName)+)?
    ;

// ==================== PERFORM STATEMENT ====================
performStatement
    : PERFORM procedureName ((THROUGH | THRU) procedureName)?
      (WITH? TEST (BEFORE | AFTER))?
      (timesPhrase | untilPhrase | varyingPhrase)?
    | PERFORM (WITH? TEST (BEFORE | AFTER))? (timesPhrase | untilPhrase | varyingPhrase)? (statement)* END_PERFORM
    ;

timesPhrase
    : (identifier | INTEGER_LITERAL) TIMES
    ;

untilPhrase
    : (WITH? TEST (BEFORE | AFTER))? UNTIL condition
    ;

varyingPhrase
    : VARYING identifier FROM (identifier | literal) BY (identifier | literal) UNTIL condition
      (AFTER identifier FROM (identifier | literal) BY (identifier | literal) UNTIL condition)*
    ;

// ==================== READ STATEMENT ====================
readStatement
    : READ fileName (NEXT | PREVIOUS)? RECORD? (INTO identifier)?
      (KEY IS? dataName)?
      (INVALID KEY? statement+)?
      (NOT INVALID KEY? statement+)?
      (atEndClause)?
      (notAtEndClause)?
      (END_READ)?
    ;

atEndClause
    : AT? END statement+
    ;

notAtEndClause
    : NOT AT? END statement+
    ;

// ==================== RELEASE STATEMENT ====================
releaseStatement
    : RELEASE recordName (FROM identifier)?
    ;

recordName
    : IDENTIFIER
    ;

// ==================== RETURN STATEMENT ====================
returnStatement
    : RETURN fileName RECORD? (INTO identifier)? (atEndClause)? (notAtEndClause)? (END_RETURN)?
    ;

// ==================== REWRITE STATEMENT ====================
rewriteStatement
    : REWRITE recordName (FROM identifier)? (INVALID KEY? statement+)? (NOT INVALID KEY? statement+)? (END_REWRITE)?
    ;

// ==================== SEARCH STATEMENT ====================
searchStatement
    : SEARCH identifier (VARYING identifier)? (atEndClause)? (whenPhrase)+ (END_SEARCH)?
    | SEARCH ALL identifier (atEndClause)? (WHEN condition (AND condition)* (statement)+) (END_SEARCH)?
    ;

// ==================== SET STATEMENT ====================
setStatement
    : SET (identifier)+ TO (identifier | literal | TRUE | FALSE | ON | OFF)
    | SET (identifier)+ (UP BY | DOWN BY) (identifier | literal)
    ;

// ==================== SORT STATEMENT ====================
sortStatement
    : SORT fileName (ON? (ASCENDING | DESCENDING) KEY? (dataName)+)+
      (WITH? DUPLICATES IN? ORDER?)?
      (COLLATING? SEQUENCE IS? alphabetName)?
      (INPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)? | USING fileName+)
      (OUTPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)? | GIVING fileName+)
    ;

// ==================== START STATEMENT ====================
startStatement
    : START fileName (KEY IS? (EQUAL TO? | EQ | GREATER THAN? | GT | LESS THAN? | LT | NOT LESS THAN? | NOT LT | GREATER THAN? OR EQUAL TO? | GE) dataName)?
      (INVALID KEY? statement+)?
      (NOT INVALID KEY? statement+)?
      (END_START)?
    ;

// ==================== STOP STATEMENT ====================
stopStatement
    : STOP (RUN | LITERAL_STRING)
    ;

// ==================== STRING STATEMENT ====================
stringStatement
    : STRING (identifier | literal)+ DELIMITED BY? (identifier | literal | SIZE)
      (identifier | literal)+ DELIMITED BY? (identifier | literal | SIZE)
      INTO identifier (WITH? POINTER identifier)?
      (onOverflowClause)?
      (notOnOverflowClause)?
      (END_STRING)?
    ;

// ==================== SUBTRACT STATEMENT ====================
subtractStatement
    : SUBTRACT (identifier | literal)+ FROM identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_SUBTRACT)?
    | SUBTRACT (identifier | literal)+ FROM (identifier | literal) GIVING identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_SUBTRACT)?
    | SUBTRACT (CORRESPONDING | CORR) identifier FROM identifier (ROUNDED)? (onSizeErrorClause)? (notOnSizeErrorClause)? (END_SUBTRACT)?
    ;

// ==================== UNSTRING STATEMENT ====================
unstringStatement
    : UNSTRING identifier
      (DELIMITED BY? ALL? (identifier | literal) (OR ALL? (identifier | literal))*)?
      INTO (identifier (DELIMITER IN? identifier)? (COUNT IN? identifier)?)+
      (WITH? POINTER identifier)?
      (TALLYING IN? identifier)?
      (onOverflowClause)?
      (notOnOverflowClause)?
      (END_UNSTRING)?
    ;

// ==================== USE STATEMENT ====================
useStatement
    : USE (GLOBAL)? AFTER STANDARD? (EXCEPTION | ERROR) PROCEDURE ON? (fileName+ | INPUT | OUTPUT | I_O | EXTEND)
    ;

// ==================== WRITE STATEMENT ====================
writeStatement
    : WRITE recordName (FROM identifier)?
      ((BEFORE | AFTER) ADVANCING? ((identifier | INTEGER_LITERAL) (LINE | LINES)? | mnemonicName | PAGE))?
      (atEndOfPageClause)?
      (notAtEndOfPageClause)?
      (INVALID KEY? statement+)?
      (NOT INVALID KEY? statement+)?
      (END_WRITE)?
    ;

atEndOfPageClause
    : AT? (END_OF_PAGE | EOP) statement+
    ;

notAtEndOfPageClause
    : NOT AT? (END_OF_PAGE | EOP) statement+
    ;

// ==================== COMMON CLAUSES ====================
onSizeErrorClause
    : ON? SIZE ERROR statement+
    ;

notOnSizeErrorClause
    : NOT ON? SIZE ERROR statement+
    ;

onOverflowClause
    : ON? OVERFLOW statement+
    ;

notOnOverflowClause
    : NOT ON? OVERFLOW statement+
    ;

onExceptionClause
    : ON? EXCEPTION statement+
    ;

notOnExceptionClause
    : NOT ON? EXCEPTION statement+
    ;

// ==================== LITERALS AND IDENTIFIERS ====================
identifier
    : dataName
    | fileName
    | indexName
    | mnemonicName
    | procedureName
    ;

literal
    : INTEGER_LITERAL
    | DECIMAL_LITERAL
    | LITERAL_STRING
    | FIGURATIVE_CONSTANT
    ;

// =====================================================
// LEXER RULES
// =====================================================

// ==================== KEYWORDS ====================
ACCEPT: A C C E P T;
ACCESS: A C C E S S;
ADD: A D D;
ADVANCING: A D V A N C I N G;
AFTER: A F T E R;
ALL: A L L;
ALPHABETIC: A L P H A B E T I C;
ALPHABETIC_LOWER: A L P H A B E T I C '-' L O W E R;
ALPHABETIC_UPPER: A L P H A B E T I C '-' U P P E R;
ALPHANUMERIC: A L P H A N U M E R I C;
ALPHANUMERIC_EDITED: A L P H A N U M E R I C '-' E D I T E D;
ALPHABET: A L P H A B E T;
ALSO: A L S O;
ALTER: A L T E R;
ALTERNATE: A L T E R N A T E;
AND: A N D;
ANY: A N Y;
ARE: A R E;
AREA: A R E A;
ASCENDING: A S C E N D I N G;
ASSIGN: A S S I G N;
AT: A T;
AUTHOR: A U T H O R;
BEFORE: B E F O R E;
BINARY: B I N A R Y;
BLANK: B L A N K;
BLOCK: B L O C K;
BOTTOM: B O T T O M;
BY: B Y;
CALL: C A L L;
CANCEL: C A N C E L;
CARD_PUNCH: C A R D '-' P U N C H;
CARD_READER: C A R D '-' R E A D E R;
CHARACTER: C H A R A C T E R;
CHARACTERS: C H A R A C T E R S;
CLASS: C L A S S;
CLOSE: C L O S E;
CODE_SET: C O D E '-' S E T;
COLLATING: C O L L A T I N G;
COMMA: C O M M A;
COMMON: C O M M O N;
COMP: C O M P;
COMP_1: C O M P '-' '1';
COMP_2: C O M P '-' '2';
COMP_3: C O M P '-' '3';
COMP_4: C O M P '-' '4';
COMP_5: C O M P '-' '5';
COMPUTE: C O M P U T E;
CONFIGURATION: C O N F I G U R A T I O N;
CONSOLE: C O N S O L E;
CONTAINS: C O N T A I N S;
CONTENT: C O N T E N T;
CONTINUE: C O N T I N U E;
CONVERTING: C O N V E R T I N G;
CORR: C O R R;
CORRESPONDING: C O R R E S P O N D I N G;
COUNT: C O U N T;
CURRENCY: C U R R E N C Y;
CYCLE: C Y C L E;
DATA: D A T A;
DATE: D A T E;
DATE_COMPILED: D A T E '-' C O M P I L E D;
DATE_WRITTEN: D A T E '-' W R I T T E N;
DAY: D A Y;
DAY_OF_WEEK: D A Y '-' O F '-' W E E K;
DEBUGGING: D E B U G G I N G;
DECIMAL_POINT: D E C I M A L '-' P O I N T;
DECLARATIVES: D E C L A R A T I V E S;
DEFINITION: D E F I N I T I O N;
DELETE: D E L E T E;
DELIMITED: D E L I M I T E D;
DELIMITER: D E L I M I T E R;
DEPENDING: D E P E N D I N G;
DESCENDING: D E S C E N D I N G;
DISK: D I S K;
DISPLAY: D I S P L A Y;
DIVIDE: D I V I D E;
DIVISION: D I V I S I O N;
DOWN: D O W N;
DUPLICATES: D U P L I C A T E S;
DYNAMIC: D Y N A M I C;
EBCDIC: E B C D I C;
ELSE: E L S E;
END: E N D;
END_ACCEPT: E N D '-' A C C E P T;
END_ADD: E N D '-' A D D;
END_CALL: E N D '-' C A L L;
END_COMPUTE: E N D '-' C O M P U T E;
END_DELETE: E N D '-' D E L E T E;
END_DISPLAY: E N D '-' D I S P L A Y;
END_DIVIDE: E N D '-' D I V I D E;
END_EVALUATE: E N D '-' E V A L U A T E;
END_IF: E N D '-' I F;
END_MULTIPLY: E N D '-' M U L T I P L Y;
END_OF_PAGE: E N D '-' O F '-' P A G E;
END_PERFORM: E N D '-' P E R F O R M;
END_READ: E N D '-' R E A D;
END_RETURN: E N D '-' R E T U R N;
END_REWRITE: E N D '-' R E W R I T E;
END_SEARCH: E N D '-' S E A R C H;
END_START: E N D '-' S T A R T;
END_STRING: E N D '-' S T R I N G;
END_SUBTRACT: E N D '-' S U B T R A C T;
END_UNSTRING: E N D '-' U N S T R I N G;
END_WRITE: E N D '-' W R I T E;
ENTRY: E N T R Y;
ENVIRONMENT: E N V I R O N M E N T;
EOP: E O P;
EQUAL: E Q U A L;
ERROR: E R R O R;
EVALUATE: E V A L U A T E;
EXCEPTION: E X C E P T I O N;
EXIT: E X I T;
EXTEND: E X T E N D;
EXTERNAL: E X T E R N A L;
FALSE: F A L S E;
FD: F D;
FILE: F I L E;
FILE_CONTROL: F I L E '-' C O N T R O L;
FILLER: F I L L E R;
FIRST: F I R S T;
FOOTING: F O O T I N G;
FOR: F O R;
FROM: F R O M;
GIVING: G I V I N G;
GLOBAL: G L O B A L;
GO: G O;
GOBACK: G O B A C K;
GREATER: G R E A T E R;
GREATER_EQUAL: '>=';
I_O: I '-' O;
I_O_CONTROL: I '-' O '-' C O N T R O L;
IDENTIFICATION: I D E N T I F I C A T I O N;
IF: I F;
IN: I N;
INDEX: I N D E X;
INDEXED: I N D E X E D;
INITIAL: I N I T I A L;
INITIALIZE: I N I T I A L I Z E;
INPUT: I N P U T;
INPUT_OUTPUT: I N P U T '-' O U T P U T;
INSPECT: I N S P E C T;
INSTALLATION: I N S T A L L A T I O N;
INTO: I N T O;
INVALID: I N V A L I D;
IS: I S;
JUST: J U S T;
JUSTIFIED: J U S T I F I E D;
KEY: K E Y;
LABEL: L A B E L;
LEADING: L E A D I N G;
LEFT: L E F T;
LENGTH: L E N G T H;
LESS: L E S S;
LESS_EQUAL: '<=';
LIBRARY: L I B R A R Y;
LINAGE: L I N A G E;
LINE: L I N E;
LINES: L I N E S;
LINKAGE: L I N K A G E;
LOCK: L O C K;
MERGE: M E R G E;
MODE: M O D E;
MOVE: M O V E;
MULTIPLE: M U L T I P L E;
MULTIPLY: M U L T I P L Y;
NATIVE: N A T I V E;
NEGATIVE: N E G A T I V E;
NEXT: N E X T;
NO: N O;
NOT: N O T;
NUMERIC: N U M E R I C;
NUMERIC_EDITED: N U M E R I C '-' E D I T E D;
OBJECT_COMPUTER: O B J E C T '-' C O M P U T E R;
OCCURS: O C C U R S;
OF: O F;
OFF: O F F;
OMITTED: O M I T T E D;
ON: O N;
OPEN: O P E N;
OPTIONAL: O P T I O N A L;
OR: O R;
ORDER: O R D E R;
ORGANIZATION: O R G A N I Z A T I O N;
OTHER: O T H E R;
OUTPUT: O U T P U T;
OVERFLOW: O V E R F L O W;
PACKED_DECIMAL: P A C K E D '-' D E C I M A L;
PAGE: P A G E;
PERFORM: P E R F O R M;
PIC: P I C;
PICTURE: P I C T U R E;
POINTER: P O I N T E R;
POSITION: P O S I T I O N;
POSITIVE: P O S I T I V E;
PREVIOUS: P R E V I O U S;
PRINTER: P R I N T E R;
PROCEDURE: P R O C E D U R E;
PROCEED: P R O C E E D;
PROGRAM: P R O G R A M;
PROGRAM_ID: P R O G R A M '-' I D;
RANDOM: R A N D O M;
READ: R E A D;
RECORD: R E C O R D;
RECORDING: R E C O R D I N G;
RECORDS: R E C O R D S;
RECURSIVE: R E C U R S I V E;
REDEFINES: R E D E F I N E S;
REEL: R E E L;
REFERENCE: R E F E R E N C E;
RELATIVE: R E L A T I V E;
RELEASE: R E L E A S E;
REMAINDER: R E M A I N D E R;
REMOVAL: R E M O V A L;
REPLACING: R E P L A C I N G;
RERUN: R E R U N;
RETURN: R E T U R N;
RETURNING: R E T U R N I N G;
REWIND: R E W I N D;
REWRITE: R E W R I T E;
RIGHT: R I G H T;
ROUNDED: R O U N D E D;
RUN: R U N;
SAME: S A M E;
SCREEN: S C R E E N;
SEARCH: S E A R C H;
SECTION: S E C T I O N;
SECURITY: S E C U R I T Y;
SEGMENT_LIMIT: S E G M E N T '-' L I M I T;
SELECT: S E L E C T;
SENTENCE: S E N T E N C E;
SEPARATE: S E P A R A T E;
SEQUENCE: S E Q U E N C E;
SEQUENTIAL: S E Q U E N T I A L;
SET: S E T;
SIGN: S I G N;
SIZE: S I Z E;
SORT: S O R T;
SORT_MERGE: S O R T '-' M E R G E;
SOURCE_COMPUTER: S O U R C E '-' C O M P U T E R;
SPACE: S P A C E;
SPACES: S P A C E S;
SPECIAL_NAMES: S P E C I A L '-' N A M E S;
STANDARD: S T A N D A R D;
STANDARD_1: S T A N D A R D '-' '1';
START: S T A R T;
STATUS: S T A T U S;
STOP: S T O P;
STRING: S T R I N G;
SUBTRACT: S U B T R A C T;
SYNC: S Y N C;
SYNCHRONIZED: S Y N C H R O N I Z E D;
TALLYING: T A L L Y I N G;
TAPE: T A P E;
TEST: T E S T;
THAN: T H A N;
THEN: T H E N;
THROUGH: T H R O U G H;
THRU: T H R U;
TIME: T I M E;
TIMES: T I M E S;
TO: T O;
TOP: T O P;
TRAILING: T R A I L I N G;
TRUE: T R U E;
UNIT: U N I T;
UNSTRING: U N S T R I N G;
UNTIL: U N T I L;
UP: U P;
UPON: U P O N;
USAGE: U S A G E;
USE: U S E;
USING: U S I N G;
VALUE: V A L U E;
VARYING: V A R Y I N G;
WHEN: W H E N;
WITH: W I T H;
WORKING_STORAGE: W O R K I N G '-' S T O R A G E;
WRITE: W R I T E;
ZERO: Z E R O;
ZEROS: Z E R O S;
ZEROES: Z E R O E S;

// Figurative constants
FIGURATIVE_CONSTANT
    : H I G H '-' V A L U E S?
    | L O W '-' V A L U E S?
    | Q U O T E S?
    | S P A C E S?
    | Z E R O S?
    | Z E R O E S?
    ;

// Operators
EQ: '=' | E Q U A L;
GT: '>';
LT: '<';
GE: '>=';
LE: '<=';
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
POWER: '**';
LPAREN: '(';
RPAREN: ')';
DOT: '.';

// Level numbers
LEVEL_NUMBER_01: '01';
LEVEL_NUMBER_66: '66';
LEVEL_NUMBER_77: '77';
LEVEL_NUMBER_88: '88';
LEVEL_NUMBER: [0-4][0-9] | '0'[1-9];

// Picture string
PICTURE_STRING
    : ([9AXSVPZa-z]+ | '(' [0-9]+ ')' | [,./+\-$*])+
    ;

// Identifiers - In COBOL, identifiers can start with digits if they contain hyphen (e.g., 0000-MAIN)
IDENTIFIER
    : [A-Za-z][A-Za-z0-9\-_]*           // Standard: starts with letter
    | [0-9]+'-'[A-Za-z0-9\-_]+          // Numeric prefix with hyphen (e.g., 0000-MAIN)
    ;

// Literals
INTEGER_LITERAL
    : [0-9]+
    ;

DECIMAL_LITERAL
    : [0-9]+ '.' [0-9]+
    ;

LITERAL_STRING
    : '\'' (~['\r\n] | '\'\'')* '\''
    | '"' (~["\r\n] | '""')* '"'
    ;

// Comments
COMMENT_LINE
    : ('*' | '/') [ \t]* ~[\r\n]* -> channel(HIDDEN)
    ;

// Whitespace
WS
    : [ \t\r\n]+ -> skip
    ;

// Case-insensitive fragments
fragment A: [aA];
fragment B: [bB];
fragment C: [cC];
fragment D: [dD];
fragment E: [eE];
fragment F: [fF];
fragment G: [gG];
fragment H: [hH];
fragment I: [iI];
fragment J: [jJ];
fragment K: [kK];
fragment L: [lL];
fragment M: [mM];
fragment N: [nN];
fragment O: [oO];
fragment P: [pP];
fragment Q: [qQ];
fragment R: [rR];
fragment S: [sS];
fragment T: [tT];
fragment U: [uU];
fragment V: [vV];
fragment W: [wW];
fragment X: [xX];
fragment Y: [yY];
fragment Z: [zZ];
