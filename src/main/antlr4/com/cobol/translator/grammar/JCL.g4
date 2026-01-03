grammar JCL;

// Parser Rules

jclFile
    : statement+ EOF
    ;

statement
    : jobStatement
    | stepStatement
    | commentLine
    ;

jobStatement
    : '//' jobName JOB accountInfo (',' jobParameters)* comment?
    ;

stepStatement
    : '//' stepName EXEC (pgmExec | procExec) (',' execParameter)* comment? ddStatements*
    ;

commentLine
    : COMMENT
    ;

pgmExec
    : PGM '=' programName
    ;

procExec
    : PROC '=' procName
    ;

ddStatements
    : ddStatement
    | ddDummy
    | ddInline
    | ddConcat
    ;

ddStatement
    : '//' ddName DD ddParameters comment?
    ;

ddDummy
    : '//' ddName DD DUMMY comment?
    ;

ddInline
    : '//' ddName DD '*' comment?  // Inline data (DD *)
    | '//' ddName DD 'DATA' comment?  // Inline data (DD DATA)
    ;

ddConcat
    : '//' DD ddParameters comment?
    ;

ddParameters
    : ddParameter (',' ddParameter)*
    ;

ddParameter
    : dsnParameter
    | dispParameter
    | dcbParameter
    | spaceParameter
    | unitParameter
    | volserParameter
    | sysoutParameter
    | recfmParameter
    | lreclParameter
    | blksizeParameter
    | otherParameter
    ;

dsnParameter
    : DSN '=' datasetName
    ;

dispParameter
    : DISP '=' '(' disposition (',' normalDisp)? (',' abnormalDisp)? ')'
    | DISP '=' disposition  // Shorthand form without parentheses
    ;

dcbParameter
    : DCB '=' '(' dcbSubParameter (',' dcbSubParameter)* ')'
    ;

dcbSubParameter
    : RECFM '=' recfmValue
    | LRECL '=' number
    | BLKSIZE '=' number
    | DSORG '=' dsorgValue
    ;

spaceParameter
    : SPACE '=' '(' spaceUnit ',' '(' primary (',' secondary)? (',' directory)? ')' (',' IDENTIFIER)* ')'  // Optional parameters like RLSE, CONTIG
    ;

unitParameter
    : UNIT '=' unitValue
    ;

volserParameter
    : VOL '=' '(' SER '=' volserValue ')'
    | VOL '=' SER '=' volserValue  // Shorthand form without parentheses
    ;

sysoutParameter
    : SYSOUT '=' sysoutClass
    ;

recfmParameter
    : RECFM '=' recfmValue
    ;

lreclParameter
    : LRECL '=' number
    ;

blksizeParameter
    : BLKSIZE '=' number
    ;

otherParameter
    : paramName '=' paramValue
    ;

// Parameter values

accountInfo
    : STRING | IDENTIFIER | number
    ;

jobParameters
    : IDENTIFIER '=' (STRING | IDENTIFIER | number)
    ;

execParameter
    : IDENTIFIER '=' (STRING | IDENTIFIER | number)
    ;

disposition
    : NEW | OLD | SHR | MOD
    ;

normalDisp
    : CATLG | DELETE | KEEP | PASS
    ;

abnormalDisp
    : CATLG | DELETE | KEEP
    ;

spaceUnit
    : TRK | CYL | BLKS
    ;

primary
    : number
    ;

secondary
    : number
    ;

directory
    : number
    ;

recfmValue
    : FB | VB | FBA | VBA | U | IDENTIFIER
    ;

dsorgValue
    : PS | PO | DA | IDENTIFIER
    ;

unitValue
    : SYSDA | SYSALLDA | IDENTIFIER | STRING
    ;

sysoutClass
    : IDENTIFIER | '*'
    ;

volserValue
    : IDENTIFIER
    ;

datasetName
    : datasetComponent ('.' datasetComponent)*
    | STRING
    ;

datasetComponent
    : IDENTIFIER ('&' IDENTIFIER)*  // Regular identifier with optional symbolic parameters
    | '&' '&' IDENTIFIER  // Temporary dataset (&&name)
    | '&' IDENTIFIER  // Symbolic parameter (&name)
    ;

programName
    : IDENTIFIER
    ;

procName
    : IDENTIFIER
    ;

jobName
    : IDENTIFIER
    ;

stepName
    : IDENTIFIER
    ;

ddName
    : IDENTIFIER
    | SYSOUT  // Allow SYSOUT keyword as DD name
    ;

paramName
    : IDENTIFIER
    ;

paramValue
    : STRING
    | IDENTIFIER
    | number
    | '(' paramValue (',' paramValue)* ')'  // Parenthesized list of values
    ;

number
    : NUMBER
    ;

comment
    : COMMENT
    ;

// Lexer Rules

// Keywords - JCL
JOB     : 'JOB' ;
EXEC    : 'EXEC' ;
DD      : 'DD' ;
STEP    : 'STEP' ;
PGM     : 'PGM' ;
PROC    : 'PROC' ;

// Keywords - DD Parameters
DSN     : 'DSN' ;
DISP    : 'DISP' ;
DCB     : 'DCB' ;
SPACE   : 'SPACE' ;
UNIT    : 'UNIT' ;
VOL     : 'VOL' ;
SER     : 'SER' ;
SYSOUT  : 'SYSOUT' ;
DUMMY   : 'DUMMY' ;
RECFM   : 'RECFM' ;
LRECL   : 'LRECL' ;
BLKSIZE : 'BLKSIZE' ;
DSORG   : 'DSORG' ;

// Disposition values
NEW     : 'NEW' ;
OLD     : 'OLD' ;
SHR     : 'SHR' ;
MOD     : 'MOD' ;
CATLG   : 'CATLG' ;
DELETE  : 'DELETE' ;
KEEP    : 'KEEP' ;
PASS    : 'PASS' ;

// Space units
TRK     : 'TRK' ;
CYL     : 'CYL' ;
BLKS    : 'BLKS' ;

// RECFM values
FB      : 'FB' ;
VB      : 'VB' ;
FBA     : 'FBA' ;
VBA     : 'VBA' ;
U       : 'U' ;

// DSORG values
PS      : 'PS' ;
PO      : 'PO' ;
DA      : 'DA' ;

// Unit values
SYSDA   : 'SYSDA' ;
SYSALLDA: 'SYSALLDA' ;

// Identifiers and literals
// NUMBER must come before IDENTIFIER to have priority
NUMBER
    : [0-9]+
    ;

IDENTIFIER
    : [A-Z0-9@#$]+
    ;

STRING
    : '\'' (~'\'')* '\''
    ;

COMMENT
    : '//*' ~[\r\n]*
    ;

// Whitespace
WS
    : [ \t\r\n]+ -> skip
    ;

// Continuation (// followed by spaces at beginning of line)
CONTINUATION
    : '//' [ \t]+ -> skip
    ;
