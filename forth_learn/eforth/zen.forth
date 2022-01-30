: DoNotRun
\ performance considerations
\ 1. + - UM* UM/MOD 
\ 2. parse NUMBER?
\ 3. doUSER doVAR ( use| CALL doUSE | CALL doVAR )
\ 4. find
\ 5. ONLY-ALSO

\ Address interpreter
\ Text interpreter
\ Dual stack architecture
\ User variables
\ List of execution addresses
\ Linked vocabulary
\ Memory map

\ typical Forth command
\ ' ?TX HERE OVER - DUMP \ dump eforth system binary
\ 
\ ?TX ( first command in memory )
\ HERE ( last Forth command in memory )
\ 
\ ' ?TX HERE OVER - (length of memory with Forth system)
\ DUMP ( display contents of memory )

\ : dumpCode ['] ?TX HERE OVER - DUMP ;

\ Virtual Forth Engine

\ minimal components
\ 1. dictionary of procedural words
\ 2. return stack holding yet to execute procedures
\ 3. data stack holding parameters
\ 4. user space RAM (holds variables)
\ 5. CPU moves data between stack and memory,
\               and ALU operations to data stack items

\ requirements
\ IP    (SI)         Interpreter Pointer
\ SP    (SP)         Data Stack Pointer
\ RP    (RP)         Return Stack Pointer
\ WP    (AX)         Word or Work Pointer
\ UP    in memory    User Area Pointer

\ Word types:
\ code word  ( code field: only machine instructions )
\ colon word ( code field: colon word interpreter 
\                                and list of tokens )
\ token ( execution address of word in dictionary )
\ 
\ 4 bytes allocated for colon interpreter (32 bit?)
\ tokens are 2 bytes (16 bit?) and are pointers
\ code field length: variable
\ 
\ machine instructions terminated by macro $NEXT
\ $NEXT -> Interpreter Pointer IP 
\       -> IP increment ( next token in list [ xt ]? ) 
\       -> execute
\ 
\ 80x86 implementation

\ TODO double check src
\ 
\ BASEE   = 10 // default radix
\ CELLL   = 2  // size of cell
\ VOCSS   = 8
\ EM      = 0x04000        // top of memory
\ COLDD   = 0x00100        // cold start vector
\ US      = 64 * CELLL     // user area size in cells
\ RTS     = 64 * CELLL     // return stack/TIB size
\ RPP     = EM - 8*CELLL   // start of return stack (RP0)
\ TIBB    = RPP - RTS      // terminal input buffer (TIB)
\ SPP     = TIBB - 8*CELLL // start of data stack (SP0)
\ UPP     = EM - 256*CELLL // start of user area (UP0)
\ NAMEE   = UPP - 8*CELLL  //name dictionary
\ CODEE   = COLDD + US     // code dictionary
\ CALLL   = 2
\ VERSION = 1
\ COMPO   = 0x040
\ IMEDD   = 0x080
\ MASKK   = 0x07F1F // for checking COMPO or IMEDD flags in name dict

\ 
\ ; Assemble inline direct threaded code ending.
\ $NEXT MACRO
\       LODSW        ;; load next token into WP (AX)
\       JMP   AX     ;; jump directly to the token thru WP
\       ENDM         ;; IP (SI) now points to the next token

\ colon word first four bytes must be machine instruction to 
\        process token list following this first instruction
\ 
\ to complex for 4 bytes so implement as 
\ CALL DOLST
\ 
\ DOLST ( IP to return stack -> 1st token to IP -> $NEXT )
\ 
\ last token must be EXIT, EXIT undoes DOLST
\ 
\ EXIT ( pop return stack to IP -> $NEXT )

\ ; doLIST ( a -- )
\ ; Process colo list.
\   $CODE      COMPO+6, 'doLIST',DOLST
\   XCHG BP,SP            ; exchange pointers
\   PUSH SI          ; push return stack
\   XCHG BP,SP            ; restore the pointers
\   POP SI           ; new list address
\   $NEXT
\ 
\ ; EXIT ( -- )
\ ; Terminate a colon definition.
\   $CODE      4,'EXIT',EXIT
\   XCHG BP,SP           ; exchange pointers
\   POP SI               ; pop return stack
\   XCHG BP,SP           ; restore the pointers
\   $NEXT

\ $NEXT, DOLST, EXIT ~ inner interpreters 
\                      and address interpreters of Forth


\ eForth Word Set
\ 
\ Device Dependant I/O Words
 BYE ?RX TX! !IO
\ 
\ Kernel Words
 doLIT doLIST EXIT EXECUTE next ?branch branch 
 ! @ C! C@ RP@ RP! R> R@ >R SP@ SP! 
 DROP DUP SWAP OVER 
 0< AND OR XOR UM+
\ 
\ System Variables
 doVAR UP doUSER SP0 RP0
 '?KEY 'EMIT 'EXPECT 'TAP 'ECHO 'PROMPT
 BASE tmp SPAN
 >IN #TIB CSP 'EVAL 'NUMBER HLD HANDLER CONTEXT CURRENT
 CP NP LAST
\ 
\ Common Functions
 doVOC FORTH 
 ?DUP ROT 2DROP 2DUP 
 + NOT NEGATE DNEGATE - ABS = U< < MAX MIN WITHIN
\ 
\ Divide and Multiply
UM/MOD M/MOD /MOD MOD / UM* * M* */MOD */
\ 
\ Miscellaneous
CELL+ CELL- CELLS ALIGNED BL >CHAR DEPTH PICH
\ 
\ Memory Access
+! 2! 2@ COUNT HERE PAD TIB @EXECUTE CMOVE FILL -TRAILING PACK$
\ 
\ Numeric Output
DIGIT EXTRACT <# HOLD # #S SIGN #> str HEX DECIMAL
\ 
\ Numeric Input
DIGIT? NUMBER?
\ 
\ BASIC I/O
?KEY KEY EMIT NUF? PACE SPACE TYPE CR do$ $"| ."| .R U.R U. . ?
\ 
\ Parsing
parse PARSE CHAR TOKEN WORD .( \ (
\ 
\ Dictionary  Search
Name> SAME? find NAME?
\ 
\ Terminal Response
^H TAP kTAP accept EXPECT QUERY
\ 
\ Error Handling
CATCH THROW NULL$ ABORT abort"
\ 
\ Text Interpreter
$INTERPRET [ ( <- not required) .OK ?STACK EVAL
\ 
\ Shell 
PRESET xio FILE HAND I/O CONSOLE QUIT
\ 
\ Compiler
' ALLOT , [COMPILE] COMPILE LITERAL $." RECURSE
\ 
\ Structures
FOR BEGIN NEXT UNTIL AGAIN IF AHEAD REPEAT THEN AFT ELSE WHILE
ABORT" $" ."
\ 
\ Name Compiler
?UNIQUE $,n
\ 
\ Forth Compiler
$COMPILE OVERT ; ] call, : IMMEDIATE
\ 
\ Defining Words
USER CREATE VARIABLE
\ 
\ Tools
_TYPE dm+ DUMP .S !CSP > ANME .ID SEE WORDS
\ 
\ Hardware Reset
VER hi 'BOOT COLD


\ Memory Map
\ ;; Memory allocation 0//code>--//--<name//up>--<sp//tib>--rp//em
\ EM   EQU  04000H     ;top of memory
\ COLDD     EQU  00100H      ;cold start vector
\ US   EQU  64*CELLL   ;user area size in cells
\ RTS  EQU  64*CELLL   ;return stack/TIB size
\ RPP  EQU  EM-8*CELLL       ;start of return stack (RP0)
\ TIBB EQU  RPP-RTS    ;terminal input buffer (TIB)
\ SPP  EQU  TIBB-8*CELLL     ;start of data stack (SP0)
\ UPP  EQU  EM-256*CELLL     ;start of user area (UP0)
\ NAMEE     EQU  UPP-8*CELLL ;name dictionary
\ CODEE     EQU COLDD+US     ;code dictionary

\ Memory used in eForth
\ Cold Boot    100H-17FH    Cold start and variable initial values
\ Code Dict    180H-1344H   Code dictionary growing upward
\ Free space  1346H-33E4H   Shared by code and name dictionaries
\ Name/token  33E6H-3BFFH   Name dictionary growing downwards
\ Data stack  3C00H-3E7FH   Growing downward
\ TIB         3E80H-        Growing upward
\ Return stack     -3F7FH   Growing downward
\ User variables   3F80H-3FFFH
\ 
\ Change according to convenience

\ Feild    Length    Function
\ Token    2 bytes   code address (ca)
\ Link     2 bytes   name address (na) of previous word
\ Length   1 byte    length and lexicon bits
\ Name     n bytes   name of word
\ Filler   0/1 byte  fill to cell boundary

\ Code Word
\ Machine Instrucions | LODSW | JMP AX
\ 
\ Colon Word
\ CALL doLST | Token LIST | EXIT
\ 
\ User Variable 
\ CALL doLST | doUSER | n
\ 
\ Variable
\ CALL doLST | doVAR | n
\ 
\ Create Array
\ CALL doLST | doVAR | Array

\ ;; Imitialize assembly variables
\ _LINK = 0       ; force a null link
\ _NAME = NAMEE   ; initialize name pointer
\ _CODE = CODEE   ; initialize code pointer
\ _USER = 4*CELLL ; first user variable offset
\ 
\ ;; Define assembly macros
\ ;    Adjust an address to the next cell boundary.
\ $ALIGN MACRO
\      EVEN               ;;for 16bit systems
\      ENDM
\ ;    compile a code definition header.
\ $CODE     MACRO     LEX,NAME,LABEL
\      $ALIGN              ;;force to cell boundary
\ LABEL:                   ;;assembly label
\      _CODE    =          ;;save code pointer
\      _LEN = (LEX AND 01FH)/CELLL   ;;string cell count, round down
\      _NAME    = _NAME-((_LEN+3)*CELLL);;new header on cell boundary
\ ORG  _NAME               ;;set name pointer
\      DW    _CODE,_LINK   ;;token pointer and link
\      _LINK    = $        ;;link points to a name string
\      DB   LEX,NAME       ;;name string
\ ORG  _CODE               ;;restore code pointer
\      ENDM
\ ;    Compile a colon definition header.

\ $COLON MACRO LEX,NAME,LABEL
\      $CODE     LEX,NAME,LABEL
\      NOP                 ;;align to cell boundary
\      CALL DOLST          ;;include CALL doLIST
\      ENDM
\ ;    Compile a user variable header.
\ $USER     MACRO        LEX,NAME,LABEL
\      $CODE     LEX,NAME,LABEL
\      NOP                 ;;align to cell boundary
\      CALL DOLST          ;;include CALL doLIST
\      DW   DOUSE,_USER    ;;followed by doUSER and offset
\      ENDM
\ ;    Compile an inline string.
\ D$   MACRO     FUNCT,STRNG
\      DW   FUNCT          ;;fuction
\      _LEN = $            ;;save address of count byte
\      DB   0,STRNG        ;;count byte and string
\      _CODE     = $       ;;point to count byte
\ ORG  _LEN                ;;point to count byte
\      DB   _CODE-_LEN-1   ;;set count
\ ORG  _CODE               ;;restore code pointer
\      $ALIGN
\      ENDM

\ ;; Main entry point and COLD start data
\ MAIN SEGMENT
\ ASSUME CS:MAIN,DS:MAIN,ES:MAIN,SS:MAIN
\ ORG  COLDD              ;beginning of cold boot
\ ORIG:     MOV AX,CS
\      MOV  DS,AX          ;DS is same as CS
\      CLI                 ;disable interrupts, old 808x CPU bug
\      MOV  SS,AX          ;SS is same as CS
\      MOV  SP,SPP         ;initialize SP
\      STI                 ;initialize interrrupts
\      MOV  BP,RPP         ;initialize RP
\      MOV  AL,023H        ;interrupt 23H
\      MOV  DX,OFFSET CTRLC
\      MOV  AH,025H        ;MS-DOS set interrupt vector
\      INT  021H
\      CLD                 ;direction flag, increment
\      JMP COLD            ;to high level cold start
\ CTRLC:IRET               ;control C interrupt routine

\ ; COLD start moves the following to USER variables.
\ ; MUST BE IN SAME ORDER AS USER VARIABLES.
\ $ALIGN              ;align to cell boundary
\ UZERO:    DW   4 DUP (0) ;reserved
\      DW   SPP       ;SP0
\      DW   RPP       ;RP0
\      DW   QRX       ;'KEY?
\      DW   TXSTO     ;'EMIT
\      DW   ACCEP     ;'EXPECT
\      DW   KTAP      ;'TAP
\      DW   TXSTO     ;'ECHO
\      DW   DOTOK     ;'PROMPT
\      DW   BASEE     ;BASE
\      DW   0         ;tmp
\      DW   0         ;SPAN
\      DW   0         ;>IN
\      DW   0         ;#TIB
\      DW   TIBB      ;TIB
\      DW   0         ;CSP
\      DW   INTER     ;'EVAL
\      DW   NUMBQ     ;'NUMBER
\      DW   0         ;HLD
\      DW   0         ;HANDLER
\      DW   0         ;CONTEXT pointer
\      DW   VOCSS DUP (0)  ;vocabulary stack
\      DW   0         ;CURRENT pointer
\      DW   0         ;vocabulary link pointer
\      DW   CTOP      ;CP
\      DW   NTOP      ;NP
\      DW   LASTN     ;LAST
\ ULAST:

\ eForth Kernel
\ 
\ System interface:    BYE ?rx txt !io
\ Inner interpreters:  doLIT doLIST next ?branch branch EXECUTE EXIT
\ Memory access:       ! , @ C! C@
\ Return stack:        RP@ RP! R> R@ R>
\ Data stack:          SP@ SP! DROP DUP SWAP OVER
\ Logic:               0< AND OR XOR
\ Arithmetic:          UM+

\ DOS Service calls
\ 
\ ORG CODEE
\ ;; Device dependant I/O
\ ;    BYE ( -- )
\ ;    Exit eForth.
\      $CODE     3, 'BYE',BYE
\      INT 020H        ;MS-DOS terminate process

\ ;    ?RX ( -- c T | F )
\ ;    Return input character and true, or a false if no input.
\      $CODE     3,'?RX',QRX
\      XOR  BX,BX           ;BX=0 setup for false flag
\      MOV  DL,0FFH         ;input command
\      MOV  AH,6            ;MS-DOS Direct Console I/0
\      INT  021H
\      JZ   QRX1            ;?key ready
\      OR   AL,AL           ;AL=0 if extended char
\      JNZ  QRX1            ;?extended character code
\      INT  021H
\      MOV  BH,AL           ;extended code in msb
\ QRX1:     MOV BL,AL
\ QRX2:     PUSH BX         ;save character
\      MOV  BX,-1           ;true flag
\ QRX3:     PUSH BX
\      $NEXT
\ 
\ ;    TX! (c -- )
\ ;    Send character c to the output device.
\      $CODE     3,'TX!',TXSTO
\      POP  DX              ;char in DL
\      CMP  DL,0FFH         ;0FFH is interpretted as input
\      JNZ  TX1             ;do NOT allow input
\      MOV  DL,32           ;change to blank
\ TX1: MOV  AH,6            ;MS-DOS Direct Console I/O
\      INT  021H            ;display character
\      $NEXT
\ 
\ ;    !IO  ( -- )
\ ;    Initialize the serial I/O devices.
\      CODE          3,'IO',STDIO
\      $NEXT

\ Colon Word Interpreter
\ 
\ ;    doLIST    ( a -- )
\ ;    Process colon list.
\      $CODE     COMPO+6,'doLIST',DOLST
\      XCHG BP,SP          ;exchange pointers
\      PUSH SI             ;push return stack
\      XCHG BP,SP          ;restore the pointers
\      POP  SI             ;new list address
\      $NEXT
\ 
\ ;    EXIT ( -- )
\ ;    Terminate a colon definition.
\      $CODE     4,'EXIT',EXIT
\      XCHG BP,SP          ;exchange pointers
\      POP  SI             ;pop return stack
\      XCHG BP,SP          ;restore the pointers
\      $NEXT
\ 
\ ;    EXECUTE   ( ca -- )
\ ;    Execute the word at ca.
\      $CODE     7,'EXECUTE',EXECU
\      POP  BX
\      JMP  BX             ;jump to the code address

\ Integer Literals
\ 
\ ;    doLIT     ( -- w )
\ ;    h an inline literal.
\      $CODE  COMPO+5,'doLIT',DOLIT
\      LODSW               ;get the literal compiled in-line
\      PUSH AX             ;push literal on the stack
\      $NEXT               ;execute next token after literal

\ Address Literals
\ 
\ ;    next ( -- )
\ ;    Run time code for the single index loop.
\      $CODE     COMPO+4,'next',DONXT
\      SUB  WORD PTR [BP],1     ;decrement the index
\      JC   NEXT1          ;?decrement below 0
\      MOV  SI,0[SI]       ;no, continue loop
\      $NEXT
\ NEXT1:ADD BP,CELLL       ;yes, pop the index
\      ADD SI,CELLL        ;exit loop
\      $NEXT
\ 
\ ;    ?branch  ( f -- )
\ ;    Branch if flag is zero.
\      $CODE     COMPO+7,'?branch',QBRAN
\      POP  BX             ;pop flag
\      OR   BX,BX          ;?flag=0
\      JZ   BRAN1          ;yes, so branch
\      ADD  SI,CELLL       ;point IP to next cell
\      $NEXT
\ BRAN1:MOV SI,0[SI]       ;IP:=(IP), jump to new address
\      $NEXT
\ 
\ ;    branch    ( -- )
\ ;    Branch to an inline address.
\      $CODE     COMPO+6, 'branch',BRAN
\      MOV SI,0[SI]        ;jump to new address unconditionally
\      $NEXT

\ Memory Access
\ 
\ ;    !    ( w a -- )
\ ;    Pop the data stack to memory.
\      $CODE     1,'!',STORE
\      POP  BX             ;get address from top of stack (tos)
\      POP  0[BX]               ;store data to that address
\      $NEXT
\ 
\ ;    @    ( a -- w )
\ ;    Push memory location to the data stack.
\      $CODE     1,'@',AT
\      POP  BX             ;get address
\      PUSH 0[BX]               ;fetch data
\      $NEXT
\ 
\ ;    C!   ( c b -- )
\ ;    Pop the data stack to byte memory.
\      $CODE     2,'C!',CSTOR
\      POP  BX             ;get address
\      POP  AX             ;get data in a cell
\      MOV  0[BX],AL       ;store one byte
\      $NEXT
\ 
\ ;    C@   ( b -- c )
\ ;    Push byte memory location to the data stack.
\      $CODE     2,'C@',CAT
\      POP  BX             ; get address
\      XOR  AX,AX          ;AX=0 zero the hi byte
\      MOV  AL,0[BX]       ;get the low byte
\      PUSH AX             ;push on stack
\      $NEXT

\ Returm Stack Words
\ 
\ ;    RP@  ( -- a )
\ ;    Push the current RP to the data stack.
\      $CODE     3,'RP@',RPAT
\      PUSH BP             ;copy address to return stack
\      $NEXT               ;pointer register BP
\ 
\ ;    RP!  ( a -- )
\ ;    Set the return stack pointer.
\      $CODE     COMPO+3,'RP!',RPSTO
\      POP  BP             ;copy (BP) to top of stack
\      $NEXT
\ 
\ ;    R>   ( -- w )
\ ;    Pop the return stack to the data stack.
\      $CODE     2,'R>',RFROM
\      PUSH 0[BP]          ;copy w to the data stack
\      ADD  BP,CELLL       ;adjust RP for popping
\      $NEXT
\ 
\ ;    R@   ( -- w )
\ ;    Copy top of return stack to the data stack.
\      $CODE     2,'R@',RAT
\      PUSH 0[BP]          ;copy w to the data stack
\      $NEXT
\ 
\ ;    >R   ( w -- )
\ ;    Push the data stack  to the return stack.
\      $CODE     COMPO+2,'>R',TOR
\      SUB  BP,CELLL       ;adjust RP for pushing
\      POP  0[BP]          ;push w to return stack
\      $NEXT

\ Data Stack Initialization
\ 
\ ;    SP@  ( -- a )
\ ;    Push the current data stack pointer.
\      $CODE     3,'SP@',SPAT
\      MOV  BX,SP          ;use BX to index the stack
\      PUSH BX             ;push SP back
\      $NEXT
\ 
\ ;    SP!  ( a -- )
\ ;    Set the data stack pointer.
\      $CODE     3,'SP!',SPSTO
\      POP  SP             ;safe
\     $NEXT

\ Classic Data Stack Words
\ 
\ ;    DROP ( w -- )
\ ;    DIscards top stack item.
\      $CODE      4,'DROP',DROP
\      ADD  SP,CELLL       ;adjust SP to pop
\      $NEXT
\ 
\ ;    SWAP ( w1 w2 -- w2 w1 )
\ ;    Exchange top two stack items.
\      $CODE     4,'SWAP',SWAP
\      POP  BX             ;get w2
\      POP  AX             ;get w1
\      PUSH BX             ;push w2
\      PUSH AX             ;push w1
\      $NEXT
\ 
\ ;    OVER ( w1 w2 -- w1 w2 w1 )
\ ;    Copy second stack item to top.
\      $CODE     4,'OVER',OVER
\      MOV BX,SP           ;use BX to index the stack
\      PUSH CELLL[BX]      ;get w1 and push on the stack
\      $NEXT
 
\ Logical Words
\ 
\ ;    0<   ( n -- f )     ;Return true if n is negative.
\      $CODE     2,'0<',ZLESS
\      POP  AX
\      CWD                 ;sign extend AX into DX
\      PUSH DX             ;push 0 or -1
\      $NEXT
\ 
\ ;    AND  ( w w -- w )   ;Bitwise AND.
\      $CODE     3,'AND',ANDD
\      POP  BX
\      POP  AX
\      AND  BX
\      AND  BX,AX
\      PUSH BX
\      $NEXT
\ 
\ ;    OR   ( w w -- w )   ;Bitwise inclusive OR.
\      $CODE     3,'AND',ANDD
\      POP  BX
\      POP  AX
\      AND  BX,AX
\      PUSH BX
\      $NEXT
\ 
\ ;    XOR  ( w w -- )     ;Bitwise inclusive OR.
\      $CODE     2,'XOR',XORR
\      POP  BX
\      POP  AX
\      XOR  BX,AX
\      PUSH BX
\      $NEXT
 
\ Primitive Arithmetic Word
\ 
\ ;    UM+  ( w w -- w cy )
\      Add two numbers, return the sum and carry flag.
\      $CODE     3,'UM+',UPLUS
\      XOR  CX,CX          ;CX=0 initial carry flag
\      POP  BX
\      POP  AX
\      ADD  AX,BX
\      RCL  CX,1           ;get carry
\      PUSH AX             ;push sum
\      PUSH CX             ;push carry
\      $NEXT

\ From here you need to study the assembly from the src

\ User variables

: doVAR \ -- a
\ Run time routine for VARIABLE and CREATE.
R> ;
\ 
\ $COLON COMPO+5,'doVAR',DOVAR
\ DW     RFROM,EXIT

VARIABLE UP \ -- a
\ Pointer to the user area.
\ 
\ $COLON 2,'UP',UP
\ DW     DOVAR
\ DW     UPP

: doUSER \ -- a
\ Run time routine for user variables.
R> @       \ retrieve user area offset
UP @ + ;   \ add to user area base addr
\ 
\ $COLON  COMPO+6,'doUSER',DOUSE
\ DW      RFROM,AT,UP,AT,PLUS,EXIT

USER SP0 \ -- a
\ Pointer to the bottom of the data stack
\ $USER   3,'SP0',SZERO

USER RP0 \ -- a
\ Pointer to the bottom of the return stack.
\ $USER   3,'RP0',RZERO

USER '?KEY \ -- a
\ Execution vector of ?KEY. Default to ?rx.
\ $USER   5,"'?KEY",TQKEY

USER 'EMIT \ -- a
\ Execution vector of EMIT. Default to tx!
\ $USER   5,"'EMIT",TEMIT

USER 'EXPECT \ -- a
\ Execution vector of EXPECT. Defaut to 'accept'.
\ $USER   7,"'EXPECT",TEXPE

USER 'TAP \ -- a
\ Execution vector of TAP. Default to kTAP.
\ $USER   4,"'TAP",TTAP

USER 'ECHO \ -- a
\ Execution vector of ECHO. Default the kTAP.
\ $USER   5,"'ECHO",TECHO

USER 'PROMPT \ -- a
\ Execution vector of PROMPT. Default to '.ok'.
\ $USER   7,"'PROMPT",TPROM

USER BASE \ -- a
\ Storage of the radix base for numeric I/O. Default to 10.
\ $USER   4,'BASE',BASE

USER tmp \ --a
\ A temporary storage location used in parse and find.
\ $USER   COMPO+3,'tmp',TEMP

USER SPAN \ -- a
\ Hold character count recieved by EXPECT.
\ $USER   4,'SPAN',SPAN

USER >IN \ -- a
\ Hold the character pointer while parsing input stream.
\ $USER   3,'>IN',INN

USER #TIB \ -- a
\ Hold the current count and adress of the terminal input buffer.
\ Terminal Input Buffer used one cell after #TIB.
\ 
\ $USER   4,'#TIB',NTIB
\ _USER = _USER+CELLL

USER CSP \ -- a
\ Hold the stack pointer for error checking.
\ $USER  3,'CSP',CSP

USER 'EVAL \ -- a
\ Execution vector of EVAL. Default to EVAL.
\ $USER   5,"'EVAL",TEVAL

USER 'NUMBER \ --a
\ Execution vector of number conversion. Default to NUMBER?.
\ $USER   7,"'NUMBER",TNUM

USER HLD \ -- a
\ Hold a pointer in building a numeric output string.
\ $USER   3,'HLD',HLD

USER HANDLER \ -- a
\ Hold a pointer in building a numeric output string.
\ $USER   7,'HANDLER',HANDL

USER CONTEXT \ -- a
\ An area to specify vocabulary search order. Default to FORTH.
\ Vocabulary stack, 8 cells following CONTEXT.
\ $USER   7,'CONTEXT',CNTXT
\ _USER = _USER+VOCSS*CELLL    ;vocabulary link pointer

USER CURRENT \ -- a
\ Point to the vocabulary to be extended. Default to FORTH.
\ Vocabulary link uses one cell after CURRENT.
\ $USER   7,'CURRENT',CRRNT
\ _USER = _USER+CELLL          ;vocabulary link pointer

USER CP \ -- a
\ Point to the top of the code dictionary.
\ $USER   2,'CP',CP

USER NP \ -- a
\ Point to the top of the name dictionary.
\ $USER   2,'NP',NP

USER LAST \ -- a
\ Point to the last name in the name dictionary.
\ $USER   4,'LAST',LAST

\ Vocabulary and Search Order

\ look up ONLY-ALSO Forth 83 Bill Ragsdale

: doVOC \ --
\ Run time action of VOCABULARY's.
R>             \ get vocab pointer
CONTEXT ! ;    \ make it context vocab
\ 
\ $COLON  COMPO+5,'doVOC',DOVOC
\ DW      RFROM,CNTXT,STORE,EXIT

: FORTH \ --
\ MAKE FORTH the context vocabulary.
doVOC          \ retrieve FORTH pointer and make it context vocab
0 ,            \ vocabulary head pointer
0 ,            \ vocabulary link pointer
\ 
\ $COLON  5,'FORTH',FORTH
\ DW      DOVOC
\ DW      0
\ DW      0

\ More Stack Words

: ?DUP \ w -- w w | 0 
\ Dup top of stack if it is not zero
DUP
IF DUP THEN ;  \ add another copy if not 0
\ 
\         $COLON  4,'?DUP',QDUP
\         DW      DUPP
\         DW      QBRAN, QDUP1
\         DW      DUPP
\ QDUP1:  DW      EXIT

: ROT \ w1 w2 w3 -- w2 w3 w1
\ Rotate 3rd item to the top.
>R       \ save top item
SWAP     \ get 3rd to top
R>       \ retrieve top
SWAP ;   \ get 3rd to top
\ 
\ $COLON  3,'ROT',ROT
\ DW      TOR,SWAP,RFROM,SWAP,EXIT

: 2DROP \ w w --
\ Discard two items on stack.
DROP DROP ;
\ 
\ $COLON  4,'2DUP',DDUP
\ DW      DROP,DROP,EXIT

: 2DUP \ w1 w2 -- w1 w2 w1 w2
\ Duplicate top two items.
OVER OVER ;
\ 
\ $COLON  4,'2DUP',DDUP
\ DW      OVER,OVER,EXIT

\ More Arithmetic Operators

; + \ w w -- sum
\ Add top two items.
UM+      \ return sum and carry
DROP ;   \ discard carry
\ 
\ $COLON  1,'+',PLUS
\ DW      UPLUS,DROP,EXIT

; D+ \ d d -- d
\ Double addition, as an example using UM+.
>R SWAP >R    \ save high parts
UM+           \ add low parts with carry
R> R> +       \ add high parts
+ ;           \ add carry
\ 
\ $COLON  2,'D+',DPLUS
\ DW      TOR,SWAP,TOR,UPLUS
\ DW      RFROM,RFROM,PLUS,PLUS,EXIT

: NOT \ w -- w
\ One's complement of top of stack
-1 XOR ;
\ 
\ $COLON  3,'NOT',INVER
\ DW      DOLIT,-1,XORR,EXIT

: NEGATE \ n -- -n
\ Two's compliment of tos
NOT 1 + ;
\ $COLON  6,'NEGATE',NEGAT
\ DW      INVER,DOLIT,1,PLUS,EXIT

: DNEGATE \ d -- -d
\ Two's compliment of top double.
NOT >R       \ compliment and save high
NOT 1 UM+    \ complement low part
R> + ;       \ add carry to high
\ 
\ $COLON  7,'DNEGATE',DNEGA
\ DW      INVER,TOR
\ DW      INVER,DOLIT,1,UPLUS
\ DW      RFROM,PLUS,EXIT

: - \ n1 n2 -- n1-n2
\ Subtraction.
NEGATE + ;
\ 
\ $COLON  1,'-',SUBB
\ DW      NEGAT,PLUS,EXIT

: ABS \ n -- n
\ return the absolute value of n.
DUP 0<
IF NEGATE THEN ;
\ 
\ $COLON  3,'ABS',ABSS
\         DW      DUPP,ZLESS
\         DW      QBRAN,ABS1
\         DW      NEGAT
\ ABS1:   DW      EXIT

: = \ w w -- t
\ Return true if top two are equal.
XOR            \ compare all bits
IF 0 EXIT THEN \ return 0 if mismash
-1 ;           \ match completely, return -1
\ 
\ $COLON  1,'=',EQUAL
\         DW      XORR
\         DW      QBRAN,EQU1
\         DW      DOLIT,0,EXIT   ;false flag
\ EQU1:   DW      DOLIT,-1,EXIT  ;true  flag

: U< \ u1 u2 -- t
\ Unsigned compare of top two items.
2DUP XOR 0<    \ compare sign bits
IF SWAP DROP   \ sign bit different
 0< EXIT       \ t follows u1
THEN
- 0< ;         \ same sign, subtract
\ $COLON  2,'U<',ULESS
\         DW      DDUP,XORR,ZLESS
\         DW      QBRAN,ULES1
\         DW      SWAP,DROP,ZLESS,EXIT
\ ULES:   DW      SUBB,ZLESS,EXIT

: < \ n1 n2 -- t
\ Signed compare of top two items.
2DUP XOR 0<       \ compare sign bits
IF DROP           \ sign bit different
 0< EXIT          \ t follows n1
THEN
- 0< ;            \ same sign, subtract
\ 
\ $COLON  1,'<',LESS
\         DW      DDUP,XORR,ZLESS
\         DW      QBRAN,LESS1
\         DW      DROP,ZLESS,EXIT
\ LESS1:  DW      SUBB,ZLESS,EXIT

: MAX \ n1 n2 -- n
\ Return the greater of two stack items.
2DUP <            \ if n1 < n2
IF SWAP THEN      \ drop n1
DROP ;            \ else drop n2
\ 
\ $COLON  3,'MAX',MAX
\         DW      DDUP,LESS
\         DW      QBRAN,MAX1
\         DW      SWAP
\ MAX1:   DROP,EXIT

: MIN \ n1 n2 -- n
\ Return the smaller of top two stack items.
2DUP SWAP <       \ (if n1 > n2)
IF SWAP THEN      \ drop n1
DROP ;            \ else drop n2
\ 
\ : MIN   SWAP MAX ;   \ refactored alternative ?
\ 
\ $COLON  3,'MIN',MIN
\         DW      DDUP,SWAP,LESS
\         DW      QBRAN,MIN1
\         DW      SWAP
\ MIN1:   DROP,EXIT

: WITHIN \ u ul uh -- t
\ Return true if u is within the range of ul and uh; ul<=u<uh.
\ unsigned assummed
OVER - >R         \ distance between u1 and uh
-                 \ distance between u and ul
R> U< ;           \ compare the distances
\ 
\ $COLON  6,'WITHIN',WITHI
\ DW      OVER,SUBB,TOR
\ DW      SUBB,RFROM,ULESS

: UM/MOD \ ud1 udh u -- ur uq
\ Unsigned divide of a double by a single. Return mod and quotient.
\ 
\ think long division but the subtraction stays put and the 
\ operand moves forward to compensate
\ CAREFUL: ud1 morphs into the quotient uq
\          udh becomes the remainder ur
2DUP U<
IF NEGATE         \ negate u for subtraction
 15 FOR           \ repeat 16 times for 16 bits
  >R              \ save -u
  DUP UM+         \ left shift udh
  >R >R DUP UM+   \ left shift udl 
  R> + DUP        \ add carry to udh
  R> R@           \ retrieve -u
  SWAP >R         \ carry to RS
  UM+             \ add -u from udh
  R> OR           \ a borrow?
  IF >R DROP      \ yes, add a bit to quotient
   1 + R>
  ELSE DROP       \ no borrow
  THEN R>         \ retrieve -u
 NEXT             \ repeat for 16 bits
 DROP SWAP EXIT   \ return remainder and quotient
THEN DROP 2DROP   \ overflow, return -1's
-1 DUP ;

\ $COLON  6,'UM/MOD',UMMOD
\         DW      DDUP,U<
\         DW      QBRAN,UMM4
\         DW      NEGAT,DOLIT,15,TOR
\ UMM1:   DW      TOR,DUPP,UPLUS
\         DW      TOR,TOR,DUPP,UPLUS
\         DW      RFROM,PLUS,DUPP
\         DW      RFROM,RAT,SWAP,TOR
\         DW      UPLUS,RFROM,ORR
\         DW      QBRAN,UMM2
\         DW      TOR,DROP,DOLIT,1,PLUS,RFROM
\         DW      BRAN,UMM3
\ UMM2:   DW      DROP
\ UMM3:   DW      RFROM
\         DW      DONXT,UMM1
\         DW      DROP,SWAP,EXIT
\ UMM4:   DW      DROP,DDROP
\         DW      DOLIT,-1,DUPP,EXIT    ;overflow, return max

: M/MOD \ d n -- r q
\ Signed floor divide of DOUBLE by single. Return mod and quotient.
\                        dl dh n -- r q
\ TODO understand the floor
DUP 0<            \ n negative?
DUP >R            \ save a copy of flag
IF NEGATE >R      \ take abs of n
   DNEGATE R>     \ negative d also
THEN >R
DUP 0<            \ if d is negative
IF R@ + THEN      \ floor it
R> UM/MOD         \ now divide
R>                \ in n is negative
IF SWAP NEGATE SWAP THEN ;  \ negative remainder also
\ 
\ $COLON  5,'M/MOD',MSMOD
\         DW      DUPP,ZLESS,DUPP,TOR
\         DW      QBRAN,MMOD1
\         DW      NEGAT,TOR,DNEGA,RFROM
\ MMOD1:  DW      TOR,DUPP,ZLESS
\         DW      QBRAN,MMOD2
\         DW      RAT,PLUS
\ MMOD2:  DW      RFROM,UMMOD,RFROM
\         DW      QBRAN,MMOD3
\         DW      SWAP,NEGAT,SWAP
\ MMOD3:  DW      EXIT

: /MOD \ n1 n2 -- r q
\ Signed divide. Return mod and quotient.
OVER 0<           \ sign extend n1
SWAP M/MOD ;      \ floored divide
\ 
\ $COLON  4,'/MOD',SLMOD
\         DW      OVER,ZLESS
\         DW      SWAP,MSMOD,EXIT

: MOD \ n n -- r
\ Signed divide. Return mod only.
/MOD DROP ;       \ discard quotient
\ 
\ $COLON  3,'MOD',MODD
\         DW      SLMOD,DROP,EXIT

: /   \ n n -- q
\ Signed divide. Return quotient only.
/MOD SWAP DROP ;  \ discard remainder
\ 
\ $ COLON 1,'/',SLASH
\         DW      SLMOD,SWAP,DROP,EXIT

: UM* \ u1 u2 -- ud
\ Unsigned multiply. Return double product.
0 SWAP            \ u1 sum u2
15 FOR            \ repeat for 16 bits
 DUP UM+ >R >R    \ left shift u2
 DUP UM+          \ left shift sum
 R> +             \ add carry to u2
 R>               \ carry shifted out of u2
 IF >R OVER UM+   \ add u1 to sum
  R> +            \ carry into u2
 THEN
NEXT              \ repeat 16 times to form ud
ROT DROP ;        \ discard u1
\ 
\ $COLON  3,'UM*',UMSTA
\         DW      DOLIT,0,SWAP,DOLIT,15,TOR
\ UMST1:  DW      DUPP,UPLUS,TOR,TOR
\         DW      DUPP,UPLUS
\         DW      RFROM,PLUS,RFROM
\         DW      QBRAN,UMST2
\         DW      TOR,OVER,UPLUS,RFROM,PLUS
\ UMST2:  DW      DONXT,UMST1
\         DW      ROT,DROP,EXIT

: * \ n n -- n
\ Signed multiply. Return single product.
UM* DROP ;        / retain only low part
\ 
\ $COLON  1,'*',STAR
\         DW      UMSTA,DROP,EXIT

: M* \ n1 n2 -- d
\ Signed multiply. Return double product.
2DUP XOR 0< >R    \ n1 n2 have same sign?
ABS SWAP ABS UM*  \ multiply absolutes
R> IF DNEGATE THEN ; \ negate if signs are different
\ 
\ $COLON  2,'M*',MSTAR
\         DW      DDUP,XORR,ZLESS,TOR
\         DW      ABSS,SWAP,ABSS,UMSTA,RFROM
\         DW      QBRAN,MSTA1
\         DW      DENEGA
\ MSTA1:  DW      EXIT

\ Scaling Words

: */MOD \ n1 n2 n3 -- r q
\ Multiply n1 and n2, then divide by n3. Return mod and quotient.
>R M*             \ n1*n2
R> M/MOD ;        \ n1+n2/n3 with remainder
\ 
\ $COLON  5,'*/MOD',SSMOD
\         DW      TOR,MSTAR,RFROM,MSMOD,EXIT

: */    \ n1 n2 n3 -- Multiply n1 by n2, then divide by n3. 
\                     Return quotient.
*/MOD SWAP DROP ; \ discard remainder
\ 
\ $COLON  2,'*/',STASL
\         DW      SSMOD,SWAP,DROP,EXIT

\ Memory Alignment Words

: CELL+ \ a -- a
\ Add cell size in byte to address.
2 + ;
\ 
\ $COLON  5,'CELL+',CELLP
\         DW      DOLIT,CELLL,PLUS,EXIT

: CELL- \ a -- a 
\ Subtract cell size in byte from address.
-2 + ;
\ 
\ $COLON  5,'CELL-',CELLM
\         DW      DOLIT,0-CELLL,PLUS,EXIT

: CELLS \ n -- n
\ Multiply TOS by cell size in bytes.
2 * ;
\ 
\ $COLON  5,'CELLS',CELLS
\         DW      DOLIT,CELLL,STAR,EXIT

: ALIGNED \ b -- a
\ Align address to the cell boundary.
DUP 0 2 UM/MOD    \ divide b by 2 : CELL for 2 bytes (16bit Forth)
DROP DUP          \ drop quotient, save remainder
IF 2 SWAP - THEN  \ add 1 if remainder is 1  ;this step is wierd
+ ;
\ 
\ $COLON  7,'ALIGNED',ALGND
\         DW      DUPP,DOLIT,0,DOLIT,CELLL
\         DW      UMMOD,DROP,DUP
\         DW      QBRAN,ALGN1
\         DW      DOLIT,CELLL,SWAP,SUBB
\ ALGN1:  DW      PLUS,EXIT

: BL \ -- 32
\ Return 32, the blank character.
32 ; 
\ 
\ $COLON  2,'BL',BLANK
\         DW      DOLIT,' ',EXIT

: >CHAR \ c -- c
\ Filler non-printing characters.
127 AND DUP       \ mask off the MSB
127 BL WITHIN     \ if it is a control character
IF DROP 95 THEN ; \ replace it by an underscore
\ 
\ $COLOL  5,'>CHAR',TCHAR
\         DW      DOLIT,07FH,ANDD,DUPP    ;mask msb
\         DW      DOLIT,07FH,BLANK,WITHI  ;check for printable
\         DW      QBRAN,TCHA1
\         DW      DROP,DOLIT,'_'          ;replace non prinatble
\ TCHA1:  DW      EXIT

\ Managing Data Stack

\ 0 PICK == DUP
\ 1 PICK == OVER



\         DW      .











\ .      .
\      .    .              ;
;
