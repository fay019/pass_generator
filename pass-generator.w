&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME pass-generator
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS pass-generator 
Using System.Text.RegularExpressions.*. 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR hf-strong-lvl AS INT NO-UNDO.

DEFINE TEMP-TABLE tt-lang NO-UNDO
    FIELD hf-lang AS CHARACTER
    FIELD hf-title AS CHARACTER
    FIELD hf-minmax AS CHARACTER
    FIELD hf-lenght AS CHARACTER 
    FIELD hf-az AS CHARACTER
    FIELD hf-degit AS CHARACTER
    FIELD hf-symbol AS CHARACTER
    FIELD hf-t-copy AS CHARACTER
    FIELD hf-btn-close AS CHARACTER
    FIELD hf-btn-generate AS CHARACTER
    FIELD hf-btn-copy AS CHARACTER
    FIELD hf-btn-clear AS CHARACTER
    FIELD hf-very-weak AS CHARACTER
    FIELD hf-weak AS CHARACTER 
    FIELD hf-medium AS CHARACTER
    FIELD hf-strong AS CHARACTER
    FIELD hf-very-strong AS CHARACTER
    INDEX idxLang IS PRIMARY hf-lang.

CREATE tt-lang.

/* Ajouter des valeurs pour "en" */
ASSIGN 
   tt-lang.hf-lang = "en"
   tt-lang.hf-title = "Password generator"
   tt-lang.hf-minmax = "min. 4 and max. 60 chars"
   tt-lang.hf-lenght = "Length"
   tt-lang.hf-az = "Use capital letters (A-Z)"
   tt-lang.hf-degit = "Use digits (0-9)"
   tt-lang.hf-symbol = "Use symbols (@!$%&*...)"
   tt-lang.hf-t-copy = "Text copied to clipboard"
   tt-lang.hf-btn-close = "Close APP"
   tt-lang.hf-btn-generate = "Generate"
   tt-lang.hf-btn-copy = "Copy"
   tt-lang.hf-btn-clear = "Clear"
   tt-lang.hf-very-weak = "Very weak"
   tt-lang.hf-weak = "Weak"
   tt-lang.hf-medium = "Medium"
   tt-lang.hf-strong = "Strong"
   tt-lang.hf-very-strong = "Very strong".

/* Ajouter des valeurs pour "de" */
CREATE tt-lang.
ASSIGN 
   tt-lang.hf-lang = "de"
   tt-lang.hf-title = "Passwort-Generator"
   tt-lang.hf-minmax = "min. 4 und max. 60 Zeichen"
   tt-lang.hf-lenght = "L„nge"
   tt-lang.hf-az = "Groábuchstaben verwenden (A-Z)"
   tt-lang.hf-degit = "Ziffern verwenden (0-9)"
   tt-lang.hf-symbol = "Symbole verwenden (@!$%&*...)"
   tt-lang.hf-t-copy = "Text in Zwischenablage kopiert"
   tt-lang.hf-btn-close = "App schlieáen"
   tt-lang.hf-btn-generate = "Erzeugen"
   tt-lang.hf-btn-copy = "kopieren"
   tt-lang.hf-btn-clear = "L”schen"
   tt-lang.hf-very-weak = "Sehr schwach"
   tt-lang.hf-weak = "Schwach"
   tt-lang.hf-medium = "Mittel"
   tt-lang.hf-strong = "Stark"
   tt-lang.hf-very-strong = "Sehr stark".
   
/* Ajouter des valeurs pour "fr" */
CREATE tt-lang.
ASSIGN 
   tt-lang.hf-lang = "fr"
   tt-lang.hf-title = "G‚n‚rateur de mot de passe"
   tt-lang.hf-minmax = "min. 4 et max. 60 caractŠres"
   tt-lang.hf-lenght = "Longueur"
   tt-lang.hf-az = "Utiliser des lettres majuscules (A-Z)"
   tt-lang.hf-degit = "Utiliser des chiffres (0-9)"
   tt-lang.hf-symbol = "Utiliser des symboles (@!$%&*...)"
   tt-lang.hf-t-copy = "Texte copi‚ dans le presse-papiers"
   tt-lang.hf-btn-close = "Fermer APP"
   tt-lang.hf-btn-generate = "G‚n‚rer"
   tt-lang.hf-btn-copy = "Copier"
   tt-lang.hf-btn-clear = "Effacer"
   tt-lang.hf-very-weak = "TrŠs faible"
   tt-lang.hf-weak = "Faible "
   tt-lang.hf-medium = "Moyen "
   tt-lang.hf-strong = "Fort"
   tt-lang.hf-very-strong = "TrŠs fort".


FIND FIRST tt-lang WHERE tt-lang.hf-lang = "de" NO-LOCK NO-ERROR. 
IF NOT AVAILABLE tt-lang THEN LEAVE.                                 

ASSIGN hf-strong-lvl = 5.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-13 strong-bar-1 strong-bar-2 ~
i-lang i-slider i-choice-az btn-exit i-choice-09 btn-generate i-choice-symb ~
i-result t-title t-minmax i-length t-copy 
&Scoped-Define DISPLAYED-OBJECTS i-lang i-slider i-choice-az i-choice-09 ~
i-choice-symb i-result t-title t-minmax i-length t-copy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD password-lvl pass-generator 
FUNCTION password-lvl RETURNS INTEGER
  ( INPUT pass AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR pass-generator AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-clear 
     LABEL "L”schen" 
     SIZE 10.43 BY .81.

DEFINE BUTTON btn-copy 
     LABEL "kopieren" 
     SIZE 10 BY .81.

DEFINE BUTTON btn-exit 
     LABEL "App schlieáen" 
     SIZE 15 BY 1.12.

DEFINE BUTTON btn-generate 
     LABEL "Erzeugen" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE i-lang AS CHARACTER FORMAT "X(256)":U INITIAL "de" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "EN","en",
                     "DE","de",
                     "FR","fr"
     DROP-DOWN-LIST
     SIZE 6 BY .92 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE i-result AS CHARACTER INITIAL ? 
     VIEW-AS EDITOR NO-BOX
     SIZE 53 BY 1.77
     FONT 0 DROP-TARGET NO-UNDO.

DEFINE VARIABLE i-length AS INTEGER FORMAT "->9":U INITIAL 4 
     LABEL "L„nge" 
      VIEW-AS TEXT 
     SIZE 4 BY .62 NO-UNDO.

DEFINE VARIABLE t-copy AS CHARACTER FORMAT "X(256)":U INITIAL "Text in Zwischenablage kopiert" 
      VIEW-AS TEXT 
     SIZE 31.14 BY .62 NO-UNDO.

DEFINE VARIABLE t-minmax AS CHARACTER FORMAT "X(256)":U INITIAL "min. 4 und max. 60 Zeichen" 
      VIEW-AS TEXT 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE t-strong AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     FONT 0 NO-UNDO.

DEFINE VARIABLE t-title AS CHARACTER FORMAT "X(256)":U INITIAL "Passwort-Generator" 
      VIEW-AS TEXT 
     SIZE 33 BY 1.08
     FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 13.1.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 1.62.

DEFINE RECTANGLE strong-bar-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 1.

DEFINE RECTANGLE strong-bar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 56 BY 1.

DEFINE VARIABLE i-slider AS INTEGER INITIAL 4 
     VIEW-AS SLIDER MIN-VALUE 4 MAX-VALUE 60 HORIZONTAL NO-CURRENT-VALUE 
     TIC-MARKS NONE 
     SIZE 39 BY 1.31 NO-UNDO.

DEFINE VARIABLE i-choice-09 AS LOGICAL INITIAL yes 
     LABEL "Ziffern verwenden (0-9)" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE i-choice-az AS LOGICAL INITIAL yes 
     LABEL "Groábuchstaben verwenden (A-Z)" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1.1 NO-UNDO.

DEFINE VARIABLE i-choice-symb AS LOGICAL INITIAL yes 
     LABEL "Symbole verwenden (@!$%&*...)" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-lang AT ROW 1.54 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     i-slider AT ROW 4.12 COL 17 NO-LABEL WIDGET-ID 2 NO-TAB-STOP 
     i-choice-az AT ROW 6.23 COL 3 WIDGET-ID 6
     btn-exit AT ROW 7.58 COL 41 WIDGET-ID 16
     i-choice-09 AT ROW 7.73 COL 3 WIDGET-ID 8
     btn-generate AT ROW 9.19 COL 41 WIDGET-ID 14
     i-choice-symb AT ROW 9.23 COL 3 WIDGET-ID 10
     btn-copy AT ROW 10.81 COL 3 WIDGET-ID 20
     btn-clear AT ROW 10.81 COL 55 RIGHT-ALIGNED WIDGET-ID 34
     i-result AT ROW 11.88 COL 3 NO-LABEL WIDGET-ID 22
     t-title AT ROW 1.62 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     t-minmax AT ROW 2.88 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     i-length AT ROW 4.38 COL 10.43 COLON-ALIGNED WIDGET-ID 4
     t-copy AT ROW 10.88 COL 11.86 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     t-strong AT ROW 14.85 COL 21.43 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     RECT-1 AT ROW 1.35 COL 1.86 WIDGET-ID 12
     RECT-13 AT ROW 3.96 COL 1.86 WIDGET-ID 30
     strong-bar-1 AT ROW 14.65 COL 2 WIDGET-ID 44
     strong-bar-2 AT ROW 14.65 COL 2 WIDGET-ID 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 58.14 BY 16.58 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW pass-generator ASSIGN
         HIDDEN             = YES
         TITLE              = "Passwort-Generator"
         HEIGHT             = 17.73
         WIDTH              = 58.86
         MAX-HEIGHT         = 21.58
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 21.58
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT pass-generator:LOAD-ICON("adeicon/comp%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/comp%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW pass-generator
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btn-clear IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       btn-clear:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btn-copy IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn-copy:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       i-length:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR i-result IN FRAME F-Main
   SHARED                                                               */
ASSIGN 
       i-result:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       i-slider:AUTO-RESIZE IN FRAME F-Main      = TRUE.

ASSIGN 
       t-copy:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       t-minmax:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN t-strong IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       t-title:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(pass-generator)
THEN pass-generator:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME pass-generator
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pass-generator pass-generator
ON END-ERROR OF pass-generator /* Passwort-Generator */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pass-generator pass-generator
ON WINDOW-CLOSE OF pass-generator /* Passwort-Generator */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-clear pass-generator
ON CHOOSE OF btn-clear IN FRAME F-Main /* L”schen */
DO:
  RUN p-Clear.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy pass-generator
ON CHOOSE OF btn-copy IN FRAME F-Main /* kopieren */
DO:
   CLIPBOARD:VALUE = i-result:SCREEN-VALUE.
   DISABLE btn-copy WITH FRAME {&FRAME-NAME}.
   ASSIGN 
      btn-copy:HIDDEN = TRUE
      t-copy:HIDDEN = FALSE.
   WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW OR CHOOSE OF btn-generate
   FOCUS btn-exit PAUSE 2. 
   ASSIGN 
      btn-copy:HIDDEN = FALSE
      t-copy:HIDDEN = TRUE.     
   ENABLE btn-copy WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exit pass-generator
ON CHOOSE OF btn-exit IN FRAME F-Main /* App schlieáen */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-generate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-generate pass-generator
ON CHOOSE OF btn-generate IN FRAME F-Main /* Erzeugen */
DO:
  RUN p-Generator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-lang
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-lang pass-generator
ON VALUE-CHANGED OF i-lang IN FRAME F-Main /* Combo 1 */
DO:
   FIND FIRST tt-lang WHERE tt-lang.hf-lang = i-lang:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tt-lang THEN LEAVE.
      ASSIGN 
         t-title:SCREEN-VALUE = tt-lang.hf-title
         t-minmax:SCREEN-VALUE = tt-lang.hf-minmax
         i-length:LABEL = tt-lang.hf-lenght
         i-choice-az:LABEL = tt-lang.hf-az     
         i-choice-09:LABEL = tt-lang.hf-degit
         i-choice-symb:LABEL = tt-lang.hf-symbol
         t-copy:SCREEN-VALUE = tt-lang.hf-t-copy
         btn-exit:LABEL = tt-lang.hf-btn-close
         btn-generate:LABEL = tt-lang.hf-btn-generate
         btn-copy:LABEL = tt-lang.hf-btn-copy
         btn-clear:LABEL = tt-lang.hf-btn-clear.
      CASE hf-strong-lvl:  
         WHEN 1 THEN 
            ASSIGN t-strong:SCREEN-VALUE = tt-lang.hf-very-weak. 
         WHEN 2 THEN 
            ASSIGN t-strong:SCREEN-VALUE = tt-lang.hf-weak.
         WHEN 3 THEN            
            ASSIGN t-strong:SCREEN-VALUE = tt-lang.hf-medium.
         WHEN 4 THEN 
            ASSIGN t-strong:SCREEN-VALUE = tt-lang.hf-strong.
         WHEN 5 THEN 
            ASSIGN t-strong:SCREEN-VALUE = tt-lang.hf-very-strong.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-slider
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-slider pass-generator
ON VALUE-CHANGED OF i-slider IN FRAME F-Main
DO:
   ASSIGN 
      i-length:SCREEN-VALUE = i-slider:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK pass-generator 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN enable_UI.
   ASSIGN 
      btn-copy:HIDDEN = TRUE
      t-copy:HIDDEN = TRUE.
   // reduce the main frame
   ASSIGN
      RECT-1:HEIGHT = 9.3
      pass-generator:HEIGHT = 9.8.
   
   
   
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI pass-generator  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(pass-generator)
  THEN DELETE WIDGET pass-generator.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI pass-generator  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY i-lang i-slider i-choice-az i-choice-09 i-choice-symb i-result t-title 
          t-minmax i-length t-copy 
      WITH FRAME F-Main IN WINDOW pass-generator.
  ENABLE RECT-1 RECT-13 strong-bar-1 strong-bar-2 i-lang i-slider i-choice-az 
         btn-exit i-choice-09 btn-generate i-choice-symb i-result t-title 
         t-minmax i-length t-copy 
      WITH FRAME F-Main IN WINDOW pass-generator.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW pass-generator.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-Clear pass-generator 
PROCEDURE p-Clear :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   ASSIGN 
      btn-clear:HIDDEN = TRUE 
      btn-copy:HIDDEN = TRUE
      t-copy:HIDDEN = TRUE
      i-result:SCREEN-VALUE = "".
   // reduce the main frame
   ASSIGN
      RECT-1:HEIGHT = 9.3
      pass-generator:HEIGHT = 9.8.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-generator pass-generator 
PROCEDURE p-generator :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   DEF VAR hf-i     AS INT NO-UNDO.
   DEF VAR hf-temp  AS CHAR NO-UNDO. 
   DYNAMIC-FUNCTION('password-lvl':U).
   ASSIGN hf-temp = "abcdefghijklmnopqrstuvwxyz".
   
   IF i-choice-az:CHECKED = TRUE THEN
      ASSIGN hf-temp = hf-temp + "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
   IF i-choice-09:CHECKED = TRUE THEN
      ASSIGN hf-temp = hf-temp + "0123456789". 
   IF i-choice-symb:CHECKED = TRUE THEN
      ASSIGN hf-temp = hf-temp + "!~"#$%&'()*+,-./:;<=>?@[/]^_`~{|~}~~".    
      
      
   ASSIGN i-result:SCREEN-VALUE = "".
   
   DO hf-i = 1 TO INTEGER(i-length:SCREEN-VALUE):
      ASSIGN i-result:SCREEN-VALUE = i-result:SCREEN-VALUE +  SUBSTRING(hf-temp, RANDOM(1, LENGTH(hf-temp)), 1).           
   END.
   
   //Enlarge the main frame
   ASSIGN             
      RECT-1:HEIGHT = 12.80
      pass-generator:HEIGHT = 16.5.
   // Button Copy enable
   ASSIGN 
      t-copy:HIDDEN = TRUE
      btn-clear:HIDDEN = FALSE.
   ENABLE btn-copy WITH FRAME {&FRAME-NAME}.
   ENABLE btn-clear WITH FRAME {&FRAME-NAME}.
   ENABLE t-strong WITH FRAME {&FRAME-NAME}.
   i-result:SENSITIVE = TRUE.
   
   //w 56 / 5 = 11,2
   
   CASE password-lvl(i-result:SCREEN-VALUE):  
      WHEN 1 THEN DO: 
         strong-bar-2:WIDTH = 11.2.
         strong-bar-2:BGCOLOR =  12.
         t-strong:HIDDEN = FALSE. 
         t-strong:SCREEN-VALUE = tt-lang.hf-very-weak. 
         t-strong:BGCOLOR = ?.
      END.
      WHEN 2 THEN DO:
         strong-bar-2:WIDTH = 11.2 * 2.
         strong-bar-2:BGCOLOR =  6.   
         t-strong:HIDDEN = FALSE. 
         t-strong:SCREEN-VALUE = tt-lang.hf-weak.  
         t-strong:BGCOLOR = ?.
      END.
      WHEN 3 THEN DO:
         strong-bar-2:WIDTH = 11.2 * 3.
         strong-bar-2:BGCOLOR =  14.  
         t-strong:HIDDEN = FALSE.            
         t-strong:SCREEN-VALUE = tt-lang.hf-medium.
         t-strong:BGCOLOR =  14.
      END.
      WHEN 4 THEN DO:
         strong-bar-2:WIDTH = 11.2 * 4.
         strong-bar-2:BGCOLOR =  10.   
         t-strong:HIDDEN = FALSE. 
         t-strong:SCREEN-VALUE = tt-lang.hf-strong.
         t-strong:BGCOLOR =  10.
      END.
      WHEN 5 THEN DO: 
         strong-bar-2:WIDTH = 11.2 * 5.
         strong-bar-2:BGCOLOR =  2.   
         t-strong:HIDDEN = FALSE.
         t-strong:SCREEN-VALUE = tt-lang.hf-very-strong.
         t-strong:BGCOLOR = 2.
      END.
   END CASE.
   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION password-lvl pass-generator 
FUNCTION password-lvl RETURNS INTEGER
  ( INPUT pass AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR strength AS INT NO-UNDO INIT 0.     
   DEFINE VARIABLE regexp AS CLASS Regex NO-UNDO. 
   
   regexp = NEW Regex("[a-z]").
   /* V‚rifier si le mot de passe contient des caractŠres en minuscules */
   IF regexp:IsMatch(pass) THEN
      ASSIGN strength = strength + 1.   
   
   regexp = NEW Regex("[A-Z]").  
   /* V‚rifier si le mot de passe contient des caractŠres en majuscules */
   IF regexp:IsMatch(pass) THEN
      ASSIGN strength = strength + 1.
   
   regexp = NEW Regex("[\d]").  
   /* V‚rifier si le mot de passe contient des chiffres */
   IF regexp:IsMatch(pass) THEN
      ASSIGN strength = strength + 1.    
   
   regexp = NEW Regex("[!@#$%^&*()_+~~`|~}~{~[\~]~\:;?><,./-=]").  
   /* V‚rifier si le mot de passe contient des caractŠres sp‚ciaux */
   IF regexp:IsMatch(pass) THEN
      ASSIGN strength = strength + 1.

   /* V‚rifier si le mot de passe a une longueur suffisante */
   IF LENGTH(pass) > 8 THEN
      ASSIGN strength = strength + 1.

   RETURN strength.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

