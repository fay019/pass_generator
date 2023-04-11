&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME pass-generator
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS pass-generator 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-13 i-slider i-choice-az btn-exit ~
i-choice-09 btn-generate i-choice-symb i-result i-length t-copy 
&Scoped-Define DISPLAYED-OBJECTS i-slider i-choice-az i-choice-09 ~
i-choice-symb i-result i-length t-copy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR pass-generator AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-clear 
     LABEL "Clear" 
     SIZE 9 BY .81.

DEFINE BUTTON btn-copy 
     LABEL "Copy" 
     SIZE 9 BY .81.

DEFINE BUTTON btn-exit 
     LABEL "Close App" 
     SIZE 15 BY 1.12.

DEFINE BUTTON btn-generate 
     LABEL "Generate" 
     SIZE 15 BY 1.12.

DEFINE {&NEW} SHARED VARIABLE i-result AS CHARACTER INITIAL ? 
     VIEW-AS EDITOR NO-BOX
     SIZE 53 BY 1.77
     FONT 0 DROP-TARGET NO-UNDO.

DEFINE VARIABLE i-length AS INTEGER FORMAT "->9":U INITIAL 4 
     LABEL "Length" 
      VIEW-AS TEXT 
     SIZE 4 BY .62 NO-UNDO.

DEFINE VARIABLE t-copy AS CHARACTER FORMAT "X(256)":U INITIAL "Text in Zwischenablage kopiert" 
      VIEW-AS TEXT 
     SIZE 30 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 13.1.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 1.62.

DEFINE VARIABLE i-slider AS INTEGER INITIAL 4 
     VIEW-AS SLIDER MIN-VALUE 4 MAX-VALUE 60 HORIZONTAL NO-CURRENT-VALUE 
     TIC-MARKS NONE 
     SIZE 42 BY 1.31 NO-UNDO.

DEFINE VARIABLE i-choice-09 AS LOGICAL INITIAL yes 
     LABEL "Use digits (0-9)" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE i-choice-az AS LOGICAL INITIAL yes 
     LABEL "Use capital letters (A-Z)" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE i-choice-symb AS LOGICAL INITIAL yes 
     LABEL "Use symbols (@!$%&*...)" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-slider AT ROW 4.12 COL 14 NO-LABEL WIDGET-ID 2 NO-TAB-STOP 
     i-choice-az AT ROW 6.23 COL 3 WIDGET-ID 6
     btn-exit AT ROW 7.58 COL 41 WIDGET-ID 16
     i-choice-09 AT ROW 7.73 COL 3 WIDGET-ID 8
     btn-generate AT ROW 9.19 COL 41 WIDGET-ID 14
     i-choice-symb AT ROW 9.23 COL 3 WIDGET-ID 10
     btn-copy AT ROW 10.81 COL 4 WIDGET-ID 20
     btn-clear AT ROW 10.81 COL 53.5 RIGHT-ALIGNED WIDGET-ID 34
     i-result AT ROW 11.88 COL 3 NO-LABEL WIDGET-ID 22
     i-length AT ROW 4.38 COL 7.29 COLON-ALIGNED WIDGET-ID 4
     t-copy AT ROW 10.88 COL 11.86 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     "min. 4 und max. 60 Zeichen" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 3.15 COL 33 WIDGET-ID 32
     "Passwort-Generator" VIEW-AS TEXT
          SIZE 22 BY 1.08 AT ROW 1.58 COL 20.14 WIDGET-ID 28
          FONT 0
     RECT-1 AT ROW 1.35 COL 1.86 WIDGET-ID 12
     RECT-13 AT ROW 3.96 COL 1.86 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 58.14 BY 13.88 WIDGET-ID 100.


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
         HEIGHT             = 14.15
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
ON CHOOSE OF btn-clear IN FRAME F-Main /* Clear */
DO:
  RUN p-Clear.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy pass-generator
ON CHOOSE OF btn-copy IN FRAME F-Main /* Copy */
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
ON CHOOSE OF btn-exit IN FRAME F-Main /* Close App */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-generate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-generate pass-generator
ON CHOOSE OF btn-generate IN FRAME F-Main /* Generate */
DO:
  RUN p-Generator.
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
  DISPLAY i-slider i-choice-az i-choice-09 i-choice-symb i-result i-length 
          t-copy 
      WITH FRAME F-Main IN WINDOW pass-generator.
  ENABLE RECT-1 RECT-13 i-slider i-choice-az btn-exit i-choice-09 btn-generate 
         i-choice-symb i-result i-length t-copy 
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
      pass-generator:HEIGHT = 13.5.
   // Button Copy enable
   ASSIGN 
      t-copy:HIDDEN = TRUE
      btn-clear:HIDDEN = FALSE.
   ENABLE btn-copy WITH FRAME {&FRAME-NAME}.
   ENABLE btn-clear WITH FRAME {&FRAME-NAME}.
   i-result:SENSITIVE = TRUE.  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

