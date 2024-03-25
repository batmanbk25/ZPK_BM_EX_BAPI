*&---------------------------------------------------------------------*
*& Include ZIN_BM_CS_UPLOADTOP         - Report ZPG_BM_CS_UPLOAD
*&---------------------------------------------------------------------*

**********************************************************************
* TYPES                                                          *
**********************************************************************
TYPES:
  BEGIN OF GTY_USR_INFO,
    UNAME      TYPE UNAME,
    PERSNUMBER TYPE USR21-PERSNUMBER,
    NAME_TEXT  TYPE ADRP-NAME_TEXT,
  END OF GTY_USR_INFO.

**********************************************************************
* CONSTANTS                                                          *
**********************************************************************
CONSTANTS:
  BEGIN OF GC_CSFSTS,
    NULL    TYPE ZDD_BM_EXBA_UPSTS VALUE ' ', "Null
    NOT_YET TYPE ZDD_BM_EXBA_UPSTS VALUE '1', "Not yet
    WIP     TYPE ZDD_BM_EXBA_UPSTS VALUE '2', "WIP
    ERROR   TYPE ZDD_BM_EXBA_UPSTS VALUE '3', "Error
    SUCCESS TYPE ZDD_BM_EXBA_UPSTS VALUE '4', "Done
    CANCEL  TYPE ZDD_BM_EXBA_UPSTS VALUE '5', "Cancelled
  END OF GC_CSFSTS,
  BEGIN OF GC_UPSTS,
    NULL    TYPE ZDD_BM_EXBA_UPSTS VALUE ' ', "Null
    NOT_YET TYPE ZDD_BM_EXBA_UPSTS VALUE '1', "Not yet
    WIP     TYPE ZDD_BM_EXBA_UPSTS VALUE '2', "WIP
    ERROR   TYPE ZDD_BM_EXBA_UPSTS VALUE '3', "Error
    SUCCESS TYPE ZDD_BM_EXBA_UPSTS VALUE '4', "Done
    CANCEL  TYPE ZDD_BM_EXBA_UPSTS VALUE '5', "Cancelled
  END OF GC_UPSTS,
  BEGIN OF GC_PARAMTYPE,
    IMPORTING TYPE ZDD_BM_EXBA_PARTY VALUE '20',
    EXPORTING TYPE ZDD_BM_EXBA_PARTY VALUE '10',
    TABLES    TYPE ZDD_BM_EXBA_PARTY VALUE '30',
    CHANGING  TYPE ZDD_BM_EXBA_PARTY VALUE '40',
  END OF GC_PARAMTYPE.

**********************************************************************
* DATA                                                               *
**********************************************************************
DATA:
  GT_EXBA_FM     TYPE TABLE OF ZTB_BM_EXBA_FM,
  GT_EXBA_FFM    TYPE TABLE OF ZTB_BM_EXBA_FFM,
  GT_EXBA_FFM_D  TYPE TABLE OF ZST_BM_EXBA_FFM,
  GS_EXBA_FFM_D  TYPE ZST_BM_EXBA_FFM,
  GS_EXBA_FM     TYPE ZTB_BM_EXBA_FM,
  GS_EXBA_FFM    TYPE ZTB_BM_EXBA_FFM,
  GS_FCAT_DOCID  TYPE LVC_S_FCAT,
  GT_EXBA_PAR    TYPE TABLE OF ZTB_BM_EXBA_PAR,
  GT_FUPARAREF   TYPE TABLE OF FUPARAREF,
  GT_EXBA_FPAR   TYPE TABLE OF ZTB_BM_EXBA_FPAR,
  GT_EXBA_FPAR_D TYPE TABLE OF ZST_BM_EXBA_FPAR,
  GT_FVAL_D      TYPE TABLE OF ZST_BM_EXBA_FVAL,
  GT_FMSG_DB     TYPE TABLE OF ZTB_BM_EXBA_FMSG,
  GT_FVAL_DB     TYPE TABLE OF ZTB_BM_EXBA_FVAL,
  GT_DATA_RAW    TYPE TABLE OF ZST_BM_EXBA_DATA_RAW,
  GT_USR_INFO    TYPE TABLE OF GTY_USR_INFO,
  GS_USR_INFO    TYPE GTY_USR_INFO,
  GO_ALV_0100    TYPE REF TO CL_GUI_ALV_GRID,
  GO_ALV_GRID    TYPE REF TO CL_GUI_ALV_GRID,
  GO_ALV_GRID_D  TYPE REF TO CL_GUI_ALV_GRID.

TABLES: ZTB_BM_EXBA_FFM.

TYPES:
  GTY_BAPI_CHAR_BATCH         TYPE TABLE OF BAPI_CHAR_BATCH,
  GTY_BAPI_LINK_GM_CHAR_BATCH TYPE TABLE OF BAPI_LINK_GM_CHAR_BATCH,
  GTY_ACC_DOC_ASSET_FIELDS    TYPE TABLE OF ZST_ACC_DOC_ASSET_FIELDS.
**********************************************************************
* PARAMETERS                                                         *
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-005.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT  1(10) TEXT-003.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS:
    P_UPLOAD RADIOBUTTON GROUP ID  DEFAULT 'X' USER-COMMAND UC1.
    SELECTION-SCREEN COMMENT  34(17) TEXT-001 FOR FIELD P_UPLOAD.
    PARAMETERS:
    P_DOWNTP RADIOBUTTON GROUP ID.
    SELECTION-SCREEN COMMENT  55(17) TEXT-004  FOR FIELD P_DOWNTP.

  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS:
    P_DIPLAY RADIOBUTTON GROUP ID.
    SELECTION-SCREEN COMMENT  34(17) TEXT-002  FOR FIELD P_DIPLAY.
    PARAMETERS:
    P_HISRP RADIOBUTTON GROUP ID.
    SELECTION-SCREEN COMMENT  55(20) TEXT-022  FOR FIELD P_HISRP.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) FOR FIELD P_FNAME.
    PARAMETERS:
      P_FNAME TYPE ZTB_BM_EXBA_FM-FNAME MODIF ID GR3.
    SELECTION-SCREEN POSITION 66.
    PARAMETERS:
      P_FTEXT TYPE ZTB_BM_EXBA_FM-FTEXT MODIF ID GR3.
  SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS:
    S_CRUSR FOR ZTB_BM_EXBA_FFM-CRUSR MODIF ID GR1,
    S_CRDAT FOR ZTB_BM_EXBA_FFM-CRDAT DEFAULT SY-DATUM MODIF ID GR1,
    S_CRTIM FOR ZTB_BM_EXBA_FFM-CRTIM MODIF ID GR1,
    S_FILE   FOR ZTB_BM_EXBA_FFM-FILENAME MODIF ID GR1 MATCHCODE OBJECT ZSH_BM_EXBA_FILENAME.
  PARAMETERS:
    P_UPSTS  TYPE ZDD_BM_EXBA_UPSTS_S DEFAULT '*'
                AS LISTBOX VISIBLE LENGTH 18 MODIF ID GR1,
    P_FILENM TYPE LOCALFILE MATCHCODE OBJECT ICL_DIAGFILENAME MODIF ID GR2.

  PARAMETERS:
    P_TEST  TYPE XMARK AS CHECKBOX MODIF ID UPL.

SELECTION-SCREEN END OF BLOCK B1.
