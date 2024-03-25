*&---------------------------------------------------------------------*
*& Include          ZIN_BM_EXBA_RPTF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form 0000_MAIN_PROC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0000_MAIN_PROC .
* Get data
  PERFORM 0010_GET_DATA.

* Process data
  PERFORM 0020_PROCESS_DATA.

* Display data
  PERFORM 0030_DISPLAY_DATA.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0010_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0010_GET_DATA .

  SELECT C~OBJTYPE AS OBJTYP,
         F~FILENAME,
         F~FNAME,
         F~FTEXT,
         F~FNHASH,
         V~ZDOCID,
         DOCRS,
         P~PARAM,
         PARTX,
         STRTY,
         ROWNO,
         COLNO,
         FIELDNAME,
         ' ' AS SCRTEXT_L,
         OVALUE,
         NVALUE,
         MATCHING,
         A~NAME_TEXT,
         F~CRUSR,
         F~CRDAT,
         F~CRTIM
    FROM ZTB_BM_EXBA_FM AS C
   INNER JOIN  ZTB_BM_EXBA_FFM AS F
      ON C~FNAME  = F~FNAME
   INNER JOIN ZTB_BM_EXBA_FPAR AS P
      ON F~FNAME  = P~FNAME
     AND F~FNHASH = P~FNHASH
   INNER JOIN ZTB_BM_EXBA_FVAL AS V
      ON F~FNAME  = V~FNAME
     AND F~FNHASH = V~FNHASH
     AND P~PARAM  = V~PARAM
     AND P~ZDOCID  = V~ZDOCID
   INNER JOIN USR21 AS U
      ON F~CRUSR  = U~BNAME
    LEFT JOIN ADRP AS A ON U~PERSNUMBER = A~PERSNUMBER
    INTO TABLE @GT_FVAL_FL
   WHERE F~FNAME IN @S_FNAME[]
     AND F~CRUSR IN @S_CRUSR[]
     AND F~CRDAT IN @S_CRDAT[]
     AND F~CRTIM IN @S_CRTIM[]
     AND DOCRS <> ''
     AND P~UPSTS = @GC_UPSTS-SUCCESS.
  SORT GT_FVAL_FL BY FNAME FNHASH ZDOCID PARAM ROWNO COLNO.
  MOVE-CORRESPONDING GT_FVAL_FL TO GT_FVAL_D.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0020_PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0020_PROCESS_DATA .

  DATA:
    LR_DATA    TYPE REF TO DATA,
    LT_FCAT    TYPE LVC_T_FCAT,
    LW_XSTRING TYPE XSTRING,
    LW_TABNAME TYPE TABNAME.
  FIELD-SYMBOLS:
    <LF_FVAL_D>   TYPE ZST_BM_EXBA_FVAL,
    <LFT_DATA>    TYPE STANDARD TABLE,
    <LF_NEST_VAL> TYPE ANY.

  LOOP AT GT_FVAL_D ASSIGNING <LF_FVAL_D>.
    AT NEW PARAM.
      LW_TABNAME = <LF_FVAL_D>-STRTY.
      CLEAR: LT_FCAT.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          I_STRUCTURE_NAME   = LW_TABNAME
          I_INTERNAL_TABNAME = LW_TABNAME
        CHANGING
          CT_FIELDCAT        = LT_FCAT.

      SORT LT_FCAT BY FIELDNAME.
    ENDAT.

    READ TABLE LT_FCAT INTO DATA(LS_FCAT) BINARY SEARCH
      WITH KEY FIELDNAME = <LF_FVAL_D>-FIELDNAME.
    IF SY-SUBRC IS INITIAL.
      <LF_FVAL_D>-SCRTEXT_L = LS_FCAT-SCRTEXT_L.
    ELSE.
      <LF_FVAL_D>-SCRTEXT_L = LS_FCAT-FIELDNAME.
    ENDIF.

    CLEAR: <LF_FVAL_D>-CFNAME.
    IF <LF_FVAL_D>-MATCHING IS INITIAL.
      CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
        EXPORTING
          I_MTYPE    = 'W'
        IMPORTING
          E_ROWCOLOR = <LF_FVAL_D>-CFNAME
        CHANGING
          C_ROW_DATA = <LF_FVAL_D>.
    ENDIF.
  ENDLOOP.
  SORT GT_FVAL_D BY FNAME FNHASH ZDOCID PARAM ROWNO COLNO.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0030_DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0030_DISPLAY_DATA .

  CALL SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ZGS_0100'.
  SET TITLEBAR 'ZGT_0100'.
  PERFORM 0100_PBO.
ENDMODULE.

*&---------------------------------------------------------------------*
*& FORM 0100_PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM 0100_PBO.
  DATA:
    LT_FCAT      TYPE LVC_T_FCAT,
    LS_EXBA_FPAR TYPE ZTB_BM_EXBA_FPAR,
    LS_LAYOUT    TYPE LVC_S_LAYO,
    LS_VARIANT   TYPE DISVARIANT,
    LT_EXCLUDING TYPE UI_FUNCTIONS.

  LS_LAYOUT-CWIDTH_OPT = 'X'.
  LS_LAYOUT-INFO_FNAME = 'CFNAME'.
  LS_VARIANT-REPORT = SY-REPID.
  LS_VARIANT-HANDLE = '0100'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZST_BM_EXBA_FVAL'
    CHANGING
      CT_FIELDCAT      = LT_FCAT.
  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'FNAME' OR 'FTEXT'  OR 'PARAM' OR 'PARTX' OR 'FILENAME'
        OR 'ROWNO' OR 'COLNO' OR 'FIELDNAME' OR 'NAME_TEXT'
        OR 'SCRTEXT_L' OR 'OVALUE' OR 'NVALUE' OR 'MATCHING'
        OR 'CRUSR' OR 'CRDAT' OR 'CRTIM' OR 'NAME_TEXT'.
      WHEN 'FNHASH' OR 'ZDOCID' OR 'STRTY' OR 'OBJTYP'.
        <LF_FCAT>-TECH = 'X'.
      WHEN 'DOCRS'.
        <LF_FCAT>-HOTSPOT = 'X'.
      WHEN OTHERS.
        <LF_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'ZFM_ALV_EXCL_EDIT_FC'
    IMPORTING
      T_EXCL_FC = LT_EXCLUDING.


  IF GO_ALV_0100 IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_HOSPOT_CLICK = '0100_HOSPOT_CLICK'
        I_CUS_CONTROL_NAME      = 'CUS_ALV'
        IS_VARIANT              = LS_VARIANT
        I_SAVE                  = 'A'
        IS_LAYOUT               = LS_LAYOUT
        IT_TOOLBAR_EXCLUDING    = LT_EXCLUDING
        I_SHOW_ERRBTN           = 'X'
      IMPORTING
        E_ALV_GRID              = GO_ALV_0100
      CHANGING
        IT_OUTTAB               = GT_FVAL_D
        IT_FIELDCATALOG         = LT_FCAT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_ALV_0100.

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_HOSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0100_HOSPOT_CLICK
  USING LPS_COLUMN    TYPE LVC_S_COL
        LPS_ROW_NO    TYPE LVC_S_ROID.
  DATA:
    LS_DETAIL  TYPE ZST_LVC_S_DETA,

    LR_DATA    TYPE REF TO DATA,
    LT_FCAT    TYPE LVC_T_FCAT,
    LW_XSTRING TYPE XSTRING,
    LW_TABNAME TYPE TABNAME,
    LS_FVAL_D  TYPE ZST_BM_EXBA_FVAL,
    LS_LAYOUT  TYPE LVC_S_LAYO,
    LS_VARIANT TYPE DISVARIANT.
  FIELD-SYMBOLS:
    <LFT_DATA>    TYPE STANDARD TABLE,
    <LF_NEST_VAL> TYPE ANY.

  CASE LPS_COLUMN-FIELDNAME.
    WHEN 'DOCRS'.
      INCLUDE <CNTN01>.
      DATA:
        LS_SAP_OBJECT TYPE SWC_OBJECT,
        LS_DFIES      TYPE DFIES,
        LW_FNAME      TYPE CHAR30,
        LFIELDNAME    TYPE DFIES-LFIELDNAME.

      READ TABLE GT_FVAL_D INTO LS_FVAL_D INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL AND LS_FVAL_D-DOCRS IS NOT INITIAL.

        SELECT *
          FROM SWOTDV
          INTO TABLE @DATA(LT_SWOTDV)
         WHERE OBJTYPE = @LS_FVAL_D-OBJTYP
           AND VERBTYPE = 'K'.

        SWC_CONTAINER CONTAINER.

        IF LINES( LT_SWOTDV ) = 1.
          READ TABLE LT_SWOTDV INTO DATA(LS_SWOTV) INDEX 1.
          LFIELDNAME = LS_SWOTV-REFFIELD.
          CALL FUNCTION 'DDIF_FIELDINFO_GET'
            EXPORTING
              TABNAME        = LS_SWOTV-REFSTRUCT
*             FIELDNAME      = LS_SWOTV-REFFIELD
              LFIELDNAME     = LFIELDNAME
            IMPORTING
              DFIES_WA       = LS_DFIES
            EXCEPTIONS
              NOT_FOUND      = 1
              INTERNAL_ERROR = 2
              OTHERS         = 3.
          IF LS_DFIES-CONVEXIT IS NOT INITIAL.
            CREATE DATA  LR_DATA  TYPE (LS_DFIES-ROLLNAME).
            ASSIGN LR_DATA->* TO <LF_NEST_VAL>.
            <LF_NEST_VAL> = LS_FVAL_D-DOCRS.
            LW_FNAME = 'CONVERSION_EXIT_' && LS_DFIES-CONVEXIT && '_INPUT'.
            CALL FUNCTION LW_FNAME
              EXPORTING
                INPUT  = <LF_NEST_VAL>
              IMPORTING
                OUTPUT = <LF_NEST_VAL>.
          ENDIF.

          SWC_CREATE_OBJECT LS_SAP_OBJECT LS_FVAL_D-OBJTYP <LF_NEST_VAL>.
        ELSE.
          SWC_CREATE_OBJECT LS_SAP_OBJECT LS_FVAL_D-OBJTYP LS_FVAL_D-DOCRS.
        ENDIF.
        SWC_CALL_METHOD LS_SAP_OBJECT 'DISPLAY' CONTAINER.
*      SAP-OBJECT://BUS2093 {{RSNUM}}"
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CALL FUNCTION 'ZFM_SCR_SIMPLE_FC_PROCESS'.

ENDMODULE.
