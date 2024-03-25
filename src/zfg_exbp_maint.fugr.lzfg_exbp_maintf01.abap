*&---------------------------------------------------------------------*
*& Include          LZFG_EXBP_MAINTF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  9999_GET_OBJTYPE_FROM_FNAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 9999_GET_OBJTYPE_FROM_FNAME INPUT.
  IF ZTB_BM_EXBA_FM-FNAME IS NOT INITIAL .
    IF ZTB_BM_EXBA_FM-OBJTYPE IS INITIAL.
      SELECT SINGLE OBJTYPE
        FROM SWOTDV
        INTO ZTB_BM_EXBA_FM-OBJTYPE
       WHERE ABAPTYPE = 'F'
         AND ABAPNAME = ZTB_BM_EXBA_FM-FNAME.
    ENDIF.
    IF ZTB_BM_EXBA_FM-FTEXT IS INITIAL.
      SELECT SINGLE STEXT
        FROM TFTIT
        INTO ZTB_BM_EXBA_FM-FTEXT
       WHERE SPRAS = SY-LANGU
         AND FUNCNAME = ZTB_BM_EXBA_FM-FNAME.
    ENDIF.
  ENDIF.

ENDMODULE.

FORM 9999_ZTB_BM_EXBA_FM_05.
  DATA:
    LW_EU_LNAME     TYPE EU_LNAME,
    LS_INTERFACE    TYPE RSFBINTFV,
    LW_READED_STATE TYPE R3STATE,
    LT_PARAM        TYPE RSFB_PARA,
    LS_PARAM        TYPE RSFBPARA,
    LS_EXBA_PAR     LIKE ZVI_BM_EXBA_PAR_TOTAL,
    LT_EXBA_PAR     LIKE TABLE OF ZVI_BM_EXBA_PAR_TOTAL.

  IF ZTB_BM_EXBA_FM-FNAME IS NOT INITIAL
  AND STATUS_ZTB_BM_EXBA_FM-FCODE = 'NEWL'.
    LW_EU_LNAME = ZTB_BM_EXBA_FM-FNAME.

    SELECT F~PARAMETER
           F~STRUCTURE
           F~PARAMTYPE
           T~STEXT
      FROM FUPARAREF AS F INNER JOIN FUNCT AS T
        ON F~FUNCNAME = T~FUNCNAME
       AND F~PARAMETER = T~PARAMETER
      INTO CORRESPONDING FIELDS OF TABLE LT_PARAM
     WHERE F~FUNCNAME = ZTB_BM_EXBA_FM-FNAME
       AND F~R3STATE  = 'A'
       AND T~KIND     = 'P'
*       AND F~PARAMTYPE IN ('I', 'T')
       AND SPRAS = SY-LANGU
     ORDER BY PARAMTYPE PPOSITION.

    LOOP AT LT_PARAM INTO LS_PARAM.
      LS_EXBA_PAR-MANDT = SY-MANDT.
      LS_EXBA_PAR-FNAME = ZTB_BM_EXBA_FM-FNAME.
      LS_EXBA_PAR-FTEXT = ZTB_BM_EXBA_FM-FTEXT.
      LS_EXBA_PAR-PARAM = LS_PARAM-PARAMETER.
      LS_EXBA_PAR-STRTY = LS_PARAM-STRUCTURE.
      LS_EXBA_PAR-PARTX = LS_PARAM-STEXT.
      LS_EXBA_PAR-SHPOS = SY-TABIX.
      CASE LS_PARAM-PARAMTYPE.
        WHEN 'E'.
          LS_EXBA_PAR-PARTY = '20'.
        WHEN 'I'.
          LS_EXBA_PAR-PARTY = '10'.
        WHEN 'T'.
          LS_EXBA_PAR-PARTY = '30'.
        WHEN 'C'.
          LS_EXBA_PAR-PARTY = '40'.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
      LS_EXBA_PAR-ACTION = 'I'.
      APPEND LS_EXBA_PAR TO LT_EXBA_PAR.
    ENDLOOP.
    EXPORT ZVI_BM_EXBA_PAR_TOTAL = LT_EXBA_PAR TO MEMORY ID 'ZFG_EXBA_BAPI'.
  ENDIF.

ENDFORM.

FORM 9999_ZVI_BM_EXBA_PAR_AA.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT *
    FROM ZTB_BM_EXBA_PAR
    WHERE (VIM_WHERETAB) .
    CLEAR ZVI_BM_EXBA_PAR .
    ZVI_BM_EXBA_PAR-MANDT =    ZTB_BM_EXBA_PAR-MANDT .
    ZVI_BM_EXBA_PAR-FNAME =    ZTB_BM_EXBA_PAR-FNAME .
    ZVI_BM_EXBA_PAR-PARAM =    ZTB_BM_EXBA_PAR-PARAM .
    ZVI_BM_EXBA_PAR-PARTX =    ZTB_BM_EXBA_PAR-PARTX .
    ZVI_BM_EXBA_PAR-STRTY =    ZTB_BM_EXBA_PAR-STRTY .
    ZVI_BM_EXBA_PAR-SHPOS =    ZTB_BM_EXBA_PAR-SHPOS .
    ZVI_BM_EXBA_PAR-HDRFL =    ZTB_BM_EXBA_PAR-HDRFL .
    SELECT SINGLE *
      FROM ZTB_BM_EXBA_FM
      WHERE FNAME = ZTB_BM_EXBA_PAR-FNAME .
    IF SY-SUBRC EQ 0.
      ZVI_BM_EXBA_PAR-FTEXT =      ZTB_BM_EXBA_FM-FTEXT .
    ENDIF.
    <VIM_TOTAL_STRUC> = ZVI_BM_EXBA_PAR.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.

  RETURN.
  SELECT *
    FROM ZTB_BM_EXBA_PAR
    INTO CORRESPONDING FIELDS OF TABLE ZVI_BM_EXBA_PAR_TOTAL.
  TOTAL[] = ZVI_BM_EXBA_PAR_TOTAL[].
*  EXTRACT[] = ZVI_BM_EXBA_PAR_TOTAL[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  0102_BAPIDET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 0102_BAPIDET INPUT.

  IF SY-UCOMM = 'BAPIDET'.
    PERFORM 0102_BAPIDET.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form 0102_BAPIDET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 0102_BAPIDET .
  DATA:
    LW_OBJECT TYPE DOKHL-OBJECT.

  LW_OBJECT  = ZVI_BM_EXBA_PAR-FNAME.


  CALL FUNCTION 'DOCU_CALL'
    EXPORTING
      DISPL         = 'X'
      DISPL_MODE    = '2'
      ID            = 'FU'
      LANGU         = SY-LANGU
      OBJECT        = LW_OBJECT
      SUPPRESS_EDIT = 'X'.
ENDFORM.
