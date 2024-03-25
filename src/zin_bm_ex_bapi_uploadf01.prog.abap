*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_UPLOADF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form 0000_init_proc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 0000_INIT_PROC .
  DATA:
    LT_FIELDCAT_DOCID TYPE LVC_T_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_BM_EXBA_ZDOCID'
*     I_INTERNAL_TABNAME     = 'ZST_BM_EXBA_ZDOCID'
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT_DOCID
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  READ TABLE LT_FIELDCAT_DOCID INTO GS_FCAT_DOCID INDEX 1.
  GS_FCAT_DOCID-COL_POS  = 0.
  CLEAR: GS_FCAT_DOCID-REF_TABLE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 1000_PAI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 1000_PAI .
  DATA:
    LW_FILENAME TYPE STRING,
    LW_RESULT   TYPE XMARK.

  IF P_FTEXT IS INITIAL
  AND P_FNAME IS NOT INITIAL.
    SELECT SINGLE FTEXT
      FROM ZTB_BM_EXBA_FM
      INTO P_FTEXT
     WHERE FNAME = P_FNAME.
  ENDIF.

  IF SY-UCOMM = 'ONLI' AND P_UPLOAD IS NOT INITIAL.
    LW_FILENAME = P_FILENM.
    IF LW_FILENAME IS NOT INITIAL.
      CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
        EXPORTING
          FILE                 = LW_FILENAME
        RECEIVING
          RESULT               = LW_RESULT
        EXCEPTIONS
          CNTL_ERROR           = 1
          ERROR_NO_GUI         = 2
          WRONG_PARAMETER      = 3
          NOT_SUPPORTED_BY_GUI = 4
          OTHERS               = 5.
      IF SY-SUBRC <> 0 OR LW_RESULT IS INITIAL.
        MESSAGE S001(ZMS_BM_CS) DISPLAY LIKE 'E' WITH LW_FILENAME.
        SET CURSOR FIELD 'P_FILENM'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0000_MAIN_PROC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0000_MAIN_PROC .
  CASE 'X'.
    WHEN P_UPLOAD.
      PERFORM 0010_GET_CONFIG.

      PERFORM 0020_UPLOAD.

      PERFORM 0100_SAVE.

      PERFORM 0300_FC_PROCESS.

      PERFORM 0030_DISPLAY.

    WHEN P_DIPLAY.
      PERFORM 0060_GET_UPLOADED_DATA.

      PERFORM 0070_DISPLAY_UPLOADED.

    WHEN P_DOWNTP.
      PERFORM 0080_DOWNLOAD_TEMPLATE.
    WHEN P_HISRP.
      SUBMIT ZPG_BM_EXBA_RPT
        WITH S_FNAME = P_FNAME
        WITH S_CRUSR IN S_CRUSR
        WITH S_CRDAT IN S_CRDAT
        WITH S_CRTIM IN S_CRTIM

        AND RETURN .

    WHEN OTHERS.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0010_Get_Config
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0010_GET_CONFIG.
  DATA:
    LS_EXBA_PAR_D TYPE ZST_BM_EXBA_PAR,
    LS_DATA_RAW   TYPE ZST_BM_EXBA_DATA_RAW,
    LW_TABNAME    TYPE TABNAME,
    LT_FIELDCAT   TYPE LVC_T_FCAT,
    LS_FIELDCAT   TYPE LVC_S_FCAT,
    LW_LEN        TYPE INT4,
    LW_PARNAME    TYPE CHAR30.

  SELECT SINGLE *
    FROM ZTB_BM_EXBA_FM
    INTO GS_EXBA_FM
   WHERE FNAME = P_FNAME.

  SELECT *
    FROM ZTB_BM_EXBA_PAR
    INTO TABLE GT_EXBA_PAR
   WHERE FNAME = P_FNAME
*     AND SHPOS > 0
   ORDER BY SHPOS.

  SELECT *
    FROM FUPARAREF
    INTO TABLE GT_FUPARAREF
   WHERE FUNCNAME = P_FNAME
     AND R3STATE = 'A'.

  LOOP AT GT_EXBA_PAR INTO DATA(LS_EXBA_PAR).
    CLEAR: LS_DATA_RAW, LT_FIELDCAT.
    MOVE-CORRESPONDING LS_EXBA_PAR TO LS_DATA_RAW.
    MOVE-CORRESPONDING LS_EXBA_PAR TO LS_EXBA_PAR_D.
    IF LS_EXBA_PAR-STRTY IS NOT INITIAL.
      IF 1 = 2.
        CREATE DATA LS_DATA_RAW-REFDATA TYPE TABLE OF (LS_EXBA_PAR-STRTY).
      ELSE.
        LW_LEN = STRLEN( LS_EXBA_PAR-PARAM ) - 1.
        LW_PARNAME = LS_EXBA_PAR-PARAM(LW_LEN).
        IF LS_EXBA_PAR-PARAM+LW_LEN(1) = 'X'.
          READ TABLE GT_EXBA_PAR TRANSPORTING NO FIELDS
            WITH KEY PARAM = LW_PARNAME.
          CHECK SY-SUBRC IS NOT INITIAL.
        ENDIF.
        CLEAR: LT_FIELDCAT.

        PERFORM 9999_GET_FCAT_FROM_PARAM_TYPE
          CHANGING LS_EXBA_PAR_D.
        LT_FIELDCAT = LS_EXBA_PAR_D-FCATS.

        LS_FIELDCAT  = GS_FCAT_DOCID.
        LS_FIELDCAT-TABNAME = LS_EXBA_PAR-STRTY.
        LS_FIELDCAT-TABNAME = '1'.
        LS_FIELDCAT-COL_POS  = 0.

        INSERT LS_FIELDCAT INTO LT_FIELDCAT INDEX 1.
        LOOP AT LT_FIELDCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
          CASE <LF_FCAT>-INTTYPE.
            WHEN 'C'.
*              <LF_FCAT>-INTLEN = <LF_FCAT>-DD_OUTLEN = <LF_FCAT>-OUTPUTLEN = 255 .
            WHEN 'N'.
*              <LF_FCAT>-INTLEN = <LF_FCAT>-DD_OUTLEN = <LF_FCAT>-OUTPUTLEN = 255 .
            WHEN OTHERS.
          ENDCASE.
          ADD 1 TO <LF_FCAT>-COL_POS.
          CLEAR: <LF_FCAT>-REF_TABLE.
        ENDLOOP.
        LS_DATA_RAW-FIELDCAT = LT_FIELDCAT.

        IF LS_EXBA_PAR-PARTY = GC_PARAMTYPE-EXPORTING
        OR LS_EXBA_PAR-PARTY = GC_PARAMTYPE-CHANGING
        OR LS_EXBA_PAR-PARTY = GC_PARAMTYPE-TABLES.
          CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
            EXPORTING
              IT_FIELDCATALOG           = LT_FIELDCAT
            IMPORTING
              EP_TABLE                  = LS_DATA_RAW-REFDATA
            EXCEPTIONS
              GENERATE_SUBPOOL_DIR_FULL = 1
              OTHERS                    = 2.
        ENDIF.
      ENDIF.
    ENDIF.
    APPEND LS_DATA_RAW TO GT_DATA_RAW.
  ENDLOOP.

  SELECT SINGLE BNAME
         U~PERSNUMBER
         NAME_TEXT
    FROM USR21 AS U
   INNER JOIN ADRP AS A ON U~PERSNUMBER = A~PERSNUMBER
    INTO GS_USR_INFO
   WHERE BNAME = SY-UNAME.
  APPEND GS_USR_INFO TO GT_USR_INFO.

  IF GT_DATA_RAW[] IS INITIAL.
    MESSAGE S002(ZMS_EXBA) WITH P_FNAME DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0020_Upload
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0020_UPLOAD .
  DATA:
    LS_DATA_RAW    TYPE ZST_BM_EXBA_DATA_RAW,
    LT_FILENAME    TYPE TABLE OF RLGRAP-FILENAME,
    LS_EXBA_FPAR   TYPE ZTB_BM_EXBA_FPAR,
    LS_USRLOG      TYPE ZST_BM_USRLOG,
    LW_KEYHASH     TYPE CHAR32,
    LW_LEN         TYPE INT4,
    LS_EXBA_FPAR_D TYPE ZST_BM_EXBA_FPAR,
    LT_FVAL_DB     TYPE TABLE OF ZTB_BM_EXBA_FVAL,
    LS_FVAL_DB     TYPE ZTB_BM_EXBA_FVAL,
    LS_FMSG_DB     TYPE ZTB_BM_EXBA_FMSG,
    LW_ROWNO       TYPE INT4.
  FIELD-SYMBOLS:
    <LFT_DATA_D> TYPE TABLE.

  MOVE-CORRESPONDING GS_EXBA_FM TO GS_EXBA_FFM.
  GS_EXBA_FFM-FULLPATH = P_FILENM.
  SPLIT P_FILENM AT '\' INTO TABLE LT_FILENAME.
  IF LT_FILENAME IS NOT INITIAL.
    READ TABLE LT_FILENAME INTO GS_EXBA_FFM-FILENAME INDEX LINES( LT_FILENAME ).
  ENDIF.

  TRY.
      CALL METHOD CL_SYSTEM_UUID=>IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32
        RECEIVING
          UUID = GS_EXBA_FFM-FNHASH.
    CATCH CX_UUID_ERROR.
  ENDTRY.

*  CALL FUNCTION 'MD5_CALCULATE_HASH_FOR_CHAR'
*    EXPORTING
*      DATA           = GS_EXBA_FFM-FILENAME
*      LENGTH         = 128
*    IMPORTING
*      HASH           = GS_EXBA_FFM-FNHASH
*    EXCEPTIONS
*      NO_DATA        = 1
*      INTERNAL_ERROR = 2
*      OTHERS         = 3.
*
*  SELECT SINGLE FNHASH
*    FROM ZTB_BM_EXBA_FFM
*    INTO GS_EXBA_FFM-FNHASH
*   WHERE FNAME = P_FNAME
*     AND FNHASH = GS_EXBA_FFM-FNHASH .
*  IF SY-SUBRC IS INITIAL.
*    MESSAGE S001(ZMS_EXBA) WITH GS_EXBA_FFM-FILENAME DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  CALL FUNCTION 'ZFM_BM_BAPI_EXCEL_UPLOAD'
    EXPORTING
      I_TECHNAME_LINE   = 5
      I_LINE_DATA       = 7
      I_FILENAME        = P_FILENM
    CHANGING
      CT_DATA_RAW       = GT_DATA_RAW
    EXCEPTIONS
      CONVERSION_FAILED = 1
      OTHERS            = 2.

  GS_EXBA_FFM-UPSTS   = GC_UPSTS-NOT_YET.
  GS_EXBA_FFM-MANDT   = SY-MANDT.
  GS_EXBA_FFM-CRUSR   = SY-UNAME.
  GS_EXBA_FFM-CRDAT   = SY-DATUM.
  GS_EXBA_FFM-CRTIM   = SY-UZEIT.

  LOOP AT GT_DATA_RAW INTO LS_DATA_RAW.
    IF LS_DATA_RAW-REFDATA IS NOT INITIAL.

      ASSIGN LS_DATA_RAW-REFDATA->* TO <LFT_DATA_D>.
      IF SY-SUBRC IS INITIAL.
        IF LS_DATA_RAW-HDRFL IS NOT INITIAL.
          GS_EXBA_FFM-SUMREC = LINES( <LFT_DATA_D> ).
        ENDIF.

        LOOP AT <LFT_DATA_D> ASSIGNING FIELD-SYMBOL(<LF_DATA_D>).
          CLEAR: LS_EXBA_FPAR, LS_EXBA_FPAR_D.
          LW_ROWNO = SY-TABIX.
          MOVE-CORRESPONDING LS_DATA_RAW TO LS_EXBA_FPAR.
          LS_EXBA_FPAR-FNAME  = GS_EXBA_FFM-FNAME.
          LS_EXBA_FPAR-FNHASH = GS_EXBA_FFM-FNHASH.
          LS_EXBA_FPAR-UPSTS  = GC_UPSTS-NOT_YET.

          LS_EXBA_FPAR-MANDT = SY-MANDT.
          LS_EXBA_FPAR-CRUSR = SY-UNAME.
          LS_EXBA_FPAR-CRDAT = SY-DATUM.
          LS_EXBA_FPAR-CRTIM = SY-UZEIT.

          CALL TRANSFORMATION ID
            SOURCE DATA = <LF_DATA_D>
            RESULT XML LS_EXBA_FPAR-XMLSTR.

          LW_LEN = STRLEN( LS_EXBA_FPAR-XMLSTR ).
          CALL FUNCTION 'MD5_CALCULATE_HASH_FOR_CHAR'
            EXPORTING
              DATA           = LS_EXBA_FPAR-XMLSTR
              LENGTH         = LW_LEN
            IMPORTING
              HASH           = LW_KEYHASH
            EXCEPTIONS
              NO_DATA        = 1
              INTERNAL_ERROR = 2
              OTHERS         = 3.

          ASSIGN COMPONENT 'ZDOCID' OF STRUCTURE <LF_DATA_D>
            TO FIELD-SYMBOL(<LF_DOCID>).
          IF SY-SUBRC IS INITIAL.
            LS_EXBA_FPAR-ZDOCID = <LF_DOCID>.
          ENDIF.
          LS_EXBA_FPAR-TABKEY = LW_KEYHASH.

          APPEND LS_EXBA_FPAR TO GT_EXBA_FPAR.
          CLEAR: LS_EXBA_FPAR_D.
          MOVE-CORRESPONDING LS_EXBA_FPAR TO LS_EXBA_FPAR_D.
          LOOP AT LS_DATA_RAW-MSGDETAIL INTO DATA(LS_MSGDETAIL)
            WHERE ROWNO = LW_ROWNO.
            CALL FUNCTION 'ZFM_ALV_MESSAGE_PUT'
              EXPORTING
                I_MTYPE       = LS_MSGDETAIL-MTYPE
                I_MESSAGE     = LS_MSGDETAIL-MESSAGE
                I_RETURN      = LS_MSGDETAIL
              IMPORTING
                E_STATUS_ICON = LS_EXBA_FPAR_D-ICON
                E_ROWCOLOR    = LS_EXBA_FPAR_D-CFNAME
              CHANGING
                C_ALVLINE     = LS_EXBA_FPAR_D.

            MOVE-CORRESPONDING LS_EXBA_FPAR_D TO LS_FMSG_DB.
            MOVE-CORRESPONDING LS_MSGDETAIL TO LS_FMSG_DB.
            TRY.
                CALL METHOD CL_SYSTEM_UUID=>IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32
                  RECEIVING
                    UUID = LS_FMSG_DB-MSGID.
              CATCH CX_UUID_ERROR.
            ENDTRY.
            APPEND LS_FMSG_DB TO GT_FMSG_DB.
          ENDLOOP.

          LOOP AT LS_DATA_RAW-CELL_VALS ASSIGNING FIELD-SYMBOL(<LF_CELL_VALS>)
            WHERE ROWNO = LW_ROWNO.
            MOVE-CORRESPONDING LS_EXBA_FPAR_D TO <LF_CELL_VALS>.
            MOVE-CORRESPONDING LS_EXBA_FPAR_D TO LS_FVAL_DB.
            MOVE-CORRESPONDING <LF_CELL_VALS> TO LS_FVAL_DB.
            APPEND <LF_CELL_VALS> TO LS_EXBA_FPAR_D-CELL_VALS.
            APPEND LS_FVAL_DB TO GT_FVAL_DB.
          ENDLOOP.

          APPEND LS_EXBA_FPAR_D TO GT_EXBA_FPAR_D.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT GT_EXBA_FPAR BY ZDOCID SHPOS.
  MESSAGE TEXT-017 TYPE 'S'.

* Convert to display
  MOVE-CORRESPONDING GS_EXBA_FFM TO GS_EXBA_FFM_D.
  GS_EXBA_FFM_D-DETAIL_BTN = 'Detail'.
  CASE GS_EXBA_FFM_D-UPSTS.
    WHEN GC_UPSTS-NULL OR GC_UPSTS-NOT_YET  .
      GS_EXBA_FFM_D-MTYPE = SPACE.
    WHEN GC_UPSTS-WIP.
      GS_EXBA_FFM_D-MTYPE = 'W'.
    WHEN GC_UPSTS-SUCCESS OR GC_UPSTS-CANCEL.
      GS_EXBA_FFM_D-MTYPE = 'S'.
    WHEN GC_UPSTS-ERROR.
      GS_EXBA_FFM_D-MTYPE = 'E'.
    WHEN OTHERS.
  ENDCASE.
  CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
    EXPORTING
      I_MTYPE    = GS_EXBA_FFM_D-MTYPE
    CHANGING
      C_ROW_DATA = GS_EXBA_FFM_D.

  APPEND GS_EXBA_FFM_D TO GT_EXBA_FFM_D.
  MOVE-CORRESPONDING GS_EXBA_FFM_D TO ZTB_BM_EXBA_FFM.

  SORT GT_EXBA_FPAR_D BY ZDOCID SHPOS.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0030_Display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0030_DISPLAY.

*  PERFORM 0200_PARAM_SET_DISPLAY_DATA
*    USING GT_EXBA_FPAR_D.

  CALL SCREEN 0300.
*  CALL SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0100_PBO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0100_PBO .
  DATA:
    LT_FCAT      TYPE LVC_T_FCAT,
    LS_EXBA_FPAR TYPE ZTB_BM_EXBA_FPAR,
    LS_LAYOUT    TYPE LVC_S_LAYO,
    LS_VARIANT   TYPE DISVARIANT.

  LS_LAYOUT-CWIDTH_OPT = 'X'.
  LS_VARIANT-REPORT = SY-REPID.
  LS_VARIANT-HANDLE = SY-DYNNR.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME   = 'ZST_BM_EXBA_FPAR'
*     I_STRUCTURE_NAME   = 'ZTB_BM_EXBA_FPAR'
      I_INTERNAL_TABNAME = 'ZST_BM_EXBA_FPAR'
    CHANGING
      CT_FIELDCAT        = LT_FCAT.
  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'XMLSTR' OR 'PARAM'.
        <LF_FCAT>-HOTSPOT = 'X'.
      WHEN 'FNHASH' OR 'TABKEY' OR 'STRTY' OR 'SHPOS' OR 'HDRFL'
         OR 'CHUSR' OR 'CHDAT' OR 'CHTIM' OR 'SELECTED' OR 'MTYPE' OR 'DISABLE'
         OR 'CFNAME'  OR 'DETAIL_BTN' OR 'STYLEFNAME'.
        <LF_FCAT>-TECH = 'X'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.


  IF GO_ALV_0100 IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
*       I_CALLBACK_BUTTON_CLICK = '0100_BUTTON_CLICK'
        I_CALLBACK_HOSPOT_CLICK = '0100_HOSPOT_CLICK'
        I_CUS_CONTROL_NAME      = 'CUS_ALV'
        IS_VARIANT              = LS_VARIANT
        I_SAVE                  = 'A'
        IS_LAYOUT               = LS_LAYOUT
*       IT_TOOLBAR_EXCLUDING    =
        I_SHOW_ERRBTN           = 'X'
      IMPORTING
        E_ALV_GRID              = GO_ALV_0100
      CHANGING
        IT_OUTTAB               = GT_EXBA_FPAR_D
*       IT_OUTTAB               = GT_EXBA_FPAR
        IT_FIELDCATALOG         = LT_FCAT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_ALV_0100.

  ENDIF.

*  PERFORM 0100_HOSPOT_CLICK.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0100_SAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0100_SAVE .
  DATA:
    LS_EXBA_FPAR   TYPE ZTB_BM_EXBA_FPAR,
    LT_EXBA_FPAR   TYPE TABLE OF ZTB_BM_EXBA_FPAR,
    LT_EXBA_FPAR_O TYPE TABLE OF ZTB_BM_EXBA_FPAR.

  MOVE-CORRESPONDING GT_EXBA_FPAR TO LT_EXBA_FPAR.

  MODIFY ZTB_BM_EXBA_FFM FROM GS_EXBA_FFM.
  MODIFY ZTB_BM_EXBA_FPAR FROM TABLE GT_EXBA_FPAR.
  MODIFY ZTB_BM_EXBA_FMSG FROM TABLE GT_FMSG_DB.
  MODIFY ZTB_BM_EXBA_FVAL FROM TABLE GT_FVAL_DB.
  COMMIT WORK AND WAIT.

*  CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
*    EXPORTING
*      I_STRUCTURE            = 'ZTB_BM_EXBA_FPAR'
*      T_TABLE_CHANGED        = LT_EXBA_FPAR
*      T_TABLE_ORIGINAL       = LT_EXBA_FPAR_O
*    EXCEPTIONS
*      NO_STRUCTURE           = 1
*      CONFLICT_STRUCTURE     = 2
*      NO_ORIGINAL_DATA       = 3
*      CONFLICT_ORIGINAL_DATA = 4
*      NO_DATA                = 5
*      UPDATE_ERROR           = 6
*      INSERT_ERROR           = 7
*      DELETE_ERROR           = 8
*      DEL_FIELD_NOT_EXISTS   = 9
*      OTHERS                 = 10.

  MESSAGE TEXT-018 TYPE 'S'.
*----------------------------- End of change ----------------------------
*------------------------------------------------------------------------
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0100_HOSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0100_HOSPOT_CLICK
  USING LPS_COLUMN    TYPE LVC_S_COL
        LPS_ROW_NO    TYPE LVC_S_ROID.
  DATA:
    LR_DATA    TYPE REF TO DATA,
    LT_FCAT    TYPE LVC_T_FCAT,
    LW_XSTRING TYPE XSTRING,
    LW_TABNAME TYPE TABNAME.
  FIELD-SYMBOLS:
    <LFS_DATA> TYPE ANY,
    <LFT_DATA> TYPE STANDARD TABLE.

  CASE LPS_COLUMN-FIELDNAME.
    WHEN 'PARAM'."'TABKEY'.
      READ TABLE GT_EXBA_FPAR_D INTO DATA(LS_EXBA_FPAR) INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.

        CREATE DATA LR_DATA TYPE (LS_EXBA_FPAR-STRTY).
        ASSIGN LR_DATA->* TO <LFS_DATA>.

        LW_TABNAME = LS_EXBA_FPAR-STRTY.

        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            I_STRUCTURE_NAME   = LW_TABNAME
            I_INTERNAL_TABNAME = LW_TABNAME
          CHANGING
            CT_FIELDCAT        = LT_FCAT.

        CALL TRANSFORMATION ID
          SOURCE XML LS_EXBA_FPAR-XMLSTR
          RESULT DATA = <LFS_DATA>.

        CALL FUNCTION 'ZFM_BM_ALV_ITEM_DETAIL'
          EXPORTING
            IT_FCAT   = LT_FCAT
            IS_DETAIL = <LFS_DATA>.
      ENDIF.
    WHEN 'XMLSTR'.
      READ TABLE GT_EXBA_FPAR_D INTO LS_EXBA_FPAR INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            TEXT   = LS_EXBA_FPAR-XMLSTR
          IMPORTING
            BUFFER = LW_XSTRING.

        CALL FUNCTION 'DISPLAY_XML_STRING'
          EXPORTING
            XML_STRING      = LW_XSTRING
          EXCEPTIONS
            NO_XML_DOCUMENT = 1
            OTHERS          = 2.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*---------------------------------------------------------------------
*Author: DongVC
*Date: 14.09.2022
*Purpose: Write log of Upload Data
*---------------------------------------------------------------------
FORM 1000_PBO.

  IF P_UPLOAD = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR2'.
        SCREEN-ACTIVE   = '1'.
        SCREEN-REQUIRED = '2'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR3'.
        SCREEN-ACTIVE   = '1'.
        SCREEN-REQUIRED = '2'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'UPL'.
        SCREEN-ACTIVE   = '1'.
        SCREEN-REQUIRED = '2'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF P_DIPLAY = 'X' OR P_HISRP = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR2'.
        SCREEN-ACTIVE   = '0'.
        SCREEN-REQUIRED = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR3'.
        SCREEN-ACTIVE   = '1'.
        SCREEN-REQUIRED = '2'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'UPL'.
        SCREEN-ACTIVE   = '0'.
        SCREEN-REQUIRED = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF P_DOWNTP = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR2'.
        SCREEN-ACTIVE   = '0'.
        SCREEN-REQUIRED = '2'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR3'.
        SCREEN-ACTIVE   = '1'.
        SCREEN-REQUIRED = '2'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'UPL'.
        SCREEN-ACTIVE   = '0'.
        SCREEN-REQUIRED = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_FTEXT'.
      SCREEN-INPUT = 0.
      SCREEN-OUTPUT = 0.
      SCREEN-DISPLAY_3D = 0.
      SCREEN-REQUIRED = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF P_FTEXT IS INITIAL
  AND P_FNAME IS NOT INITIAL.
    SELECT SINGLE FTEXT
      FROM ZTB_BM_EXBA_FM
      INTO P_FTEXT
     WHERE FNAME = P_FNAME.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0060_GET_UPLOADED_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0060_GET_UPLOADED_DATA .
  DATA:
    LS_DATA_RAW TYPE ZST_BM_EXBA_DATA_RAW,
    LS_FFM_D    TYPE ZST_BM_EXBA_FFM,
    LR_UPSTS    TYPE RSDS_SELOPT_T.

  CALL FUNCTION 'ZFM_SUBMIT_DATA_TO_SELOPT'
    EXPORTING
      I_LOW     = P_UPSTS
    IMPORTING
      ER_SELOPT = LR_UPSTS.

* Get uploaded data
  SELECT *
    FROM ZTB_BM_EXBA_FFM
    INTO TABLE GT_EXBA_FFM
    WHERE FILENAME IN S_FILE[]
    AND FNAME = P_FNAME
    AND CRUSR IN S_CRUSR[]
    AND CRDAT IN S_CRDAT[]
    AND CRTIM IN S_CRTIM[]
    AND UPSTS IN LR_UPSTS.
  SORT GT_EXBA_FFM BY CRDAT DESCENDING CRTIM DESCENDING.

  IF GT_EXBA_FFM IS NOT INITIAL.
    SELECT BNAME
           U~PERSNUMBER
           NAME_TEXT
      FROM USR21 AS U
     INNER JOIN ADRP AS A ON U~PERSNUMBER = A~PERSNUMBER
      INTO TABLE GT_USR_INFO
       FOR ALL ENTRIES IN GT_EXBA_FFM
     WHERE BNAME = GT_EXBA_FFM-CRUSR.
    SORT GT_USR_INFO BY UNAME.
  ENDIF.

  ZTB_BM_EXBA_FFM-FNAME = P_FNAME.
  ZTB_BM_EXBA_FFM-FTEXT = P_FTEXT.

  LOOP AT GT_EXBA_FFM INTO DATA(LS_FFM).
    MOVE-CORRESPONDING LS_FFM TO LS_FFM_D.
    LS_FFM_D-DETAIL_BTN = 'Detail'.
    CASE LS_FFM_D-UPSTS.
      WHEN GC_UPSTS-NULL OR GC_UPSTS-NOT_YET  .
        LS_FFM_D-MTYPE = SPACE.
      WHEN GC_UPSTS-WIP.
        LS_FFM_D-MTYPE = 'W'.
      WHEN GC_UPSTS-SUCCESS OR GC_UPSTS-CANCEL.
        LS_FFM_D-MTYPE = 'S'.
      WHEN GC_UPSTS-ERROR.
        LS_FFM_D-MTYPE = 'E'.
      WHEN OTHERS.
    ENDCASE.
    CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
      EXPORTING
        I_MTYPE    = LS_FFM_D-MTYPE
      CHANGING
        C_ROW_DATA = LS_FFM_D.

    READ TABLE GT_USR_INFO INTO DATA(LS_USR_INFO)
      WITH KEY UNAME = LS_FFM_D-CRUSR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      LS_FFM_D-NAME_TEXT = LS_USR_INFO-NAME_TEXT.
    ENDIF.
    APPEND LS_FFM_D TO GT_EXBA_FFM_D.
  ENDLOOP.

  IF GT_EXBA_FFM_D[] IS NOT INITIAL.
    SELECT *
      FROM ZTB_BM_EXBA_FPAR
      INTO TABLE GT_EXBA_FPAR
       FOR ALL ENTRIES IN GT_EXBA_FFM_D
      WHERE FNHASH EQ GT_EXBA_FFM_D-FNHASH
        AND FNAME  EQ GT_EXBA_FFM_D-FNAME.
    SORT GT_EXBA_FPAR BY FNAME FNHASH ZDOCID SHPOS.

    SELECT *
      FROM ZTB_BM_EXBA_FMSG
      INTO TABLE GT_FMSG_DB
       FOR ALL ENTRIES IN GT_EXBA_FFM_D
      WHERE FNHASH EQ GT_EXBA_FFM_D-FNHASH
        AND FNAME  EQ GT_EXBA_FFM_D-FNAME
        AND INACTIVE EQ ''.
    SORT GT_FMSG_DB BY FNAME FNHASH ZDOCID.

    SELECT *
      FROM ZTB_BM_EXBA_FVAL
      INTO TABLE GT_FVAL_DB
       FOR ALL ENTRIES IN GT_EXBA_FFM_D
      WHERE FNHASH EQ GT_EXBA_FFM_D-FNHASH
        AND FNAME  EQ GT_EXBA_FFM_D-FNAME.
    SORT GT_FVAL_DB BY FNAME FNHASH ZDOCID PARAM ROWNO COLNO.

  ELSE.
    MESSAGE S003(ZMS_EXBA) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Get configuration data
  SELECT SINGLE *
    FROM ZTB_BM_EXBA_FM
    INTO GS_EXBA_FM
   WHERE FNAME = P_FNAME.

  SELECT *
    FROM ZTB_BM_EXBA_PAR
    INTO TABLE GT_EXBA_PAR.

  SELECT *
    FROM FUPARAREF
    INTO TABLE GT_FUPARAREF
   WHERE FUNCNAME = P_FNAME
     AND R3STATE = 'A'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0070_DISPLAY_UPLOADED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0070_DISPLAY_UPLOADED .
  CALL SCREEN 0200.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0200_PBO .
  DATA: LW_COLTEXT TYPE TEXT255,
        LW_POS     TYPE INT4.
  DATA:
    LS_VARIANT TYPE DISVARIANT,
    LS_LAYOUT  TYPE LVC_S_LAYO,
    LT_EXCL_FC TYPE UI_FUNCTIONS,
    LT_FCAT    TYPE LVC_T_FCAT.

  CLEAR: LS_LAYOUT.

  LS_LAYOUT-CWIDTH_OPT = 'X'.

  LS_VARIANT-REPORT = SY-REPID.
  LS_VARIANT-HANDLE = 'DH01'.
  CALL FUNCTION 'ZFM_ALV_EXCL_EDIT_FC'
    IMPORTING
      T_EXCL_FC = LT_EXCL_FC.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_BM_EXBA_FFM'
    CHANGING
      CT_FIELDCAT            = LT_FCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'DETAIL_BTN'.
        <LF_FCAT>-SCRTEXT_S = <LF_FCAT>-SCRTEXT_M = <LF_FCAT>-SCRTEXT_L
                            = TEXT-019.
      WHEN 'FNHASH' OR 'CFNAME'  OR 'DETAIL_BTN' OR 'STYLEFNAME'
         OR 'CHUSR' OR 'CHDAT' OR 'CHTIM' OR 'SELECTED' OR 'MTYPE' OR 'DISABLE'.
        <LF_FCAT>-TECH = 'X'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  IF GO_ALV_GRID  IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
*       I_STRUCTURE_NAME        = 'ZST_BM_EXBA_FFM'
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_BUTTON_CLICK = '0200_BUTTON_CLICK'
        I_CUS_CONTROL_NAME      = 'CONTAINER'
        IS_VARIANT              = LS_VARIANT
        I_SAVE                  = 'A'
        IS_LAYOUT               = LS_LAYOUT
        IT_TOOLBAR_EXCLUDING    = LT_EXCL_FC
      IMPORTING
        E_ALV_GRID              = GO_ALV_GRID
      CHANGING
        IT_OUTTAB               = GT_EXBA_FFM_D
        IT_FIELDCATALOG         = LT_FCAT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_ALV_GRID
        IS_LAYOUT  = LS_LAYOUT.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_HOSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0300_HOSPOT_CLICK
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
    LS_VARIANT TYPE DISVARIANT,
    LT_EXCL_FC TYPE  UI_FUNCTIONS.
  FIELD-SYMBOLS:
    <LFT_DATA>    TYPE STANDARD TABLE,
    <LF_NEST_VAL> TYPE ANY.

  CASE LPS_COLUMN-FIELDNAME.
    WHEN 'PARAM'."'TABKEY'.
      READ TABLE GT_EXBA_FPAR_D INTO DATA(LS_EXBA_FPAR) INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.

        CREATE DATA LR_DATA TYPE (LS_EXBA_FPAR-STRTY).
        ASSIGN LR_DATA->* TO <LF_NEST_VAL>.

        LW_TABNAME = LS_EXBA_FPAR-STRTY.

        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            I_STRUCTURE_NAME   = LW_TABNAME
            I_INTERNAL_TABNAME = LW_TABNAME
          CHANGING
            CT_FIELDCAT        = LT_FCAT.

        TRY.
            CALL TRANSFORMATION ID
              SOURCE XML LS_EXBA_FPAR-XMLSTR
              RESULT DATA = <LF_NEST_VAL>
              OPTIONS VALUE_HANDLING = 'accept_data_loss'.
          CATCH CX_XSLT_DESERIALIZATION_ERROR  CX_TRANSFORMATION_ERROR  INTO DATA(EXC).
        ENDTRY.

        CALL FUNCTION 'ZFM_BM_ALV_ITEM_DETAIL'
          EXPORTING
            IT_FCAT   = LT_FCAT
            IS_DETAIL = <LF_NEST_VAL>.
      ENDIF.
    WHEN 'XMLSTR'.
      READ TABLE GT_EXBA_FPAR_D INTO LS_EXBA_FPAR INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            TEXT   = LS_EXBA_FPAR-XMLSTR
          IMPORTING
            BUFFER = LW_XSTRING.

        CALL FUNCTION 'DISPLAY_XML_STRING'
          EXPORTING
            XML_STRING      = LW_XSTRING
          EXCEPTIONS
            NO_XML_DOCUMENT = 1
            OTHERS          = 2.
      ENDIF.
    WHEN 'ZDOCID'.
      CLEAR: GT_FVAL_D.

      READ TABLE GT_EXBA_FPAR_D INTO LS_EXBA_FPAR INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.

        LOOP AT GT_FVAL_DB INTO DATA(LS_FVAL_DB)
          WHERE FNAME = LS_EXBA_FPAR-FNAME
            AND FNHASH = LS_EXBA_FPAR-FNHASH
            AND ZDOCID = LS_EXBA_FPAR-ZDOCID.
          AT NEW PARAM.
*            READ TABLE GT_EXBA_PAR INTO DATA(LS_PAR)
            READ TABLE GT_EXBA_FPAR_D INTO DATA(LS_PAR)
              WITH KEY FNAME = LS_FVAL_DB-FNAME
                       PARAM = LS_FVAL_DB-PARAM
                       FNHASH = LS_EXBA_FPAR-FNHASH.
            IF SY-SUBRC IS INITIAL.
              LW_TABNAME = LS_PAR-STRTY.
              CLEAR: LT_FCAT.
              CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
                EXPORTING
                  I_STRUCTURE_NAME   = LW_TABNAME
                  I_INTERNAL_TABNAME = LW_TABNAME
                CHANGING
                  CT_FIELDCAT        = LT_FCAT.

              SORT LT_FCAT BY FIELDNAME.
            ENDIF.
          ENDAT.

          CLEAR: LS_FVAL_D.
*          MOVE-CORRESPONDING LS_EXBA_FPAR TO LS_FVAL_D.
          MOVE-CORRESPONDING LS_PAR TO LS_FVAL_D.
          MOVE-CORRESPONDING LS_FVAL_DB TO LS_FVAL_D.
          LS_FVAL_D-FTEXT = ZTB_BM_EXBA_FFM-FTEXT.

          READ TABLE LT_FCAT INTO DATA(LS_FCAT) BINARY SEARCH
            WITH KEY FIELDNAME = LS_FVAL_D-FIELDNAME.
          IF SY-SUBRC IS INITIAL.
            LS_FVAL_D-SCRTEXT_L = LS_FCAT-SCRTEXT_L.
          ELSE.
            LS_FVAL_D-SCRTEXT_L = LS_FCAT-FIELDNAME.
          ENDIF.

          CLEAR: LS_FVAL_D-CFNAME.
          IF LS_FVAL_D-OVALUE = LS_FVAL_D-NVALUE.
            LS_FVAL_D-MATCHING = 'X'.
          ELSE.
            CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
              EXPORTING
                I_MTYPE    = 'W'
              IMPORTING
                E_ROWCOLOR = LS_FVAL_D-CFNAME
              CHANGING
                C_ROW_DATA = LS_FVAL_D.
          ENDIF.
          READ TABLE GT_USR_INFO INTO DATA(LS_USR_INFO)
            WITH KEY UNAME = LS_FVAL_D-CRUSR BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            LS_FVAL_D-NAME_TEXT = LS_USR_INFO-NAME_TEXT.
          ENDIF.
          APPEND LS_FVAL_D TO GT_FVAL_D.
        ENDLOOP.
        SORT GT_FVAL_D BY FNAME FNHASH ZDOCID PARAM ROWNO COLNO.

        LS_LAYOUT-CWIDTH_OPT = 'X'.
        LS_LAYOUT-INFO_FNAME = 'CFNAME'.
        LS_VARIANT-REPORT = SY-REPID.
        LS_VARIANT-HANDLE = '0400'.


        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            I_STRUCTURE_NAME = 'ZST_BM_EXBA_FVAL'
          CHANGING
            CT_FIELDCAT      = LT_FCAT.
        LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
          CASE <LF_FCAT>-FIELDNAME.
            WHEN 'FNAME' OR 'FTEXT'  OR 'PARAM' OR 'PARTX'
              OR 'DOCRS' OR 'ROWNO' OR 'COLNO' OR 'FIELDNAME'
              OR 'SCRTEXT_L' OR 'OVALUE' OR 'NVALUE' OR 'MATCHING'
              OR 'CRUSR' OR 'CRDAT' OR 'CRTIM' OR 'NAME_TEXT'.
            WHEN 'FNHASH' OR 'ZDOCID' OR 'STRTY'.
              <LF_FCAT>-TECH = 'X'.
            WHEN OTHERS.
              <LF_FCAT>-TECH = 'X'.
          ENDCASE.
        ENDLOOP.
        CALL FUNCTION 'ZFM_ALV_EXCL_EDIT_FC'
          IMPORTING
            T_EXCL_FC = LT_EXCL_FC.
        APPEND '&DATA_SAVE' TO LT_EXCL_FC.

        CALL FUNCTION 'ZFM_ALV_DISPLAY'
          EXPORTING
*           I_STRUCTURE_NAME = 'ZST_BM_EXBA_FVAL'
*           I_GRID_TITLE  =
            IS_LAYOUT_LVC = LS_LAYOUT
            I_SAVE        = 'A'
            IS_VARIANT    = LS_VARIANT
            IT_FIELDCAT   = LT_FCAT
            IT_EXCLUDING  = LT_EXCL_FC
          TABLES
            T_OUTTAB      = GT_FVAL_D.

      ENDIF.
    WHEN 'DOCRS'.
      INCLUDE <CNTN01>.
      DATA:
        LS_SAP_OBJECT TYPE SWC_OBJECT,
        LS_DFIES      TYPE DFIES,
        LW_FNAME      TYPE CHAR30,
        LFIELDNAME    TYPE DFIES-LFIELDNAME.

      SELECT *
        FROM SWOTDV
        INTO TABLE @DATA(LT_SWOTDV)
       WHERE OBJTYPE = @GS_EXBA_FM-OBJTYPE
         AND VERBTYPE = 'K'.

      SWC_CONTAINER CONTAINER.

      READ TABLE GT_EXBA_FPAR_D INTO LS_EXBA_FPAR INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL AND LS_EXBA_FPAR-DOCRS IS NOT INITIAL.
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
            <LF_NEST_VAL> = LS_EXBA_FPAR-DOCRS.
            LW_FNAME = 'CONVERSION_EXIT_' && LS_DFIES-CONVEXIT && '_INPUT'.
            CALL FUNCTION LW_FNAME
              EXPORTING
                INPUT  = <LF_NEST_VAL>
              IMPORTING
                OUTPUT = <LF_NEST_VAL>.
          ENDIF.

          SWC_CREATE_OBJECT LS_SAP_OBJECT GS_EXBA_FM-OBJTYPE <LF_NEST_VAL>.
        ELSE.
          SWC_CREATE_OBJECT LS_SAP_OBJECT GS_EXBA_FM-OBJTYPE LS_EXBA_FPAR-DOCRS.
        ENDIF.
        SWC_CALL_METHOD LS_SAP_OBJECT 'DISPLAY' CONTAINER.
*      SAP-OBJECT://BUS2093 {{RSNUM}}"
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form 0300_HOSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0200_BUTTON_CLICK
  USING LPS_COLUMN    TYPE LVC_S_COL
        LPS_ROW_NO    TYPE LVC_S_ROID.
  DATA:
    LS_DETAIL    TYPE ZST_LVC_S_DETA,
    LR_DATA      TYPE REF TO DATA,
    LT_FCAT      TYPE LVC_T_FCAT,
    LS_MSGDETAIL TYPE ZST_ALV_MSG_DETAIL.

  CLEAR: GS_EXBA_FFM_D.

  CASE LPS_COLUMN-FIELDNAME.
    WHEN 'DETAIL_BTN'.
      READ TABLE GT_EXBA_FFM_D INTO GS_EXBA_FFM_D INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.
*      READ TABLE GT_EXBA_FM INTO DATA(LS_EXBA_FM)
*        with KEY FNAME = LS_EXBA_FM-fname.
*      IF sy-subrc IS INITIAL.
*        ZTB_BM_EXBA_FFM-ftext = LS_EXBA_FM-FTEXT..
*      ENDIF.
        MOVE-CORRESPONDING GS_EXBA_FFM_D TO ZTB_BM_EXBA_FFM.
        MOVE-CORRESPONDING GT_EXBA_FPAR TO GT_EXBA_FPAR_D.
        DELETE GT_EXBA_FPAR_D
          WHERE FNAME NE GS_EXBA_FFM_D-FNAME OR FNHASH NE GS_EXBA_FFM_D-FNHASH.

        PERFORM 0200_PARAM_SET_DISPLAY_DATA
          USING GT_EXBA_FPAR_D.

        CALL SCREEN 0300 .

        CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
          EXPORTING
            I_ALV_GRID = GO_ALV_GRID.

      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_PBO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 0300_PBO .
  DATA:
    LW_COLTEXT        TYPE TEXT255,
    LW_POS            TYPE INT4,
    LT_FCAT           TYPE LVC_T_FCAT,
    LT_ERRFIELD_TITLE TYPE LVC_T_FCAT,
    LS_FCAT           TYPE LVC_S_FCAT,
    LT_EXCL_FC        TYPE UI_FUNCTIONS,
    LS_LOGO           TYPE ZST_BM_ALV_LOGO,
    LS_HEADER         TYPE ZST_ALV_HEADER,
    LT_HEADER         TYPE ZTT_ALV_HEADER,
    LS_VARIANT        TYPE DISVARIANT,
    LS_LAYOUT         TYPE LVC_S_LAYO.

  LS_LAYOUT-CWIDTH_OPT  = 'X'.
  LS_LAYOUT-INFO_FNAME  = 'CFNAME'.
  LS_VARIANT-REPORT  = SY-REPID.
  LS_VARIANT-HANDLE  = 'DD01'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_BM_EXBA_FPAR'
      I_INTERNAL_TABNAME     = 'ZST_BM_EXBA_FPAR'
    CHANGING
      CT_FIELDCAT            = LT_FCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  CALL FUNCTION 'ZFM_ALV_EXCL_EDIT_FC'
    IMPORTING
      T_EXCL_FC = LT_EXCL_FC.

  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'ICON'.
        <LF_FCAT>-COL_POS = 1.
      WHEN 'UPSTS'.
        <LF_FCAT>-COL_POS = 2.
      WHEN 'DOCRS'.
        <LF_FCAT>-COL_POS = 3.
        <LF_FCAT>-HOTSPOT = 'X'.
      WHEN 'ZDOCID'.
*        APPEND <LF_FCAT> TO LT_ERRFIELD_TITLE.
        <LF_FCAT>-COL_POS = 4.
        <LF_FCAT>-HOTSPOT = 'X'.
      WHEN 'PARAM'.
        <LF_FCAT>-COL_POS = 5.
        <LF_FCAT>-HOTSPOT = 'X'.
        <LF_FCAT>-SCRTEXT_S = <LF_FCAT>-SCRTEXT_M = <LF_FCAT>-SCRTEXT_L
                             = <LF_FCAT>-REPTEXT = TEXT-020.
      WHEN 'PARTX'.
        <LF_FCAT>-COL_POS = 6.
      WHEN 'DOCRS'.
        <LF_FCAT>-COL_POS = 7.
        <LF_FCAT>-HOTSPOT = 'X'.
      WHEN 'XMLSTR'.
        <LF_FCAT>-COL_POS = 8.
        <LF_FCAT>-HOTSPOT = 'X'.
      WHEN 'CRUSR'.
        <LF_FCAT>-COL_POS = 9.
      WHEN 'CRDAT'.
        <LF_FCAT>-COL_POS = 10.
      WHEN 'CRTIM'.
        <LF_FCAT>-COL_POS = 11.
      WHEN 'XMLSTR'.
        <LF_FCAT>-COL_POS = 8.
      WHEN 'FNHASH' OR 'TABKEY' OR 'STRTY' OR 'SHPOS' OR 'HDRFL'
         OR 'CHUSR' OR 'CHDAT' OR 'CHTIM' OR 'SELECTED' OR 'MTYPE' OR 'DISABLE'
         OR 'CFNAME'  OR 'DETAIL_BTN' OR 'STYLEFNAME'.
        <LF_FCAT>-TECH = 'X'.
      WHEN OTHERS.
        <LF_FCAT>-NO_OUT = 'X'.
    ENDCASE.
  ENDLOOP.

  IF GO_ALV_GRID_D  IS INITIAL.
*L  Logo
    LS_HEADER-TYP = 'H'.      "Heading
    LS_HEADER-INFO = TEXT-006.
    APPEND LS_HEADER TO LT_HEADER.

    LS_HEADER-TYP = 'S'.      "Selection
    LS_HEADER-KEY = TEXT-007. "Message
    LS_HEADER-INFO = TEXT-008. "Detail messages
    APPEND LS_HEADER TO LT_HEADER.

    LS_HEADER-TYP = 'S'.      "Selection
    LS_HEADER-KEY = TEXT-009.	"Document ID
    LS_HEADER-INFO = TEXT-010.  "Upload log
    APPEND LS_HEADER TO LT_HEADER.

    LS_HEADER-TYP = 'S'.      "Selection
    LS_HEADER-KEY = TEXT-011.	"Result document
    LS_HEADER-INFO = TEXT-012.  "Business dialog
    APPEND LS_HEADER TO LT_HEADER.

    LS_HEADER-TYP = 'S'.      "Selection
    LS_HEADER-KEY = TEXT-013.	"Table key
    LS_HEADER-INFO = TEXT-014."Technical data
    APPEND LS_HEADER TO LT_HEADER.

    LS_HEADER-TYP = 'S'.      "Selection
    LS_HEADER-KEY = TEXT-015.	"XML
    LS_HEADER-INFO = TEXT-016.  "XML data
    APPEND LS_HEADER TO LT_HEADER.

    IF 1 = 2.
      LS_LOGO-LOGO  = 'CITEK'.
    ELSE.
      LS_LOGO-LOGO  = 'NIKOP'.
      LS_LOGO-WIDTH = '80'.
    ENDIF.

    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CALLBACK_HOSPOT_CLICK = '0300_HOSPOT_CLICK'
*       I_STRUCTURE_NAME        = 'ZST_BM_EXBA_FPAR'
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CUS_CONTROL_NAME      = 'CONTAINER_D'
        IS_VARIANT              = LS_VARIANT
        I_SAVE                  = 'A'
        IS_LAYOUT               = LS_LAYOUT
        IT_TOOLBAR_EXCLUDING    = LT_EXCL_FC
        I_SHOW_ERRBTN           = 'X'
        IT_ERRFIELD_TITLE       = LT_ERRFIELD_TITLE
        I_LOGO                  = LS_LOGO
        IT_HEADER               = LT_HEADER
      IMPORTING
        E_ALV_GRID              = GO_ALV_GRID_D
      CHANGING
        IT_OUTTAB               = GT_EXBA_FPAR_D
        IT_FIELDCATALOG         = LT_FCAT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_ALV_GRID_D
        IS_LAYOUT  = LS_LAYOUT.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0080_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0080_DOWNLOAD_TEMPLATE .
  DATA:
    LS_EXCEL          TYPE ZST_BM_EXBA_TEMPLATE,
    LS_PARAMS         TYPE ZST_BM_EXBA_PAR,
    LT_PARAMS         TYPE TABLE OF ZST_BM_EXBA_PAR,
    LW_LINEIX         TYPE INT4,
    LW_LINKIX         TYPE INT4,
    LW_TABNM          TYPE TABNAME,
    LS_FIELDCAT_DOCID TYPE LVC_S_FCAT,
    LW_LEN            TYPE INT4,
    LW_PARNAME        TYPE CHAR30.

  SELECT SINGLE *
    FROM ZTB_BM_EXBA_FM
    INTO CORRESPONDING FIELDS OF LS_EXCEL
   WHERE FNAME = P_FNAME.

  SELECT *
    FROM ZTB_BM_EXBA_PAR
    INTO CORRESPONDING FIELDS OF TABLE LS_EXCEL-PARAMS
   WHERE FNAME = P_FNAME
     AND SHPOS > 0.
  SORT LS_EXCEL-PARAMS BY SHPOS.

*  LT_PARAMS = LS_EXCEL-PARAMS.
  DELETE LS_EXCEL-PARAMS WHERE PARTY = GC_PARAMTYPE-IMPORTING.
  LOOP AT LS_EXCEL-PARAMS ASSIGNING FIELD-SYMBOL(<LF_PARAMS>).
    LW_LINEIX = SY-TABIX.
    LW_LEN = STRLEN( <LF_PARAMS>-PARAM ) - 1.
    LW_PARNAME = <LF_PARAMS>-PARAM(LW_LEN).
    IF <LF_PARAMS>-PARAM+LW_LEN(1) = 'X'.
      READ TABLE LS_EXCEL-PARAMS TRANSPORTING NO FIELDS
        WITH KEY PARAM = LW_PARNAME.
      IF SY-SUBRC IS INITIAL.
        DELETE LS_EXCEL-PARAMS INDEX LW_LINEIX.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: LW_LINEIX.
  LOOP AT LS_EXCEL-PARAMS ASSIGNING <LF_PARAMS>.
    LW_LINEIX = LW_LINEIX + 1.
    LW_LINKIX = LW_LINEIX + 4.
    <LF_PARAMS>-LINEIX = LW_LINEIX.
    <LF_PARAMS>-HLINK = `=HYPERLINK("#"&"'" & B` && LW_LINKIX  && `& "'!A1",B` && LW_LINKIX  && `)`.
*    <LF_PARAMS>-HLINK = `=HYPERLINK("#"&"'" & B`&& LW_LINKIX  && `& "'!A1",B` && LW_LINKIX  && `)`.
*    =HYPERLINK("#"&"'" & C6 & "'!A1";C6)

    PERFORM 9999_GET_FCAT_FROM_PARAM_TYPE
     CHANGING <LF_PARAMS>.

    LS_FIELDCAT_DOCID = GS_FCAT_DOCID.
    LS_FIELDCAT_DOCID-TABNAME = <LF_PARAMS>-STRTY.
    INSERT LS_FIELDCAT_DOCID INTO <LF_PARAMS>-FCATS INDEX 1.

  ENDLOOP.

  SORT LS_EXCEL-PARAMS BY LINEIX .
  CALL FUNCTION 'ZXLWB_CALLFORM'
    EXPORTING
      IV_FORMNAME    = 'ZXLSX_EXBA_TEMPLATE'
      IV_CONTEXT_REF = LS_EXCEL
    EXCEPTIONS
      OTHERS         = 2.
  IF SY-SUBRC NE 0 .
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_FC_PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0300_FC_PROCESS .
  DATA:
    LT_FPAR_D   TYPE TABLE OF ZST_BM_EXBA_FPAR,
    LS_FPAR_TMP TYPE ZST_BM_EXBA_FPAR,
    LT_FPAR_TMP TYPE TABLE OF ZST_BM_EXBA_FPAR,
    LW_SUCCESS  TYPE XMARK,
    LT_FPAR_DB  TYPE TABLE OF ZTB_BM_EXBA_FPAR,
    LT_RETURN   TYPE BAPIRET2_T,
    LW_DOCRS    TYPE ZTB_BM_EXBA_FPAR-DOCRS,
    LW_LEN      TYPE I.

*  CLEAR: GS_EXBA_FFM_D-SUCCREC.
  LOOP AT GT_EXBA_FPAR_D INTO DATA(LS_FPAR_D)
    WHERE UPSTS <> GC_UPSTS-SUCCESS.
*   Init for new document
    AT NEW ZDOCID.
      CLEAR: LT_FPAR_D, LT_FPAR_DB, LW_SUCCESS, LT_RETURN, LW_DOCRS.
    ENDAT.

*   Collect parameters info
    APPEND LS_FPAR_D TO LT_FPAR_D.

*   All parameters collected, run BAPI
    AT END OF ZDOCID.
      CASE GS_EXBA_FFM_D-FNAME.
*        WHEN 'BAPI_SALESORDER_CREATEFROMDAT2'.
*          PERFORM 9002_CREATE_SALESORDER CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
        WHEN 'BAPI_ACC_DOCUMENT_POST'.
          PERFORM 9003_ACCOUNTING_POSTING CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
*        WHEN 'BAPI_PR_CREATE'.
*          PERFORM 9004_PR_CREATE CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
        WHEN 'BAPI_PO_CREATE1'.
          PERFORM 9005_PO_CREATE CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
        WHEN 'BAPI_BILLINGDOC_CREATEMULTIPLE'.
          PERFORM 9006_BILL_CREATE CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
*        WHEN 'BAPI_OUTB_DELIVERY_CREATE_SLS'.
*          PERFORM 9007_OUTB_CREATE  CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
        WHEN 'BAPI_GOODSMVT_CREATE'.
          PERFORM 9008_GOODSMVT_CREATE CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
        WHEN 'BAPI_PRODORDCONF_CREATE_HDR'.
          PERFORM 9009_PRODORDCONF_CREATE  CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
        WHEN 'BAPI_INCOMINGINVOICE_CREATE'.
          PERFORM 9010_INCOMINGINVOICE_CREATE CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
*        WHEN 'BAPI_BILLINGDOC_CREATEFROMDATA'.
*          PERFORM 9011_BILLINGDOC_CREATEFROMDATA CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
        WHEN OTHERS.
          LOOP AT GT_EXBA_PAR INTO DATA(LS_PAR)
            WHERE FNAME = GS_EXBA_FFM_D-FNAME.

            READ TABLE GT_FUPARAREF INTO DATA(LS_FUPARAREF)
              WITH KEY FUNCNAME = GS_EXBA_FFM_D-FNAME
                       PARAMETER = LS_PAR-PARAM.

            LW_LEN = STRLEN( LS_PAR-PARAM ) - 1.
            IF LS_PAR-PARAM+LW_LEN(1) = 'X' OR LS_PAR-PARDOC = 'X' OR LS_PAR-PARMSG = 'X'
              OR LS_FUPARAREF-OPTIONAL IS INITIAL  OR LS_PAR-TESTRUN = 'X'.
              READ TABLE LT_FPAR_D TRANSPORTING NO FIELDS
                WITH KEY PARAM = LS_PAR-PARAM.
              IF SY-SUBRC IS NOT INITIAL.
                CLEAR: LS_FPAR_TMP.
                MOVE-CORRESPONDING LS_PAR TO LS_FPAR_TMP.
                APPEND LS_FPAR_TMP TO LT_FPAR_D.
              ENDIF.
            ENDIF.
          ENDLOOP.
          PERFORM 0300_RUN_BAPI USING GS_EXBA_FFM_D CHANGING LT_FPAR_D LT_RETURN LW_DOCRS.
          DELETE LT_FPAR_D WHERE FNHASH IS INITIAL.
      ENDCASE.

*     Save log
      PERFORM 0300_SAVE_LOG_AFTER_RUN_BAPI
        CHANGING GS_EXBA_FFM_D
                 LT_FPAR_D
                 GT_EXBA_FPAR_D
                 LT_RETURN
                 LW_DOCRS.
    ENDAT.
  ENDLOOP.

  IF SY-SUBRC IS INITIAL.
*   Update for file screen alv
    CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
      EXPORTING
        I_MTYPE    = GS_EXBA_FFM_D-MTYPE
      IMPORTING
        E_ROWCOLOR = GS_EXBA_FFM_D-CFNAME
      CHANGING
        C_ROW_DATA = GS_EXBA_FFM_D.

    READ TABLE GT_EXBA_FFM_D ASSIGNING FIELD-SYMBOL(<LF_FFM_D>)
      WITH KEY FNHASH = GS_EXBA_FFM_D-FNHASH.
    IF SY-SUBRC IS INITIAL.
      <LF_FFM_D> = GS_EXBA_FFM_D.
    ENDIF.
    MOVE-CORRESPONDING GS_EXBA_FFM_D TO ZTB_BM_EXBA_FFM.

    MESSAGE S005(ZMS_EXBA) DISPLAY LIKE GS_EXBA_FFM_D-MTYPE
      WITH GS_EXBA_FFM_D-SUCCREC GS_EXBA_FFM_D-SUMREC.
  ELSE.
    MESSAGE S006(ZMS_EXBA) DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_SAVE_LOG_AFTER_RUN_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GS_EXBA_FFM_D
*&      <-- LT_FPAR_D
*&      <-- GT_EXBA_FPAR_D
*&      <-- LW_SUCCESS
*&      <-- LT_RETURN
*&      <-- LW_DOCRS
*&---------------------------------------------------------------------*
FORM 0300_SAVE_LOG_AFTER_RUN_BAPI
  CHANGING LPS_EXBA_FFM_D TYPE ZST_BM_EXBA_FFM
           LPT_FPAR_PROCESS TYPE ZTT_BM_EXBA_FPAR
           LPT_FPAR_DISPLAY TYPE ZTT_BM_EXBA_FPAR
           LPT_RETURN TYPE BAPIRET2_T
           LPW_DOCRS    TYPE ZTB_BM_EXBA_FPAR-DOCRS.

  DATA:
    LS_FPAR_D   TYPE ZST_BM_EXBA_FPAR,
    LT_FPAR_DB  TYPE TABLE OF ZTB_BM_EXBA_FPAR,
    LT_FMSG_DB  TYPE TABLE OF ZTB_BM_EXBA_FMSG,
    LT_FMSG_INA TYPE TABLE OF ZTB_BM_EXBA_FMSG,
    LS_FMSG_DB  TYPE ZTB_BM_EXBA_FMSG,
    LW_SUCCESS  TYPE XMARK,
    LW_MESSAGE  TYPE TEXT256,
    BEGIN OF LS_MESSAGE,
      ZDOCID TYPE ZTB_BM_EXBA_FMSG-ZDOCID.
      INCLUDE TYPE BAPIRET2.
  DATA:
    END OF LS_MESSAGE.
  FIELD-SYMBOLS:
    <LF_FPAR_HDR> TYPE ZST_BM_EXBA_FPAR.

  LW_SUCCESS = 'X'.

  IF <LF_FPAR_HDR> IS NOT ASSIGNED.
    READ TABLE LPT_FPAR_PROCESS ASSIGNING <LF_FPAR_HDR>
    WITH KEY HDRFL = 'X'.
    IF SY-SUBRC IS NOT INITIAL.
      RETURN.
    ENDIF.
    CLEAR: <LF_FPAR_HDR>-MSGDETAIL, <LF_FPAR_HDR>-MTYPE, <LF_FPAR_HDR>-ICON.
  ENDIF.

  LOOP AT LPT_RETURN INTO DATA(LS_RETURN).
*   Put all message to header structure data

*    MESSAGE ID LS_RETURN-ID TYPE LS_RETURN-TYPE NUMBER LS_RETURN-NUMBER
    MESSAGE ID LS_RETURN-ID TYPE 'S' NUMBER LS_RETURN-NUMBER
      WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
           LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2 INTO LW_MESSAGE.

    LS_FMSG_DB-FNAME    = LPS_EXBA_FFM_D-FNAME.
    LS_FMSG_DB-FNHASH   = LPS_EXBA_FFM_D-FNHASH.
    LS_FMSG_DB-ZDOCID   = <LF_FPAR_HDR>-ZDOCID.
    LS_FMSG_DB-TABKEY   = <LF_FPAR_HDR>-TABKEY.

    TRY.
        CALL METHOD CL_SYSTEM_UUID=>IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32
          RECEIVING
            UUID = LS_FMSG_DB-MSGID.
      CATCH CX_UUID_ERROR.
    ENDTRY.
    LS_FMSG_DB-MTYPE       = LS_RETURN-TYPE.
    LS_FMSG_DB-MESSAGE     = LW_MESSAGE.
    LS_FMSG_DB-ID          = LS_RETURN-ID.
    LS_FMSG_DB-MSGNO       = LS_RETURN-NUMBER.
    LS_FMSG_DB-MESSAGE_V1  = LS_RETURN-MESSAGE_V1.
    LS_FMSG_DB-MESSAGE_V2  = LS_RETURN-MESSAGE_V2.
    LS_FMSG_DB-MESSAGE_V3  = LS_RETURN-MESSAGE_V3.
    LS_FMSG_DB-MESSAGE_V4  = LS_RETURN-MESSAGE_V4.

    LS_FMSG_DB-CRUSR    = SY-UNAME.
    LS_FMSG_DB-CRDAT    = SY-DATUM.
    LS_FMSG_DB-CRTIM    = SY-UZEIT.
    APPEND LS_FMSG_DB TO LT_FMSG_DB.

*   Check exist of message type A, E
    IF LS_RETURN-TYPE CA 'EA' OR LPW_DOCRS IS INITIAL.
      CLEAR: LW_SUCCESS.
    ENDIF.
    MOVE-CORRESPONDING LS_FMSG_DB TO LS_MESSAGE.
    MOVE-CORRESPONDING LS_RETURN TO LS_MESSAGE.
    CALL FUNCTION 'ZFM_ALV_MESSAGE_PUT'
      EXPORTING
        I_MTYPE   = LS_RETURN-TYPE
        I_MESSAGE = LW_MESSAGE
        I_RETURN  = LS_MESSAGE
      CHANGING
        C_ALVLINE = <LF_FPAR_HDR>.
  ENDLOOP.

  IF <LF_FPAR_HDR> IS ASSIGNED.
    LOOP AT GT_FMSG_DB ASSIGNING FIELD-SYMBOL(<LF_FMSG_DB>)
      WHERE FNAME    = LPS_EXBA_FFM_D-FNAME
        AND FNHASH   = LPS_EXBA_FFM_D-FNHASH
        AND ZDOCID   = <LF_FPAR_HDR>-ZDOCID
        AND ID IS NOT INITIAL.

      <LF_FMSG_DB>-INACTIVE = 'X'.
      APPEND <LF_FMSG_DB> TO LT_FMSG_INA.
    ENDLOOP.
  ENDIF.

* If test run, cancel job
  IF P_TEST IS NOT INITIAL.
    ROLLBACK WORK.
  ENDIF.

* Update status, icon, screen color for rows
  IF LW_SUCCESS IS INITIAL.
    ROLLBACK WORK.
    LOOP AT LPT_FPAR_PROCESS ASSIGNING <LF_FPAR_HDR>.
      <LF_FPAR_HDR>-UPSTS = GC_UPSTS-ERROR.
      <LF_FPAR_HDR>-MTYPE = 'E'.
      CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
        EXPORTING
          I_MTYPE    = <LF_FPAR_HDR>-MTYPE
        IMPORTING
          E_ROWCOLOR = <LF_FPAR_HDR>-CFNAME
        CHANGING
          C_ROW_DATA = <LF_FPAR_HDR>.
    ENDLOOP.
    LPS_EXBA_FFM_D-UPSTS = GC_UPSTS-WIP.
  ELSE.
    LPS_EXBA_FFM_D-SUCCREC = LPS_EXBA_FFM_D-SUCCREC + 1.
    LOOP AT LPT_FPAR_PROCESS ASSIGNING <LF_FPAR_HDR>.
      <LF_FPAR_HDR>-DOCRS = LPW_DOCRS.
      <LF_FPAR_HDR>-UPSTS = GC_UPSTS-SUCCESS.
      <LF_FPAR_HDR>-MTYPE = 'S'.
      CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
        EXPORTING
          I_MTYPE    = <LF_FPAR_HDR>-MTYPE
        IMPORTING
          E_ROWCOLOR = <LF_FPAR_HDR>-CFNAME
        CHANGING
          C_ROW_DATA = <LF_FPAR_HDR>.
    ENDLOOP.
  ENDIF.

* Prepare database update
  MOVE-CORRESPONDING LPT_FPAR_PROCESS TO LT_FPAR_DB.
  IF LPS_EXBA_FFM_D-SUCCREC = LPS_EXBA_FFM_D-SUMREC.
    LPS_EXBA_FFM_D-UPSTS = GC_UPSTS-SUCCESS.
    LPS_EXBA_FFM_D-MTYPE = 'S'.
  ELSEIF LPS_EXBA_FFM_D-SUCCREC IS INITIAL.
    LPS_EXBA_FFM_D-UPSTS = GC_UPSTS-ERROR.
    LPS_EXBA_FFM_D-MTYPE = 'E'.
  ELSE.
    LPS_EXBA_FFM_D-UPSTS = GC_UPSTS-WIP.
    LPS_EXBA_FFM_D-MTYPE = 'W'.
  ENDIF.

* If test run, cancel job
  IF P_TEST IS INITIAL.

* Update to database
    UPDATE ZTB_BM_EXBA_FFM
      SET SUCCREC   = LPS_EXBA_FFM_D-SUCCREC
          UPSTS     = LPS_EXBA_FFM_D-UPSTS
      WHERE FNAME   = LPS_EXBA_FFM_D-FNAME
        AND FNHASH  = LPS_EXBA_FFM_D-FNHASH.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S004(ZMS_EXBA) WITH 'ZTB_BM_EXBA_FFM'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    UPDATE ZTB_BM_EXBA_FPAR FROM TABLE LT_FPAR_DB.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S004(ZMS_EXBA) WITH 'ZTB_BM_EXBA_FPAR'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    UPDATE ZTB_BM_EXBA_FMSG FROM TABLE LT_FMSG_INA.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S004(ZMS_EXBA) WITH 'ZTB_BM_EXBA_FMSG'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    INSERT ZTB_BM_EXBA_FMSG FROM TABLE LT_FMSG_DB.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S004(ZMS_EXBA) WITH 'ZTB_BM_EXBA_FMSG'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ENDIF.

* Update back to ALV screen
  LOOP AT LPT_FPAR_PROCESS INTO LS_FPAR_D.
    READ TABLE LPT_FPAR_DISPLAY ASSIGNING <LF_FPAR_HDR>
      WITH KEY FNAME  = LS_FPAR_D-FNAME
               FNHASH = LS_FPAR_D-FNHASH
               ZDOCID = LS_FPAR_D-ZDOCID
               PARAM  = LS_FPAR_D-PARAM
               TABKEY = LS_FPAR_D-TABKEY.
    IF SY-SUBRC IS INITIAL.
      <LF_FPAR_HDR> = LS_FPAR_D.
    ENDIF.

    READ TABLE GT_EXBA_FPAR ASSIGNING FIELD-SYMBOL(<LF_FPAR_DB>)
      WITH KEY FNAME  = LS_FPAR_D-FNAME
               FNHASH = LS_FPAR_D-FNHASH
               ZDOCID = LS_FPAR_D-ZDOCID
               PARAM  = LS_FPAR_D-PARAM
               TABKEY = LS_FPAR_D-TABKEY.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING <LF_FPAR_HDR> TO <LF_FPAR_DB>.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF LT_FMSG_DB TO GT_FMSG_DB.
  DELETE GT_FMSG_DB WHERE INACTIVE = 'X'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form 0200_PARAM_SET_DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_EXBA_FPAR_D
*&---------------------------------------------------------------------*
FORM 0200_PARAM_SET_DISPLAY_DATA
  USING LPT_EXBA_FPAR_D TYPE ZTT_BM_EXBA_FPAR.
  DATA:
    LS_MSGDETAIL TYPE ZST_ALV_MSG_DETAIL,
    LS_RETURN    TYPE BAPIRET2.

  LOOP AT LPT_EXBA_FPAR_D ASSIGNING FIELD-SYMBOL(<LF_EXBA_FPAR_D>).

    READ TABLE GT_USR_INFO INTO DATA(LS_USR_INFO)
      WITH KEY UNAME = <LF_EXBA_FPAR_D>-CRUSR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      <LF_EXBA_FPAR_D>-NAME_TEXT = LS_USR_INFO-NAME_TEXT.
    ENDIF.

    IF <LF_EXBA_FPAR_D>-HDRFL IS NOT INITIAL.
      LOOP AT GT_FMSG_DB INTO DATA(LS_FMSG_DB)
        WHERE FNAME  = <LF_EXBA_FPAR_D>-FNAME
          AND FNHASH = <LF_EXBA_FPAR_D>-FNHASH
          AND ZDOCID = <LF_EXBA_FPAR_D>-ZDOCID .
*        MOVE-CORRESPONDING LS_FMSG_DB TO LS_RETURN.
        CALL FUNCTION 'ZFM_ALV_MESSAGE_PUT'
          EXPORTING
            I_MTYPE   = LS_FMSG_DB-MTYPE
            I_MESSAGE = LS_FMSG_DB-MESSAGE
            I_RETURN  = LS_FMSG_DB
          CHANGING
            C_ALVLINE = <LF_EXBA_FPAR_D>.
      ENDLOOP.
    ENDIF.

    CASE <LF_EXBA_FPAR_D>-UPSTS.
      WHEN GC_UPSTS-NULL OR GC_UPSTS-NOT_YET  .
        <LF_EXBA_FPAR_D>-MTYPE = SPACE.
      WHEN GC_UPSTS-WIP.
        <LF_EXBA_FPAR_D>-MTYPE = 'W'.
      WHEN GC_UPSTS-SUCCESS OR GC_UPSTS-CANCEL.
        <LF_EXBA_FPAR_D>-MTYPE = 'S'.
      WHEN GC_UPSTS-ERROR.
        <LF_EXBA_FPAR_D>-MTYPE = 'E'.
      WHEN OTHERS.
    ENDCASE.
    CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
      EXPORTING
        I_MTYPE    = <LF_EXBA_FPAR_D>-MTYPE
      IMPORTING
        E_ROWCOLOR = <LF_EXBA_FPAR_D>-CFNAME
      CHANGING
        C_ROW_DATA = <LF_EXBA_FPAR_D>.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_RUN_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LPT_FPAR_D Parameters data
*&      <-- LPT_RETURN Return messages
*&      <-- LPW_DOCRS  Document result
*&---------------------------------------------------------------------*
FORM 0300_RUN_BAPI
  USING LPS_EXBA_FFM_D  TYPE ZST_BM_EXBA_FFM
  CHANGING
    LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
    LPT_RETURN TYPE BAPIRET2_T
    LPW_DOCRS  TYPE ZTB_BM_EXBA_FPAR-DOCRS.

  DATA:
    LT_PARAM_TAB TYPE ABAP_FUNC_PARMBIND_TAB,
    LS_PARAM_TAB TYPE ABAP_FUNC_PARMBIND,
    LT_ETAB      TYPE ABAP_FUNC_EXCPBIND_TAB,
    LS_FPAR_D    TYPE ZST_BM_EXBA_FPAR,
    LS_RETURN    TYPE BAPIRET2,
    LT_RETURN    TYPE BAPIRET2_T.
  FIELD-SYMBOLS:
    <LF_PARAM>  TYPE ANY,
    <LFT_PARAM> TYPE TABLE.

  DATA:
    LR_DATA     TYPE REF TO DATA,
    LW_TYPENAME TYPE TYPENAME,
    LW_TYPEKIND TYPE DDTYPEKIND.
  FIELD-SYMBOLS:
    <LFS_STRUCTURE> TYPE ANY,
    <LFT_TABLE>     TYPE TABLE.

  PERFORM 0300_GET_BAPI_PARAMS
    USING LPT_FPAR_D
    CHANGING LT_PARAM_TAB
             LT_ETAB.

* Run BAPI
  CALL FUNCTION LPS_EXBA_FFM_D-FNAME
    PARAMETER-TABLE LT_PARAM_TAB
    EXCEPTION-TABLE LT_ETAB.

* Get Doc result
  READ TABLE LPT_FPAR_D INTO LS_FPAR_D
    WITH KEY PARDOC = 'X'.
  IF SY-SUBRC IS INITIAL.
    READ TABLE LT_PARAM_TAB INTO LS_PARAM_TAB
      WITH KEY KIND = LS_FPAR_D-PARTY
               NAME = LS_FPAR_D-PARAM.
    IF SY-SUBRC IS INITIAL.
      IF LS_FPAR_D-PARTY = GC_PARAMTYPE-TABLES.
        ASSIGN LS_PARAM_TAB-VALUE->* TO <LFT_PARAM>.
        IF SY-SUBRC IS INITIAL.
          READ TABLE <LFT_PARAM> ASSIGNING <LF_PARAM> INDEX 1.
          IF SY-SUBRC IS INITIAL.
            IF LS_FPAR_D-PARMSG IS INITIAL.
              LPW_DOCRS = <LF_PARAM>.
            ELSE.
              MOVE-CORRESPONDING <LF_PARAM> TO LS_RETURN.
              LPW_DOCRS = LS_RETURN-MESSAGE_V1.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        ASSIGN LS_PARAM_TAB-VALUE->* TO <LF_PARAM>.
        IF SY-SUBRC IS INITIAL.
          IF LS_FPAR_D-PARMSG IS INITIAL.
            LPW_DOCRS = <LF_PARAM>.
          ELSE.
            MOVE-CORRESPONDING <LF_PARAM> TO LS_RETURN.
            LPW_DOCRS = LS_RETURN-MESSAGE_V1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* Get return messages
  LOOP AT LPT_FPAR_D INTO LS_FPAR_D
    WHERE PARMSG = 'X'.
    READ TABLE LT_PARAM_TAB INTO LS_PARAM_TAB
      WITH KEY KIND = LS_FPAR_D-PARTY
               NAME = LS_FPAR_D-PARAM.
    IF SY-SUBRC IS INITIAL.
      CASE LS_FPAR_D-PARTY.
*       Importing parameter
        WHEN GC_PARAMTYPE-IMPORTING.
          ASSIGN LS_PARAM_TAB-VALUE->* TO <LF_PARAM>.
          DESCRIBE FIELD <LF_PARAM> HELP-ID LW_TYPENAME.

          CALL FUNCTION 'DDIF_TYPEINFO_GET'
            EXPORTING
              TYPENAME = LW_TYPENAME
            IMPORTING
              TYPEKIND = LW_TYPEKIND.
          IF LW_TYPEKIND = 'TABL'. "Structure
            ASSIGN LS_PARAM_TAB-VALUE->* TO <LF_PARAM>.
            IF SY-SUBRC IS INITIAL.
              MOVE-CORRESPONDING <LF_PARAM> TO LS_RETURN.
              APPEND LS_RETURN TO LPT_RETURN.
            ENDIF.
          ELSEIF LW_TYPEKIND = 'TTYP'. "Table.
            ASSIGN LS_PARAM_TAB-VALUE->* TO <LFT_TABLE>.
            MOVE-CORRESPONDING <LFT_TABLE> TO LT_RETURN.
            APPEND LINES OF LT_RETURN TO LPT_RETURN.
          ENDIF.
*       Table parameter
        WHEN GC_PARAMTYPE-TABLES.
          ASSIGN LS_PARAM_TAB-VALUE->* TO <LFT_TABLE>.
          MOVE-CORRESPONDING <LFT_TABLE> TO LT_RETURN.
          APPEND LINES OF LT_RETURN TO LPT_RETURN.
        WHEN OTHERS.
      ENDCASE.


    ENDIF.
  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_GET_BAPI_PARAMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_ORDER_ITEMS_IN
*&---------------------------------------------------------------------*
FORM 0300_GET_BAPI_PARAMS
  USING LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
  CHANGING
    LPT_PARAM_TAB TYPE ABAP_FUNC_PARMBIND_TAB
    LPT_ETAB      TYPE ABAP_FUNC_EXCPBIND_TAB.

  DATA:
    LR_DATA      TYPE REF TO DATA,
    LW_MAIN_PAR  TYPE ZST_BM_EXBA_FPAR-PARAM,
    LW_TYPENAME  TYPE TYPENAME,
    LW_TYPEKIND  TYPE DDTYPEKIND,
    LS_PARAM     TYPE ABAP_FUNC_PARMBIND,
    LS_PARAMX    TYPE ABAP_FUNC_PARMBIND,
    LS_EXCEPTION TYPE ABAP_FUNC_EXCPBIND,
    LW_LEN       TYPE I.
  FIELD-SYMBOLS:
    <LF_PARAM>      TYPE ANY,
    <LF_PARAMX>     TYPE ANY,
    <LFS_STRUCTURE> TYPE ANY,
    <LFT_TABLE>     TYPE TABLE,
    <LFT_TABLEX>    TYPE TABLE.

  LOOP AT LPT_FPAR_D ASSIGNING FIELD-SYMBOL(<LF_FPAR_D>).
    CASE <LF_FPAR_D>-PARTY.
      WHEN GC_PARAMTYPE-IMPORTING OR GC_PARAMTYPE-EXPORTING OR  GC_PARAMTYPE-CHANGING.
        CREATE DATA LS_PARAM-VALUE TYPE (<LF_FPAR_D>-STRTY).
        ASSIGN LS_PARAM-VALUE->* TO <LF_PARAM>.
        DESCRIBE FIELD <LF_PARAM> HELP-ID LW_TYPENAME.
        CALL FUNCTION 'DDIF_TYPEINFO_GET'
          EXPORTING
            TYPENAME = LW_TYPENAME
          IMPORTING
            TYPEKIND = <LF_FPAR_D>-TYPEKIND.
      WHEN GC_PARAMTYPE-TABLES.
        <LF_FPAR_D>-TYPEKIND = 'TTYP'.
    ENDCASE.
  ENDLOOP.

  LOOP AT LPT_FPAR_D INTO DATA(LS_FPAR_D).
    AT NEW PARAM.
      CLEAR: LS_PARAM.
      LS_PARAM-NAME = LS_FPAR_D-PARAM.
    ENDAT.

    LS_PARAM-KIND = LS_FPAR_D-PARTY.

    CASE LS_FPAR_D-PARTY.
      WHEN GC_PARAMTYPE-IMPORTING OR GC_PARAMTYPE-EXPORTING OR  GC_PARAMTYPE-CHANGING.
        CREATE DATA LS_PARAM-VALUE TYPE (LS_FPAR_D-STRTY).
        IF LS_FPAR_D-TESTRUN IS NOT INITIAL
        AND P_TEST IS NOT INITIAL.
          ASSIGN LS_PARAM-VALUE->* TO <LF_PARAM>.
          <LF_PARAM> = 'X'.
        ELSEIF LS_FPAR_D-XMLSTR IS NOT INITIAL.
          ASSIGN LS_PARAM-VALUE->* TO <LF_PARAM>.
          IF LS_FPAR_D-TYPEKIND = 'TABL' OR LS_FPAR_D-TYPEKIND = 'DTEL'  . "Element
            CALL TRANSFORMATION ID
              SOURCE XML LS_FPAR_D-XMLSTR
              RESULT DATA = <LF_PARAM>.

            PERFORM 9999_CONVERSION_VALUE
              USING LS_FPAR_D-STRTY
              CHANGING <LF_PARAM>.
          ELSEIF LS_FPAR_D-TYPEKIND = 'TTYP'. "Table.
            ASSIGN LS_PARAM-VALUE->* TO <LFT_TABLE>.
            CREATE DATA LR_DATA LIKE LINE OF <LFT_TABLE>.
            ASSIGN LR_DATA->* TO <LFS_STRUCTURE>.
            CALL TRANSFORMATION ID
              SOURCE XML LS_FPAR_D-XMLSTR
              RESULT DATA = <LFS_STRUCTURE>.

            DESCRIBE FIELD <LFS_STRUCTURE> HELP-ID LW_TYPENAME.
            PERFORM 9999_CONVERSION_VALUE
              USING LW_TYPENAME
              CHANGING <LFS_STRUCTURE>.
            APPEND <LFS_STRUCTURE> TO <LFT_TABLE>.
          ENDIF.
        ENDIF.
      WHEN GC_PARAMTYPE-TABLES.
        IF LS_PARAM-VALUE IS INITIAL.
          CREATE DATA LS_PARAM-VALUE TYPE TABLE OF (LS_FPAR_D-STRTY).
        ENDIF.
        IF LS_FPAR_D-XMLSTR IS NOT INITIAL.
          CREATE DATA LR_DATA TYPE (LS_FPAR_D-STRTY).
          ASSIGN LR_DATA->* TO <LFS_STRUCTURE>.
          ASSIGN LS_PARAM-VALUE->* TO <LFT_TABLE>.
          CALL TRANSFORMATION ID
            SOURCE XML LS_FPAR_D-XMLSTR
            RESULT DATA = <LFS_STRUCTURE>.

          PERFORM 9999_CONVERSION_VALUE
            USING LS_FPAR_D-STRTY
            CHANGING <LFS_STRUCTURE>.
          APPEND <LFS_STRUCTURE> TO <LFT_TABLE>.

        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    INSERT LS_PARAM INTO TABLE LPT_PARAM_TAB.
  ENDLOOP.

  LOOP AT LPT_PARAM_TAB INTO LS_PARAMX.
    READ TABLE LPT_FPAR_D INTO LS_FPAR_D
      WITH KEY PARAM = LS_PARAMX-NAME.

    LW_LEN = STRLEN( LS_PARAMX-NAME ) - 1.
    IF LS_PARAMX-NAME+LW_LEN(1) = 'X'.
      LW_MAIN_PAR = LS_PARAMX-NAME(LW_LEN).
      READ TABLE LPT_PARAM_TAB INTO LS_PARAM
        WITH KEY NAME = LW_MAIN_PAR.
      IF SY-SUBRC IS INITIAL.
        ASSIGN LS_PARAM-VALUE->* TO <LF_PARAM>.
        ASSIGN LS_PARAMX-VALUE->* TO <LF_PARAMX>.

        IF LS_FPAR_D-TYPEKIND = 'TABL'.
          CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
            EXPORTING
              I_SOURCE = <LF_PARAM>
            CHANGING
              C_DATA   = <LF_PARAMX>.
        ELSEIF LS_FPAR_D-TYPEKIND = 'TTYP'. "Table.
          ASSIGN LS_PARAM-VALUE->* TO <LFT_TABLE>.
          ASSIGN LS_PARAMX-VALUE->* TO <LFT_TABLEX>.
          LOOP AT <LFT_TABLE> ASSIGNING <LF_PARAM>.
            CREATE DATA LR_DATA LIKE LINE OF <LFT_TABLEX>.
            ASSIGN LR_DATA->* TO <LF_PARAMX>.
            CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
              EXPORTING
                I_SOURCE = <LF_PARAM>
              CHANGING
                C_DATA   = <LF_PARAMX>.
            APPEND <LF_PARAMX> TO <LFT_TABLEX>.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LS_EXCEPTION-NAME = 'OTHERS'.
  LS_EXCEPTION-VALUE = 10.
  INSERT LS_EXCEPTION INTO TABLE LPT_ETAB.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 9999_CONVERSION_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_FPAR_D_STRTY
*&      <-- <LF_PARAM>
*&---------------------------------------------------------------------*
FORM 9999_CONVERSION_VALUE
   USING    LPW_TYPENAME
   CHANGING LPS_PARAM TYPE ANY.
  DATA:
    LW_TABNAME TYPE TABNAME,
    LT_FCAT    TYPE TABLE OF LVC_S_FCAT,
    LW_FNAME   TYPE CHAR30.

  LW_TABNAME = LPW_TYPENAME.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = LW_TABNAME
    CHANGING
      CT_FIELDCAT            = LT_FCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  LOOP AT LT_FCAT INTO DATA(LS_FCAT)
    WHERE CONVEXIT IS NOT INITIAL
*      AND CONVEXIT = 'ALPHA'
    .
    ASSIGN COMPONENT LS_FCAT-FIELDNAME OF STRUCTURE LPS_PARAM
      TO FIELD-SYMBOL(<LF_VALUE>).
    IF SY-SUBRC IS INITIAL AND <LF_VALUE> IS NOT INITIAL.
      LW_FNAME = 'CONVERSION_EXIT_' && LS_FCAT-CONVEXIT && '_INPUT'.
*    CALL FUNCTION 'CONVERSION_EXIT_ZEBUS_INPUT'
      CALL FUNCTION LW_FNAME
        EXPORTING
          INPUT  = <LF_VALUE>
        IMPORTING
          OUTPUT = <LF_VALUE>.
    ENDIF.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 9999_GET_FCAT_FROM_PARAM_TYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LF_PARAMS>
*&---------------------------------------------------------------------*
FORM 9999_GET_FCAT_FROM_PARAM_TYPE
  CHANGING LPS_PARAMS TYPE ZST_BM_EXBA_PAR.
  DATA:
    LW_TABNAME TYPE TABNAME,
    LW_FNAME   TYPE FNAM_____4.

  IF LPS_PARAMS-STRTY CA '-'.
*   If type is 'like', get table name, field name
    SPLIT LPS_PARAMS-STRTY AT '-' INTO LW_TABNAME LW_FNAME.
  ELSE.
    LW_TABNAME = LPS_PARAMS-STRTY.
  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = LW_TABNAME
    CHANGING
      CT_FIELDCAT            = LPS_PARAMS-FCATS
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF LW_FNAME IS NOT INITIAL.
    DELETE LPS_PARAMS-FCATS WHERE FIELDNAME <> LW_FNAME.
    READ TABLE LPS_PARAMS-FCATS ASSIGNING FIELD-SYMBOL(<LF_FCAT>) INDEX 1.
    IF SY-SUBRC IS INITIAL.
      <LF_FCAT>-COL_POS = 1.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_FC_MSGLIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0300_FC_MSGLIST .
  DATA:
    LS_MSG_DETAIL TYPE ZST_ALV_MSG_DETAIL,
    LT_MSG_DETAIL TYPE TABLE OF ZST_ALV_MSG_DETAIL,
    LS_VARIANT    TYPE DISVARIANT.

  LOOP AT GT_FMSG_DB INTO DATA(LS_MSG_DB)
    WHERE FNHASH = GS_EXBA_FFM_D-FNHASH.
    MOVE-CORRESPONDING LS_MSG_DB TO LS_MSG_DETAIL.
    CALL FUNCTION 'ZFM_ALV_GENERATE_ICON_BY_MTYPE'
      EXPORTING
        I_MTYPE       = LS_MSG_DB-MTYPE
      IMPORTING
        E_STATUS_ICON = LS_MSG_DETAIL-ICON
        E_ROWCOLOR    = LS_MSG_DETAIL-CFNAME.
    APPEND LS_MSG_DETAIL TO LT_MSG_DETAIL.
  ENDLOOP.

  LS_VARIANT-REPORT   = 'ALV_MSG_PROG'.
  LS_VARIANT-HANDLE   = 'MSGL'.

  CALL FUNCTION 'ZFM_ALV_DISPLAY'
    EXPORTING
      I_GRID_TITLE     = TEXT-021
      I_STRUCTURE_NAME = 'ZST_ALV_MSG_DETAIL'
      IS_VARIANT       = LS_VARIANT
    TABLES
      T_OUTTAB         = LT_MSG_DETAIL.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0300_FC_CMPRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0300_FC_CMPRP .
  DATA:
    LT_FCAT    TYPE LVC_T_FCAT,
    LS_LAYOUT  TYPE LVC_S_LAYO,
    LT_EXCL_FC TYPE  UI_FUNCTIONS,
    LS_VARIANT TYPE DISVARIANT,
    LS_FVAL_D  TYPE ZST_BM_EXBA_FVAL,
    LW_TABNAME TYPE TABNAME.

  CLEAR: GT_FVAL_D.

  SORT GT_FVAL_DB BY FNAME FNHASH ZDOCID PARAM ROWNO COLNO.
  SORT GT_EXBA_FPAR_D BY FNAME FNHASH ZDOCID PARAM.
  LOOP AT GT_FVAL_DB INTO DATA(LS_FVAL_DB)
          WHERE FNHASH = GS_EXBA_FFM_D-FNHASH.
    AT NEW PARAM.
      READ TABLE GT_EXBA_FPAR_D INTO DATA(LS_PAR)
        WITH KEY FNAME = LS_FVAL_DB-FNAME
                 PARAM = LS_FVAL_DB-PARAM
                 ZDOCID = LS_FVAL_DB-ZDOCID
                 FNHASH = LS_FVAL_DB-FNHASH BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LW_TABNAME = LS_PAR-STRTY.
        CLEAR: LT_FCAT.
        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            I_STRUCTURE_NAME   = LW_TABNAME
            I_INTERNAL_TABNAME = LW_TABNAME
          CHANGING
            CT_FIELDCAT        = LT_FCAT.

        SORT LT_FCAT BY FIELDNAME.
      ENDIF.
    ENDAT.

    CLEAR: LS_FVAL_D.
    MOVE-CORRESPONDING LS_PAR TO LS_FVAL_D.
    MOVE-CORRESPONDING LS_FVAL_DB TO LS_FVAL_D.
    LS_FVAL_D-FTEXT = ZTB_BM_EXBA_FFM-FTEXT.

    READ TABLE LT_FCAT INTO DATA(LS_FCAT) BINARY SEARCH
      WITH KEY FIELDNAME = LS_FVAL_D-FIELDNAME.
    IF SY-SUBRC IS INITIAL.
      LS_FVAL_D-SCRTEXT_L = LS_FCAT-SCRTEXT_L.
    ELSE.
      LS_FVAL_D-SCRTEXT_L = LS_FCAT-FIELDNAME.
    ENDIF.

    CLEAR: LS_FVAL_D-CFNAME.
    IF LS_FVAL_D-OVALUE = LS_FVAL_D-NVALUE.
      LS_FVAL_D-MATCHING = 'X'.
    ELSE.
      CALL FUNCTION 'ZFM_ALV_ROW_STATUS_SET'
        EXPORTING
          I_MTYPE    = 'W'
        IMPORTING
          E_ROWCOLOR = LS_FVAL_D-CFNAME
        CHANGING
          C_ROW_DATA = LS_FVAL_D.
    ENDIF.

    READ TABLE GT_USR_INFO INTO DATA(LS_USR_INFO)
      WITH KEY UNAME = LS_FVAL_D-CRUSR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      LS_FVAL_D-NAME_TEXT = LS_USR_INFO-NAME_TEXT.
    ENDIF.
    APPEND LS_FVAL_D TO GT_FVAL_D.
  ENDLOOP.
  SORT GT_FVAL_D BY FNAME FNHASH ZDOCID PARAM ROWNO COLNO.

  LS_LAYOUT-CWIDTH_OPT = 'X'.
  LS_LAYOUT-INFO_FNAME = 'CFNAME'.
  LS_VARIANT-REPORT = SY-REPID.
  LS_VARIANT-HANDLE = '0400'.


  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZST_BM_EXBA_FVAL'
    CHANGING
      CT_FIELDCAT      = LT_FCAT.
  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'FNAME' OR 'FTEXT'  OR 'PARAM' OR 'PARTX'
        OR 'DOCRS' OR 'ROWNO' OR 'COLNO' OR 'FIELDNAME'
        OR 'SCRTEXT_L' OR 'OVALUE' OR 'NVALUE' OR 'MATCHING'
        OR 'CRUSR' OR 'CRDAT' OR 'CRTIM' OR 'NAME_TEXT'.
      WHEN 'FNHASH' OR 'ZDOCID' OR 'STRTY'.
        <LF_FCAT>-TECH = 'X'.
      WHEN OTHERS.
        <LF_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.
  CALL FUNCTION 'ZFM_ALV_EXCL_EDIT_FC'
    IMPORTING
      T_EXCL_FC = LT_EXCL_FC.
  APPEND '&DATA_SAVE' TO LT_EXCL_FC.

  CALL FUNCTION 'ZFM_ALV_DISPLAY'
    EXPORTING
      IS_LAYOUT_LVC = LS_LAYOUT
      I_SAVE        = 'A'
      IS_VARIANT    = LS_VARIANT
      IT_FIELDCAT   = LT_FCAT
      IT_EXCLUDING  = LT_EXCL_FC
    TABLES
      T_OUTTAB      = GT_FVAL_D.
ENDFORM.
