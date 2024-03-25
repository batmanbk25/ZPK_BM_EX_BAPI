FUNCTION ZFM_BM_BAPI_EXCEL_UPLOAD .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_DATA) TYPE  I DEFAULT 7
*"     VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME
*"     VALUE(I_STEP) TYPE  I DEFAULT 1
*"     VALUE(I_TECHNAME_LINE) TYPE  I DEFAULT 5
*"  CHANGING
*"     REFERENCE(CT_DATA_RAW) TYPE  ZTT_BM_EXBA_DATA_RAW
*"  EXCEPTIONS
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------

* Copy from TEXT_CONVERT_XLS_TO_SAP

  TYPE-POOLS: SOI, CNTL.
  CONSTANTS: G_CON_EXCEL      TYPE CHAR80 VALUE 'Excel.Sheet',
             G_MAX_EMPTY_ROWS TYPE I VALUE 5.

  DATA LR_OREF_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
  DATA LR_IREF_CONTROL TYPE REF TO I_OI_CONTAINER_CONTROL.
  DATA LR_IREF_ERROR TYPE REF TO I_OI_ERROR.
  DATA LR_IREF_DOCUMENT TYPE REF TO I_OI_DOCUMENT_PROXY.
  DATA: L_CNTL_HANDLE TYPE CNTL_HANDLE.
  DATA  LR_SPREADSHEET TYPE REF TO I_OI_SPREADSHEET.
  DATA  LS_RETCODE TYPE SOI_RET_STRING.
  DATA: LW_CURRENT_ROW TYPE I,
        LW_TOP         TYPE I,
        LW_LEFT        TYPE I,
        LW_ROWS        TYPE I,
        LW_COLUMNS     TYPE I,
        LW_COUNTER     TYPE I,
        LW_LINE_FROM   TYPE I,
        LW_LINE_TO     TYPE I.
  DATA: L_OREF_STRUCTURE TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA: L_RANGE_LIST TYPE SOI_RANGE_LIST.
  DATA: LS_TABLE       TYPE SOI_GENERIC_TABLE.
  DATA: L_TABLE_RANGE TYPE SOI_GENERIC_TABLE.
  DATA: LW_GEN_TAB LIKE LINE OF LS_TABLE.
  FIELD-SYMBOLS <FS_DATA>.
  DATA:
    LW_TEXT6(6),
    LW_TEXT80(80).
  DATA:
    LW_TABNAME TYPE TABNAME.

  PERFORM GET_SPREADSHEET_INTERFACE USING G_CON_EXCEL
                                    CHANGING
                                      I_FILENAME
                                      LR_OREF_CONTAINER LR_IREF_CONTROL
                                      LR_IREF_ERROR     LR_IREF_DOCUMENT
                                      LR_SPREADSHEET.

  IF LR_SPREADSHEET IS INITIAL.
    MESSAGE E893(UX) WITH I_FILENAME RAISING CONVERSION_FAILED.
  ENDIF.

**********************************************************************
* Sheets processing - Start
**********************************************************************
  DATA:
    LT_SHEETS    TYPE SOI_SHEETS_TABLE,
    LR_LINE_DATA TYPE REF TO DATA,
    LT_FCAT      TYPE LVC_T_FCAT.
  FIELD-SYMBOLS:
    <LFT_CONVERTED_DATA> TYPE STANDARD TABLE,
    <LF_CONVERTED_DATA>  TYPE ANY.

  CALL METHOD LR_SPREADSHEET->GET_SHEETS
    IMPORTING
      SHEETS = LT_SHEETS.
  LOOP AT LT_SHEETS INTO DATA(LS_SHEET).
**********************************************************************
* Sheets processing - End
**********************************************************************
*   Init
    CLEAR: LW_LEFT, LW_ROWS, LW_TOP, LW_CURRENT_ROW, L_TABLE_RANGE,
           LW_COUNTER, LW_LINE_FROM, LW_LINE_FROM, LW_COLUMNS.

*   Check sheet name match with function
    READ TABLE CT_DATA_RAW ASSIGNING FIELD-SYMBOL(<LF_DATA_RAW>)
      WITH KEY PARAM = LS_SHEET-SHEET_NAME.
    CHECK SY-SUBRC IS INITIAL.

*   Active sheet
    LR_SPREADSHEET->SELECT_SHEET( NAME = LS_SHEET-SHEET_NAME ).

    ASSIGN <LF_DATA_RAW>-REFDATA->* TO <LFT_CONVERTED_DATA>.
    CREATE DATA LR_LINE_DATA LIKE LINE OF <LFT_CONVERTED_DATA>.
    ASSIGN LR_LINE_DATA->* TO <LF_CONVERTED_DATA>.

    L_OREF_STRUCTURE ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA(
                        <LF_CONVERTED_DATA> ).
*    L_OREF_STRUCTURE ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA(
*                        I_TAB_CONVERTED_DATA ).

**********************************************************************
*   Mapping Heading Data
**********************************************************************
    LT_FCAT = <LF_DATA_RAW>-FIELDCAT.
    PERFORM GET_MAPPING_WITH_TECH_LINE
      USING I_TECHNAME_LINE
      CHANGING <LF_DATA_RAW>
               LR_SPREADSHEET
               LT_FCAT.
*   adjust column and rows ...
*    DESCRIBE TABLE L_OREF_STRUCTURE->COMPONENTS LINES LW_COLUMNS.
    DESCRIBE TABLE LT_FCAT LINES LW_COLUMNS.

    LW_LEFT = 1.
    LW_ROWS = 100.
    IF NOT I_LINE_DATA IS INITIAL.
      LW_TOP = I_LINE_DATA.
    ELSE.
      LW_TOP = 1.
    ENDIF.

    WHILE LW_CURRENT_ROW <= G_MAX_EMPTY_ROWS.
      REFRESH L_TABLE_RANGE.
      ADD 1 TO LW_COUNTER.
      LW_LINE_FROM = LW_LINE_TO + 1.

      CALL METHOD LR_SPREADSHEET->SET_SELECTION
        EXPORTING
          TOP     = LW_TOP
          LEFT    = LW_LEFT
          ROWS    = LW_ROWS
          COLUMNS = LW_COLUMNS
        IMPORTING
          RETCODE = LS_RETCODE.
      CALL METHOD LR_SPREADSHEET->INSERT_RANGE
        EXPORTING
          COLUMNS = LW_COLUMNS
          ROWS    = LW_ROWS
          NAME    = 'SAP_range1'
        IMPORTING
          RETCODE = LS_RETCODE.

      CALL METHOD LR_SPREADSHEET->GET_RANGES_NAMES
        IMPORTING
          RANGES  = L_RANGE_LIST
          RETCODE = LS_RETCODE.

      DELETE L_RANGE_LIST WHERE NAME <> 'SAP_range1'.

      CALL METHOD LR_SPREADSHEET->GET_RANGES_DATA
*               exporting all      = 'X'
        IMPORTING
          CONTENTS = L_TABLE_RANGE
          RETCODE  = LS_RETCODE
        CHANGING
          RANGES   = L_RANGE_LIST.
      LOOP AT L_TABLE_RANGE INTO LW_GEN_TAB.
        AT NEW ROW.
          REFRESH LS_TABLE.
        ENDAT.

        APPEND LW_GEN_TAB TO LS_TABLE.

        AT END OF ROW.
**********************************************************************
*         Tuan.Bui Modify to validate data - Start
**********************************************************************
*          PERFORM PARSE_TABLE_LINE USING    LS_TABLE
*                                            SY-TABIX
*                                   CHANGING  I_TAB_CONVERTED_DATA.

          PERFORM ZEXBA_PARSE_TABLE_LINE
            USING LS_TABLE
                  SY-TABIX
            CHANGING <LF_CONVERTED_DATA>
                     <LF_DATA_RAW>
                     LT_FCAT.
**********************************************************************
*         Tuan.Bui Modify to validate data - End
**********************************************************************
          IF <LF_CONVERTED_DATA> IS INITIAL.
            LW_CURRENT_ROW = LW_CURRENT_ROW + 1.
          ELSE.
            APPEND <LF_CONVERTED_DATA> TO <LFT_CONVERTED_DATA>.
            CLEAR LW_CURRENT_ROW.
          ENDIF.
        ENDAT.
      ENDLOOP.
      LW_TOP = LW_TOP + LW_ROWS.

      IF LW_COUNTER MOD I_STEP = 0
        AND SY-BATCH IS INITIAL.
*        DESCRIBE TABLE I_TAB_CONVERTED_DATA LINES LW_LINE_TO.
        DESCRIBE TABLE <LFT_CONVERTED_DATA> LINES LW_LINE_TO.
        LW_TEXT80 = TEXT-KOY.
        LW_TEXT6 = LW_LINE_FROM.
        REPLACE '&&&&&&' WITH LW_TEXT6 INTO LW_TEXT80.
        LW_TEXT6 = LW_LINE_TO.
        REPLACE '&&&&&&' WITH LW_TEXT6 INTO LW_TEXT80.
        REPLACE '&&&&&&' WITH TEXT-EXL INTO LW_TEXT80.
        CONDENSE LW_TEXT80.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            TEXT = LW_TEXT80.
      ENDIF.
    ENDWHILE.
  ENDLOOP.

  FREE: LR_SPREADSHEET.
  CALL METHOD LR_IREF_DOCUMENT->CLOSE_DOCUMENT.
  CALL METHOD LR_IREF_DOCUMENT->RELEASE_DOCUMENT.
  FREE LR_IREF_DOCUMENT.

  CALL METHOD LR_IREF_CONTROL->RELEASE_ALL_DOCUMENTS.
  CALL METHOD LR_IREF_CONTROL->DESTROY_CONTROL.

* correct the decimals
  PERFORM CORRECT_DECIMALS_FOR_CURRENT TABLES <LFT_CONVERTED_DATA>.


ENDFUNCTION.

*---------------------------------------------------------------------*
*       FORM GET_CURRENCY_DECIMALS                                    *
*---------------------------------------------------------------------*
*       get decimals of a selected currency (table TCURX)             *
*---------------------------------------------------------------------*
FORM GET_CURRENCY_DECIMALS USING    VALUE(I_WAERS)  TYPE WAERS
                           CHANGING C_CURR_DECIMALS TYPE CURRDEC.
  STATICS: L_WRK_CURR   TYPE BAPI1090_1.
  DATA:    L_WRK_RETURN TYPE BAPIRETURN.
  IF I_WAERS IS INITIAL.
    C_CURR_DECIMALS = 2.
    EXIT.
  ENDIF.
  IF I_WAERS <> L_WRK_CURR-CURRENCY.
    L_WRK_CURR-CURRENCY = I_WAERS.
    SELECT SINGLE CURRDEC FROM TCURX
           INTO L_WRK_CURR-CURDECIMALS
           WHERE  CURRKEY     = I_WAERS.
    IF SY-SUBRC <> 0.
*     default: 2 decimals
      L_WRK_CURR-CURDECIMALS = 2.
    ENDIF.
  ENDIF.
  C_CURR_DECIMALS = L_WRK_CURR-CURDECIMALS.
ENDFORM.
