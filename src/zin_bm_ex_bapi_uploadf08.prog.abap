*&---------------------------------------------------------------------*
*& Include          ZIN_BM_EX_BAPI_UPLOADF08
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form 9008_GOODSMVT_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 9008_GOODSMVT_CREATE
  CHANGING
    LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
    LPT_RETURN TYPE BAPIRET2_T
    LPW_DOCRS    TYPE ZTB_BM_EXBA_FPAR-DOCRS.

  DATA:
    LS_GOODSMVT_HEADER     TYPE  BAPI2017_GM_HEAD_01,
    LS_GOODSMVT_CODE       TYPE  BAPI2017_GM_CODE,
    LV_TESTRUN             TYPE  BAPI2017_GM_GEN-TESTRUN,
    LS_GOODSMVT_REF_EWM    TYPE  /SPE/BAPI2017_GM_REF_EWM,
    LS_GOODSMVT_PRINT_CTRL TYPE  BAPI2017_GM_PRINT.

  DATA:
    LS_GOODSMVT_HEADRET TYPE  BAPI2017_GM_HEAD_RET,
    LV_MATERIALDOCUMENT TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC,
    LV_MATDOCUMENTYEAR  TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR.

  DATA:
    LT_GOODSMVT_ITEM           TYPE TABLE OF BAPI2017_GM_ITEM_CREATE, "Material Document Items
    LT_GOODSMVT_SERIALNUMBER   TYPE TABLE OF BAPI2017_GM_SERIALNUMBER, "Serial Number
    LT_GOODSMVT_SERV_PART_DATA TYPE TABLE OF /SPE/BAPI2017_SERVICEPART_DATA, "Communication Structure: Service Parts Data
    LT_EXTENSIONIN             TYPE TABLE OF BAPIPAREX, "Reference Structure for BAPI Parameters EXTENSIONIN/EXTENSIONOUT
    LT_GOODSMVT_ITEM_CWM       TYPE TABLE OF /CWM/BAPI2017_GM_ITEM_CREATE, "Create Item Data for CW
    LT_RETURN                  TYPE TABLE OF BAPIRET2, "Return Messages
    LS_RETURN                  TYPE BAPIRET2.

  PERFORM 9008_GET_GOODS_CREATE_PARAMS
  USING LPT_FPAR_D
  CHANGING LS_GOODSMVT_HEADER
           LS_GOODSMVT_CODE
           LS_GOODSMVT_REF_EWM
           LT_GOODSMVT_ITEM
           LT_GOODSMVT_SERIALNUMBER
           LT_GOODSMVT_SERV_PART_DATA
           LT_EXTENSIONIN
           LT_GOODSMVT_ITEM_CWM.

*   Create data
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      GOODSMVT_HEADER         = LS_GOODSMVT_HEADER
      GOODSMVT_CODE           = LS_GOODSMVT_CODE
*     TESTRUN                 = ' '
      GOODSMVT_REF_EWM        = LS_GOODSMVT_REF_EWM
*     GOODSMVT_PRINT_CTRL     =
    IMPORTING
      GOODSMVT_HEADRET        = LS_GOODSMVT_HEADRET
      MATERIALDOCUMENT        = LV_MATERIALDOCUMENT
      MATDOCUMENTYEAR         = LV_MATDOCUMENTYEAR
    TABLES
      GOODSMVT_ITEM           = LT_GOODSMVT_ITEM
      GOODSMVT_SERIALNUMBER   = LT_GOODSMVT_SERIALNUMBER
      RETURN                  = LT_RETURN
      GOODSMVT_SERV_PART_DATA = LT_GOODSMVT_SERV_PART_DATA
      EXTENSIONIN             = LT_EXTENSIONIN
      GOODSMVT_ITEM_CWM       = LT_GOODSMVT_ITEM_CWM.

  LPT_RETURN = LT_RETURN.
  LPW_DOCRS  = LV_MATERIALDOCUMENT && LV_MATDOCUMENTYEAR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form 9008_GET_GOODS_CREATE_PARAMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_GOODSMVT_HEADER
*&      <-- LS_GOODSMVT_REF_EWM
*&      <-- LT_GOODSMVT_ITEM
*&      <-- LT_GOODSMVT_SERIALNUMBER
*&      <-- LT_GOODSMVT_SERV_PART_DATA
*&      <-- LT_EXTENSIONIN
*&      <-- LT_GOODSMVT_ITEM_CWM
*&---------------------------------------------------------------------*
FORM 9008_GET_GOODS_CREATE_PARAMS
  USING LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
  CHANGING LPS_GOODSMVT_HEADER TYPE BAPI2017_GM_HEAD_01
           LPS_GOODSMVT_CODE TYPE BAPI2017_GM_CODE
           LPS_GOODSMVT_REF_EWM TYPE /SPE/BAPI2017_GM_REF_EWM
           LPT_GOODSMVT_ITEM TYPE TAB_BAPI_GOODSMVT_ITEM
           LPT_GOODSMVT_SERIALNUMBER TYPE TAB_BAPI_GOODSMVT_SERIALNUMBER
           LPT_GOODSMVT_SERV_PART_DATA TYPE /SYCLO/MM_SERVICEPART_DATA_TAB
           LPT_EXTENSIONIN TYPE BAPIPAREX_TAB
           LPT_GOODSMVT_ITEM_CWM TYPE /SYCLO/MM_ITEM_CREATE_TAB.
  DATA:
      LR_DATA TYPE REF TO DATA.

  LOOP AT LPT_FPAR_D INTO DATA(LPS_FPAR_D).
    CASE LPS_FPAR_D-PARAM.
      WHEN 'GOODSMVT_HEADER'.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LPS_GOODSMVT_HEADER.
      WHEN 'GOODSMVT_CODE'.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LPS_GOODSMVT_CODE.
      WHEN 'GOODSMVT_REF_EWM'.
        CALL TRANSFORMATION ID
           SOURCE XML LPS_FPAR_D-XMLSTR
           RESULT DATA = LPS_GOODSMVT_REF_EWM.
      WHEN 'GOODSMVT_ITEM'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_GOODSMVT_ITEM.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_GOODSMVT_ITEM.
      WHEN 'GOODSMVT_SERIALNUMBER'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_GOODSMVT_SERIALNUMBER.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_GOODSMVT_SERIALNUMBER.
      WHEN 'GOODSMVT_SERV_PART_DATA'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_GOODSMVT_SERV_PART_DATA.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_GOODSMVT_SERV_PART_DATA.
      WHEN 'EXTENSIONIN'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_EXTENSIONIN.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_EXTENSIONIN.
      WHEN 'GOODSMVT_ITEM_CWM'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_GOODSMVT_ITEM_CWM.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_GOODSMVT_ITEM_CWM.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  " Convesion Assign code
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LPS_GOODSMVT_CODE-GM_CODE
    IMPORTING
      OUTPUT = LPS_GOODSMVT_CODE-GM_CODE.


  " Convesion Material
  LOOP AT LPT_GOODSMVT_ITEM ASSIGNING FIELD-SYMBOL(<LFS_GOODSMVT_ITEM>).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <LFS_GOODSMVT_ITEM>-MATERIAL
      IMPORTING
        OUTPUT = <LFS_GOODSMVT_ITEM>-MATERIAL.

    <LFS_GOODSMVT_ITEM>-MATERIAL_LONG = <LFS_GOODSMVT_ITEM>-MATERIAL.

  ENDLOOP.

ENDFORM.
