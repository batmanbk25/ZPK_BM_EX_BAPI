*&---------------------------------------------------------------------*
*& Include          ZIN_BM_EX_BAPI_UPLOADF11
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form 9011_BILLINGDOC_CREATEFROMDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 9011_BILLINGDOC_CREATEFROMDATA
  CHANGING
    LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
    LPT_RETURN TYPE BAPIRET2_T
    LPW_DOCRS    TYPE ZTB_BM_EXBA_FPAR-DOCRS.

  DATA: LV_VBELN   TYPE VBELN.

  DATA:
    LT_BILLING_DATA_IN   TYPE TABLE OF  BAPIVBRK, "Item Data
    LT_CONDITION_DATA_IN TYPE TABLE OF  BAPIKOMV, "Conditions
    LT_CCARD_DATA_IN     TYPE TABLE OF  BAPICCARD_VF, "Means of Payment Order/Billing Document
    LT_RETURN            TYPE TABLE OF  BAPIRETURN1, "Return Parameter
    LS_RETURN            TYPE BAPIRETURN1.

  PERFORM 9011_BILL_FROMDATA_PARAMS
    USING LPT_FPAR_D
    CHANGING LT_BILLING_DATA_IN
             LT_CONDITION_DATA_IN
             LT_CCARD_DATA_IN.

  " Create data
  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEFROMDATA'
* EXPORTING
*   POSTING                 =
    TABLES
      BILLING_DATA_IN   = LT_BILLING_DATA_IN
      CONDITION_DATA_IN = LT_CONDITION_DATA_IN
      RETURNLOG_OUT     = LT_RETURN
      CCARD_DATA_IN     = LT_CCARD_DATA_IN.

  " Get Billing Number
  READ TABLE LT_RETURN INTO LS_RETURN
  WITH KEY TYPE = 'S' ID = 'VF' NUMBER = '311'.
  IF SY-SUBRC IS INITIAL.
    LV_VBELN = LS_RETURN-MESSAGE_V1+0(10).
    IF LV_VBELN CN '0123456789'.
      LV_VBELN = LS_RETURN-MESSAGE_V4+0(10).
      IF LV_VBELN CN '0123456789'.
        LV_VBELN = LS_RETURN-MESSAGE+9(10).
      ENDIF.
    ENDIF.
  ENDIF.

  LPT_RETURN = LT_RETURN.
  LPW_DOCRS  = LV_VBELN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form 9011_BILL_FROMDATA_PARAMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_BILLING_DATA_IN
*&      <-- LT_CONDITION_DATA_IN
*&      <-- LT_CCARD_DATA_IN
*&---------------------------------------------------------------------*
FORM 9011_BILL_FROMDATA_PARAMS
  USING LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR  CHANGING LPT_BILLING_DATA_IN TYPE BAPIVBRK_T
                                         LPT_CONDITION_DATA_IN TYPE BAPIKOMV_T
                                         LPT_CCARD_DATA_IN TYPE /SYCLO/SD_BAPICCARD_VF_TAB.

  DATA:
         LR_DATA TYPE REF TO DATA.

  LOOP AT LPT_FPAR_D INTO DATA(LPS_FPAR_D).
    CASE LPS_FPAR_D-PARAM.
      WHEN 'BILLING_DATA_IN'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_BILLING_DATA_IN.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_BILLING_DATA_IN.
      WHEN 'CONDITION_DATA_IN'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_CONDITION_DATA_IN.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_CONDITION_DATA_IN.
      WHEN 'CCARD_DATA_IN'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_CCARD_DATA_IN.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_CCARD_DATA_IN.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.
