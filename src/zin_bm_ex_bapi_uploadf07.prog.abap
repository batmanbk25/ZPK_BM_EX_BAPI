*&---------------------------------------------------------------------*
*& Include          ZIN_BM_EX_BAPI_UPLOADF07
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form 9007_OUTB_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 9007_OUTB_CREATE
  CHANGING
    LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
    LPT_RETURN TYPE BAPIRET2_T
    LPW_DOCRS  TYPE ZTB_BM_EXBA_FPAR-DOCRS.

  DATA:
    LV_DUE_DATE       TYPE BAPIDLVCREATEHEADER-DUE_DATE,
    LV_DELIVERY       TYPE BAPISHPDELIVNUMB-DELIV_NUMB,
    LV_NUM_DELIVERIES TYPE BAPIDLVCREATEHEADER-NUM_DELIVERIES.

  DATA:
    LT_SALES_ORDER_ITEMS TYPE TABLE OF BAPIDLVREFTOSALESORDER, "Delivery Item with Reference to a Sales Order
    LT_SERIAL_NUMBERS    TYPE TABLE OF BAPIDLVSERIALNUMBER, "Serial Number
    LT_EXTENSION_IN      TYPE TABLE OF BAPIPAREX, "Reference Structure for BAPI Parameters EXTENSIONIN/EXTENSIONOUT
    LT_DELIVERIES        TYPE TABLE OF BAPISHPDELIVNUMB, "Delivery Number
    LT_CREATED_ITEMS     TYPE TABLE OF BAPIDLVITEMCREATED, "Generated Delivery Items
    LT_EXTENSION_OUT     TYPE TABLE OF BAPIPAREX, "Reference Structure for BAPI Parameters EXTENSIONIN/EXTENSIONOUT
    LT_RETURN            TYPE TABLE OF BAPIRET2, "Return Parameter(s)
    LS_RETURN            TYPE BAPIRET2.


  PERFORM 9007_GET_OUTB_CREATE_PARAMS
    USING LPT_FPAR_D
    CHANGING
      LT_SALES_ORDER_ITEMS
      LT_SERIAL_NUMBERS
      LT_EXTENSION_IN
      LT_DELIVERIES
      LT_CREATED_ITEMS
      LT_EXTENSION_OUT.

  LV_DUE_DATE   = '99991231'.

*   Create data
  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
    EXPORTING
*     SHIP_POINT        =
      DUE_DATE          = LV_DUE_DATE
*     DEBUG_FLG         =
*     NO_DEQUEUE        = ' '
    IMPORTING
      DELIVERY          = LV_DELIVERY
      NUM_DELIVERIES    = LV_NUM_DELIVERIES
    TABLES
      SALES_ORDER_ITEMS = LT_SALES_ORDER_ITEMS
      SERIAL_NUMBERS    = LT_SERIAL_NUMBERS
      EXTENSION_IN      = LT_EXTENSION_IN
      DELIVERIES        = LT_DELIVERIES
      CREATED_ITEMS     = LT_CREATED_ITEMS
      EXTENSION_OUT     = LT_EXTENSION_OUT
      RETURN            = LT_RETURN.

  LPT_RETURN = LT_RETURN.
  LPW_DOCRS  = LV_DELIVERY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form 9007_GET_OUTB_CREATE_PARAMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_SALES_ORDER_ITEMS
*&      <-- LT_SERIAL_NUMBERS
*&      <-- LT_EXTENSION_IN
*&      <-- LT_EXTENSION_OUT
*&      <-- LT_DELIVERIES
*&      <-- LT_CREATED_ITEMS
*&      <-- LT_EXTENSION_OUT
*&---------------------------------------------------------------------*
FORM 9007_GET_OUTB_CREATE_PARAMS
  USING LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
  CHANGING
      LPT_SALES_ORDER_ITEMS TYPE TT_BAPIDLVREFTOSALESORDER
      LPT_SERIAL_NUMBERS TYPE /SYCLO/SD_DLVSERIALNUMBER_TAB
      LPT_EXTENSION_IN TYPE BAPIPAREX_TAB
      LPT_DELIVERIES TYPE /SYCLO/SD_SHPDELIVNUMB_TAB
      LPT_CREATED_ITEMS TYPE /SYCLO/SD_DLVITEMCREATED_TAB
      LPT_EXTENSION_OUT TYPE BAPIPAREX_TAB .

  DATA:
    LR_DATA TYPE REF TO DATA.

  LOOP AT GT_EXBA_FPAR_D INTO DATA(LPS_FPAR_D).
    CASE LPS_FPAR_D-PARAM.
      WHEN 'SALES_ORDER_ITEMS'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_SALES_ORDER_ITEMS.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_SALES_ORDER_ITEMS.
      WHEN 'SERIAL_NUMBERS'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_SERIAL_NUMBERS.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_SERIAL_NUMBERS.
      WHEN 'EXTENSION_IN'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_EXTENSION_IN.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_EXTENSION_IN.
      WHEN 'DELIVERIES'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_DELIVERIES.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_DELIVERIES.
      WHEN 'EXTENSION_OUT'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_EXTENSION_OUT.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_EXTENSION_OUT.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

*  " Check SO
*  IF LPT_SALES_ORDER_ITEMS IS NOT INITIAL.
*    SELECT
*      VBELN,
*      POSNR,
*      KWMENG, "Order Quantity
*      VRKME, "Sales unit
*      VSTEL "Shipping Point/Receiving Pt
*      FROM VBAP
*      FOR ALL ENTRIES IN @LPT_SALES_ORDER_ITEMS
*      WHERE VBELN = @LPT_SALES_ORDER_ITEMS-REF_DOC
*      INTO TABLE @DATA(LT_VBAP).
*
*    " Kiểm tra Sales Document có tồn tại không?
*    IF LT_VBAP IS INITIAL.
*      MESSAGE S000(ZMS_BM_EX_BAPI_UPLOA) DISPLAY LIKE 'E'.
*      RETURN.
*    ELSE.
*      SORT LPT_SALES_ORDER_ITEMS BY REF_DOC REF_ITEM.
*      SORT LT_VBAP BY VBELN POSNR.
*    ENDIF.
*  ENDIF.

  LOOP AT LPT_SALES_ORDER_ITEMS ASSIGNING FIELD-SYMBOL(<LFS_SALES_ORDER_ITEMS>).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <LFS_SALES_ORDER_ITEMS>-REF_ITEM
      IMPORTING
        OUTPUT = <LFS_SALES_ORDER_ITEMS>-REF_ITEM.

*    " Kiểm tra Sales Document Item có tồn tại không?
*    READ TABLE LT_VBAP INTO DATA(LS_VBAP)
*    WITH KEY VBELN = <LFS_SALES_ORDER_ITEMS>-REF_DOC
*    POSNR = <LFS_SALES_ORDER_ITEMS>-REF_DOC
*    BINARY SEARCH.
*    IF SY-SUBRC IS NOT INITIAL.
*      MESSAGE S001(ZMS_BM_EX_BAPI_UPLOA) WITH <LFS_SALES_ORDER_ITEMS>-REF_DOC DISPLAY LIKE 'E'.
*    ENDIF.
  ENDLOOP.

ENDFORM.
