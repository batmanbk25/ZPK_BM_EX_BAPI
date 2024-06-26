*&---------------------------------------------------------------------*
*& Include          ZIN_BM_EX_BAPI_UPLOADF05
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form 9005_PO_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 9005_PO_CREATE
  CHANGING
    LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
    LPT_RETURN TYPE BAPIRET2_T
    LPW_DOCRS  TYPE ZTB_BM_EXBA_FPAR-DOCRS.

  DATA:
    LS_POHEADER        TYPE BAPIMEPOHEADER,
    LS_POHEADERX       TYPE BAPIMEPOHEADERX,
    LS_POADDRVENDOR    TYPE BAPIMEPOADDRVENDOR,
    LS_POEXPIMPHEADER  TYPE BAPIEIKP,
    LS_POEXPIMPHEADERX TYPE BAPIEIKPX,
    LS_VERSIONS        TYPE BAPIMEDCM.

  DATA: LV_EXPPURCHASEORDER TYPE BAPIMEPOHEADER-PO_NUMBER.

  DATA:
    LT_POITEM                 TYPE TABLE OF BAPIMEPOITEM,
    LT_POITEMX                TYPE TABLE OF BAPIMEPOITEMX,
    LT_POADDRDELIVERY         TYPE TABLE OF BAPIMEPOADDRDELIVERY,
    LT_POSCHEDULE             TYPE TABLE OF BAPIMEPOSCHEDULE,
    LT_POSCHEDULEX            TYPE TABLE OF BAPIMEPOSCHEDULX,
    LT_POACCOUNT              TYPE TABLE OF BAPIMEPOACCOUNT,
    LT_POACCOUNTPROFITSEGMENT TYPE TABLE OF BAPIMEPOACCOUNTPROFITSEGMENT,
    LT_POACCOUNTX             TYPE TABLE OF BAPIMEPOACCOUNTX,
    LT_POCONDHEADER           TYPE TABLE OF BAPIMEPOCONDHEADER,
    LT_POCONDHEADERX          TYPE TABLE OF BAPIMEPOCONDHEADERX,
    LT_POCOND                 TYPE TABLE OF BAPIMEPOCOND,
    LT_POCONDX                TYPE TABLE OF BAPIMEPOCONDX,
    LT_POLIMITS               TYPE TABLE OF BAPIESUHC,
    LT_POCONTRACTLIMITS       TYPE TABLE OF BAPIESUCC,
    LT_POSERVICES             TYPE TABLE OF BAPIESLLC,
    LT_POSRVACCESSVALUES      TYPE TABLE OF BAPIESKLC,
    LT_POSERVICESTEXT         TYPE TABLE OF BAPIESLLTX,
    LT_EXTENSIONIN            TYPE TABLE OF BAPIPAREX,
    LT_EXTENSIONOUT           TYPE TABLE OF BAPIPAREX,
    LT_POEXPIMPITEM           TYPE TABLE OF BAPIEIPO,
    LT_POEXPIMPITEMX          TYPE TABLE OF BAPIEIPOX,
    LT_POTEXTHEADER           TYPE TABLE OF BAPIMEPOTEXTHEADER,
    LT_POTEXTITEM             TYPE TABLE OF BAPIMEPOTEXT,
    LT_ALLVERSIONS            TYPE TABLE OF BAPIMEDCM_ALLVERSIONS,
    LT_POPARTNER              TYPE TABLE OF BAPIEKKOP,
    LT_POCOMPONENTS           TYPE TABLE OF BAPIMEPOCOMPONENT,
    LT_POCOMPONENTSX          TYPE TABLE OF BAPIMEPOCOMPONENTX,
    LT_POSHIPPING             TYPE TABLE OF BAPIITEMSHIP,
    LT_POSHIPPINGX            TYPE TABLE OF BAPIITEMSHIPX,
    LT_POSHIPPINGEXP          TYPE TABLE OF BAPIMEPOSHIPPEXP,
    LT_SERIALNUMBER           TYPE TABLE OF BAPIMEPOSERIALNO,
    LT_SERIALNUMBERX          TYPE TABLE OF BAPIMEPOSERIALNOX,
    LT_INVPLANHEADER          TYPE TABLE OF BAPI_INVOICE_PLAN_HEADER,
    LT_INVPLANHEADERX         TYPE TABLE OF BAPI_INVOICE_PLAN_HEADERX,
    LT_INVPLANITEM            TYPE TABLE OF BAPI_INVOICE_PLAN_ITEM,
    LT_INVPLANITEMX           TYPE TABLE OF BAPI_INVOICE_PLAN_ITEMX,
    LT_NFMETALLITMS           TYPE TABLE OF /NFM/BAPIDOCITM,
    LT_RETURN                 TYPE TABLE OF BAPIRET2,
    LS_RETURN                 TYPE BAPIRET2.

  PERFORM 9005_GET_PO_CREATE_PARAMS
  USING LPT_FPAR_D
  CHANGING LS_POHEADER
    LS_POHEADERX
    LS_POADDRVENDOR
    LS_POEXPIMPHEADER
    LS_POEXPIMPHEADERX
    LS_VERSIONS
    LT_POITEM
    LT_POITEMX
    LT_POADDRDELIVERY
    LT_POSCHEDULE
    LT_POSCHEDULEX
    LT_POACCOUNT
    LT_POACCOUNTPROFITSEGMENT
    LT_POACCOUNTX
    LT_POCONDHEADER
    LT_POCONDHEADERX
    LT_POCOND
    LT_POCONDX
    LT_POLIMITS
    LT_POCONTRACTLIMITS
    LT_POSERVICES
    LT_POSRVACCESSVALUES
    LT_POSERVICESTEXT
    LT_EXTENSIONIN
    LT_EXTENSIONOUT
    LT_POEXPIMPITEM
    LT_POEXPIMPITEMX
    LT_POTEXTHEADER
    LT_POTEXTITEM
    LT_ALLVERSIONS
    LT_POPARTNER
    LT_POCOMPONENTS
    LT_POCOMPONENTSX
    LT_POSHIPPING
    LT_POSHIPPINGX
    LT_POSHIPPINGEXP
    LT_SERIALNUMBER
    LT_SERIALNUMBERX
    LT_INVPLANHEADER
    LT_INVPLANHEADERX
    LT_INVPLANITEM
    LT_INVPLANITEMX
    LT_NFMETALLITMS.

*   Create data
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      POHEADER               = LS_POHEADER
      POHEADERX              = LS_POHEADERX
      POADDRVENDOR           = LS_POADDRVENDOR
*     TESTRUN                =
*     MEMORY_UNCOMPLETE      =
*     MEMORY_COMPLETE        =
      POEXPIMPHEADER         = LS_POEXPIMPHEADER
      POEXPIMPHEADERX        = LS_POEXPIMPHEADERX
*     VERSIONS               =
*     NO_MESSAGING           =
*     NO_MESSAGE_REQ         =
*     NO_AUTHORITY           =
*     NO_PRICE_FROM_PO       =
*     PARK_COMPLETE          =
*     PARK_UNCOMPLETE        =
    IMPORTING
      EXPPURCHASEORDER       = LV_EXPPURCHASEORDER
*     EXPHEADER              =
*     EXPPOEXPIMPHEADER      =
    TABLES
      RETURN                 = LT_RETURN
      POITEM                 = LT_POITEM
      POITEMX                = LT_POITEMX
      POADDRDELIVERY         = LT_POADDRDELIVERY
      POSCHEDULE             = LT_POSCHEDULE
      POSCHEDULEX            = LT_POSCHEDULEX
      POACCOUNT              = LT_POACCOUNT
      POACCOUNTPROFITSEGMENT = LT_POACCOUNTPROFITSEGMENT
      POACCOUNTX             = LT_POACCOUNTX
      POCONDHEADER           = LT_POCONDHEADER
      POCONDHEADERX          = LT_POCONDHEADERX
      POCOND                 = LT_POCOND
      POCONDX                = LT_POCONDX
      POLIMITS               = LT_POLIMITS
      POCONTRACTLIMITS       = LT_POCONTRACTLIMITS
      POSERVICES             = LT_POSERVICES
      POSRVACCESSVALUES      = LT_POSRVACCESSVALUES
      POSERVICESTEXT         = LT_POSERVICESTEXT
      EXTENSIONIN            = LT_EXTENSIONIN
      EXTENSIONOUT           = LT_EXTENSIONOUT
      POEXPIMPITEM           = LT_POEXPIMPITEM
      POEXPIMPITEMX          = LT_POEXPIMPITEMX
      POTEXTHEADER           = LT_POTEXTHEADER
      POTEXTITEM             = LT_POTEXTITEM
      ALLVERSIONS            = LT_ALLVERSIONS
      POPARTNER              = LT_POPARTNER
      POCOMPONENTS           = LT_POCOMPONENTS
      POCOMPONENTSX          = LT_POCOMPONENTSX
      POSHIPPING             = LT_POSHIPPING
      POSHIPPINGX            = LT_POSHIPPINGX
      POSHIPPINGEXP          = LT_POSHIPPINGEXP
      SERIALNUMBER           = LT_SERIALNUMBER
      SERIALNUMBERX          = LT_SERIALNUMBERX
      INVPLANHEADER          = LT_INVPLANHEADER
      INVPLANHEADERX         = LT_INVPLANHEADERX
      INVPLANITEM            = LT_INVPLANITEM
      INVPLANITEMX           = LT_INVPLANITEMX
      NFMETALLITMS           = LT_NFMETALLITMS.

  LPT_RETURN = LT_RETURN.
  LPW_DOCRS  = LV_EXPPURCHASEORDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form 9005_GET_PO_CREATE_PARAMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LPT_FPAR_D
*&      <-- LS_POHEADER
*&      <-- LS_POHEADERX
*&      <-- LS_POADDRVENDOR
*&      <-- LS_POEXPIMPHEADER
*&      <-- LS_POEXPIMPHEADERX
*&      <-- LS_VERSIONS
*&      <-- LT_POITEM
*&      <-- LT_POITEMX
*&      <-- LT_POADDRDELIVERY
*&      <-- LT_POSCHEDULE
*&      <-- LT_POSCHEDULEX
*&      <-- LT_POACCOUNT
*&      <-- LT_POACCOUNTPROFITSEGMENT
*&      <-- LT_POACCOUNTX
*&      <-- LT_POCONDHEADER
*&      <-- LT_POCONDHEADERX
*&      <-- LT_POCOND
*&      <-- LT_POCONDX
*&      <-- LT_POLIMITS
*&      <-- LT_POCONTRACTLIMITS
*&      <-- LT_POSERVICES
*&      <-- LT_POSRVACCESSVALUES
*&      <-- LT_POSERVICESTEXT
*&      <-- LT_EXTENSIONIN
*&      <-- LT_EXTENSIONOUT
*&      <-- LT_POEXPIMPITEM
*&      <-- LT_POEXPIMPITEMX
*&      <-- LT_POTEXTHEADER
*&      <-- LT_POTEXTITEM
*&      <-- LT_ALLVERSIONS
*&      <-- LT_POPARTNER
*&      <-- LT_POCOMPONENTS
*&      <-- LT_POCOMPONENTSX
*&      <-- LT_POSHIPPING
*&      <-- LT_POSHIPPINGX
*&      <-- LT_POSHIPPINGEXP
*&      <-- LT_SERIALNUMBER
*&      <-- LT_SERIALNUMBERX
*&      <-- LT_INVPLANHEADER
*&      <-- LT_INVPLANHEADERX
*&      <-- LT_INVPLANITEM
*&      <-- LT_INVPLANITEMX
*&      <-- LT_NFMETALLITMS
*&---------------------------------------------------------------------*
FORM 9005_GET_PO_CREATE_PARAMS
USING P_LPT_FPAR_D TYPE ZTT_BM_EXBA_FPAR
CHANGING
LPS_POHEADER TYPE BAPIMEPOHEADER
LPS_POHEADERX TYPE BAPIMEPOHEADERX
LPS_POADDRVENDOR TYPE BAPIMEPOADDRVENDOR
LPS_POEXPIMPHEADER TYPE BAPIEIKP
LPS_POEXPIMPHEADERX TYPE BAPIEIKPX
LPS_VERSIONS TYPE BAPIMEDCM
LPT_POITEM TYPE BAPIMEPOITEM_TP
LPT_POITEMX TYPE BAPIMEPOITEMX_TP
LPT_POADDRDELIVERY TYPE BAPIMEPOADDRDELIVERY_TP
LPT_POSCHEDULE TYPE BAPIMEPOSCHEDULE_TP
LPT_POSCHEDULEX TYPE BAPIMEPOSCHEDULX_TP
LPT_POACCOUNT TYPE BAPIMEPOACCOUNT_TP
LPT_POACCOUNTPROFITSEGMENT TYPE BAPIMEPOACCOUNTPROFIT_TP
LPT_POACCOUNTX TYPE BAPIMEPOACCOUNTX_TP
LPT_POCONDHEADER TYPE BAPIMEPOCONDHEADER_TP
LPT_POCONDHEADERX TYPE BAPIMEPOCONDHEADERX_TP
LPT_POCOND TYPE BAPIMEPOCOND_TP
LPT_POCONDX TYPE BAPIMEPOCONDX_TP
LPT_POLIMITS TYPE BAPIESUHC_TP
LPT_POCONTRACTLIMITS TYPE BAPIESUCC_TP
LPT_POSERVICES TYPE BAPIESLLC_TP
LPT_POSRVACCESSVALUES TYPE BAPIESKLC_TP
LPT_POSERVICESTEXT TYPE BAPIESLLTX_TP
LPT_EXTENSIONIN TYPE BAPIPAREX_TAB
LPT_EXTENSIONOUT TYPE BAPIPAREX_TAB
LPT_POEXPIMPITEM TYPE BAPIEIPO_TP
LPT_POEXPIMPITEMX TYPE BAPIEIPOX_TP
LPT_POTEXTHEADER TYPE BAPIMEPOTEXTHEADER_TP
LPT_POTEXTITEM TYPE BAPIMEPOTEXT_TP
LPT_ALLVERSIONS TYPE /SYCLO/MM_BAPIMEDCM_ALLVER_TAB
LPT_POPARTNER TYPE BAPIEKKOP_TP
LPT_POCOMPONENTS TYPE BAPIMEPOCOMPONENT_TP
LPT_POCOMPONENTSX TYPE BAPIMEPOCOMPONENTX_TP
LPT_POSHIPPING TYPE BAPIITEMSHIP_TP
LPT_POSHIPPINGX TYPE BAPIITEMSHIPX_TP
LPT_POSHIPPINGEXP TYPE BAPIMEPOSHIPPEXP_TP
LPT_SERIALNUMBER TYPE BAPIMEPO_T_SERIALNO
LPT_SERIALNUMBERX TYPE BAPIMEPO_T_SERIALNOX
LPT_INVPLANHEADER TYPE BAPI_INVOICE_PLAN_HEADER_TTY
LPT_INVPLANHEADERX TYPE BAPI_INVOICE_PLAN_HEADERX_TTY
LPT_INVPLANITEM TYPE BAPI_INVOICE_PLAN_ITEM_TTY
LPT_INVPLANITEMX TYPE BAPI_INVOICE_PLAN_ITEMX_TTY
LPT_NFMETALLITMS TYPE /NFM/BAPIDOCITM_T.

  DATA:
    LR_DATA            TYPE REF TO DATA,
    LS_POHEADERX       TYPE BAPIMEPOHEADERX,
    LS_POEXPIMPHEADERX TYPE BAPIEIKPX,
    LS_POITEMX         TYPE BAPIMEPOITEMX,
    LS_POSCHEDULEX     TYPE BAPIMEPOSCHEDULX,
    LS_POACCOUNTX      TYPE BAPIMEPOACCOUNTX,
    LS_POCONDHEADERX   TYPE BAPIMEPOCONDHEADERX,
    LS_POCONDX         TYPE BAPIMEPOCONDX,
    LS_POEXPIMPITEMX   TYPE BAPIEIPOX,
    LS_POCOMPONENTSX   TYPE BAPIMEPOCOMPONENTX,
    LS_POSHIPPINGX     TYPE BAPIITEMSHIPX,
    LS_SERIALNUMBERX   TYPE BAPIMEPOSERIALNOX,
    LS_INVPLANHEADERX  TYPE BAPI_INVOICE_PLAN_HEADERX,
    LS_INVPLANITEMX    TYPE BAPI_INVOICE_PLAN_ITEMX.


  LOOP AT GT_EXBA_FPAR_D INTO DATA(LPS_FPAR_D).
    CASE LPS_FPAR_D-PARAM.
      WHEN 'POHEADER'.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LPS_POHEADER.
      WHEN 'POADDRVENDOR'.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LPS_POADDRVENDOR.
      WHEN 'POEXPIMPHEADER'.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LPS_POEXPIMPHEADER.
      WHEN 'VERSIONS'.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LPS_VERSIONS.
      WHEN 'POITEM'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POITEM.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POITEM.
      WHEN 'POADDRDELIVERY'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POADDRDELIVERY.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POADDRDELIVERY.
      WHEN 'POSCHEDULE'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POSCHEDULE.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POSCHEDULE.
      WHEN 'POACCOUNT'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POACCOUNT.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POACCOUNT.
      WHEN 'POACCOUNTPROFITSEGMENT'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POACCOUNTPROFITSEGMENT.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POACCOUNTPROFITSEGMENT.
      WHEN 'POCONDHEADER'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POCONDHEADER.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POCONDHEADER.
      WHEN 'POCOND'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POCOND.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POCOND.
      WHEN 'POLIMITS'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POLIMITS.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POLIMITS.
      WHEN 'POCONTRACTLIMITS'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POCONTRACTLIMITS.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POCONTRACTLIMITS.
      WHEN 'POSERVICES'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POSERVICES.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POSERVICES.
      WHEN 'POSRVACCESSVALUES'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POSRVACCESSVALUES.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POSRVACCESSVALUES.
      WHEN 'POSERVICESTEXT'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POSERVICESTEXT.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POSERVICESTEXT.
      WHEN 'EXTENSIONIN'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_EXTENSIONIN.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_EXTENSIONIN.
      WHEN 'EXTENSIONOUT'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_EXTENSIONOUT.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_EXTENSIONOUT.
      WHEN 'POEXPIMPITEM'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POEXPIMPITEM.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POEXPIMPITEM.
      WHEN 'POTEXTHEADER'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POTEXTHEADER.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POTEXTHEADER.
      WHEN 'POTEXTITEM'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POTEXTITEM.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POTEXTITEM.
      WHEN 'ALLVERSIONS'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_ALLVERSIONS.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_ALLVERSIONS.
      WHEN 'POPARTNER'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POPARTNER.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POPARTNER.
      WHEN 'POCOMPONENTS'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POCOMPONENTS.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POCOMPONENTS.
      WHEN 'POSHIPPING'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POSHIPPING.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POSHIPPING.
      WHEN 'POSHIPPINGEXP'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_POSHIPPINGEXP.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_POSHIPPINGEXP.
      WHEN 'SERIALNUMBER'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_SERIALNUMBER.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_SERIALNUMBER.
      WHEN 'INVPLANHEADER'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_INVPLANHEADER.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_INVPLANHEADER.
      WHEN 'INVPLANITEM'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_INVPLANITEM.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_INVPLANITEM.
      WHEN 'NFMETALLITMS'.
        CREATE DATA LR_DATA LIKE LINE OF LPT_NFMETALLITMS.
        CALL TRANSFORMATION ID
          SOURCE XML LPS_FPAR_D-XMLSTR
          RESULT DATA = LR_DATA->*.
        APPEND LR_DATA->* TO LPT_NFMETALLITMS.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
    EXPORTING
      I_SOURCE = LPS_POHEADER
    CHANGING
      C_DATA   = LPS_POHEADERX.

  CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
    EXPORTING
      I_SOURCE = LPS_POEXPIMPHEADER
    CHANGING
      C_DATA   = LPS_POEXPIMPHEADERX.

  LOOP AT LPT_POITEM ASSIGNING FIELD-SYMBOL(<LFS_POITEM>).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <LFS_POITEM>-MATERIAL
      IMPORTING
        OUTPUT = <LFS_POITEM>-MATERIAL.
    <LFS_POITEM>-MATERIAL_LONG = <LFS_POITEM>-MATERIAL.

    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = <LFS_POITEM>
      CHANGING
        C_DATA   = LS_POITEMX.
    LS_POITEMX-PO_ITEM = <LFS_POITEM>-PO_ITEM.
    LS_POITEMX-PO_ITEMX = 'X'.
    APPEND LS_POITEMX TO LPT_POITEMX.
  ENDLOOP.

  LOOP AT LPT_POACCOUNT INTO DATA(LS_POACCOUNT).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_POACCOUNT
      CHANGING
        C_DATA   = LS_POACCOUNTX.
    LS_POACCOUNTX-PO_ITEM = LS_POACCOUNT-PO_ITEM.
    LS_POACCOUNTX-PO_ITEMX = 'X'.
    APPEND LS_POACCOUNTX TO LPT_POACCOUNTX.
  ENDLOOP.


*  LPT_POCONDHEADERX
  LOOP AT LPT_POCONDHEADER INTO DATA(LS_POCONDHEADER).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_POCONDHEADER
      CHANGING
        C_DATA   = LS_POCONDHEADERX.
    LS_POCONDHEADERX-ITM_NUMBER = LS_POCONDHEADER-ITM_NUMBER.
    LS_POCONDHEADERX-COND_ST_NO = LS_POCONDHEADER-COND_ST_NO.
    APPEND LS_POCONDHEADERX TO LPT_POCONDHEADERX.
  ENDLOOP.

*  LPT_POCONDX
  LOOP AT LPT_POCOND INTO DATA(LS_POCOND).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_POCOND
      CHANGING
        C_DATA   = LS_POCONDX.
    LS_POCONDX-ITM_NUMBER = LS_POCOND-ITM_NUMBER.
    LS_POCONDX-COND_ST_NO = LS_POCOND-COND_ST_NO.
    APPEND LS_POCONDX TO LPT_POCONDX.
  ENDLOOP.

*  LPT_POEXPIMPITEMX
  LOOP AT LPT_POEXPIMPITEM INTO DATA(LS_POEXPIMPITEM).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_POEXPIMPITEM
      CHANGING
        C_DATA   = LS_POEXPIMPITEMX.
    LS_POEXPIMPITEMX-PO_ITEM = LS_POEXPIMPITEM-PO_ITEM.
    APPEND LS_POEXPIMPITEMX TO LPT_POEXPIMPITEMX.
  ENDLOOP.

*  LPT_POCOMPONENTSX
  LOOP AT LPT_POCOMPONENTS INTO DATA(LS_POCOMPONENTS).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_POCOMPONENTS
      CHANGING
        C_DATA   = LS_POCOMPONENTSX.
    LS_POCOMPONENTSX-PO_ITEM = LS_POCOMPONENTS-PO_ITEM.
    LS_POCOMPONENTSX-SCHED_LINE = LS_POCOMPONENTS-SCHED_LINE.
    LS_POCOMPONENTSX-ITEM_NO = LS_POCOMPONENTS-ITEM_NO.
    APPEND LS_POCOMPONENTSX TO LPT_POCOMPONENTSX.
  ENDLOOP.

*  LPT_POSHIPPINGX
  LOOP AT LPT_POSHIPPING INTO DATA(LS_POSHIPPING).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_POSHIPPING
      CHANGING
        C_DATA   = LS_POSHIPPINGX.
    LS_POSHIPPINGX-PO_ITEM = LS_POSHIPPING-PO_ITEM.
    LS_POSHIPPINGX-PO_ITEM = 'X'.
    APPEND LS_POSHIPPINGX TO LPT_POSHIPPINGX.
  ENDLOOP.

*  LPT_SERIALNUMBERX
  LOOP AT LPT_SERIALNUMBER INTO DATA(LS_SERIALNUMBER).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_SERIALNUMBER
      CHANGING
        C_DATA   = LS_SERIALNUMBERX.
    LS_SERIALNUMBERX-PO_ITEM = LS_SERIALNUMBER-PO_ITEM.
    LS_SERIALNUMBERX-PO_ITEM = 'X'.
    LS_SERIALNUMBERX-SCHED_LINE = LS_SERIALNUMBER-SCHED_LINE.
    LS_SERIALNUMBERX-SCHED_LINEX = 'X'.
    APPEND LS_SERIALNUMBERX TO LPT_SERIALNUMBERX.
  ENDLOOP.

*  LPT_INVPLANHEADERX
  LOOP AT LPT_INVPLANHEADERX INTO DATA(LS_INVPLANHEADER).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_INVPLANHEADER
      CHANGING
        C_DATA   = LS_INVPLANHEADERX.
    LS_INVPLANHEADERX-DOC_ITEM = LS_INVPLANHEADER-DOC_ITEM.
    APPEND LS_INVPLANHEADERX TO LPT_INVPLANHEADERX.
  ENDLOOP.

*  LPT_INVPLANITEMX
  LOOP AT LPT_INVPLANITEMX INTO DATA(LS_INVPLANITEM).
    CALL FUNCTION 'ZFM_DATA_SET_MARK_FR_SOURCE'
      EXPORTING
        I_SOURCE = LS_INVPLANITEM
      CHANGING
        C_DATA   = LS_INVPLANITEMX.
    LS_INVPLANITEMX-DOC_ITEM = LS_INVPLANITEM-DOC_ITEM.
    APPEND LS_INVPLANITEMX TO LPT_INVPLANITEMX.
  ENDLOOP.

ENDFORM.
