*&---------------------------------------------------------------------*
*& Report ZPG_BM_CS_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZPG_BM_EX_BAPI_UPLOAD.

INCLUDE ZIN_BM_EX_BAPI_UPLOADTOP.                  " Global Data
INCLUDE ZIN_BM_EX_BAPI_UPLOADO01.                  " PBO-Modules
INCLUDE ZIN_BM_EX_BAPI_UPLOADI01.                  " PAI-Modules
INCLUDE ZIN_BM_EX_BAPI_UPLOADF01.                  " FORM-Routines

INCLUDE ZIN_BM_EX_BAPI_UPLOADF02.                  " FORM-Routines-BAPI_SALESORDER_CREATEFROMDAT2
INCLUDE ZIN_BM_EX_BAPI_UPLOADF03.                  " FORM-Routines-BAPI_ACC_DOCUMENT_POST
INCLUDE ZIN_BM_EX_BAPI_UPLOADF04.                  " FORM-Routines-BAPI_PR_CREATE
INCLUDE ZIN_BM_EX_BAPI_UPLOADF05.                  " FORM-Routines-BAPI_PO_CREATE1
INCLUDE ZIN_BM_EX_BAPI_UPLOADF06.                  " FORM-Routines-BAPI_BILLINGDOC_CREATEMULTIPLE
INCLUDE ZIN_BM_EX_BAPI_UPLOADF07.                  " FORM-Routines-BAPI_OUTB_DELIVERY_CREATE_SLS
INCLUDE ZIN_BM_EX_BAPI_UPLOADF08.                  " FORM-Routines-BAPI_GOODSMVT_CREATE
INCLUDE ZIN_BM_EX_BAPI_UPLOADF09.                  " FORM-Routines-BAPI_PRODORDCONF_CREATE_HDR
INCLUDE ZIN_BM_EX_BAPI_UPLOADF10.                  " FORM-Routines-BAPI_INCOMINGINVOICE_CREATE
INCLUDE ZIN_BM_EX_BAPI_UPLOADF11.                  " FORM-Routines-BAPI_BILLINGDOC_CREATEFROMDATA

INITIALIZATION.
  PERFORM 0000_INIT_PROC.

AT SELECTION-SCREEN.
  PERFORM 1000_PAI.

AT SELECTION-SCREEN OUTPUT.
  PERFORM 1000_PBO.

START-OF-SELECTION.
  PERFORM 0000_MAIN_PROC.
