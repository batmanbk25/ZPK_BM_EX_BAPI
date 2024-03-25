*&---------------------------------------------------------------------*
*& Include ZIN_BM_EXBA_RPTTOP              - Report ZPG_BM_EXBA_RPT
*&---------------------------------------------------------------------*

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
  GT_EXBA_FFM_D  TYPE TABLE OF ZST_BM_EXBA_FFM,
  GT_EXBA_FPAR_D TYPE TABLE OF ZST_BM_EXBA_FPAR,
  GT_FVAL_FL     TYPE TABLE OF ZST_BM_EXBA_FVAL_FL,
  GT_FVAL_D      TYPE TABLE OF ZST_BM_EXBA_FVAL,
  GO_ALV_0100    TYPE REF TO CL_GUI_ALV_GRID.
TABLES:
  ZTB_BM_EXBA_FFM, ZTB_BM_EXBA_FVAL.

SELECT-OPTIONS:
  S_FNAME FOR ZTB_BM_EXBA_FFM-FNAME MATCHCODE OBJECT ZSH_BM_EXBA_FNAME,
  S_CRUSR FOR ZTB_BM_EXBA_FFM-CRUSR,
  S_CRDAT FOR ZTB_BM_EXBA_FFM-CRDAT DEFAULT SY-DATUM,
  S_CRTIM FOR ZTB_BM_EXBA_FFM-CRTIM.
