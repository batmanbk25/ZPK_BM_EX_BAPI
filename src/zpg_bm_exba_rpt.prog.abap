*&---------------------------------------------------------------------*
*& Report ZPG_BM_EXBA_RPT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZPG_BM_EXBA_RPT.
INCLUDE ZIN_BM_EXBA_RPTTOP                      .    " Global Data

* INCLUDE ZIN_BM_EXBA_RPTO01                      .  " PBO-Modules
* INCLUDE ZIN_BM_EXBA_RPTI01                      .  " PAI-Modules
 INCLUDE ZIN_BM_EXBA_RPTF01                      .  " FORM-Routines

 START-OF-SELECTION.
 PERFORM 0000_MAIN_PROC.
