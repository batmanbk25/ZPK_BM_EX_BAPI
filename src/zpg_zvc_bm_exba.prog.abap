*&---------------------------------------------------------------------*
*& Report ZPG_ZVC_BM_EXBA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_ZVC_BM_EXBA.
INCLUDE LSVCMCOD.

FORM 9999_ZVC_BM_EXBA_03.
* Table for all entries loaded from database
  DATA: BEGIN OF ZVI_BM_EXBA_PAR_TOTAL OCCURS 0010.
          INCLUDE STRUCTURE ZVI_BM_EXBA_PAR.
          INCLUDE STRUCTURE VIMFLAGTAB.
  DATA: END OF ZVI_BM_EXBA_PAR_TOTAL.
  DATA:
    LW_OBJECT     TYPE VCLSTRUC-OBJECT,
    LW_ERROR_FLAG TYPE VCL_FLAG_TYPE,
    LT_EXBA_PAR   LIKE TABLE OF ZVI_BM_EXBA_PAR_TOTAL.

  IMPORT ZVI_BM_EXBA_PAR_TOTAL = LT_EXBA_PAR FROM MEMORY ID 'ZFG_EXBA_BAPI'.
  FREE MEMORY ID 'ZFG_EXBA_BAPI'.

  IF LT_EXBA_PAR IS NOT INITIAL.
    LW_OBJECT = 'ZVI_BM_EXBA_PAR'.

    PERFORM VCL_SET_TABLE_ACCESS_FOR_OBJ(ZPG_ZVC_BM_EXBA)
      USING LW_OBJECT
      CHANGING LW_ERROR_FLAG.
    APPEND LINES OF LT_EXBA_PAR TO <VCL_TOTAL>.
  ENDIF.

ENDFORM.
