*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_EXBP_MAINT
*   generation date: 26.09.2022 at 23:01:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_EXBP_MAINT     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
