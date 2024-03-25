FUNCTION CONVERSION_EXIT_ZEBMA_INPUT.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ANY
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ANY
*"--------------------------------------------------------------------

  CALL FUNCTION 'CONVERSION_EXIT_ZZALL_INPUT'
    EXPORTING
      INPUT     = INPUT
      I_DOMNAME = 'ZDO_BM_EXBA_MATCHING'
    IMPORTING
      OUTPUT    = OUTPUT.

ENDFUNCTION.
