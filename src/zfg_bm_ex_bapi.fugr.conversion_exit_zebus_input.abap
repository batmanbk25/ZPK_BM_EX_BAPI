FUNCTION CONVERSION_EXIT_ZEBUS_INPUT.
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
      I_DOMNAME = 'ZDO_BM_EXBA_UPSTS'
    IMPORTING
      OUTPUT    = OUTPUT.

ENDFUNCTION.
