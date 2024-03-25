*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTB_BM_EXBA_FM..................................*
DATA:  BEGIN OF STATUS_ZTB_BM_EXBA_FM                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTB_BM_EXBA_FM                .
CONTROLS: TCTRL_ZTB_BM_EXBA_FM
            TYPE TABLEVIEW USING SCREEN '0100'.
*...processing: ZVI_BM_EXBA_PAR.................................*
TABLES: ZVI_BM_EXBA_PAR, *ZVI_BM_EXBA_PAR. "view work areas
CONTROLS: TCTRL_ZVI_BM_EXBA_PAR
TYPE TABLEVIEW USING SCREEN '0102'.
DATA: BEGIN OF STATUS_ZVI_BM_EXBA_PAR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVI_BM_EXBA_PAR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVI_BM_EXBA_PAR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVI_BM_EXBA_PAR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVI_BM_EXBA_PAR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVI_BM_EXBA_PAR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVI_BM_EXBA_PAR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVI_BM_EXBA_PAR_TOTAL.

*.........table declarations:.................................*
TABLES: *ZTB_BM_EXBA_FM                .
TABLES: ZTB_BM_EXBA_FM                 .
TABLES: ZTB_BM_EXBA_PAR                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
