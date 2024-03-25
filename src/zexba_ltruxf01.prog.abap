*-------------------------------------------------------------------
***COPY FROM INCLUDE LTRUXF01 S4/2021.
*---------------------------------------------------------------------*
*       FORM GET_REQUESTED_FIELDS                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RFIELDS                                                       *
*  -->  OFIELDS                                                       *
*  -->  VALUE(TYPE)                                                   *
*  -->  VALUE(AKTYP)                                                  *
*  -->  VALUE(RC)                                                     *
*---------------------------------------------------------------------*
FORM GET_REQUESTED_FIELDS TABLES   RFIELDS     STRUCTURE TPZ3R
                                   OFIELDS     STRUCTURE TPZ3R
                          USING    VALUE(TYPE)  LIKE BP000-TYPE
                                   VALUE(AKTYP) LIKE TP105-AKTYP
                          CHANGING VALUE(RC)    LIKE SY-SUBRC.
  CALL FUNCTION 'BPAR_M_FIELDMOD_REQU_FIELDS'
    EXPORTING
      AKTYP       = AKTYP
      I_TYPE      = TYPE
    TABLES
      REQU_FIELDS = RFIELDS
      OPT_FIELDS  = OFIELDS
    EXCEPTIONS
      OTHERS      = C_RC4.
  RC = SY-SUBRC.
  SORT RFIELDS BY TABNM.
  SORT OFIELDS BY TABNM.
ENDFORM.                               " GET_REQUESTED_FIELDS

*---------------------------------------------------------------------*
*       FORM MUSSFELD_INFO_HOLEN                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  MF                                                            *
*  -->  REQU_FIELDS                                                   *
*  -->  VALUE(STRUCTURE)                                              *
*---------------------------------------------------------------------*
FORM MUSSFELD_INFO_HOLEN TABLES MF               STRUCTURE DFIES
                                REQU_FIELDS      STRUCTURE TPZ3R
                         USING  VALUE(MODUS)     LIKE TP105-AKTYP
                                VALUE(STRUCTURE) LIKE DD02L-TABNAME
                                VALUE(MOD_FIELDS) TYPE DDFIELDS. "N3016442

  DATA:    BEGIN OF REQU_FIELDS_TEMP OCCURS 10.
             INCLUDE STRUCTURE TPZ3R.
  DATA:    END   OF REQU_FIELDS_TEMP.

  MF-TABNAME = STRUCTURE.
  CASE STRUCTURE.
    WHEN C_JBIUPDA1.
*     Folgende Felder müssen für alle Sätze angeliefert werden:
      MF-LOGFLAG   = C_SPACE.
      MF-FIELDNAME = 'BUKRS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SFEMODE'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'STYPE'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'RANL'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'DERF'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Relevant für Allgemeine Stammdaten
      MF-LOGFLAG   = C_ST_VDARL.
      MF-FIELDNAME = 'GSART'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'RDARNEHM'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'ROLETYP'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SANTWHR'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     note 1819790
*     the field may be set as optional
*     mf-fieldname = 'BZUSAGE'.          " Zusagekapital
*     perform add_to_mf_tab tables mf
*                            using  mf-fieldname mf-logflag.

*     Partnerzuordnung
      MF-LOGFLAG   = C_KON_PART.
      MF-FIELDNAME = 'RDARNEHM'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'ROLETYP'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Relevant für R/2 Stammdaten
      MF-LOGFLAG   = C_FISD.
      MF-FIELDNAME = 'GSART'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'RDARNEHM'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     mf-fieldname = 'ROLE'.
*     perform add_to_mf_tab tables mf
*                           using  mf-fieldname mf-logflag.
      MF-FIELDNAME = 'ROLETYP'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SANTWHR'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Relevant für Kopfdaten allgemein
      MF-FIELDNAME = 'DGUEL_KK'.
      MF-LOGFLAG   = C_KON_KOKO.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.

*     note 1819790
*     the field may be set as optional
*     mf-fieldname = 'BZUSAGE'.
*     perform add_to_mf_tab tables mf
*                          using  mf-fieldname mf-logflag.

*     Relevant für Konditonspositionsdaten
      MF-FIELDNAME = 'DGUEL_KK'.
      MF-LOGFLAG   = C_KON_KOPO.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Relevant für R/2 Kopfdaten
      MF-FIELDNAME = 'DGUEL_KK'.
      MF-LOGFLAG   = C_BAKO.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Relevant für Positionsdaten allgemein
      MF-LOGFLAG   = C_KON_KOPO.
      MF-FIELDNAME = 'DFAELL'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'DGUEL_KP'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'DVALUT'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SKOART'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Relevant für R/2 Positionsdaten
      MF-LOGFLAG   = C_BAKO.
      MF-FIELDNAME = 'DFAELL'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'DGUEL_KP'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'DVALUT'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SKOART'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Relevant für Formeln zu Positionsdaten allgemein
      MF-LOGFLAG   = C_KON_KOPA.
      MF-FIELDNAME = 'BUKRS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'DGUEL_KK'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'DGUEL_KP'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SKOART'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SFORMREF'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SVARNAME'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*   Bewegungsdaten
    WHEN C_JBIUPDAB.
*     Folgende Felder müssen für alle Sätze angeliefert werden:
      MF-LOGFLAG   = C_SPACE.
      MF-FIELDNAME = 'BUKRS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SDBMODE'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'RANL'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'DFAELL'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Feld 'BBWHR' wird an anderer Stelle geprüft
*     darf hier wg. Devisenbewertungen nicht geprüft werden
*      mf-fieldname = 'BBWHR'.
*      PERFORM add_to_mf_tab TABLES mf
*                            USING  mf-fieldname mf-logflag.
*      mf-fieldname = 'BBASIS'.
*      perform add_to_mf_tab tables mf
*                            using  mf-fieldname mf-logflag.
*      mf-fieldname = 'DBERBIS'.
*      perform add_to_mf_tab tables mf
*                            using  mf-fieldname mf-logflag.
*   Geschäftspartner relevante Felder
    WHEN C_JBIUPPA1.
*     Für Übernahme erforderliche Daten
      MF-LOGFLAG   = C_SPACE.
      MF-FIELDNAME = 'PARTNR'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'SFEMODE'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'STYPE'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Allgemeine Partnerdaten (BP000)
      MF-LOGFLAG   = C_ALLG_KOPF.
*     mf-logflag   = c_baan.
      IF MODUS <> C_ACT_CHNG.
        MF-FIELDNAME = 'GROUP_ID'.
        PERFORM ADD_TO_MF_TAB TABLES MF
                              USING  MF-FIELDNAME MF-LOGFLAG.
        MF-FIELDNAME = 'ADR_REF_K'.
        PERFORM ADD_TO_MF_TAB TABLES MF
                              USING  MF-FIELDNAME MF-LOGFLAG.
      ENDIF.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BP000.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BP011.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDADREF.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDADR.
*     Rollendatem
      MF-LOGFLAG   = C_ALLG_ROLE.
      MF-FIELDNAME = 'ROLETYP'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'ROLE'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Adreßdaten
      MF-LOGFLAG   = C_ALLG_ADR.
      MF-FIELDNAME = 'ADR_REF_K'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'ORT01'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'LAND1'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Allgemeine Bankverbindung (bpdkto)
      MF-LOGFLAG   = C_BASK.
      MF-FIELDNAME = 'BUKRS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'AKONT'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDKTO.
*     Kontosteuerung (Nebenbuch)
      MF-LOGFLAG   = C_ALLG_NEBEN.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDCON1.
*     Zahlungssteuerung
      MF-LOGFLAG   = C_ALLG_ZAHLU.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDCON2.
*     allg. Steuerung
      MF-LOGFLAG   = C_ALLG_ALLGS.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDCON3.
*     Bankverbindungen  (bpdbank)
      MF-LOGFLAG   = C_ALLG_BV.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDBANK.
      MF-FIELDNAME = 'PARTNR'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'BANKL'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'BANKN'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
      MF-FIELDNAME = 'BANKS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Allgemeine autom. Zahlungsverkehr (bpdazah)
      MF-LOGFLAG   = C_ALLG_ZV.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDAZAH.
*     Bonitätsdaten
      MF-LOGFLAG   = C_ALLG_BONI.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDBONI.
*     Allgemeine Meldedaten
      MF-LOGFLAG   = C_ALLG_MELD.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDMELD.
*     Buchungkreis Meldedaten
      MF-LOGFLAG   = C_ALLG_REPB.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDREPB.
      MF-FIELDNAME = 'BUKRS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Mahndaten
      MF-LOGFLAG   = C_ALLG_MAHN.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDMAHN.
      MF-FIELDNAME = 'BUKRS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Kontoverzinsung
      MF-LOGFLAG   = C_ALLG_VZIN.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDVZIN.
      MF-FIELDNAME = 'BUKRS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Zahldaten
      MF-LOGFLAG   = C_ALLG_ZAHL.
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                            MF
                                     USING  C_BPDZAHL.
      MF-FIELDNAME = 'BUKRS'.
      PERFORM ADD_TO_MF_TAB TABLES MF
                            USING  MF-FIELDNAME MF-LOGFLAG.
*     Fiskal. Daten
      MF-LOGFLAG   = C_ALLG_TAX .
      REQU_FIELDS_TEMP[] = REQU_FIELDS[].
      PERFORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP
                                           MF
                                    USING  C_BPDTAX.
    WHEN OTHERS.
  ENDCASE.
  LOOP AT MF.
    READ TABLE MOD_FIELDS TRANSPORTING NO FIELDS
      WITH KEY TABNAME   = MF-TABNAME
               FIELDNAME = MF-FIELDNAME
               LOGFLAG   = MF-LOGFLAG.
    IF SY-SUBRC EQ 0.
      DELETE MF.
    ENDIF.
  ENDLOOP.
  SORT MF BY TABNAME FIELDNAME.
ENDFORM.                               " MUSSFELD_INFO_HOLEN

*---------------------------------------------------------------------*
*       FORM ADD_TO_MF_TAB                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  MF                                                            *
*  -->  FIELDNAME                                                     *
*  -->  LOGFLAG                                                       *
*---------------------------------------------------------------------*
FORM ADD_TO_MF_TAB TABLES MF        STRUCTURE DFIES
                   USING  FIELDNAME LIKE DFIES-FIELDNAME
                          LOGFLAG   LIKE DFIES-LOGFLAG.

  MF-FIELDNAME = FIELDNAME.
  MF-LOGFLAG   = LOGFLAG.
  APPEND MF.
ENDFORM.                               " ADD_TO_MF_TAB

*---------------------------------------------------------------------*
*       FORM FILL_REQU_FIELD_TABLE                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  REQU_FIELDS_TEMP                                              *
*  -->  MF                                                            *
*  -->  VALUE(STRUCTURE)                                              *
*---------------------------------------------------------------------*
FORM FILL_REQU_FIELD_TABLE TABLES REQU_FIELDS_TEMP STRUCTURE TPZ3R
                                  MF               STRUCTURE DFIES
                           USING  VALUE(STRUCTURE) LIKE DD02L-TABNAME.

  CALL FUNCTION 'ISB_TR_CHECK_REQUIRED_FIELDS'
    EXPORTING
      I_STRUCTURE    = STRUCTURE
    TABLES
      IT_REQU_FIELDS = REQU_FIELDS_TEMP
    EXCEPTIONS
      OTHERS         = C_RC4.
  LOOP AT REQU_FIELDS_TEMP.
    MF-FIELDNAME = REQU_FIELDS_TEMP-FLDNM.
    PERFORM ADD_TO_MF_TAB TABLES MF
                          USING  MF-FIELDNAME MF-LOGFLAG.
  ENDLOOP.
ENDFORM.                               " FILL_REQU_FIELD_TABLE

*---------------------------------------------------------------------*
*       FORM DDIC_INFO_HOLEN                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DFIES                                                         *
*  -->  VALUE(STRUCTURE)                                              *
*  -->  VALUE(FMODULE)                                                *
*  -->  VALUE(RC)                                                     *
*---------------------------------------------------------------------*
FORM DDIC_INFO_HOLEN TABLES DFIES STRUCTURE DFIES
                     USING    VALUE(STRUCTURE)  LIKE DCOBJDEF-NAME
                     CHANGING VALUE(FMODULE)    LIKE TFDIR-FUNCNAME
                              VALUE(RC)         LIKE SY-SUBRC.
  DATA:
        TMP_TABNAME LIKE DCOBJDEF-NAME.

  DATA: BEGIN OF TMP_DFIES OCCURS 100.
          INCLUDE STRUCTURE DFIES.
  DATA: END   OF TMP_DFIES.

  TMP_TABNAME = STRUCTURE.
  IF TMP_TABNAME(1) = '*'.
    STRUCTURE = STRUCTURE+1.
  ENDIF.

  FMODULE = 'DDIF_FIELDINFO_GET'.
* fmodule = 'DDIF_NAMETAB_GET'   tut nicht DOMNAME kommt nicht zurück
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME   = STRUCTURE
    TABLES
      DFIES_TAB = TMP_DFIES
    EXCEPTIONS
      OTHERS    = C_RC4.
  RC = SY-SUBRC.
  CHECK RC = C_RC0.
  IF TMP_TABNAME(1) = '*'.
    TMP_DFIES-TABNAME = TMP_TABNAME.
    MODIFY TMP_DFIES FROM TMP_DFIES TRANSPORTING TABNAME
                                    WHERE TABNAME = STRUCTURE.
  ENDIF.
* 170502alf analog Hinweis 447169 Beginn
* offset must be specified in characters but dfies returns bytes
* causes problems in UNICODE environment
  DATA: H_OFFSET LIKE TMP_DFIES-LENG.


  H_OFFSET = 0.
  SORT TMP_DFIES BY POSITION.
  LOOP AT TMP_DFIES.
    TMP_DFIES-OFFSET = H_OFFSET.
    ADD TMP_DFIES-LENG TO H_OFFSET.
    MODIFY TMP_DFIES.
  ENDLOOP.

* 170502alf analog Hinweis 447169 Ende

  APPEND LINES OF TMP_DFIES TO DFIES.
ENDFORM.                               " DDIC_INFO_HOLEN

*---------------------------------------------------------------------*
*       FORM FIND_DOMAEN_VALUE                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(DOMAEN)                                                 *
*  -->  VALUE(DOMVAL)                                                 *
*  -->  VALUE(RC)                                                     *
*---------------------------------------------------------------------*
FORM FIND_DOMAEN_VALUE USING    VALUE(DOMAEN) LIKE DD07V-DOMNAME
                                VALUE(DOMVAL) TYPE ANY
                       CHANGING VALUE(RC)     LIKE SY-SUBRC.
  CLEAR RC.

  CALL FUNCTION 'LOAN_DOMAEN_MANAGER'
    EXPORTING
      I_DOMNAME    = DOMAEN
      I_DOMVALUE_L = DOMVAL
    EXCEPTIONS
      OTHERS       = C_RC4.
  RC = SY-SUBRC.

ENDFORM.                    "find_domaen_value

**&---------------------------------------------------------------------
*&      Form  BUILD_ERROR_HEADER
*&---------------------------------------------------------------------*
FORM BUILD_ERROR_HEADER   TABLES   ERROR_ITAB      STRUCTURE SPROT_X
                          USING    VALUE(SEVERITY) LIKE SPROT_X-SEVERITY
                                   VALUE(VAR1)
                                   VALUE(VAR2)
                          CHANGING VALUE(RC)       LIKE SY-SUBRC.
  CLEAR ERROR_ITAB.
  ERROR_ITAB-MSGNR    = C_ERROR_HEADER.
  ERROR_ITAB-VAR1     = VAR1.
  CONDENSE ERROR_ITAB-VAR1 NO-GAPS.
  ERROR_ITAB-VAR2     = VAR2.
  CONDENSE ERROR_ITAB-VAR2 NO-GAPS.
  ERROR_ITAB-SEVERITY = SEVERITY.
  READ TABLE ERROR_ITAB.
  IF SY-SUBRC <> C_RC0.
    APPEND ERROR_ITAB.
  ELSE.
    PERFORM CHANGE_ERROR_HEADER TABLES   ERROR_ITAB
                                USING VAR1 VAR2
                                CHANGING RC.
  ENDIF.
  CLEAR RC.
ENDFORM.                               " BUILD_ERROR_HEADER

*&---------------------------------------------------------------------*
*&      Form  CHANGE_ERROR_HEADER
*&---------------------------------------------------------------------*
FORM CHANGE_ERROR_HEADER   TABLES   ERROR_ITAB      STRUCTURE SPROT_X
                           USING    VALUE(VAR1)
                                    VALUE(VAR2)
                           CHANGING VALUE(RC)       LIKE SY-SUBRC.

  LOOP AT ERROR_ITAB WHERE ( INDEX IS INITIAL OR INDEX = SPACE )
                     AND     MSGNR  = C_ERROR_HEADER
                     AND     VAR1  >= VAR1.               "#EC PORTABLE
    ERROR_ITAB-VAR2 = VAR2.
    CONDENSE ERROR_ITAB-VAR2 NO-GAPS.
    MODIFY ERROR_ITAB.
  ENDLOOP.
  RC = SY-SUBRC.

ENDFORM.                               " CHANGE_ERROR_HEADER

*&---------------------------------------------------------------------*
*&      Form  FILL_ERROR_ITAB
*&---------------------------------------------------------------------*
FORM FILL_ERROR_ITAB TABLES   ERROR_ITAB         STRUCTURE SPROT_X
                    USING    VALUE(AG)          LIKE SPROT_X-AG
                             VALUE(SEVERITY)    LIKE SPROT_X-SEVERITY
                             VALUE(MSGNR)       LIKE SPROT_X-MSGNR
                             VALUE(INDEX)       LIKE SPROT_X-INDEX
                             VALUE(VAR1)       " like sprot_x-var1
                             VALUE(VAR2)       " like sprot_x-var2
                             VALUE(VAR3)       " like sprot_x-var3
                             VALUE(VAR4)       " like sprot_x-var4
                    CHANGING VALUE(RC) LIKE SY-SUBRC.
  CLEAR RC.
*  verarbeiten des eigentlichen fehlers
  CLEAR ERROR_ITAB.
  ERROR_ITAB-AG       = AG.
  ERROR_ITAB-MSGNR    = MSGNR.
  ERROR_ITAB-INDEX    = INDEX.
  ERROR_ITAB-VAR1     = VAR1.
  ERROR_ITAB-VAR2     = VAR2.
  ERROR_ITAB-VAR3     = VAR3.
  ERROR_ITAB-VAR4     = VAR4.
  CONDENSE ERROR_ITAB-VAR1 NO-GAPS.
  CONDENSE ERROR_ITAB-VAR2 NO-GAPS.
  CONDENSE ERROR_ITAB-VAR3 NO-GAPS.
  CONDENSE ERROR_ITAB-VAR4 NO-GAPS.
  ERROR_ITAB-SEVERITY = SEVERITY.
  APPEND ERROR_ITAB.
ENDFORM.                               " FILL_ERROR_ITAB

*&---------------------------------------------------------------------*
*&      Form  CLEAN_UP_ERROR_TABLE
*&---------------------------------------------------------------------*
FORM CLEAN_UP_ERROR_TABLE TABLES   ERROR_ITAB      STRUCTURE SPROT_X
                          USING    VALUE(P_COMPLETE) LIKE C_TRUE
                          CHANGING VALUE(RC).
  DATA: BEGIN OF ERROR_TAB_COPY OCCURS 10.
          INCLUDE STRUCTURE SPROT_X.
  DATA: END   OF ERROR_TAB_COPY.
  DATA:
    MIN_REL_INDEX LIKE ERROR_ITAB-INDEX,
    MAX_REL_INDEX LIKE ERROR_ITAB-INDEX.

  CLEAR RC.

  ERROR_TAB_COPY[] = ERROR_ITAB[].
  LOOP AT ERROR_ITAB WHERE INDEX IS INITIAL
                     AND   MSGNR = C_ERROR_HEADER.
    CLEAR: MIN_REL_INDEX, MAX_REL_INDEX.
    LOOP AT ERROR_TAB_COPY WHERE MSGNR <> C_ERROR_HEADER
                           AND   INDEX >= ERROR_ITAB-VAR1 "#EC PORTABLE
                           AND   INDEX <= ERROR_ITAB-VAR2. "#EC PORTABLE
      MAX_REL_INDEX = ERROR_TAB_COPY-INDEX.
      CHECK MIN_REL_INDEX IS INITIAL.
      MIN_REL_INDEX = ERROR_TAB_COPY-INDEX.
    ENDLOOP.
    IF SY-SUBRC <> C_RC0.
      DELETE ERROR_ITAB.
    ELSE.
      CHECK P_COMPLETE = C_TRUE.
      ERROR_ITAB-VAR3 = ERROR_ITAB-VAR1.
      ERROR_ITAB-VAR4 = ERROR_ITAB-VAR2.
      ERROR_ITAB-VAR1 = MIN_REL_INDEX.
      ERROR_ITAB-VAR2 = ERROR_ITAB-VAR4 - ERROR_ITAB-VAR3 + 1.
      IF ERROR_ITAB-VAR2 < MIN_REL_INDEX OR               "#EC PORTABLE
         ERROR_ITAB-VAR2 > MAX_REL_INDEX.                 "#EC PORTABLE
        ERROR_ITAB-VAR2 = ERROR_ITAB-VAR4.
      ENDIF.
      CONDENSE ERROR_ITAB-VAR1 NO-GAPS.
      CONDENSE ERROR_ITAB-VAR2 NO-GAPS.
      MODIFY ERROR_ITAB.
    ENDIF.
  ENDLOOP.

  CHECK P_COMPLETE = C_TRUE.
* org_index füllen
  REFRESH ERROR_TAB_COPY.
  ERROR_TAB_COPY[] = ERROR_ITAB[].
  LOOP AT ERROR_TAB_COPY WHERE MSGNR = C_ERROR_HEADER.
    LOOP AT ERROR_ITAB WHERE MSGNR <> C_ERROR_HEADER
                       AND   INDEX >= ERROR_TAB_COPY-VAR3 "#EC PORTABLE
                       AND   INDEX <= ERROR_TAB_COPY-VAR4. "#EC PORTABLE
      ERROR_ITAB-ORG_INDEX = ERROR_TAB_COPY-VAR3 + ERROR_ITAB-INDEX - 1.
      IF ERROR_ITAB-ORG_INDEX > ERROR_TAB_COPY-VAR4.      "#EC PORTABLE
        ERROR_ITAB-ORG_INDEX = ERROR_ITAB-INDEX.
      ENDIF.
      MODIFY ERROR_ITAB.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                               " CLEAN_UP_ERROR_TABLE

*&---------------------------------------------------------------------*
*&      Form  PERPARE_NUMBER
*&---------------------------------------------------------------------*
FORM PERPARE_NUMBER CHANGING VALUE(P_PARAMETER) TYPE ANY
                             VALUE(PC_DECIMAL_POS) TYPE I.
  DATA L_STRLEN TYPE I.
  DATA L_LAST_DECIMAL TYPE I.
  DATA L_HLPVZ TYPE I.

  L_STRLEN = STRLEN( P_PARAMETER ).
  CLEAR: SY-SUBRC, PC_DECIMAL_POS.
  WHILE SY-SUBRC = C_RC0.
    IF P_PARAMETER CA '.'.
      IF SY-FDPOS  > L_LAST_DECIMAL.
        L_LAST_DECIMAL = SY-FDPOS + 1.
      ENDIF.
    ENDIF.
    REPLACE '.' WITH SPACE INTO P_PARAMETER.
  ENDWHILE.
  CLEAR SY-SUBRC.
  WHILE SY-SUBRC = C_RC0.
    IF P_PARAMETER CA ','.
      IF SY-FDPOS  > L_LAST_DECIMAL.
        L_LAST_DECIMAL = SY-FDPOS + 1.
      ENDIF.
    ENDIF.
    REPLACE ',' WITH SPACE INTO P_PARAMETER.
  ENDWHILE.
  CLEAR SY-SUBRC.
  WHILE SY-SUBRC = C_RC0.
    IF P_PARAMETER CA ';'.
      IF SY-FDPOS  > L_LAST_DECIMAL.
        L_LAST_DECIMAL = SY-FDPOS + 1.
      ENDIF.
    ENDIF.
    REPLACE ';' WITH SPACE INTO P_PARAMETER.
  ENDWHILE.
  CLEAR SY-SUBRC.
  WHILE SY-SUBRC = C_RC0.
    IF P_PARAMETER CA '/'.
      IF SY-FDPOS  > L_LAST_DECIMAL.
        L_LAST_DECIMAL = SY-FDPOS + 1.
      ENDIF.
    ENDIF.
    REPLACE '/' WITH SPACE INTO P_PARAMETER.
  ENDWHILE.
  IF NOT L_LAST_DECIMAL IS INITIAL.
    L_HLPVZ = L_STRLEN - 1.
    IF P_PARAMETER+L_HLPVZ(1) = '-'.
      PC_DECIMAL_POS = L_STRLEN - L_LAST_DECIMAL - 1.
    ELSE.
      PC_DECIMAL_POS = L_STRLEN - L_LAST_DECIMAL.
    ENDIF.
  ENDIF.
  CONDENSE P_PARAMETER NO-GAPS.
ENDFORM.                               " PERPARE_NUMBER

*---------------------------------------------------------------------*
*       FORM PREPARE_NUMBER                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(CHAR)                                                   *
*  -->  VALUE(NUM)                                                    *
*---------------------------------------------------------------------*
FORM PREPARE_NUMBER USING VALUE(CHAR) TYPE ANY
                    CHANGING VALUE(NUM) TYPE ANY.

  CLEAR SY-SUBRC.
  WHILE SY-SUBRC = 0.
    REPLACE '-' WITH C_SPACE INTO CHAR.
  ENDWHILE.

  CLEAR SY-SUBRC.
  WHILE SY-SUBRC = 0.
    REPLACE '.' WITH C_SPACE INTO CHAR.
  ENDWHILE.

  CLEAR SY-SUBRC.
  WHILE SY-SUBRC = 0.
    REPLACE ',' WITH C_SPACE INTO CHAR.
  ENDWHILE.
  CONDENSE CHAR NO-GAPS.
  NUM = CHAR.

ENDFORM.                              " PREPARE_NUMBER

*&---------------------------------------------------------------------*
*&      Form  SELECTION_TABSTRIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0022   text
*      <--P_DYNPRO_TRUX  text
*----------------------------------------------------------------------*
FORM SELECTION_TABSTRIP USING    VALUE(P_OKCODE) TYPE SYTCODE
                        CHANGING VALUE(P_DYNPRO) TYPE SYDYNNR.
  CONSTANTS: C_DYNPRO_FORM TYPE SYDYNNR VALUE '1002',
             C_DYNPRO_FILE TYPE SYDYNNR VALUE '1001'.
  CASE P_OKCODE.
    WHEN 'TRUX_DATAIN'.
      P_DYNPRO = C_DYNPRO_FILE.
*    when 'TRUX_DATAFORM'.
*      p_dynpro = c_dynpro_form.
    WHEN OTHERS.
      P_DYNPRO = C_DYNPRO_FILE.
  ENDCASE.

ENDFORM.                               " SELECTION_TABSTRIP

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_1000
*&---------------------------------------------------------------------*
FORM USER_COMMAND_1000  USING   VALUE(P_OKCODE) TYPE SYTCODE
                       CHANGING VALUE(P_DISPLAY) STRUCTURE TRUX_DISPLAY
                                VALUE(P_RC)  TYPE SYSUBRC.
  CLEAR P_RC.
  CASE OK_CODE.
    WHEN 'OK'.
      IF NOT (
         P_DISPLAY-FILE_NAME IS INITIAL OR
         P_DISPLAY-FILE_SERV IS INITIAL OR
         P_DISPLAY-FILE_FORMAT IS INITIAL ).
        IF P_DISPLAY-FILE_FORMAT = 'XLS'.
          P_DISPLAY-FILE_SERV = 'OLE2'.
        ENDIF.
        SET SCREEN 0.
        LEAVE SCREEN.
      ENDIF.
    WHEN OTHERS.
      P_RC = C_RC4.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDFORM.                               " USER_COMMAND_1000

*&---------------------------------------------------------------------*
*&      Form  GET_WS_FILENAME
*&---------------------------------------------------------------------*
FORM GET_WS_FILENAME USING    VALUE(P_FILESERV)
                                          LIKE TRUX_DISPLAY-FILE_SERV
                              VALUE(P_FTYPE)
                                          LIKE TRUX_DISPLAY-FILE_FORMAT
                     CHANGING VALUE(P_WSFILE)
                                          LIKE TRUX_DISPLAY-FILE_NAME.
  DATA:
    TMP_UPLOAD_PATH LIKE RLGRAP-FILENAME,
    TMP_FIELDLN     TYPE I,
    TMP_MASK(80),
    TMP_FTYPE_HEAD  LIKE TMP_MASK  VALUE ',*.*,*.',
    TMP_FTYPE_TRAIL LIKE TMP_MASK  VALUE '.',
    TMP_ASTERISK    LIKE TMP_MASK  VALUE ',*.*,*.*.'.

  CHECK P_FILESERV <> 'APP'.
  FIELD-SYMBOLS: <F_TMP_UPLOAD_PATH>.

  GET PARAMETER ID 'GR9' FIELD TMP_UPLOAD_PATH.
  IF TMP_UPLOAD_PATH IS INITIAL.
    SET EXTENDED CHECK OFF.
    CALL FUNCTION 'PROFILE_GET'
      EXPORTING
        FILENAME = 'FRONT.INI'
        KEY      = 'Path'
        SECTION  = 'Filetransfer'
      IMPORTING
        VALUE    = TMP_UPLOAD_PATH.

    IF TMP_UPLOAD_PATH IS INITIAL.     "// not found
      CALL FUNCTION 'PROFILE_GET'
        EXPORTING
          FILENAME = 'FRONT.INI'
          KEY      = 'PathUpload'
          SECTION  = 'Filetransfer'
        IMPORTING
          VALUE    = TMP_UPLOAD_PATH.
    ENDIF.
    SET EXTENDED CHECK ON.
    IF TMP_UPLOAD_PATH IS INITIAL.     "// not found
      CALL FUNCTION 'WS_QUERY'
        EXPORTING
          QUERY  = 'CD'  "// Get Current Directory
        IMPORTING
          RETURN = TMP_UPLOAD_PATH.
    ENDIF.
    SET PARAMETER ID 'GR9' FIELD TMP_UPLOAD_PATH.
  ENDIF.
  TMP_FIELDLN = STRLEN( TMP_UPLOAD_PATH ) - 1.
  ASSIGN TMP_UPLOAD_PATH+TMP_FIELDLN(1) TO <F_TMP_UPLOAD_PATH>.
  IF <F_TMP_UPLOAD_PATH> = '/' OR <F_TMP_UPLOAD_PATH> = '\'.
    CLEAR <F_TMP_UPLOAD_PATH>.
  ENDIF.

  IF P_FTYPE = 'ASC'.
    TMP_MASK =  TMP_ASTERISK.
  ELSE.
    CONCATENATE TMP_FTYPE_HEAD P_FTYPE TMP_FTYPE_TRAIL INTO TMP_MASK.
    CONDENSE TMP_MASK NO-GAPS.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_PATH = TMP_UPLOAD_PATH
      MASK     = TMP_MASK
      MODE     = 'O'
    IMPORTING
      FILENAME = P_WSFILE
    EXCEPTIONS
      OTHERS   = 4.
ENDFORM.                               " GET_WS_FILENAME

*&---------------------------------------------------------------------*
*&      Form  GET_WS_FILE_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_REPID  text
*      -->P_SY_DYNNR  text
*      <--P_TRUX_DISPLAY_FILE_FORMAT  text
*----------------------------------------------------------------------*
FORM GET_WS_FILE_FORMAT USING    VALUE(P_REPID) TYPE SYREPID
                                 VALUE(P_DYNNR) TYPE SYDYNNR
                        CHANGING VALUE(P_FTYPE) TYPE TRUXS_FILEFORMAT
                                 VALUE(P_SERV)  TYPE TRUX_SERVERTYP.
  DATA:
    T_DYNPREAD LIKE DYNPREAD OCCURS 0 WITH HEADER LINE,
    L_DYNAME   LIKE  D020S-PROG,
    L_DYNUMB   LIKE  D020S-DNUM.

  L_DYNAME = P_REPID.
  L_DYNUMB = P_DYNNR.
  T_DYNPREAD-FIELDNAME = 'TRUX_DISPLAY-FILE_FORMAT'.
  APPEND T_DYNPREAD.
  T_DYNPREAD-FIELDNAME = 'TRUX_DISPLAY-FILE_SERV'.
  APPEND T_DYNPREAD.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME     = L_DYNAME
      DYNUMB     = L_DYNUMB
    TABLES
      DYNPFIELDS = T_DYNPREAD
    EXCEPTIONS
      OTHERS     = C_RC4.
  IF SY-SUBRC <> C_RC0.
    P_FTYPE = 'ASC'.
  ELSE.
    READ TABLE T_DYNPREAD
              WITH KEY FIELDNAME = 'TRUX_DISPLAY-FILE_FORMAT'.
    IF SY-SUBRC = C_RC0.
      P_FTYPE = T_DYNPREAD-FIELDVALUE.
    ENDIF.
    READ TABLE T_DYNPREAD
              WITH KEY FIELDNAME = 'TRUX_DISPLAY-FILE_SERV'.
    IF SY-SUBRC = C_RC0.
      P_SERV = T_DYNPREAD-FIELDVALUE.
    ENDIF.
  ENDIF.

ENDFORM.                               " GET_WS_FILE_FORMAT

*---------------------------------------------------------------------*
*       CLASS lcl_btci_tree IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_BTCI_TREE IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    DATA: L_ICONQUICK TYPE ICONQUICK.
    DATA: L_FCODE     TYPE UI_FUNC.

    A_TABNAME = PI_TABNAME.

    IF SY-BATCH IS INITIAL.
      CREATE OBJECT A_OREF_CONTAINER
        EXPORTING
          WIDTH                   = A_CON_WIDTH
          NO_AUTODEF_PROGID_DYNNR = 'X'
          HEIGHT                  = 66.

      CREATE OBJECT A_OREF_ALV_GRID
        EXPORTING
          PARENT         = A_OREF_CONTAINER
          ITEM_SELECTION = 'X'
          NO_HTML_HEADER = 'X'.
      CALL METHOD A_OREF_ALV_GRID->GET_TOOLBAR_OBJECT
        IMPORTING
          ER_TOOLBAR = A_OREF_TOOLBAR.
      L_ICONQUICK = 'Zurück'(bck).                          "#EC NOTEXT
      L_FCODE = 'BACK'.                                     "#EC NOTEXT
      CALL METHOD A_OREF_TOOLBAR->ADD_BUTTON
        EXPORTING
          FCODE     = L_FCODE
          ICON      = '@2O@'
          BUTN_TYPE = CNTB_BTYPE_BUTTON
          TEXT      = ''
          QUICKINFO = L_ICONQUICK.
      CALL METHOD A_OREF_TOOLBAR->ADD_BUTTON
        EXPORTING
          FCODE     = ''
          ICON      = ''
          BUTN_TYPE = CNTB_BTYPE_SEP
          TEXT      = ''
          QUICKINFO = ''.
      CALL METHOD FIELDCAT_BUILD
        EXPORTING
          PI_TABNAME = PI_TABNAME.
      CALL METHOD SORT_BUILD.
*      append cl_gui_alv_tree=>mc_fc_current_variant to a_ui_functions.
      APPEND CL_GUI_ALV_TREE=>MC_FC_CALCULATE TO A_UI_FUNCTIONS.

      IF NOT PI_CAPTION IS INITIAL.
        CALL METHOD SET_CAPTION
          EXPORTING
            PI_CAPTION = PI_CAPTION.
      ENDIF.
      A_REPID = PI_REPID.
      CALL METHOD HIDE_CONTAINER.
      SET HANDLER HANDLE_CLOSE_REQUEST FOR A_OREF_CONTAINER.
      SET HANDLER HANDLE_ITEM_CTMENU_REQUEST FOR A_OREF_ALV_GRID.
      SET HANDLER HANDLE_NODE_CTMENU_REQUEST FOR A_OREF_ALV_GRID.
      SET HANDLER HANDLE_DOUBLE_CLICK_ON_ITEM FOR A_OREF_ALV_GRID.
      SET HANDLER HANDLE_DOUBLE_CLICK_ON_NODE FOR A_OREF_ALV_GRID.
      SET HANDLER HANDLE_SELECTION_CLICK_ON_ITEM FOR A_OREF_ALV_GRID.
      SET HANDLER HANDLE_SELECTION_CLICK_ON_NODE FOR A_OREF_ALV_GRID.
      SET HANDLER HANDLE_BUTTON_CLICK FOR A_OREF_TOOLBAR.
      CALL METHOD SET_REGISTERED_EVENTS.
    ELSE.
      A_REPID = PI_REPID.
    ENDIF.
  ENDMETHOD.                    "constructor

  METHOD SET_CAPTION.
    A_CAPTION = PI_CAPTION.
    CALL METHOD A_OREF_CONTAINER->SET_CAPTION
      EXPORTING
        CAPTION = PI_CAPTION
      EXCEPTIONS
        OTHERS  = 4.
  ENDMETHOD.                    "set_caption

  METHOD SHOW_CONTAINER.
    IF NOT A_IS_INVISIBLE IS INITIAL.
      CALL METHOD A_OREF_CONTAINER->SET_VISIBLE
        EXPORTING
          VISIBLE = CL_GUI_CONTROL=>VISIBLE_TRUE
        EXCEPTIONS
          OTHERS  = 4.
    ENDIF.
    CLEAR A_IS_INVISIBLE.
    CALL METHOD A_OREF_CONTAINER->SET_FOCUS
      EXPORTING
        CONTROL = A_OREF_CONTAINER
      EXCEPTIONS
        OTHERS  = 4.

    " We flush the automation queue of the gui framework
    CALL METHOD CL_GUI_CFW=>FLUSH.
  ENDMETHOD.                    "show_container

  METHOD SHOW_GRID.
    DATA: L_DISVARIANT TYPE DISVARIANT.
    DATA: L_HEIGTH TYPE I.
    DATA: L_STABLE TYPE LVC_S_STBL.
    DATA: L_TABIX  TYPE SYTABIX.
    DATA: L_TAB_RLFVBTCI LIKE A_TAB_RLFVBTCI.
    DATA: L_TAB_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
    DATA: L_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
    DATA: L_PRINT_ALV TYPE SLIS_PRINT_ALV.
    DATA: L_HIERARCHY_HEADER TYPE TREEV_HHDR.

    IF SY-BATCH IS INITIAL.
      IF A_TAB_RLFVBTCI IS INITIAL.
        IF A_LAYOUT-GRID_TITLE IS INITIAL.
          A_LAYOUT-GRID_TITLE = A_CAPTION.
        ENDIF.

*   Prepare Header
        L_HIERARCHY_HEADER-HEADING = 'Hierarchy'(hie).      "#EC NOTEXT
        L_HIERARCHY_HEADER-TOOLTIP = 'tootip'(hit).         "#EC NOTEXT
        L_HIERARCHY_HEADER-WIDTH = 40.
        L_HIERARCHY_HEADER-WIDTH_PIX = ''.

        L_DISVARIANT-REPORT = A_REPID.

        CALL METHOD A_OREF_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING
            IS_HIERARCHY_HEADER  = L_HIERARCHY_HEADER
            I_STRUCTURE_NAME     = PI_STRUCTURE_NAME
            I_SAVE               = 'A'
            IS_VARIANT           = L_DISVARIANT
            IT_TOOLBAR_EXCLUDING = A_UI_FUNCTIONS
          CHANGING
            IT_FIELDCATALOG      = A_FIELDCAT[]
            IT_OUTTAB            = A_TAB_RLFVBTCI[]
          EXCEPTIONS
            OTHERS               = 4.
        IF SY-SUBRC <> 0.
          RAISE DISPLAY_FAILED.
        ENDIF.

        CALL METHOD CREATE_HIERARCHY
          EXPORTING
            PI_RLFVBTCI = PI_RLFVBTCI
          IMPORTING
            PE_TABIX    = A_SIZE_OF_TAB.

        L_HEIGTH = 58 + ( A_SIZE_OF_TAB * 8 ).
        CALL METHOD A_OREF_CONTAINER->SET_POSITION
          EXPORTING
            WIDTH  = A_CON_WIDTH
            HEIGHT = L_HEIGTH.

        IF PI_ONLY_WHEN_VISIBLE IS INITIAL.
          CALL METHOD SHOW_CONTAINER.
        ENDIF.
      ELSE.
        CALL METHOD SHOW_CONTAINER.
      ENDIF.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
        EXPORTING
          I_STRUCTURE_NAME = A_TABNAME
        CHANGING
          CT_FIELDCAT      = L_TAB_FIELDCAT
        EXCEPTIONS
          OTHERS           = 4.
      IF SY-SUBRC = 0.
        LOOP AT L_TAB_FIELDCAT INTO L_FIELDCAT
                                    WHERE FIELDNAME = 'ITEMNO'.
          L_FIELDCAT-NO_OUT = 'X'.
          MODIFY L_TAB_FIELDCAT INDEX SY-TABIX
                            FROM L_FIELDCAT TRANSPORTING NO_OUT.
        ENDLOOP.
        L_PRINT_ALV-PRINT = 'X'.
        L_PRINT_ALV-NO_PRINT_LISTINFOS = 'X'.
        L_PRINT_ALV-NO_PRINT_SELINFOS = 'X'.
        CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
          EXPORTING
            IT_FIELDCAT = L_TAB_FIELDCAT
            IS_PRINT    = L_PRINT_ALV
          TABLES
            T_OUTTAB    = PI_RLFVBTCI
          EXCEPTIONS
            OTHERS      = 4.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "show_grid

  METHOD HIDE_GRID.
    CALL METHOD HIDE_CONTAINER.
  ENDMETHOD.                    "hide_grid

  METHOD CREATE_HIERARCHY.
    DATA: L_RLFVBTCI LIKE LINE OF A_TAB_RLFVBTCI.
    DATA: L_OLD_RLFVBTCI LIKE LINE OF A_TAB_RLFVBTCI.
    DATA: L_DISP_RLFVBTCI LIKE LINE OF A_TAB_RLFVBTCI.
    DATA: L_REPID_KEY       TYPE LVC_NKEY,
          L_TCODE_KEY       TYPE LVC_NKEY,
          L_OBJEKT_KEY      TYPE LVC_NKEY,
          L_RESTOFIT_KEY    TYPE LVC_NKEY,
          L_TAB_NODE_KEY    TYPE LVC_T_NKEY,
          L_NODE_TEXT       TYPE LVC_VALUE,
          L_TSTCT           TYPE TSTCT,
          L_TAB_ITEM_LAYOUT TYPE LVC_T_LAYI,
          L_ITEM_LAYOUT     TYPE LVC_S_LAYI.

    CLEAR PE_TABIX.
    LOOP AT PI_RLFVBTCI INTO L_RLFVBTCI.
      L_DISP_RLFVBTCI = L_RLFVBTCI.
      CLEAR: L_DISP_RLFVBTCI-FNAM, L_DISP_RLFVBTCI-FVAL.
*     when ever the TCODE is changing: new node
      AT NEW TCODE.
        PE_TABIX = PE_TABIX + 1.
        CLEAR: L_TAB_ITEM_LAYOUT, L_ITEM_LAYOUT.
        L_ITEM_LAYOUT-FIELDNAME =
                             A_OREF_ALV_GRID->C_HIERARCHY_COLUMN_NAME.
*        l_item_layout-style = cl_gui_column_tree=>style_intensified.
        APPEND L_ITEM_LAYOUT TO L_TAB_ITEM_LAYOUT.
        L_TSTCT-TCODE = L_RLFVBTCI-TCODE.
        CALL FUNCTION 'TSTCT_READ'
          EXPORTING
            F_TSTCT = L_TSTCT
          IMPORTING
            F_TSTCT = L_TSTCT.
        L_NODE_TEXT =  L_TSTCT-TTEXT.
        CALL METHOD A_OREF_ALV_GRID->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = SPACE
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = L_TAB_ITEM_LAYOUT
          IMPORTING
            E_NEW_NODE_KEY   = L_TCODE_KEY.
        APPEND L_TCODE_KEY TO L_TAB_NODE_KEY.
      ENDAT.
*     when ever the OBJEKT is changing: new node
      AT NEW OBJEKT.
        PE_TABIX = PE_TABIX + 1.
        CLEAR: L_TAB_ITEM_LAYOUT, L_ITEM_LAYOUT.
        L_ITEM_LAYOUT-FIELDNAME =
                             A_OREF_ALV_GRID->C_HIERARCHY_COLUMN_NAME.
        L_ITEM_LAYOUT-STYLE = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFIED.
        APPEND L_ITEM_LAYOUT TO L_TAB_ITEM_LAYOUT.
        L_NODE_TEXT =  L_RLFVBTCI-OBJEKT.
        CALL METHOD A_OREF_ALV_GRID->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_TCODE_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = L_TAB_ITEM_LAYOUT
            IS_OUTTAB_LINE   = L_DISP_RLFVBTCI
          IMPORTING
            E_NEW_NODE_KEY   = L_OBJEKT_KEY.
        APPEND L_OBJEKT_KEY TO L_TAB_NODE_KEY.
      ENDAT.
*     when ever the REPID is changing: new node
      AT NEW ITEMNO.                   "repid.
        L_NODE_TEXT =  L_RLFVBTCI-REPID.
        L_NODE_TEXT =  L_DISP_RLFVBTCI-REPID.
        CLEAR: L_TAB_ITEM_LAYOUT, L_ITEM_LAYOUT.
        L_ITEM_LAYOUT-FIELDNAME =
                            A_OREF_ALV_GRID->C_HIERARCHY_COLUMN_NAME.
        APPEND L_ITEM_LAYOUT TO L_TAB_ITEM_LAYOUT.
        CALL METHOD A_OREF_ALV_GRID->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_OBJEKT_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = L_TAB_ITEM_LAYOUT
            IS_OUTTAB_LINE   = L_DISP_RLFVBTCI
          IMPORTING
            E_NEW_NODE_KEY   = L_REPID_KEY.
        APPEND L_REPID_KEY TO L_TAB_NODE_KEY.
      ENDAT.
      L_NODE_TEXT =  L_RLFVBTCI-DYNNR.
      CALL METHOD A_OREF_ALV_GRID->ADD_NODE
        EXPORTING
          I_RELAT_NODE_KEY = L_REPID_KEY
          I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
          I_NODE_TEXT      = L_NODE_TEXT
          IS_OUTTAB_LINE   = L_RLFVBTCI
        IMPORTING
          E_NEW_NODE_KEY   = L_RESTOFIT_KEY.
      APPEND L_RESTOFIT_KEY TO L_TAB_NODE_KEY.
*     save the current copy
      L_OLD_RLFVBTCI = L_RLFVBTCI.
    ENDLOOP.
    IF SY-SUBRC = 0.
      CALL METHOD A_OREF_ALV_GRID->UPDATE_CALCULATIONS.
      CALL METHOD A_OREF_ALV_GRID->FRONTEND_UPDATE.
    ENDIF.
  ENDMETHOD.                    "create_hierarchy

  METHOD CALL_VIEWING_TRANSACTION.
    DATA: L_RLFVBTCI TYPE RLFVBTCI.
    DATA: L_BDCDATA     TYPE BDCDATA,
          L_TAB_BDCDATA TYPE TABLE OF BDCDATA INITIAL SIZE 0,
          L_BUKRS       TYPE BUKRS,
          L_RANL        TYPE RANL.
    DATA: L_NODE_TEXT TYPE LVC_VALUE.

    IF NOT PI_NODE_KEY IS INITIAL.
      CALL METHOD A_OREF_ALV_GRID->GET_OUTTAB_LINE
        EXPORTING
          I_NODE_KEY    = PI_NODE_KEY
        IMPORTING
          E_OUTTAB_LINE = L_RLFVBTCI
          E_NODE_TEXT   = L_NODE_TEXT.
      CHECK NOT L_RLFVBTCI-NEW_OBJEKT IS INITIAL.
      CASE L_RLFVBTCI-TCODE.
        WHEN 'FNO1'.
*          set parameter id 'BOB' field l_rlfvbtci-new_objekt.
*          call transaction 'FNO3' and skip first screen.
          L_BDCDATA-FNAM     = 'BOB'.
          L_BDCDATA-FVAL     = L_RLFVBTCI-NEW_OBJEKT.
          APPEND L_BDCDATA TO L_TAB_BDCDATA.
*          call function 'CALL_TRANSACTION_FROM_TABLE'
          CALL FUNCTION 'CALL_TRANSACTION_FROM_TABLE_CO' "Hw 1036092
            STARTING NEW TASK L_NODE_TEXT
            EXPORTING
              I_TCODE         = 'FNO3'
            TABLES
              T_PARAMETER_IDS = L_TAB_BDCDATA
            EXCEPTIONS
              OTHERS          = 2.
        WHEN 'FNO5'.
          CLEAR L_BDCDATA.
          L_BDCDATA-PROGRAM  = 'SAPLFVDS'.
          L_BDCDATA-DYNPRO   = '0250'.
          L_BDCDATA-DYNBEGIN = 'X'.
          APPEND L_BDCDATA TO L_TAB_BDCDATA.
          CLEAR L_BDCDATA.
          L_BDCDATA-FNAM     = 'VDARLSIC-RSICHER'.
          L_BDCDATA-FVAL     = L_RLFVBTCI-NEW_OBJEKT.
          APPEND L_BDCDATA TO L_TAB_BDCDATA.
          CLEAR L_BDCDATA.
          L_BDCDATA-FNAM     = 'BDC_OKCODE'.
          L_BDCDATA-FVAL     = '=SRCH'.
          APPEND L_BDCDATA TO L_TAB_BDCDATA.
*          call function 'CALL_TRANSACTION_FROM_TABLE'
          CALL FUNCTION 'CALL_TRANSACTION_FROM_TABLE_CO' "Hw 1036092
            STARTING NEW TASK L_NODE_TEXT
            EXPORTING
              I_TCODE   = 'FNO7'
              I_MODE    = 'E'
            TABLES
              T_BDCDATA = L_TAB_BDCDATA
            EXCEPTIONS
              OTHERS    = 2.
        WHEN 'FNVM'.
          L_BUKRS = L_RLFVBTCI-NEW_OBJEKT(4).
          L_RANL  = L_RLFVBTCI-NEW_OBJEKT+5(13).
*          set parameter id 'BUK' field l_bukrs.
*          set parameter id 'RAD' field l_ranl.
*          call transaction 'FNVS' and skip first screen.
          L_BDCDATA-FNAM     = 'BUK'.
          L_BDCDATA-FVAL     = L_BUKRS.
          APPEND L_BDCDATA TO L_TAB_BDCDATA.
          L_BDCDATA-FNAM     = 'RAD'.
          L_BDCDATA-FVAL     = L_RANL.
          APPEND L_BDCDATA TO L_TAB_BDCDATA.
*          call function 'CALL_TRANSACTION_FROM_TABLE'
          CALL FUNCTION 'CALL_TRANSACTION_FROM_TABLE_CO' "Hw 1036092
            STARTING NEW TASK L_NODE_TEXT
            EXPORTING
              I_TCODE         = 'FNVS'
            TABLES
              T_PARAMETER_IDS = L_TAB_BDCDATA
            EXCEPTIONS
              OTHERS          = 2.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "call_viewing_transaction

  METHOD HANDLE_SELECTION_CLICK_ON_ITEM.
    CHECK FCODE = A_UCOMM.
    CALL METHOD CALL_VIEWING_TRANSACTION
      EXPORTING
        PI_NODE_KEY = NODE_KEY.
  ENDMETHOD.                    "handle_selection_click_on_item

  METHOD HANDLE_SELECTION_CLICK_ON_NODE.
    CHECK FCODE = A_UCOMM.
    CALL METHOD CALL_VIEWING_TRANSACTION
      EXPORTING
        PI_NODE_KEY = NODE_KEY.
  ENDMETHOD.                    "handle_selection_click_on_node

  METHOD HANDLE_DOUBLE_CLICK_ON_ITEM.
    CALL METHOD CALL_VIEWING_TRANSACTION
      EXPORTING
        PI_NODE_KEY = NODE_KEY.
  ENDMETHOD.                    "handle_double_click_on_item

  METHOD HANDLE_DOUBLE_CLICK_ON_NODE.
    CALL METHOD CALL_VIEWING_TRANSACTION
      EXPORTING
        PI_NODE_KEY = NODE_KEY.
  ENDMETHOD.                    "handle_double_click_on_node

  METHOD HANDLE_BUTTON_CLICK.
    CASE FCODE.
      WHEN 'BACK'.                                          "#EC NOTEXT
        CALL METHOD HIDE_CONTAINER.
      WHEN OTHERS.
        CLEAR FCODE.
    ENDCASE.
  ENDMETHOD.                    "handle_button_click

  METHOD HANDLE_CLOSE_REQUEST.
    CALL METHOD HIDE_CONTAINER.
  ENDMETHOD.                    "handle_close_request

  METHOD HIDE_CONTAINER.
    CALL METHOD A_OREF_CONTAINER->SET_VISIBLE
      EXPORTING
        VISIBLE = CL_GUI_CONTROL=>VISIBLE_FALSE.

    " We flush the atumation queue of the gui framework
    CALL METHOD CL_GUI_CFW=>FLUSH.
    A_IS_INVISIBLE = 'X'.
  ENDMETHOD.                    "hide_container

  METHOD FIELDCAT_BUILD.
    DATA: LS_FIELDCATALOG TYPE LVC_S_FCAT.

    IF A_FIELDCAT IS INITIAL.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          I_STRUCTURE_NAME = PI_TABNAME
        CHANGING
          CT_FIELDCAT      = A_FIELDCAT[].
      LOOP AT A_FIELDCAT INTO LS_FIELDCATALOG.
        CASE LS_FIELDCATALOG-FIELDNAME.
          WHEN 'FNAM'.
            LS_FIELDCATALOG-OUTPUTLEN = '30'.
            MODIFY A_FIELDCAT INDEX SY-TABIX
                            FROM LS_FIELDCATALOG TRANSPORTING OUTPUTLEN.
          WHEN 'FVAL'.
            LS_FIELDCATALOG-OUTPUTLEN = '30'.
            LS_FIELDCATALOG-SCRTEXT_M = 'Feldinhalt'(fih).
            LS_FIELDCATALOG-SCRTEXT_S = 'Feldinhalt'(fih).
            MODIFY A_FIELDCAT INDEX SY-TABIX
                            FROM LS_FIELDCATALOG TRANSPORTING OUTPUTLEN
                                                              SCRTEXT_S
                                                              SCRTEXT_M.
          WHEN OTHERS.
            CLEAR LS_FIELDCATALOG-KEY.
            LS_FIELDCATALOG-NO_OUT = 'X'.
            LS_FIELDCATALOG-TECH = 'X'.
            MODIFY A_FIELDCAT INDEX SY-TABIX
                              FROM LS_FIELDCATALOG TRANSPORTING NO_OUT
                                                   TECH KEY.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "fieldcat_build

  METHOD SORT_BUILD.
    DATA LS_SORT_WA TYPE LVC_S_SORT.

    LS_SORT_WA-SPOS = 1.
    LS_SORT_WA-FIELDNAME = 'OBJEKT'.
    APPEND LS_SORT_WA TO A_SORT.

    LS_SORT_WA-SPOS = 2.
    LS_SORT_WA-FIELDNAME = 'ITEMNO'.
    LS_SORT_WA-NO_OUT = 'X'.
    APPEND LS_SORT_WA TO A_SORT.

  ENDMETHOD.                    "sort_build

  METHOD HANDLE_ITEM_CTMENU_REQUEST.
    DATA: L_RLFVBTCI TYPE RLFVBTCI.
    DATA: L_NODE_TEXT TYPE LVC_VALUE.

    IF NOT NODE_KEY IS INITIAL.
      CALL METHOD A_OREF_ALV_GRID->GET_OUTTAB_LINE
        EXPORTING
          I_NODE_KEY    = NODE_KEY
        IMPORTING
          E_OUTTAB_LINE = L_RLFVBTCI
          E_NODE_TEXT   = L_NODE_TEXT.
      CHECK NOT L_RLFVBTCI-NEW_OBJEKT IS INITIAL.
      CALL METHOD MENU->ADD_FUNCTION
        EXPORTING
          FCODE = A_UCOMM
          TEXT  = TEXT-DIS.
    ENDIF.
  ENDMETHOD.                    "handle_item_ctmenu_request

  METHOD HANDLE_NODE_CTMENU_REQUEST.
    DATA: L_RLFVBTCI TYPE RLFVBTCI.
    DATA: L_NODE_TEXT TYPE LVC_VALUE.

    IF NOT NODE_KEY IS INITIAL.
      CALL METHOD A_OREF_ALV_GRID->GET_OUTTAB_LINE
        EXPORTING
          I_NODE_KEY    = NODE_KEY
        IMPORTING
          E_OUTTAB_LINE = L_RLFVBTCI
          E_NODE_TEXT   = L_NODE_TEXT.
      CHECK NOT L_RLFVBTCI-NEW_OBJEKT IS INITIAL.
      CALL METHOD MENU->ADD_FUNCTION
        EXPORTING
          FCODE = A_UCOMM
          TEXT  = TEXT-DIS.
    ENDIF.
  ENDMETHOD.                    "handle_node_ctmenu_request

  METHOD SET_REGISTERED_EVENTS.
    DATA: L_TAB_EVENTS TYPE CNTL_SIMPLE_EVENTS,
          L_EVENT      TYPE CNTL_SIMPLE_EVENT.

    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_HEADER_CLICK.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_BUTTON_CLICK.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_CHECKBOX_CHANGE.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_KEYPRESS.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_HEADER_CONTEXT_MEN_REQ.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_EXPAND_NO_CHILDREN.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_CONTEXT_MENU_REQ.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_CONTEXT_MENU_REQ.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
    APPEND L_EVENT TO L_TAB_EVENTS.
    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
    APPEND L_EVENT TO L_TAB_EVENTS.

    CALL METHOD A_OREF_ALV_GRID->SET_REGISTERED_EVENTS
      EXPORTING
        EVENTS = L_TAB_EVENTS
      EXCEPTIONS
        OTHERS = 4.

  ENDMETHOD.                    "set_registered_events
ENDCLASS.                    "lcl_btci_tree IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  dfies_to_xml
*&---------------------------------------------------------------------*
FORM DFIES_TO_XML TABLES   PI_TAB_DFIES STRUCTURE DFIES
                  USING VALUE(PI_PDOCUMENT) TYPE REF TO IF_IXML_DOCUMENT
                        VALUE(PI_ELEM)  TYPE REF TO IF_IXML_ELEMENT
                        VALUE(PI_VALUE) TYPE ANY
                        VALUE(PI_CONTENT) TYPE BOOLEAN.

  DATA:
    L_DFIES           TYPE DFIES,
    L_SIMPLE_ELEM     TYPE REF TO IF_IXML_ELEMENT,
    L_ELEM            TYPE REF TO IF_IXML_ELEMENT,
    RESULT            TYPE I,
    L_NAME            TYPE STRING,
    L_VALUE           TYPE STRING,
    L_TABIX           TYPE SYTABIX,
    L_NUMC_2_CHAR(12),
    L_TAB_TMP_DFIES   TYPE TABLE OF DFIES,
    L_TMP_DFIES       TYPE DFIES.
  FIELD-SYMBOLS: <FS_VALUE>.

  L_ELEM = PI_ELEM.

*  LOOP AT pi_tab_dfies INTO l_dfies
*                       WHERE datatype = 'CUKY'.
*    l_tabix = sy-tabix.
*    READ TABLE pi_tab_dfies TRANSPORTING NO FIELDS
*                            WITH KEY reffield = l_dfies-fieldname.
*    CHECK sy-subrc = 0.
*    DELETE pi_tab_dfies INDEX l_tabix.
*    APPEND l_dfies TO l_tab_tmp_dfies.
*  ENDLOOP.

  LOOP AT PI_TAB_DFIES INTO L_DFIES.
*    IF l_dfies-datatype = 'CURR' AND NOT l_dfies-reffield IS INITIAL.
*      ASSIGN COMPONENT l_dfies-reffield OF STRUCTURE pi_value
*                                      TO <fs_value>.
*      IF sy-subrc = c_rc0.
**      generate a special not for currency and amound
*        CLEAR l_value.
*        l_name = c_amount_and_currency.
*        l_simple_elem = pi_pdocument->create_simple_element(
*                                             name = l_name
*                                             parent = l_elem ).
*        l_elem = l_simple_elem.
**      tag the currency first
*        l_name = l_dfies-reffield.
*        READ TABLE l_tab_tmp_dfies INTO l_tmp_dfies
*                                 WITH KEY fieldname = l_dfies-reffield.
*        IF sy-subrc = 0.
*         IF <fs_value> IS INITIAL.
*            CLEAR l_value.
*          ELSE.
*            l_value = <fs_value>.
*          ENDIF.
*          IF <fs_value> IS INITIAL AND NOT pi_content IS INITIAL.
*            CONTINUE.
*          ENDIF.
*          l_simple_elem =
*             pi_pdocument->create_simple_element( name = l_name
*                                                  value = l_value
*                                                  parent = l_elem ).
**         set attributes:
*          l_name = 'Description'.
*          l_value = l_tmp_dfies-fieldtext.
*          result = l_simple_elem->set_attribute( name = l_name
*                                                value = l_value ).
*          l_name = 'Datatype'.
*          l_value = l_tmp_dfies-datatype.
*          result = l_simple_elem->set_attribute( name = l_name
*                                                 value = l_value ).
*          l_name = 'Length'.
*          WRITE l_tmp_dfies-leng TO l_numc_2_char NO-ZERO.
*          CONDENSE l_numc_2_char NO-GAPS.
*          l_value = l_numc_2_char.
*          result = l_simple_elem->set_attribute( name = l_name
*                                                 value = l_value ).
*        ENDIF.
*      ENDIF.
*    ELSE.
*      l_elem = pi_elem.
*    ENDIF.
    L_NAME = L_DFIES-FIELDNAME.
    ASSIGN COMPONENT L_DFIES-FIELDNAME OF STRUCTURE PI_VALUE
                                       TO <FS_VALUE>.
    IF SY-SUBRC <> 0 OR <FS_VALUE> IS INITIAL.
      CLEAR L_VALUE.
    ELSE.
      L_VALUE = <FS_VALUE>.
    ENDIF.
    IF <FS_VALUE> IS INITIAL AND NOT PI_CONTENT IS INITIAL.
      CONTINUE.
    ENDIF.
    L_SIMPLE_ELEM =
       PI_PDOCUMENT->CREATE_SIMPLE_ELEMENT( NAME = L_NAME
                                            VALUE = L_VALUE
                                            PARENT = L_ELEM ).
*     set attributes:
    L_NAME = 'Description'.                                 "#EC NOTEXT
    L_VALUE = L_DFIES-FIELDTEXT.
    RESULT = L_SIMPLE_ELEM->SET_ATTRIBUTE( NAME = L_NAME
                                          VALUE = L_VALUE ).
    L_NAME = 'Datatype'.                                    "#EC NOTEXT
    L_VALUE = L_DFIES-DATATYPE.
    RESULT = L_SIMPLE_ELEM->SET_ATTRIBUTE( NAME = L_NAME
                                           VALUE = L_VALUE ).
    L_NAME = 'Length'.                                      "#EC NOTEXT
    WRITE L_DFIES-LENG TO L_NUMC_2_CHAR NO-ZERO.
    CONDENSE L_NUMC_2_CHAR NO-GAPS.
    L_VALUE = L_NUMC_2_CHAR.
    RESULT = L_SIMPLE_ELEM->SET_ATTRIBUTE( NAME = L_NAME
                                           VALUE = L_VALUE ).
    IF NOT L_DFIES-DECIMALS IS INITIAL.
      L_NAME = 'Decimals'.                                  "#EC NOTEXT
      WRITE L_DFIES-DECIMALS TO L_NUMC_2_CHAR NO-ZERO.
      CONDENSE L_NUMC_2_CHAR NO-GAPS.
      L_VALUE = L_NUMC_2_CHAR.
      RESULT = L_SIMPLE_ELEM->SET_ATTRIBUTE( NAME = L_NAME
                                             VALUE = L_VALUE ).
    ENDIF.
  ENDLOOP.

ENDFORM.                               " dfies_to_xml

*---------------------------------------------------------------------*
*       FORM xml_header                                               *
*---------------------------------------------------------------------*
FORM XML_HEADER USING VALUE(PI_PDOCUMENT) TYPE REF TO IF_IXML_DOCUMENT
                      VALUE(PI_XML_ROOT) TYPE STRING
                CHANGING VALUE(PC_ELEM)  TYPE REF TO IF_IXML_ELEMENT.

  DATA:
    L_SIMPLE_ELEM TYPE REF TO IF_IXML_ELEMENT,
    L_ELEM        TYPE REF TO IF_IXML_ELEMENT,
    L_RESULT      TYPE I,
    L_NAME        TYPE STRING.

  L_NAME = PI_XML_ROOT .
  L_ELEM = PI_PDOCUMENT->CREATE_ELEMENT( NAME = L_NAME ).
  L_RESULT = PI_PDOCUMENT->APPEND_CHILD( L_ELEM ).
  PC_ELEM = L_ELEM.
ENDFORM.                               "xml_header

*---------------------------------------------------------------------*
*       FORM xml_node                                                 *
*---------------------------------------------------------------------*
FORM XML_NODE  USING VALUE(PI_PDOCUMENT) TYPE REF TO IF_IXML_DOCUMENT
                      VALUE(PI_XML_NODE) TYPE STRING
                CHANGING VALUE(PC_ELEM)  TYPE REF TO IF_IXML_ELEMENT.

  DATA:
        L_ELEM TYPE REF TO IF_IXML_ELEMENT.

  L_ELEM = PI_PDOCUMENT->CREATE_SIMPLE_ELEMENT(
                                    NAME = PI_XML_NODE
                                    PARENT = PC_ELEM ).
  PC_ELEM = L_ELEM.
ENDFORM.                             "xml_node

*&---------------------------------------------------------------------*
*&      Form  DESCR_TO_XML
*&---------------------------------------------------------------------*
FORM DESCR_TO_XML USING VALUE(PI_TAB_DESCR) TYPE ABAP_COMPDESCR_TAB
                        VALUE(PI_PDOCUMENT) TYPE REF TO IF_IXML_DOCUMENT
                        VALUE(PI_ELEM)  TYPE REF TO IF_IXML_ELEMENT
                        VALUE(PI_VALUE) TYPE ANY
                        VALUE(PI_CONTENT) TYPE BOOLEAN.

  DATA:
    L_FIELD           LIKE LINE OF PI_TAB_DESCR,
    L_SIMPLE_ELEM     TYPE REF TO IF_IXML_ELEMENT,
    RESULT            TYPE I,
    L_NAME            TYPE STRING,
    L_VALUE           TYPE STRING,
    L_NUMC_2_CHAR(12).
  FIELD-SYMBOLS: <FS_VALUE>.

  LOOP AT PI_TAB_DESCR INTO L_FIELD.
    L_NAME = L_FIELD-NAME.
    ASSIGN COMPONENT L_FIELD-NAME OF STRUCTURE PI_VALUE
                                     TO <FS_VALUE>.
    IF SY-SUBRC <> 0 OR <FS_VALUE> IS INITIAL.
      CLEAR L_VALUE.
    ELSE.
      L_VALUE = <FS_VALUE>.
    ENDIF.
    IF <FS_VALUE> IS INITIAL AND NOT PI_CONTENT IS INITIAL.
      CONTINUE.
    ENDIF.
    L_SIMPLE_ELEM =
       PI_PDOCUMENT->CREATE_SIMPLE_ELEMENT( NAME = L_NAME
                                            VALUE = L_VALUE
                                            PARENT = PI_ELEM ).
    L_NAME = 'Datatype'.
    L_VALUE = L_FIELD-TYPE_KIND.
    RESULT = L_SIMPLE_ELEM->SET_ATTRIBUTE( NAME = L_NAME
                                           VALUE = L_VALUE ).
    L_NAME = 'Length'.
    WRITE L_FIELD-LENGTH TO L_NUMC_2_CHAR NO-ZERO.
    CONDENSE L_NUMC_2_CHAR NO-GAPS.
    L_VALUE = L_NUMC_2_CHAR.
    RESULT = L_SIMPLE_ELEM->SET_ATTRIBUTE( NAME = L_NAME
                                           VALUE = L_VALUE ).
    IF NOT L_FIELD-DECIMALS IS INITIAL.
      L_NAME = 'Decimals'.
      WRITE L_FIELD-DECIMALS TO L_NUMC_2_CHAR NO-ZERO.
      CONDENSE L_NUMC_2_CHAR NO-GAPS.
      L_VALUE = L_NUMC_2_CHAR.
      RESULT = L_SIMPLE_ELEM->SET_ATTRIBUTE( NAME = L_NAME
                                               VALUE = L_VALUE ).
    ENDIF.
  ENDLOOP.

ENDFORM.                               " DESCR_TO_XML

*&---------------------------------------------------------------------*
*&      Form  PARSE_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_IREF_NODE  text
*----------------------------------------------------------------------*
FORM PARSE_NODE USING VALUE(PI_IREF_NODE) TYPE REF TO IF_IXML_NODE
                      VALUE(PI_FIELDNAME) TYPE FIELDNAME
                CHANGING VALUE(PC_TAB_DATA) TYPE STANDARD TABLE
                         VALUE(PC_DATA) TYPE ANY.

  DATA: L_IREF_TEXT   TYPE REF TO IF_IXML_TEXT.
  DATA: L_STRING      TYPE STRING.
  DATA: L_STRING2     TYPE STRING.
  DATA: L_DATA        TYPE REF TO DATA.
  DATA: L_INDENT      TYPE I.

  FIELD-SYMBOLS: <FS_SOURCE> TYPE STRING.
  FIELD-SYMBOLS: <FS_TARGET>.
  FIELD-SYMBOLS: <FS_STRUCTURE>.

  L_INDENT = PI_IREF_NODE->GET_HEIGHT( ).
  L_STRING = PI_IREF_NODE->GET_NAME( ).

  CASE PI_IREF_NODE->GET_TYPE( ).
    WHEN IF_IXML_NODE=>CO_NODE_ELEMENT.
      CASE L_INDENT.
        WHEN 0.
        WHEN 1.
        WHEN 2.
          CLEAR L_DATA.
          CREATE DATA L_DATA TYPE (L_STRING).
          CATCH SYSTEM-EXCEPTIONS CREATE_DATA_UNKNOWN_TYPE = 4
                                  CREATE_DATA_NOT_ALLOWED_TYPE = 4.
            IF SY-SUBRC <> 0.
              EXIT.
            ENDIF.
            ASSIGN L_DATA->* TO <FS_STRUCTURE>.
            PC_DATA = <FS_STRUCTURE>.
          ENDCATCH.
        WHEN 3.
          PI_FIELDNAME = L_STRING.
        WHEN OTHERS.
      ENDCASE.
    WHEN IF_IXML_NODE=>CO_NODE_TEXT.
*      l_iref_text = cl_ixml_text=>downcast( pi_iref_node ).
*      l_iref_text ?=  pi_iref_node.
      L_STRING = PI_IREF_NODE->GET_NAME( ).
*      if l_iref_text->ws_only( ) is initial.
      L_STRING2 = PI_IREF_NODE->GET_VALUE( ).
      ASSIGN L_STRING2 TO <FS_SOURCE>.
      ASSIGN COMPONENT PI_FIELDNAME
                                OF STRUCTURE PC_DATA TO <FS_TARGET>.
      IF SY-SUBRC = 0.
        PERFORM INPUT_DATA2SAP_DATA USING <FS_SOURCE>
                                    CHANGING <FS_TARGET> SY-SUBRC.
      ENDIF.
*      endif.
  ENDCASE.

  PI_IREF_NODE = PI_IREF_NODE->GET_FIRST_CHILD( ).

  WHILE NOT PI_IREF_NODE IS INITIAL.

    PERFORM PARSE_NODE USING PI_IREF_NODE PI_FIELDNAME
                       CHANGING PC_TAB_DATA PC_DATA.
    PI_IREF_NODE = PI_IREF_NODE->GET_NEXT( ).
    CHECK L_INDENT = 1.
    IF NOT PC_DATA IS INITIAL.
      APPEND PC_DATA TO PC_TAB_DATA.
      CLEAR PC_DATA.
    ENDIF.
  ENDWHILE.

ENDFORM.                               " PARSE_NODE

*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA2SAP_DATA
*&---------------------------------------------------------------------*
FORM INPUT_DATA2SAP_DATA USING VALUE(PI_SOURCE)  TYPE ANY
                         CHANGING VALUE(PI_TARGET)  TYPE ANY
                                  VALUE(PC_SUBRC)   TYPE SYSUBRC.
  DATA: PI_FIELD_TYPE TYPE C.
  DATA: L_DATE TYPE SYDATUM.
  DATA: L_DECIMALS TYPE I.
  DATA: L_DECIMALS_TARGET TYPE I.
  DATA: L_CHAR_REPLACE TYPE CHAR1.
  FIELD-SYMBOLS: <FS_TYPE_X> TYPE X.

  DESCRIBE FIELD PI_TARGET TYPE PI_FIELD_TYPE.

  CLEAR PC_SUBRC.

  CASE PI_FIELD_TYPE.
    WHEN 'C'.
      PI_TARGET = PI_SOURCE.

    WHEN 'D'.
* Bitte beachten:
* Die Funktion CONVERT_DATE_TO_INTERNAL erwartet das Datum analog den
* Einstellungen in den Festwerten des Benutzers. Ist dort das Format
* TT.MM.JJJJ eingestellt, dann muss der Routine das Datum in der
* Form 31.12.1999 oder 31121999 übergeben werden.
* Ist bei den Benutzerfestwerten das amerikanische Format JJJJ.MM.TT
* eingestellt, dann erwartet die Funktion das Datum in der Form
* 1999.12.31 oder 19991231
* Einige Excel Datumsformate haben die Eigenart, zusätzliche Leerzeichen
* und Punkte in das Datum einzufügen. Deshalb werden Punkte in der
* DO Schleife entfernt und anschliessend noch die Leerzeichen durch
* den condense entfernt.
      IF    PI_SOURCE IS NOT INITIAL
        AND PI_SOURCE <> '00.00.0000'
        AND PI_SOURCE <> '00/00/0000'
        AND PI_SOURCE <> '00-00-0000'
        AND PI_SOURCE <> '00000000'.
        IF PI_SOURCE CA '.'.
          L_CHAR_REPLACE = '.'.
        ELSEIF PI_SOURCE CA '/'.
          L_CHAR_REPLACE = '/'.
        ELSEIF PI_SOURCE CA '-'.
          L_CHAR_REPLACE = '-'.
        ENDIF.
      ENDIF.

      IF L_CHAR_REPLACE IS NOT INITIAL.
        PI_TARGET = SPACE.
        DO 3 TIMES.
          CONDENSE PI_SOURCE NO-GAPS.
          CLEAR SY-FDPOS.
          FIND FIRST OCCURRENCE OF L_CHAR_REPLACE
             IN PI_SOURCE MATCH OFFSET SY-FDPOS.
          CASE SY-FDPOS.
            WHEN 0.
              CONCATENATE PI_TARGET PI_SOURCE INTO PI_TARGET.
              CLEAR PI_SOURCE.
            WHEN 1.
              CONCATENATE PI_TARGET '0' PI_SOURCE(1) INTO PI_TARGET.
              SHIFT PI_SOURCE.
            WHEN OTHERS.
              CONCATENATE PI_TARGET PI_SOURCE(SY-FDPOS) INTO PI_TARGET.
              SHIFT PI_SOURCE BY SY-FDPOS PLACES.
          ENDCASE.
          REPLACE L_CHAR_REPLACE WITH ' ' INTO PI_SOURCE.
        ENDDO.

        PI_SOURCE = PI_TARGET.
        CLEAR PI_TARGET.
      ENDIF.

      CONDENSE PI_SOURCE NO-GAPS.

      IF    PI_SOURCE IS NOT INITIAL
        AND PI_SOURCE <> '00.00.0000'
        AND PI_SOURCE <> '00/00/0000'
        AND PI_SOURCE <> '00-00-0000'
        AND PI_SOURCE <> '00000000'.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            DATE_EXTERNAL = PI_SOURCE
          IMPORTING
            DATE_INTERNAL = PI_TARGET
          EXCEPTIONS
            ERROR_MESSAGE = C_RC4
            OTHERS        = C_RC4.
        IF SY-SUBRC <> C_RC0.
          PC_SUBRC = 4.
        ENDIF.
      ENDIF.

    WHEN 'T'.
      IF    PI_SOURCE IS NOT INITIAL
        AND PI_SOURCE <> '00:00:00'.
        CALL FUNCTION 'CONVERT_TIME_INPUT'
          EXPORTING
            INPUT         = PI_SOURCE
          IMPORTING
            OUTPUT        = PI_TARGET
          EXCEPTIONS
            ERROR_MESSAGE = C_RC4
            OTHERS        = C_RC4.
        IF SY-SUBRC <> C_RC0.
          PC_SUBRC = 4.
        ENDIF.
      ENDIF.
    WHEN 'X'.
      ASSIGN PI_SOURCE TO <FS_TYPE_X> .
      IF SY-SUBRC <> C_RC0.
        PC_SUBRC = 8.
      ENDIF.
      PI_TARGET = <FS_TYPE_X> .
    WHEN 'N'.
      DESCRIBE FIELD PI_TARGET DECIMALS L_DECIMALS_TARGET.
      PERFORM PERPARE_NUMBER CHANGING PI_SOURCE L_DECIMALS.
      IF PI_SOURCE CN C_DARL_NUMBER.
        PC_SUBRC = C_RC8.
      ELSE.
        L_DECIMALS_TARGET = L_DECIMALS_TARGET - L_DECIMALS.
        PI_TARGET = PI_SOURCE * ( 10 ** ( L_DECIMALS_TARGET ) ).
      ENDIF.
    WHEN 'I'.
      DESCRIBE FIELD PI_TARGET DECIMALS L_DECIMALS_TARGET.
      PERFORM PERPARE_NUMBER CHANGING PI_SOURCE L_DECIMALS.
      IF PI_SOURCE CN C_DARL_NUMBER.
        PC_SUBRC = 8.
      ELSE.
        L_DECIMALS_TARGET = L_DECIMALS_TARGET - L_DECIMALS.
        PI_TARGET = PI_SOURCE * ( 10 ** ( L_DECIMALS_TARGET ) ).
      ENDIF.
    WHEN 'P'.
      DATA:
        L_LEN_SOURCE TYPE I,
        L_LEN_TARGET TYPE I,
        L_LEN_PREFIX TYPE I,
        L_MINUS      TYPE XFELD.
      L_LEN_SOURCE = STRLEN( PI_SOURCE ).
      IF L_LEN_SOURCE > 0.
        L_LEN_PREFIX = L_LEN_SOURCE - 1.
      ELSE.
        L_LEN_PREFIX = 0.
      ENDIF.
      IF PI_SOURCE(1) = '-'.
        L_MINUS = 'X'.
        SHIFT PI_SOURCE BY 1 PLACES LEFT.
      ELSEIF PI_SOURCE+L_LEN_PREFIX(1) = '-'.
        L_MINUS = 'X'.
        PI_SOURCE = PI_SOURCE(L_LEN_PREFIX).
      ENDIF.

      DESCRIBE FIELD PI_TARGET DECIMALS L_DECIMALS_TARGET.
      PERFORM PERPARE_NUMBER CHANGING PI_SOURCE L_DECIMALS.

      DESCRIBE FIELD PI_TARGET LENGTH L_LEN_TARGET IN BYTE MODE.
      L_LEN_TARGET = ( L_LEN_TARGET * 2 ) - 1.
      L_LEN_SOURCE = STRLEN( PI_SOURCE ).
      IF L_LEN_SOURCE > L_LEN_TARGET.
        L_DECIMALS_TARGET = L_DECIMALS_TARGET -  L_LEN_TARGET +
                            L_LEN_SOURCE.
        PI_SOURCE = PI_SOURCE(L_LEN_TARGET).
      ENDIF.

      IF PI_SOURCE CN '1234567890 -+'.
        PC_SUBRC = 8.
        RETURN.                                             "N2865227
      ELSE.
        L_DECIMALS_TARGET = L_DECIMALS_TARGET - L_DECIMALS.
        PACK PI_SOURCE TO PI_TARGET.
        PI_TARGET = PI_TARGET * ( 10 ** ( L_DECIMALS_TARGET ) ).
      ENDIF.

*     calculation type of .. *(10**10) is binary float
*     binary float calculations might always have an inherent
*     small loss of precision.
      IF L_DECIMALS_TARGET > 7.
        DATA L_DIFF TYPE DECFLOAT16.
        DATA L_SOURCE TYPE DECFLOAT16.
        PACK PI_SOURCE TO L_SOURCE.
        L_SOURCE = L_SOURCE / ( 10 ** ( L_DECIMALS ) ).
        L_DIFF = L_SOURCE - PI_TARGET.
        PI_TARGET = PI_TARGET + L_DIFF.
      ENDIF.

      IF NOT L_MINUS IS INITIAL.
        PI_TARGET = PI_TARGET * -1.
      ENDIF.
    WHEN 'F'.
      CALL FUNCTION 'CHAR_FLTP_CONVERSION'
        EXPORTING
          STRING = PI_SOURCE
        IMPORTING
          FLSTR  = PI_TARGET
        EXCEPTIONS
          OTHERS = C_RC4.
      IF SY-SUBRC <> 0.
        PC_SUBRC = 4.
      ENDIF.
    WHEN OTHERS.
      PI_TARGET = PI_SOURCE.
  ENDCASE.

ENDFORM.                               " INPUT_DATA2SAP_DATA

*---------------------------------------------------------------------*
*       FORM create_spreadsheet                                       *
*---------------------------------------------------------------------*
FORM CREATE_SPREADSHEET
      USING
            VALUE(PI_APPLICATION) TYPE CHAR80
      CHANGING
            VALUE(PC_FILENAME) TYPE RLGRAP-FILENAME
            VALUE(PC_OREF_CONTAINER) TYPE REF TO CL_GUI_CUSTOM_CONTAINER
            VALUE(PC_IREF_CONTROL) TYPE REF TO I_OI_CONTAINER_CONTROL
            VALUE(PC_IREF_ERROR) TYPE REF TO I_OI_ERROR
            VALUE(PC_IREF_DOCUMENT) TYPE REF TO I_OI_DOCUMENT_PROXY
            VALUE(PC_IREF_SPREADSHEET).

  DATA L_ITEM_URL(256) TYPE C.
  DATA L_RETCODE TYPE SOI_RET_STRING.
  DATA: L_HAS TYPE I.

* open an existing spreadsheet
  PERFORM GET_SPREADSHEET_INTERFACE USING PI_APPLICATION
                                    CHANGING
                                      PC_FILENAME
                                      PC_OREF_CONTAINER PC_IREF_CONTROL
                                      PC_IREF_ERROR     PC_IREF_DOCUMENT
                                      PC_IREF_SPREADSHEET.
  CHECK NOT PC_IREF_DOCUMENT IS INITIAL.
* a spreadsheet must be created
  IF PC_IREF_SPREADSHEET IS INITIAL.
    CALL METHOD PC_IREF_DOCUMENT->CREATE_DOCUMENT
      EXPORTING
        OPEN_INPLACE = 'X'
      IMPORTING
        RETCODE      = L_RETCODE.

    CALL METHOD PC_IREF_DOCUMENT->HAS_SPREADSHEET_INTERFACE
      IMPORTING
        IS_AVAILABLE = L_HAS.

    IF NOT L_HAS IS INITIAL.
      CALL METHOD PC_IREF_DOCUMENT->GET_SPREADSHEET_INTERFACE
        IMPORTING
          SHEET_INTERFACE = PC_IREF_SPREADSHEET.
    ENDIF.
  ENDIF.

ENDFORM.                           "create_spreadsheet

*&---------------------------------------------------------------------*
*&      Form  GET_SPREADSHEET_INTERFACE
*&---------------------------------------------------------------------*
FORM GET_SPREADSHEET_INTERFACE
      USING
           VALUE(PI_APPLICATION) TYPE CHAR80
      CHANGING
            VALUE(PC_FILENAME) TYPE RLGRAP-FILENAME
            VALUE(PC_OREF_CONTAINER) TYPE REF TO CL_GUI_CUSTOM_CONTAINER
            VALUE(PC_IREF_CONTROL) TYPE REF TO I_OI_CONTAINER_CONTROL
            VALUE(PC_IREF_ERROR) TYPE REF TO I_OI_ERROR
            VALUE(PC_IREF_DOCUMENT) TYPE REF TO I_OI_DOCUMENT_PROXY
            VALUE(PC_IREF_SPREADSHEET).
  DATA L_ITEM_URL(256) TYPE C.
  DATA L_RETCODE TYPE SOI_RET_STRING.
  DATA L_HAS TYPE I.

* don't do anything in batch, because there is no GUI...
  CHECK SY-BATCH IS INITIAL.

  L_ITEM_URL = PC_FILENAME.
  SET LOCALE LANGUAGE SY-LANGU.
  TRANSLATE PC_FILENAME TO UPPER CASE.
  SET LOCALE LANGUAGE SPACE.

  IF PC_FILENAME(7) <> 'HTTP://' AND PC_FILENAME(7) <> 'FILE://'.
    CONCATENATE 'FILE://' L_ITEM_URL INTO L_ITEM_URL.
  ENDIF.
  PC_FILENAME = L_ITEM_URL.

  CALL METHOD C_OI_CONTAINER_CONTROL_CREATOR=>GET_CONTAINER_CONTROL
    IMPORTING
      CONTROL = PC_IREF_CONTROL
      ERROR   = PC_IREF_ERROR.
  CREATE OBJECT PC_OREF_CONTAINER
    EXPORTING
      CONTAINER_NAME = 'TRUX_CONTAINER'.

*  call method pc_oref_container->set_visible exporting visible = space.

  CALL METHOD PC_IREF_CONTROL->INIT_CONTROL
    EXPORTING
      R3_APPLICATION_NAME      =
                                 'R/3 TR'        "#EC NOTEXT
      INPLACE_ENABLED          = 'X'
      INPLACE_SCROLL_DOCUMENTS = 'X'
      PARENT                   = PC_OREF_CONTAINER
*     register_on_close_event  = 'X'
*     register_on_custom_event = 'X'
*     no_flush                 = 'X'
    IMPORTING
      ERROR                    = PC_IREF_ERROR
    EXCEPTIONS
      JAVABEANNOTSUPPORTED     = 1
      OTHERS                   = 2.
  IF SY-SUBRC NE 0.
*   destroy the central Office Integration instance
    CALL METHOD PC_IREF_CONTROL->DESTROY_CONTROL
      IMPORTING
        RETCODE = L_RETCODE.
*   release the memory
    FREE PC_IREF_CONTROL.
  ENDIF.

  CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE
    EXPORTING
      TYPE = 'E'.

  CALL METHOD PC_IREF_CONTROL->GET_DOCUMENT_PROXY
    EXPORTING
      DOCUMENT_TYPE  = PI_APPLICATION
    IMPORTING
      DOCUMENT_PROXY = PC_IREF_DOCUMENT
      ERROR          = PC_IREF_ERROR.

  CALL METHOD PC_IREF_DOCUMENT->OPEN_DOCUMENT
    EXPORTING
      OPEN_INPLACE = 'X'
      DOCUMENT_URL = L_ITEM_URL                     "open_readonly = 'X'
    IMPORTING
      RETCODE      = L_RETCODE.

  CALL METHOD PC_IREF_DOCUMENT->HAS_SPREADSHEET_INTERFACE
    IMPORTING
      IS_AVAILABLE = L_HAS.

  IF NOT L_HAS IS INITIAL.
    CALL METHOD PC_IREF_DOCUMENT->GET_SPREADSHEET_INTERFACE
      IMPORTING
        SHEET_INTERFACE = PC_IREF_SPREADSHEET.
  ENDIF.

ENDFORM.                               " GET_SPREADSHEET_INTERFACE

*&---------------------------------------------------------------------*
*&      Form  PARSE_TABLE_LINE
*&---------------------------------------------------------------------*
FORM PARSE_TABLE_LINE USING    VALUE(PI_TABLE) TYPE SOI_GENERIC_TABLE
                               VALUE(PI_TABIX) TYPE SYTABIX
                      CHANGING VALUE(PC_STRUC_DATA) TYPE ANY.
  DATA: L_TABLE LIKE LINE OF PI_TABLE.
  DATA: L_FIELD_TYPE.
  DATA: L_COMPONENT TYPE I.

  FIELD-SYMBOLS: <FS_TABLE>, <FS_STRUC_DATA>.

  CLEAR PC_STRUC_DATA.

  LOOP AT PI_TABLE INTO L_TABLE.
    CHECK NOT L_TABLE-VALUE IS INITIAL.
    L_COMPONENT = L_TABLE-COLUMN.
    ASSIGN COMPONENT L_COMPONENT OF STRUCTURE PC_STRUC_DATA TO
                     <FS_STRUC_DATA>.
    CHECK SY-SUBRC = 0.
    PERFORM INPUT_DATA2SAP_DATA USING L_TABLE-VALUE
                                CHANGING <FS_STRUC_DATA> SY-SUBRC.
    IF SY-SUBRC <> C_RC0.
      CLEAR PC_STRUC_DATA.
      DESCRIBE FIELD <FS_STRUC_DATA> TYPE L_FIELD_TYPE.
    ENDIF.
    CASE SY-SUBRC.
      WHEN 0.
      WHEN 4.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                RAISING CONVERSION_FAILED.

      WHEN 8.
        MESSAGE ID C_UX TYPE C_ERROR NUMBER C_899
                WITH L_FIELD_TYPE  L_TABLE-VALUE
                     SY-INDEX PI_TABIX
                RAISING CONVERSION_FAILED.
    ENDCASE.
  ENDLOOP.
ENDFORM.                               " PARSE_TABLE_LINE

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_TABLE
*&---------------------------------------------------------------------*
FORM BUILD_FIELD_TABLE TABLES PC_TAB_FIELDS STRUCTURE RFC_FIELDS
                       USING  VALUE(PI_OREF_STRUCTURE)
                                       TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA L_TABNAME TYPE  DD02L-TABNAME.
  DATA L_COMPONENT TYPE ABAP_COMPDESCR.
  DATA L_FIELD TYPE RFC_FIELDS.
  DATA L_OFFSET TYPE I.

  SEARCH PI_OREF_STRUCTURE->ABSOLUTE_NAME FOR '\TYPE='.
  IF SY-SUBRC = 0.
    SY-FDPOS = SY-FDPOS + STRLEN( '\TYPE=' ) .
    L_TABNAME = PI_OREF_STRUCTURE->ABSOLUTE_NAME+SY-FDPOS.
  ELSE.
    L_TABNAME = 'UNKNOWN'.
  ENDIF.

  CLEAR L_OFFSET.
  L_FIELD-TABNAME = L_TABNAME.
  LOOP AT PI_OREF_STRUCTURE->COMPONENTS INTO L_COMPONENT.
    MOVE-CORRESPONDING L_COMPONENT TO L_FIELD.
    L_FIELD-FIELDNAME = L_COMPONENT-NAME.
    L_FIELD-EXID = L_COMPONENT-TYPE_KIND.
*   note 932775 2006 CS Unicode format
    L_FIELD-INTLENGTH = L_COMPONENT-LENGTH
                        / CL_ABAP_CHAR_UTILITIES=>CHARSIZE.
    L_FIELD-POSITION = SY-TABIX.
    L_FIELD-OFFSET = L_OFFSET.
    L_OFFSET = L_OFFSET + L_FIELD-INTLENGTH.
    APPEND L_FIELD TO PC_TAB_FIELDS.
  ENDLOOP.

ENDFORM.                               " BUILD_FIELD_TABLE

*&---------------------------------------------------------------------*
*       FORM correct_decimals_for_current                             *
*---------------------------------------------------------------------*
FORM CORRECT_DECIMALS_FOR_CURRENT TABLES I_TAB_CONVERTED_DATA.
  DATA L_OREF_DESCR_SOURCE TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA L_TAB_DFIES TYPE TABLE OF DFIES.
  DATA L_DFIES LIKE LINE OF L_TAB_DFIES.
  DATA L_DECIMALS_TARGET TYPE CURRDEC.
  DATA L_DECIMALS_DDIC TYPE CURRDEC.
  DATA L_DECIMALS_INT TYPE I.
* data l_currency type trca_company-currency.
  DATA L_TABNAME TYPE  DD02L-TABNAME.

  DATA: COMPANYCODE_DETAIL LIKE T001_BF.
  DATA: T_APPENDIX         LIKE T001Z_BF OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS: <L_AMOUNT>, <L_WAERS>, <L_BUKRS>.

  DESCRIBE TABLE I_TAB_CONVERTED_DATA LINES SY-TABIX.
  IF SY-TABIX <> 0.
* get information from DDIC
    L_OREF_DESCR_SOURCE ?=
        CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( I_TAB_CONVERTED_DATA ).

    SEARCH L_OREF_DESCR_SOURCE->ABSOLUTE_NAME FOR '\TYPE='.
    IF SY-SUBRC = 0.
      SY-FDPOS = SY-FDPOS + STRLEN( '\TYPE=' ) .
      L_TABNAME = L_OREF_DESCR_SOURCE->ABSOLUTE_NAME+SY-FDPOS.
      CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
        EXPORTING
          I_STRUCTURE_TABNAME = L_TABNAME
        TABLES
          IT_DFIES            = L_TAB_DFIES
        EXCEPTIONS
          OTHERS              = 4.
    ENDIF.
    LOOP AT L_TAB_DFIES INTO L_DFIES WHERE DATATYPE = 'CURR'.
      L_DECIMALS_DDIC = L_DFIES-DECIMALS.
      LOOP AT I_TAB_CONVERTED_DATA.
*       get the Currency
        ASSIGN COMPONENT L_DFIES-REFFIELD
               OF STRUCTURE I_TAB_CONVERTED_DATA
               TO <L_WAERS>.
        IF SY-SUBRC <> 0.     "Hauswährung!!!
          ASSIGN COMPONENT 'BUKRS'
                 OF STRUCTURE I_TAB_CONVERTED_DATA
                 TO <L_BUKRS>.
          IF SY-SUBRC <> 0.
            CONTINUE.
          ELSE.
*            call function 'TRCA_COMPANYCODE_GETDETAIL'
*                 exporting
*                      companycode = <l_bukrs>
*                 importing
*                      currency    = l_currency
*                 exceptions
*                      not_found   = 1
*                      others      = 2.
            CALL FUNCTION 'FI_COMPANYCODE_GETDETAIL'
              EXPORTING
                BUKRS_INT       = <L_BUKRS>
                AUTHORITY_CHECK = SPACE
              IMPORTING
                T001_INT        = COMPANYCODE_DETAIL
              TABLES
                T001Z_INT       = T_APPENDIX
              EXCEPTIONS
                BUKRS_NOT_FOUND = 1
                OTHERS          = 2.
            IF SY-SUBRC = 0.
              ASSIGN COMPONENT 'WAERS'
              OF STRUCTURE COMPANYCODE_DETAIL TO <L_WAERS>.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
*       get the decimals
        PERFORM GET_CURRENCY_DECIMALS USING    <L_WAERS>
                                      CHANGING L_DECIMALS_TARGET.
*       correction of ddic:
        L_DECIMALS_INT = L_DECIMALS_DDIC - L_DECIMALS_TARGET.
        IF L_DECIMALS_INT <> 0.
*         get the amount
          ASSIGN COMPONENT L_DFIES-FIELDNAME
                 OF STRUCTURE I_TAB_CONVERTED_DATA
                 TO <L_AMOUNT>.
          IF SY-SUBRC = 0.
*           calculate currency dependend
            <L_AMOUNT> = <L_AMOUNT>  / ( 10 ** L_DECIMALS_INT ).
            MODIFY I_TAB_CONVERTED_DATA.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "correct_decimals_for_current
