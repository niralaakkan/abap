*FUNCTION MODULE EXAMPLE 2

*FUNCTION zsi_bill.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(TOT_REC) TYPE  CHAR30
*"  TABLES
*"      IS_VBELN STRUCTURE  ZSI_VBELN
*"      LT_LIKP STRUCTURE  ZSI_LIKP
*"      LT_LIPS STRUCTURE  ZSI_LIPS
*"      LT_VBRP STRUCTURE  ZSI_VBRP
*"      LT_VBRK STRUCTURE  ZSI_VBRK
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------

******Extracting data from likp
******Testing GIT function
SELECT
     vbeln
     erdat
     FROM likp INTO TABLE lt_likp
     WHERE vbeln IN is_vbeln.
  IF sy-subrc <> 0.
  RAISE no_data.
  ENDIF.

 IF lt_likp[] IS NOT INITIAL.         "Checks if likp has records or not
******Extracting data from lips
SELECT
      vbeln
      posnr
      FROM lips INTO TABLE lt_lips
      FOR ALL ENTRIES IN lt_likp
      WHERE vbeln = lt_likp-vbeln.    "Checks vbeln of likp and lips are same and extracts
 IF sy-subrc = 0.
  SORT lt_lips BY vbeln.
  ENDIF.
ENDIF.

IF lt_lips[] IS NOT INITIAL.         "Checks if lips has records or not
******Extracting data from vbrp
SELECT
      vbeln
      posnr
      vgbel
      vgpos
      FROM vbrp INTO TABLE lt_vbrp
      FOR ALL ENTRIES IN lt_lips
      WHERE vgbel = lt_lips-vbeln
      AND   vgpos = lt_lips-posnr. "Checks vbeln of lips is equal to vgbel and posnr of lips is equal to vgpos and extracts
IF sy-subrc = 0.
  SORT lt_vbrp BY vbeln posnr.
  ENDIF.
ENDIF.

IF lt_vbrp[] IS NOT INITIAL.         "Checks if vbrp has records or not
******Extracting data from vbrk
SELECT
      vbeln
      fktyp
      fkdat
      FROM vbrk INTO TABLE lt_vbrk
      FOR ALL ENTRIES IN lt_vbrp
      WHERE vbeln = lt_vbrp-vbeln.    "Checks vbeln of vbrk and vbrp are same and extracts
IF sy-subrc <> 0.
  RAISE no_data.
  ENDIF.
ENDIF.

CLEAR lv_num.
DESCRIBE TABLE lt_vbrk LINES lv_num.
CONCATENATE c_txt lv_num INTO tot_rec SEPARATED BY ' '.
ENDFUNCTION.

CALL FUNCTION MODULE
*&---------------------------------------------------------------------*
*& Report ZSI_CALL_FMBILL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsi_call_fmbill.
TABLES likp.
TYPES: BEGIN OF ty_final,
       vbeln TYPE vbeln_vl,
       posnr TYPE posnr_vl,
       vbeln1 TYPE vbeln_vf,
       posnr1 TYPE posnr_vf,
       erdat TYPE erdat,
       vgbel TYPE vgbel,
       vgpos TYPE vgpos,
       fktyp TYPE fktyp,
       fkdat TYPE fkdat,
       END OF ty_final.

DATA: lt_likp TYPE STANDARD TABLE OF zsi_likp INITIAL SIZE 0,
      lt_lips TYPE STANDARD TABLE OF zsi_lips INITIAL SIZE 0,
      lt_vbrp TYPE STANDARD TABLE OF zsi_vbrp INITIAL SIZE 0,
      lt_vbrk TYPE STANDARD TABLE OF zsi_vbrk INITIAL SIZE 0,
      lt_final TYPE STANDARD TABLE OF ty_final INITIAL SIZE 0,
      wa_likp TYPE zsi_likp,
      wa_lips TYPE zsi_lips,
      wa_vbrp TYPE zsi_vbrp,
      wa_vbrk TYPE zsi_vbrk,
      wa_final TYPE ty_final,
      lv_tot  TYPE char30.

SELECT-OPTIONS: s_vbeln FOR likp-vbeln.

CALL FUNCTION 'ZSI_BILL'
IMPORTING
  tot_rec          = lv_tot
  TABLES
    is_vbeln       = s_vbeln[]
    lt_likp        = lt_likp[]
    lt_lips        = lt_lips[]
    lt_vbrp        = lt_vbrp[]
    lt_vbrk        = lt_vbrk[]
 EXCEPTIONS
     no_data        = 1
     OTHERS         = 2.
IF sy-subrc <> 0.
MESSAGE 'No data found' TYPE 'E'.
ENDIF.

IF lt_vbrk[] IS NOT INITIAL AND lt_vbrp[] IS NOT INITIAL AND lt_likp[] IS NOT INITIAL AND lt_lips[] IS NOT INITIAL.
LOOP AT lt_vbrp INTO wa_vbrp.
CLEAR wa_lips.
READ TABLE lt_lips INTO wa_lips WITH KEY vbeln = wa_vbrp-vgbel
                                         posnr = wa_vbrp-vgpos BINARY SEARCH.
IF sy-subrc = 0.
CLEAR wa_vbrk.
READ TABLE lt_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrp-vbeln BINARY SEARCH.
IF sy-subrc = 0.
CLEAR wa_likp.
READ TABLE lt_likp INTO wa_likp WITH KEY vbeln = wa_lips-vbeln BINARY SEARCH.
IF sy-subrc = 0.
  wa_final-vbeln = wa_likp-vbeln.
  wa_final-posnr = wa_lips-posnr.
  wa_final-vbeln1 = wa_vbrp-vbeln.
  wa_final-posnr1 = wa_vbrp-posnr.
  wa_final-erdat = wa_likp-erdat.
  wa_final-vgbel = wa_vbrp-vgbel.
  wa_final-vgpos = wa_vbrp-vgpos.
  wa_final-fktyp = wa_vbrk-fktyp.
  wa_final-fkdat = wa_vbrk-fkdat.
APPEND wa_final TO lt_final.
ENDIF.
ENDIF.
ENDIF.
CLEAR wa_final.
ENDLOOP.
LOOP AT lt_final INTO wa_final.
WRITE:/ wa_final-vbeln,20 wa_final-erdat, 40 wa_final-posnr, 60 wa_final-vbeln1, 80 wa_final-posnr1, 100 wa_final-fktyp, 120 wa_final-fkdat.
ENDLOOP.
ENDIF.