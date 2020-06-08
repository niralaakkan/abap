FUNCTION ZSI_EXAMPLE1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IP_WERKS) TYPE  WERKS_D
*"  TABLES
*"      IS_MATNR STRUCTURE  ZSI_MATNR
*"      LT_MAT STRUCTURE  ZSI_MARA
*"      LT_MAK STRUCTURE  ZSI_MAKT
*"      LT_MAR STRUCTURE  ZSI_MARC
*"----------------------------------------------------------------------

**SELECT
**      matnr
**      ernam
**      laeda
**FROM mara INTO TABLE lt_mat
**WHERE matnr IN is_matnr.
**  IF sy-subrc = 0.
**  SORT lt_mat BY matnr.
**  ENDIF.
**
**
**SELECT
**      matnr
**      maktx
**      maktg
**FROM makt INTO TABLE lt_mak
**WHERE matnr IN is_matnr.
**
**  SELECT
**      matnr
**      werks
**      xchar
**FROM marc INTO TABLE lt_mar
**WHERE  werks = ip_werks.
**
**
**
**  IF sy-subrc = 0.
**  SORT lt_mar BY werks.
**  ENDIF.


ENDFUNCTION.

*FUNCTION MODULE EXAMPE 2.
FUNCTION zsi_example2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IP_WERKS) TYPE  WERKS_D
*"  EXPORTING
*"     VALUE(TOT_REC) TYPE  CHAR30
*"  TABLES
*"      IS_MATNR STRUCTURE  ZSI_MATNR
*"      LT_FIN STRUCTURE  ZSI_FINAL
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------

DATA:  lt_mara  TYPE STANDARD TABLE OF zsi_mara INITIAL SIZE 0,
       lt_makt  TYPE STANDARD TABLE OF zsi_makt INITIAL SIZE 0,
       lt_marc  TYPE STANDARD TABLE OF zsi_marc INITIAL SIZE 0,
       wa_mara TYPE zsi_mara,
       wa_makt TYPE zsi_makt,
       wa_marc TYPE zsi_marc,
       wa_fin TYPE zsi_final.

  SELECT
      matnr
      werks
      xchar
      FROM marc INTO TABLE lt_marc
      WHERE matnr IN is_matnr AND
            werks = ip_werks.
IF lt_marc[] IS NOT INITIAL.
SELECT
      matnr
      ernam
      laeda
      FROM mara INTO TABLE lt_mara
      FOR ALL ENTRIES IN lt_marc
      WHERE matnr = lt_marc-matnr.
IF sy-subrc = 0.
    SORT lt_mara BY matnr.
ENDIF.
ENDIF.
IF lt_mara[] IS NOT INITIAL.
SELECT
      matnr
      maktx
      maktg
      FROM makt INTO TABLE lt_makt
      FOR ALL ENTRIES IN lt_mara
      WHERE matnr = lt_mara-matnr
        AND spras = sy-langu.
IF sy-subrc = 0.
    SORT lt_makt BY matnr.
ENDIF.
ENDIF.

IF lt_marc[] IS NOT INITIAL AND lt_makt[] IS NOT INITIAL AND lt_mara[] IS NOT INITIAL.
LOOP AT lt_marc INTO wa_marc.
CLEAR wa_mara.
READ TABLE lt_mara INTO wa_mara WITH KEY matnr = wa_marc-matnr BINARY SEARCH.
IF sy-subrc = 0.
CLEAR wa_makt.
READ TABLE lt_makt INTO wa_makt WITH KEY matnr = wa_mara-matnr BINARY SEARCH.
IF sy-subrc = 0.
wa_fin-matnr = wa_mara-matnr.
wa_fin-ernam = wa_mara-ernam.
wa_fin-laeda = wa_mara-laeda.
wa_fin-maktx = wa_makt-maktx.
wa_fin-maktg = wa_makt-maktg.
wa_fin-werks = wa_marc-werks.
wa_fin-xchar = wa_marc-xchar.
APPEND wa_fin TO lt_fin.
CLEAR wa_fin.
ENDIF.
ENDIF.
ENDLOOP.
**** Total no oif records
CLEAR lv_num.
DESCRIBE TABLE lt_fin LINES lv_num.
*CONCATENATE c_txt lv_num INTO tot_rec SEPARATED BY space.
CONCATENATE c_txt lv_num INTO tot_rec SEPARATED BY ':'.
else.
  raise no_data.
ENDIF.

ENDFUNCTION.
*CALL FUNCTION MODULE
*&---------------------------------------------------------------------*
*& Report ZSI_CALL_FM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsi_call_fm.
*** Declaration
TABLES mara.

DATA: lt_mara TYPE STANDARD TABLE OF zsi_mara INITIAL SIZE 0,
      lt_makt TYPE STANDARD TABLE OF zsi_makt INITIAL SIZE 0,
      lt_marc TYPE STANDARD TABLE OF zsi_marc INITIAL SIZE 0,
      lt_final TYPE STANDARD TABLE OF zsi_final INITIAL SIZE 0,
      wa_mara TYPE zsi_mara,
      wa_makt TYPE zsi_makt,
      wa_marc TYPE zsi_marc,
      wa_final TYPE zsi_final,
      lv_rec  TYPE char30.
***8 Selection screen
SELECT-OPTIONS: s_matnr FOR mara-matnr.
PARAMETERS: p_werks TYPE marc-werks.


**
******Display the tables
**WRITE:/ 'TABLE MARA'.
**ULINE.
**LOOP AT lt_mara INTO wa_mara.
**  WRITE:/ wa_mara-matnr, 20 wa_mara-ernam,40 wa_mara-laeda.
**ENDLOOP.
**ULINE.
**SKIP 2.
**
**WRITE:/ 'TABLE MAKT'.
**ULINE.
**LOOP AT lt_makt INTO wa_makt.
**  WRITE:/ wa_makt-matnr, 20 wa_makt-maktx,40 wa_makt-maktg.
**ENDLOOP.
**ULINE.
**SKIP 2.
**
**WRITE:/ 'TABLE MARC'.
**ULINE.
**LOOP AT lt_marc INTO wa_marc.
**  WRITE:/ wa_marc-matnr, 20 wa_marc-werks,40 wa_marc-xchar.
**ENDLOOP.
**ULINE.

CALL FUNCTION 'ZSI_EXAMPLE2'
  EXPORTING
    ip_werks       = p_werks
    IMPORTING
      tot_rec      = lv_rec
  TABLES
    is_matnr       = s_matnr[]
    lt_fin         = lt_final[]
 EXCEPTIONS
   no_data        = 1
   OTHERS         = 2.

CALL FUNCTION 'ZSI_EXAMPLE1'
  EXPORTING
    ip_werks       = p_werks
  TABLES
    is_matnr       = s_matnr[]
    lt_mat         = lt_mara[]
    lt_mak         = lt_makt[]
    lt_mar         = lt_marc[].
IF sy-subrc = 0.
ULINE.
LOOP AT lt_final INTO wa_final.
  WRITE:/ wa_final-matnr, 20 wa_final-ernam,40 wa_final-laeda ,60 wa_final-maktx,80 wa_final-maktg,100 wa_final-werks,120 wa_final-xchar.
ENDLOOP.

ELSEIF sy-subrc = 1.
 MESSAGE 'No data found' TYPE 'E'.
ENDIF.




****IF lt_mara[] IS NOT INITIAL AND lt_makt[] IS NOT INITIAL AND lt_marc[] IS NOT INITIAL.
****LOOP AT lt_makt INTO wa_makt.
****CLEAR wa_marc.
****READ TABLE lt_marc INTO wa_marc WITH KEY werks = wa_marc-werks BINARY SEARCH.
****IF sy-subrc = 0.
****CLEAR wa_mara.
****READ TABLE lt_mara INTO wa_mara WITH KEY matnr = wa_mara-matnr BINARY SEARCH.
****IS sy-subrc = 0.
