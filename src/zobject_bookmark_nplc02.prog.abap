*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC02
*&---------------------------------------------------------------------*

CLASS lcl_controller IMPLEMENTATION.


  METHOD scr100_grid2_toolbar.

    DATA : ls_toolbar TYPE stb_button.
    ls_toolbar-butn_type = 3.  "Seperate line
    APPEND ls_toolbar TO p_object->mt_toolbar.
    CLEAR ls_toolbar. "Important syntax
    ls_toolbar-function = 'INS_LINE'.
    ls_toolbar-icon      = icon_insert_row.
    ls_toolbar-text         = 'INSERT'.
    APPEND ls_toolbar TO p_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = 'DEL_LINE'.
    ls_toolbar-text         = 'DELETE'.
    APPEND ls_toolbar TO p_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = 'SAVE'.
    ls_toolbar-text         = 'SAVE'.
    APPEND ls_toolbar TO p_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 1. "Multi line
    ls_toolbar-function = 'INS_MULTI'.
    ls_toolbar-text         = 'MULTI BTN'.
    APPEND ls_toolbar TO p_object->mt_toolbar.

  ENDMETHOD.
  METHOD scr100_tree1_fcat_build.
    "-----------------------------------------
    " Screen(100)의 Field Catalog를 생성한다.
    "-----------------------------------------
    DATA : ls_fcat TYPE lvc_s_fcat.

    FREE : ct_fcat1.

    ct_fcat1 = lcl_module=>get_fcat( it_data = it_list1 ).
*######################################################################*
*   FIELD Attribute Setting
*######################################################################*
    LOOP AT ct_fcat1 INTO ls_fcat.
      CASE ls_fcat-fieldname.
        WHEN 'ZORDER'.
          ls_fcat-col_pos  = 5.
          ls_fcat-outputlen = 8.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = '순번' CHANGING cs_fcat = ls_fcat ).
          ls_fcat-key      = gc_x.

        WHEN 'DESCRIPTION'.
          ls_fcat-col_pos  = 10.
          ls_fcat-outputlen = 50.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'Description' CHANGING cs_fcat = ls_fcat ).
          ls_fcat-key      = gc_x.

*        WHEN 'WERKS'.
*          ls_fcat-col_pos  = 20.
*          ls_fcat-key      = gc_x.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'PLANT' CHANGING cs_fcat = ls_fcat ).
*
*        WHEN 'FIELD1'.
*          ls_fcat-col_pos  = 30.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'INPUT1' CHANGING cs_fcat = ls_fcat ).
*
*        WHEN 'FIELD2'.
*          ls_fcat-col_pos  = 40.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'INPUT2' CHANGING cs_fcat = ls_fcat ).
*
*        WHEN 'SPRAS'.
*          ls_fcat-col_pos  = 45.
*          ls_fcat-outputlen = 5.
*          ls_fcat-f4availabl  = gc_x.
*          ls_fcat-convexit    = 'ISOLA'.
*          CLEAR : ls_fcat-domname, ls_fcat-ref_table, ls_fcat-ref_field.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'LANG' CHANGING cs_fcat = ls_fcat ).
*
*        WHEN 'SPRAS_NM'.
*          ls_fcat-col_pos  = 46.
*          ls_fcat-outputlen = 10.
**        ls_fcat-edit =
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'LANG NAME' CHANGING cs_fcat = ls_fcat ).
*
*        WHEN 'DATA'.
*          ls_fcat-col_pos  = 50.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'DATA' CHANGING cs_fcat = ls_fcat ).
*          ls_fcat-tech     = gc_x.
*
*        WHEN 'FILESIZE'.
*          ls_fcat-col_pos  = 60.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'DATA' CHANGING cs_fcat = ls_fcat ).
*          ls_fcat-no_out     = gc_x.
*
*        WHEN 'WAERK'.
*          ls_fcat-col_pos  = 70.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'CURRENCY' CHANGING cs_fcat = ls_fcat ).
*
*        WHEN 'AMOUNT'.
*          ls_fcat-col_pos  = 80.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'AMOUNT' CHANGING cs_fcat = ls_fcat ).


*        WHEN 'VMODE'.
*          ls_fcat-no_out   = gc_x.
*          ls_fcat-col_pos  = 0.
*          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'MODE' CHANGING cs_fcat = ls_fcat ).

**######################################################################*
        WHEN OTHERS.
          ls_fcat-tech   = gc_x.

**######################################################################*
      ENDCASE.
*
      MODIFY ct_fcat1 FROM ls_fcat.
    ENDLOOP.
    ls_fcat-edit = gc_x.
    MODIFY ct_fcat1 FROM ls_fcat TRANSPORTING edit WHERE key = ' ' AND no_out = ' '.
    ls_fcat-edit = ' '.
    MODIFY ct_fcat1 FROM ls_fcat TRANSPORTING edit WHERE fieldname CP '*_NM'.
  ENDMETHOD.

ENDCLASS.
