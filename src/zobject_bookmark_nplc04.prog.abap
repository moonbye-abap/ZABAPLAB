*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC04
*&---------------------------------------------------------------------*

CLASS lcl_event IMPLEMENTATION.
  METHOD view_tree1_context_menu.
* In this case the standard menu is cleared.
    CALL METHOD menu->clear.
* The next line defines one line of the context menu.
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'APPEND'
        text  = TEXT-001.        "Delete Subtree
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'DELETE'
        text  = TEXT-002.        "Delete Subtree
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( ).
    mv_gubn = i_gubn.
  ENDMETHOD.
*======= Hot Spot Click Implementation
  METHOD handle_hotspot_click.
    CASE mv_gubn.
      WHEN 'SCR_0100'.
*     perform handle_hotspot_click1_1   using e_row_id
*                                             e_column_id.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_user_command.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_user_command1        USING e_ucomm.
    ENDCASE.
  ENDMETHOD.
*======= Toolbar Implementation
  METHOD handle_toolbar.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_toolbar1        USING e_object
*                                            e_interactive.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_data_changed_finished.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_data_changed_finished  USING e_modified
*                                                     et_good_cells.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_data_changed.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_data_changed USING er_data_changed
*                                          e_onf4
*                                          e_onf4_before
*                                          e_onf4_after
*                                          e_ucomm.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1

*======= Toolbar Implementation
  METHOD handle_onf4.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_onf4_1        USING e_fieldname
*                                            e_fieldvalue
*                                            es_row_no
*                                            er_event_data
*                                            et_bad_cells
*                                            e_display.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_button_click.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_button_click1        USING es_col_id
*                                                  es_row_no.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1


ENDCLASS.
