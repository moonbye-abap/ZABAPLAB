*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC04
*&---------------------------------------------------------------------*

CLASS lcl_event IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( ).
    mv_gubn = i_gubn.
  ENDMETHOD.

  METHOD handle_node_double_click.
    CASE mv_gubn.
      WHEN 'SCR0100_TREE1'.
        CALL METHOD lcl_scr0100=>tree1_event_node_double_click( node_key = node_key ).
    ENDCASE.

  ENDMETHOD.
  "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_user_command.
    CASE mv_gubn.
      WHEN 'SCR2000_GRID3'.
        CALL METHOD lcl_scr2000=>grid3_event_user_command( i_ucomm = e_ucomm ).
      WHEN 'SCR0100_GRID2'.
        CALL METHOD lcl_scr0100=>grid2_event_user_command( i_ucomm = e_ucomm ).
    ENDCASE.
  ENDMETHOD.
*======= Toolbar Implementation
  METHOD handle_toolbar.
    CASE mv_gubn.
      WHEN 'SCR0100_GRID2'.
        CALL METHOD lcl_scr0100=>grid2_event_toolbar( p_object = e_object p_interactive = e_interactive ).
      WHEN 'SCR2000_GRID3'.
        CALL METHOD lcl_scr2000=>grid3_event_toolbar( p_object = e_object p_interactive = e_interactive ).
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1

*======= Toolbar Implementation
  METHOD handle_data_changed.
    CASE mv_gubn.
      WHEN 'SCR2000_GRID3'.
        CALL METHOD lcl_scr2000=>grid3_event_data_changed
          EXPORTING
            er_data_changed = er_data_changed
            e_onf4          = e_onf4
            e_onf4_before   = e_onf4_before
            e_onf4_after    = e_onf4_after
            e_ucomm         = e_ucomm.
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
      WHEN 'SCR2000_GRID3'.
        CALL METHOD lcl_scr2000=>grid3_event_onf4
          EXPORTING
            e_fieldname   = e_fieldname
            e_fieldvalue  = e_fieldvalue
            es_row_no     = es_row_no
            er_event_data = er_event_data
            et_bad_cells  = et_bad_cells
            e_display     = e_display.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1



ENDCLASS.
