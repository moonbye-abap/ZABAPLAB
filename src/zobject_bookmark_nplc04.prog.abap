*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC04
*&---------------------------------------------------------------------*

CLASS lcl_event IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( ).
    mv_gubn = i_gubn.
  ENDMETHOD.
  METHOD handle_tree_on_drop.
  ENDMETHOD.
*======= Hot Spot Click Implementation
  METHOD handle_tree_on_drag.
*    DATA: dataobj       TYPE REF TO lcl_dragdropobj,
*          l_node_key    TYPE lvc_nkey,
*          l_sflight     TYPE sflight,
*          l_node_text   TYPE lvc_value,
*          l_node_layout TYPE lvc_s_layn.
** § 6.Use your data object to transfer data between the events
** § 6a.ON_DRAG
** create and fill dataobject for event ON_DROP.
*    CREATE OBJECT dataobj.
*
*    LOOP AT node_key_table INTO l_node_key.
*      CALL METHOD sender->get_outtab_line
*        EXPORTING
*          i_node_key     = l_node_key
*        IMPORTING
*          e_outtab_line  = l_sflight
*          e_node_text    = l_node_text
*          es_node_layout = l_node_layout.
*
*      IF l_node_layout-isfolder NE 'X'.
*        dataobj->wa_node_info-cp_node_key = l_node_key.
*        dataobj->wa_node_info-cps_sflight = l_sflight.
*        dataobj->wa_node_info-cp_node_text = l_node_text.
*
*        APPEND dataobj->wa_node_info TO dataobj->cp_t_node_info.
*      ENDIF.
*    ENDLOOP.
*
*    drag_drop_object->object = dataobj.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    CASE mv_gubn.
      WHEN 'SCR_0100'.
*     perform handle_hotspot_click1_1   using e_row_id
*                                             e_column_id.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_node_double_click.
    DATA : lr_table TYPE REF TO data.
    CASE mv_gubn.
      WHEN 'SCR_TREE1'.
        DATA(ls_tree1) = gt_tree1[ KEY node COMPONENTS node_key = node_key ].
        CHECK ls_tree1-type = gc_t.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (ls_tree1-name).
        ASSIGN lr_table->* to <gt_list2>.

        gt_fcat2 = lcl_module=>get_fcat( it_data = <gt_list2> ).
        CALL METHOD lcl_module=>set_f4( io_alv = go_grid2 it_fcat = gt_fcat2 ).
        gt_exld2          =  lcl_module=>get_excl_buttons( ).
        gs_layo2          =  lcl_module=>get_layout( it_tab  = <gt_list2> ).
        gs_vari2-report   =  lcl_module=>get_variant( i_name = 'GO_GRID2' ).
        BREAK-POINT.

*
*        IF gv_prg_mode <> gc_prg_mode_display.
*          PERFORM fc_set_style_base    USING   gt_fcat2
*                                    CHANGING  gt_style_append.
*        ENDIF.

*     perform handle_hotspot_click1_1   using e_row_id
*                                             e_column_id.
    ENDCASE.
  ENDMETHOD.
  "handle_hotspot_click1_1
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
      WHEN 'SCR_GRID2'.
        CALL METHOD lcl_controller=>scr100_grid2_toolbar( p_object = e_object p_interactive = e_interactive ).
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
*        CALL METHOD lcl_controller=>scr100_grid2_toolbar( p_object = p_object p_interactive = p_interactive ).
*                                                  es_row_no.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1

  METHOD handle_double_click.
    CASE mv_gubn.
      WHEN 'SCR_GRID1'.
        MESSAGE s001 WITH 'dc'.
*        PERFORM handle_button_click1        USING es_col_id
*                                                  es_row_no.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1

ENDCLASS.
