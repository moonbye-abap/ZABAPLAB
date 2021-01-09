*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC06
*&---------------------------------------------------------------------*

CLASS lcl_scr0200 IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
         EXPORTING
              iv_program_name = iv_program_name
              iv_dynpro_number = iv_dynpro_number ).
    "The GUI Title of the application is set using the following method
    set_title( TEXT-004 ).
    set_status( iv_status_key = 'PF_0100' ).
    SET HANDLER handle_pai FOR me.
  ENDMETHOD.

  "DynPro 명령 ( Call Screen xxxx)
  METHOD call_screen.
    CALL SCREEN iv_dynpro_number.
  ENDMETHOD.

  "DynPro 명령 ( Call Screen xxxx Starting At xx yy Ending At xx yy )
  METHOD call_screen_starting_at.
    CALL SCREEN iv_dynpro_number STARTING AT iv_xstart iv_ystart
      ENDING AT iv_xend iv_yend.
  ENDMETHOD.

  METHOD pbo_begin.
*    CALL METHOD pbo_init_create_container( ).
*    CALL METHOD pbo_init_event_process( ).
*    CALL METHOD pbo_init_grid_value( ).
*    CALL METHOD pbo_init_grid_display( ).
*    gv_first = 'X'.
    DATA : lt_gubn TYPE STANDARD TABLE OF gty_s_gubn .
    DATA : lv_id     TYPE vrm_id,
           lt_values TYPE vrm_values.


    IF gv_first IS NOT INITIAL.
      lv_id = 'GS_TREE_ADD-GUBN'. "gs_tree_add-gubn
      lt_values = VALUE #(
                  ( key = 'N' text = 'Node' )
                  ( key = 'T' text = 'Table' )
                    ).
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id              = lv_id
          values          = lt_values
        EXCEPTIONS
          id_illegal_name = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.
    set_status( iv_status_key = 'PF_0200' ).
    super->pbo_begin( ).
  ENDMETHOD.

  METHOD handle_pai.
    DATA : ls_node_layout TYPE lvc_s_layn.
    DATA : lt_node TYPE lvc_t_nkey.
    DATA : ls_list1 LIKE LINE OF gt_list1.
    CASE iv_function_code.
        " For the default GUI Status, global constants can be used to evaluate the function code.
        " However, if you set your own GUI Status using method set_status( ),
        " then you will have to evaluate your specific function codes.
      WHEN gc_function_code_back OR gc_function_code_exit OR
           'BACK' OR 'EXIT' OR 'CANCEL'.
        leave( ).
      WHEN gc_function_code_cancel.
        LEAVE PROGRAM.
      WHEN 'SAVE'.

        "노드로 화면에 추가한다.
        CASE gs_tree_add-gubn.
          WHEN 'N'.
            ls_node_layout-n_image    = icon_closed_folder.
            ls_node_layout-exp_image  = icon_open_folder.
            ls_node_layout-isfolder = gc_x.
        ENDCASE.
        ls_list1-description = gs_tree_add-description.
        CALL METHOD ycl_commons=>tree_add_one_node_child
          EXPORTING
            i_relat_node_key = gs_tree_add-node
            i_node_text      = gs_tree_add-name
            is_node_layout   = ls_node_layout
            is_outtab_line   = ls_list1
          IMPORTING
            e_new_node_key   = DATA(lv_nkey_rtn)
          CHANGING
            co_tree          = go_tree1.

        "추가된 Node를 DB에 반영한다.


        leave( ).
      WHEN OTHERS.
        MESSAGE s000 WITH iv_function_code.
        CALL METHOD go_tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_node.
        CALL METHOD go_tree1->get_selected_item
          IMPORTING
            e_fieldname     = DATA(lv_fieldname)
            e_selected_node = DATA(lv_node).

*       BREAK-POINT.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
