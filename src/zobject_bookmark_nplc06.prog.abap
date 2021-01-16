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
  METHOD pov_name.
    DATA(lv_bugn) = lcl_module=>read_scr_field( i_fieldname = 'GS_TREE_ADD-TYPE' ).
    DATA(lv_name) = lcl_module=>read_scr_field( i_fieldname = 'GS_TREE_ADD-NAME' ).
    CHECK lv_bugn = gc_t.
    IF lv_name IS INITIAL.
      MESSAGE s000 WITH TEXT-e02 DISPLAY LIKE gc_e.
      RETURN.
    ENDIF.
*    lv_name = |%{ lv_name }%|.
    SELECT  a~tabname ,
            b~tabclass,
            a~ddtext
      INTO TABLE @DATA(lt_f4)
      FROM dd02t AS a INNER JOIN dd02l AS b
        ON a~tabname  = b~tabname
       AND a~as4local = b~as4local
       AND a~as4vers  = b~as4vers
     WHERE a~ddlanguage = @sy-langu
       AND a~tabname LIKE @lv_name
       AND b~tabclass IN ( 'TRANSP' , 'VIEW' )
      .
    CALL METHOD lcl_module=>disp_f4(
      EXPORTING
        it_data    = lt_f4
        i_retfield = 'TABNAME'
        i_scrfield = 'GS_TREE_ADD-NAME'
      IMPORTING
        et_result  = DATA(lt_rtn)
    ).

    "Description을 갱신해 준다.
    CHECK lt_rtn IS NOT INITIAL.
    lv_name = lt_rtn[ 1 ]-fieldval.
    SELECT SINGLE a~ddtext
      INTO @gs_tree_add-description
      FROM dd02t AS a
     WHERE a~ddlanguage = @sy-langu
       AND a~tabname = @lv_name
      .
    lcl_module=>write_scr_field( i_fieldname = 'GS_TREE_ADD-DESCRIPTION' i_fieldvalue = gs_tree_add-description it_fields = lt_rtn ).

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
      lv_id = 'GS_TREE_ADD-TYPE'. "gs_tree_add-type
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
    IF gs_tree_add-ismodify = gc_x.
      LOOP AT SCREEN .
        IF screen-name = 'GS_TREE_ADD-TYPE'.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.

    set_status( iv_status_key = 'PF_0200' ).
    super->pbo_begin( ).
  ENDMETHOD.

  METHOD handle_pai.
    DATA : ls_node_layout TYPE lvc_s_layn.
    DATA : lt_node TYPE lvc_t_nkey.
    DATA : ls_list1 LIKE LINE OF gt_tree1.
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
        IF gs_tree_add-type IS INITIAL OR
           gs_tree_add-name IS INITIAL.
          MESSAGE s000 WITH TEXT-e03 DISPLAY LIKE gc_e.
          RETURN.
        ENDIF.
        "노드로 화면에 추가한다.

        IF gs_tree_add-guid IS INITIAL.
          "신규추가
          "DB에 저장할 내역을 생성한다.
          ls_list1 = gt_tree1[ KEY node COMPONENTS node_key = gs_tree_add-node ].
*          READ TABLE gt_tree1 INTO ls_list1 WITH KEY node_key = gs_tree_add-node.
          IF sy-subrc = 0.
            ls_list1-parent_guid = ls_list1-guid.
            ls_list1-guid  = lcl_module=>get_uuidx16( ).
            ls_list1-usrid = sy-uname.
            ls_list1-type = gs_tree_add-type.
            ls_list1-name = gs_tree_add-name.
            ls_list1-description = gs_tree_add-description.
            ls_list1-zorder = gs_tree_add-zorder.
          ENDIF.

          "Node를 추가한다.
          CALL METHOD go_tree_assist1->add_one_node_child
            EXPORTING
              i_relat_node_key = gs_tree_add-node
              i_node_text      = gs_tree_add-name
              is_outtab_line   = ls_list1
            IMPORTING
              e_new_node_key   = DATA(lv_nkey_rtn).


          "DB에 반영한다.
          CALL METHOD lcl_model=>tree_save( gt_tree1 ).
        ELSE.
          "수정..
          ls_list1 = VALUE #( gt_tree1[ KEY id COMPONENTS guid = gs_tree_add-guid  ] DEFAULT ls_list1  ).
          CHECK sy-subrc = 0.
          ls_list1 = CORRESPONDING #( BASE ( ls_list1 )  gs_tree_add ).
          CALL METHOD go_tree1->change_node
            EXPORTING
              i_node_key     = gs_tree_add-node
              i_outtab_line  = ls_list1
              i_u_node_text  = CONV #( gs_tree_add-name )
              i_node_text    = CONV #( gs_tree_add-name )
            EXCEPTIONS
              node_not_found = 1.
          IF sy-subrc = 0.
            CALL METHOD go_tree1->frontend_update( ).
            "DB에 반영한다.
            CALL METHOD lcl_model=>tree_update( ls_list1 ).
          ENDIF.
        ENDIF.
        leave( ).
      WHEN OTHERS.
        MESSAGE s000 WITH iv_function_code.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
