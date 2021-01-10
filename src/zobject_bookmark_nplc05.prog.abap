*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC05
*&---------------------------------------------------------------------*



CLASS lcl_scr0100 IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
         EXPORTING
              iv_program_name = iv_program_name
              iv_dynpro_number = iv_dynpro_number ).
    "The GUI Title of the application is set using the following method
    set_title( TEXT-003 ).
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
    CALL METHOD pbo_init_create_container( ).
    CALL METHOD pbo_init_event_process( ).
    CALL METHOD pbo_init_grid_value( ).
    CALL METHOD pbo_init_grid_display( ).
    gv_first = 'X'.
    super->pbo_begin( ).
  ENDMETHOD.

  METHOD handle_pai.
    DATA lt_node TYPE lvc_t_nkey.

    CASE iv_function_code.
        " For the default GUI Status, global constants can be used to evaluate the function code.
        " However, if you set your own GUI Status using method set_status( ),
        " then you will have to evaluate your specific function codes.
      WHEN gc_function_code_back OR gc_function_code_exit OR
           'BACK' OR 'EXIT' OR 'CANCEL'.
        leave( ).
      WHEN gc_function_code_cancel.
        LEAVE PROGRAM.
      WHEN 'APPEND'.
        CLEAR : gs_tree_add-name, gs_tree_add-description.
        CALL METHOD go_tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_node.
        IF lt_node IS INITIAL.
          CALL METHOD go_tree1->get_selected_item
            IMPORTING
              e_selected_node = gs_tree_add-node.

          IF gs_tree_add-node IS INITIAL.
            MESSAGE s000 WITH TEXT-e01.
            RETURN.
          ENDIF.
        ELSE.
          gs_tree_add-node = lt_node[ 1 ].
        ENDIF.
        cl_bus_abstract_screen=>get_screen(
          EXPORTING
            iv_program_name  = sy-repid
            iv_dynpro_number = '0200'
          IMPORTING
            ev_screen        = go_scr0200
        ).

        IF go_scr0200 IS BOUND.
          CALL METHOD go_scr0200->show_as_popup( ).
        ENDIF.
      WHEN 'DELETE'.
        CALL METHOD go_tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_node.
        DATA(lv_node) = lt_node[ 1 ].
        DATA(lv_node1) = lv_node.
        CONDENSE lv_node1.
        IF lv_node1 = '1'.
          MESSAGE s000 WITH TEXT-e04 DISPLAY LIKE gc_e.
          RETURN.
        ENDIF.
        CALL METHOD go_tree1->get_subtree
          EXPORTING
            i_node_key       = lv_node
          IMPORTING
            et_subtree_nodes = DATA(lt_sub).

        CALL METHOD go_tree1->delete_subtree
          EXPORTING
            i_node_key                = lv_node
            i_update_parents_expander = space
            i_update_parents_folder   = space
          EXCEPTIONS
            node_key_not_in_model     = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
*         Implement suitable error handling here
        ENDIF.
        CALL METHOD go_tree1->frontend_update( ).
      WHEN OTHERS.
        MESSAGE s000 WITH iv_function_code.
        CALL METHOD go_tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_node.
*        CALL METHOD go_tree1->get_selected_item
*          IMPORTING
*            e_fieldname     = DATA(lv_fieldname)
*            e_selected_node = DATA(lv_node).

*       BREAK-POINT.

    ENDCASE.
  ENDMETHOD.
  METHOD pbo_init_create_container.
    "Module에서의 Data선언은 Global로 인식되므로 Data선언은 모두 Class-method에서 수행해야 한다.
    IF gv_first IS INITIAL.
      "# create container object

      "-----------------------------------------
      " Screen 100에 Container를 생성한다.
      "-----------------------------------------
      CREATE OBJECT go_dock
        EXPORTING
          dynnr     = sy-dynnr
          repid     = sy-repid
          side      = cl_gui_docking_container=>dock_at_top
          extension = 6000.

      CREATE OBJECT go_easy
        EXPORTING
          parent        = go_dock
          orientation   = 1
          sash_position = 30.

      go_left = go_easy->top_left_container.
      go_right = go_easy->bottom_right_container.


      CREATE OBJECT go_tree1
        EXPORTING
          parent         = go_left
          item_selection = ' '
          no_html_header = gc_x.

      CREATE OBJECT go_grid1
        EXPORTING
          i_parent = go_right.


      "# Define ALV event             (SET HANDLER)
*    MODULE define_0100_event_process.

*    GET REFERENCE OF go_event INTO DATA(lr_event).
*    CALL METHOD go_control->define_0100_event_process(
*      CHANGING
*        co_event = lr_event
*        co_grid1 = go_grid1
*    ).
*    PERFORM define_0100_event_process.
**
**   # Define ALV Detail attributes (Field category,LAYOUT,SORT, etc)
*    PERFORM define_0100_grid_value.
**
***   # ALV Grid Display             (Display Screen)
*    PERFORM alv_grid_display1.
    ENDIF.
  ENDMETHOD.

  METHOD pbo_init_event_process.
    IF gv_first IS INITIAL.
*      CREATE OBJECT go_event
*        EXPORTING
*          i_gubn = 'SCR_TREE1'.
*      .
*
*      "TREE1
*      SET HANDLER go_event->handle_node_cm_req        "Context Menu Request
*              FOR go_tree1.
*      SET HANDLER go_event->handle_node_cm_sel        "Context Menu Selected
*              FOR go_tree1.
*      SET HANDLER go_event->handle_item_cm_req        "Context Menu Request
*              FOR go_tree1.
*      SET HANDLER go_event->handle_item_cm_sel        "Context Menu Request
*              FOR go_tree1.
*
*
*      "go_tree1의 가지고 있는 기존 event정보를 획득한다.
*      CALL METHOD go_tree1->get_registered_events
*        IMPORTING
*          events = DATA(lt_event).
*
*      "eventid에 context menu 요청의 번호를 추가해 준다.
*      APPEND VALUE #( eventid = cl_gui_column_tree=>eventid_node_context_menu_req ) TO lt_event.
*      APPEND VALUE #( eventid = cl_gui_column_tree=>eventid_item_context_menu_req ) TO lt_event.
*      APPEND VALUE #( eventid = cl_gui_column_tree=>eventid_node_context_menu_req ) TO lt_event.
*      APPEND VALUE #( eventid = cl_gui_column_tree=>eventid_item_context_menu_req ) TO lt_event.
*
*      "go_tree1의 추가된 event set정보를 등록해 준다.
*      CALL METHOD go_tree1->set_registered_events
*        EXPORTING
*          events = lt_event.


      CREATE OBJECT go_event
        EXPORTING
          i_gubn = 'SCR_GRID1'.
      .

      "GRID1
      SET HANDLER go_event->handle_hotspot_click        "hotspot Triggered
              FOR go_grid1.

*  HANDLE_DATA_CHANGED events are excuted when ALV Grid data changed
      CALL METHOD go_grid1->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.       "By Enter

      CALL METHOD go_grid1->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.    "By Changed



* * Create Event Handler
* *  IF go_event IS INITIAL.
*   CREATE OBJECT go_event
*     EXPORTING
*       i_gubn = 'SCR_0200'.
*   .
* *  ENDIF.
*   SET HANDLER go_event->handle_hotspot_click        "hotspot Triggered
*           FOR go_grid2.
*   SET HANDLER go_event->handle_toolbar
*           FOR go_grid2.
*   SET HANDLER go_event->handle_user_command
*           FOR go_grid2.
*   SET HANDLER go_event->handle_onf4
*           FOR go_grid2.
* *  SET HANDLER go_event->handle_data_changed_finished
* *          FOR go_grid2.
*   SET HANDLER go_event->handle_data_changed
*           FOR go_grid2.
*
*
*
* * HANDLE_DATA_CHANGED events are excuted when ALV Grid data changed
*   CALL METHOD go_grid2->register_edit_event
*     EXPORTING
*       i_event_id = cl_gui_alv_grid=>mc_evt_enter.       "By Enter
*
*   CALL METHOD go_grid2->register_edit_event
*     EXPORTING
*       i_event_id = cl_gui_alv_grid=>mc_evt_modified.    "By Changed


    ENDIF.
  ENDMETHOD.
  METHOD pbo_init_grid_value.
    IF gv_first IS INITIAL.
*.............> Define Field category attributes
**               -----------------------
      CALL METHOD go_control->fcat_0100_build(
        EXPORTING
          it_list1 = gt_list1
        CHANGING
          ct_fcat1 = gt_fcat1
      ).

*.............> Define ALV Standard button attributes
*               -----------------------
      gt_exld1          =  lcl_module=>get_excl_buttons( ).
      gs_layo1          =  lcl_module=>get_layout( it_tab  = gt_list1 ).
      gs_vari1-report   =  lcl_module=>get_variant( i_name = 'go_grid1' ).
*
*    IF gv_prg_mode <> gc_prg_mode_display.
*      PERFORM fc_set_style_base    USING   gt_fcat2
*                                CHANGING  gt_style_append.
*    ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD pbo_init_grid_display.
    IF gv_first IS INITIAL.
      DATA l_hierarchy_header TYPE treev_hhdr. "SET_TABLE_FOR_FIRST_DISPLAY 하면서 Header정보를 넣어주어야 하므로 구조체를 선언한다
      l_hierarchy_header-heading = 'NODE'(300).    "ALV의 [첫번째 Column]을 계층적으로 만들어줄 Header정보
      l_hierarchy_header-tooltip = 'NODE'(400).
      l_hierarchy_header-width = 50.
      l_hierarchy_header-width_pix = ' '.


      DATA : ls_fcat TYPE lvc_s_fcat.
      MODIFY gt_fcat1 FROM ls_fcat TRANSPORTING no_out WHERE fieldname > ' '.
      CALL METHOD go_tree1->set_table_for_first_display   "일반 ALV의 경우 output table에 데이타를 채운후 이 Method를 호출하나
        EXPORTING                                           "ALV Tree의 경우 null인 상태의  output table을 넘겨주워 화면을 display시킨다.
          is_hierarchy_header = l_hierarchy_header          "일반ALV에 [첫번째Column]을 계층적으로 사용하기 위한 Header정보를 받는다.
        CHANGING
          it_outtab           = gt_list1 "반드시 테이블은 Global 변수로 선언되어있어야 하며, 빈상태어야 한다.
          it_fieldcatalog     = gt_fcat1.  "FieldCatalog 1줄이라도 채워주어야 한다.



*      FCODE  , ICON            , TEXT , quickinfo , UFCODE  , is_disabled
*      APPEND , ICON_INSERT_ROW , 추가   , 추가
*      DELETE , ICON_DELETE_ROW , 삭제   , 삭제
*      |
*      GITA   , ICON_DELETE_ROW , 버튼들  , 버튼들
*      GITA1  ,                  , 버튼1  , 버튼1       , GITA
*      GITA2  ,                  , 버튼2  , 버튼2       , GITA
*      GITA3  ,                  , 버튼3  , 버튼3       , GITA
      DATA : lt_fcodes TYPE lcl_module=>mty_t_toolbar.
      lt_fcodes = VALUE #(
            ( fcode = 'APPEND'  icon = icon_insert_row text ='추가'   quickinfo ='추가' ufcode = '' )
            ( fcode = 'DELETE'  icon = icon_delete_row text ='삭제'   quickinfo ='삭제' ufcode = '' )
*            ( fcode = '-' )
*            ( fcode = 'SAVE'    icon = icon_system_save text ='저장'   quickinfo ='저장' ufcode = '' )
            ).
      CALL METHOD lcl_module=>tree_add_toolbar
        EXPORTING
          it_fcodes  = lt_fcodes
        CHANGING
          co_tree    = go_tree1
          co_toolbar = go_toolbar.



      lt_fcodes = VALUE #(
            ( fcode = 'APPEND'  icon = icon_insert_row text ='추가'   quickinfo ='추가' ufcode = '' )
            ( fcode = 'DELETE'  icon = icon_delete_row text ='삭제'   quickinfo ='삭제' ufcode = '' )
            ).
      CALL METHOD lcl_module=>tree_add_contextmenu
        EXPORTING
          it_fcodes = lt_fcodes
        CHANGING
          co_tree   = go_tree1.



      DATA : ls_treeinfo TYPE lcl_controller=>mty_s_treeinfo,
             lt_treeicon TYPE lcl_controller=>mty_t_treeicon.
      DATA :lt_expand TYPE lvc_t_nkey.
      ls_treeinfo-fmkey = 'GUID'.
      ls_treeinfo-fname = 'NAME'.
      ls_treeinfo-ficon = 'TYPE'.
      ls_treeinfo-fukey = 'PARENT_GUID'.
      ls_treeinfo-fnode = 'NODE_KEY'.

      lt_treeicon = VALUE #(
            ( gubn = 'N'  n_image = icon_closed_folder  exp_image = icon_open_folder )
            ( gubn = 'T'  n_image = icon_database_table exp_image = icon_database_table )
            ).

      DATA lt_tree1 TYPE gty_t_tree.
      CALL METHOD lcl_model=>get_nodes(
        EXPORTING
          i_usr    = sy-uname
        CHANGING
          ct_nodes = lt_tree1
      ).

      CALL METHOD lcl_controller=>tree_draw
        EXPORTING
          is_fieldinfo = ls_treeinfo
          it_fieldicon = lt_treeicon
        CHANGING
          co_tree      = go_tree1
          ct_data      = lt_tree1
          ct_tree      = gt_list1
          ct_expand    = lt_expand.

*      CALL METHOD go_tree1->update_calculations.
      CALL METHOD go_tree1->frontend_update.

      CALL METHOD go_tree1->expand_nodes
        EXPORTING
          it_node_key             = lt_expand
        EXCEPTIONS
          failed                  = 1
          cntl_system_error       = 2
          error_in_node_key_table = 3
          dp_error                = 4
          node_not_found          = 5
          OTHERS                  = 6.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
