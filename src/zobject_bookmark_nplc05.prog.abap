*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC05
*&---------------------------------------------------------------------*



CLASS lcl_scr0100 IMPLEMENTATION.
  METHOD grid2_node_double_click.
    DATA : lr_table TYPE REF TO data,
           ls_fcat  LIKE LINE OF gt_fcat2.
    DATA(ls_tree1) = gt_tree1[ KEY node COMPONENTS node_key = node_key ].
    CHECK ls_tree1-type = gc_t.

    gt_fcat2 = lcl_module=>get_fcat( iv_ddic_object = ls_tree1-name ).

    CLEAR ls_fcat.
    ls_fcat-fieldname = 'VCELLTAB'.
    ls_fcat-ref_table = 'FUGR_ANALYSIS_VIEWER_LINE'.
    ls_fcat-ref_field = 'STYLES'.
    APPEND ls_fcat TO gt_fcat2.
    ls_fcat-fieldname = 'VCOLOR'.
    ls_fcat-ref_table = 'FUGR_ANALYSIS_VIEWER_LINE'.
    ls_fcat-ref_field = 'COLORS'.
    APPEND ls_fcat TO gt_fcat2.
    CALL METHOD lcl_module=>set_f4( io_alv = go_grid2 it_fcat = gt_fcat2 ).

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = gt_fcat2
      IMPORTING
        ep_table        = lr_table.
    ASSIGN lr_table->* TO <gt_list2>.
    DELETE gt_fcat2 WHERE fieldname = 'VCELLTAB'.
    DELETE gt_fcat2 WHERE fieldname = 'VCOLOR'.
    gt_exld2          =  lcl_module=>get_excl_buttons( ).
    gs_layo2          =  lcl_module=>get_layout( it_tab  = <gt_list2> ).
    gs_vari2-report   =  lcl_module=>get_variant( i_name = 'GO_GRID2' ).

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE <gt_list2>
     FROM (ls_tree1-name).

    gs_layo2-col_opt   = space.
    CALL METHOD go_grid2->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo2
        is_variant           = gs_vari2
        it_toolbar_excluding = gt_exld2
        is_print             = gs_prnt2
        i_save               = gc_a
        i_default            = space
      CHANGING
        it_sort              = gt_sort2[]
        it_fieldcatalog      = gt_fcat2[]
        it_outtab            = <gt_list2>.

*
*        IF gv_prg_mode <> gc_prg_mode_display.
*          PERFORM fc_set_style_base    USING   gt_fcat2
*                                    CHANGING  gt_style_append.
*        ENDIF.

*     perform handle_hotspot_click1_1   using e_row_id
*                                             e_column_id.
  ENDMETHOD.
  METHOD grid2_event_toolbar.

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
  METHOD tree1_fcat_build.
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
    CALL METHOD pbo_init_tree_value( ).
    CALL METHOD pbo_init_tree_display( ).
    gv_first = 'X'.
    super->pbo_begin( ).
  ENDMETHOD.

  METHOD handle_pai.
    DATA : lt_node  TYPE lvc_t_nkey,
           ls_list1 LIKE LINE OF gt_tree1.

    CASE iv_function_code.
        " For the default GUI Status, global constants can be used to evaluate the function code.
        " However, if you set your own GUI Status using method set_status( ),
        " then you will have to evaluate your specific function codes.
      WHEN gc_function_code_back OR gc_function_code_exit OR
           'BACK' OR 'EXIT' OR 'CANCEL'.
        leave( ).
      WHEN gc_function_code_cancel.
        LEAVE PROGRAM.
      WHEN 'ADMIN'.
        CALL SELECTION-SCREEN 2000 STARTING AT 10 10 ENDING AT 120 20.
      WHEN 'REFRESH'.
        DATA : lt_expand TYPE lvc_t_nkey.
        CALL METHOD go_tree_assist1->redraw( CHANGING ct_expand = lt_expand ).
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
      WHEN 'DRAGDROP1'.
        READ TABLE lcl_tree_assist=>mt_dragdrop_nodes_change INTO DATA(ls_data) WITH KEY tree = go_tree1.
        IF sy-subrc = 0.
          LOOP AT ls_data-bags INTO DATA(ls_node).
            UPDATE zobjectbook SET parent_guid = ls_node-umkey WHERE guid = ls_node-mkey.
          ENDLOOP.
          lcl_module=>commit( ).
        ENDIF.

      WHEN 'MODIFY'.
        lt_node = go_tree_assist1->get_selected_nodes( ).
        IF lines( lt_node ) <> 1.
          MESSAGE s000 WITH TEXT-e01.
          RETURN.
        ENDIF.
        gs_tree_add-node = lt_node[ 1 ].
        ls_list1 = gt_tree1[ KEY node COMPONENTS node_key = gs_tree_add-node ].
        CHECK sy-subrc = 0.
        gs_tree_add = CORRESPONDING #( BASE ( gs_tree_add ) ls_list1 ).
        gs_tree_add-ismodify = gc_x.

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
      WHEN 'APPEND'.
        CLEAR : gs_tree_add-name,  gs_tree_add-guid, gs_tree_add-description, gs_tree_add-ismodify.
        lt_node = go_tree_assist1->get_selected_nodes( ).
        IF lines( lt_node ) <> 1.
          MESSAGE s000 WITH TEXT-e01.
          RETURN.
        ENDIF.
        gs_tree_add-node = lt_node[ 1 ].
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
        lt_node = go_tree_assist1->get_selected_nodes( ).
        IF lt_node IS INITIAL.
          MESSAGE s000 WITH TEXT-e05.
          RETURN.
        ENDIF.
        SORT lt_node.
        DATA(lv_node) = lt_node[ 1 ].
        CONDENSE lv_node.
        IF lv_node = '1'.
          MESSAGE s000 WITH TEXT-e04 DISPLAY LIKE gc_e.
          RETURN.
        ENDIF.


        DATA(lv_ans) = lcl_module=>ask_question( i_question = '정말로 삭제하시겠습니까?' ).
        CHECK lv_ans = '1'.

        DATA : lt_list1_deleted TYPE gty_t_tree,
               lt_zobjectbook   TYPE STANDARD TABLE OF zobjectbook.

        CALL METHOD go_tree_assist1->delete_subtree
          EXPORTING
            it_node         = lt_node
          IMPORTING
            et_tree_deleted = lt_list1_deleted.

        lt_zobjectbook = CORRESPONDING #( lt_list1_deleted ).
        DELETE zobjectbook FROM TABLE lt_zobjectbook.
        lcl_module=>commit( ).


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
          parent              = go_left
          item_selection      = ' '
          node_selection_mode = cl_gui_column_tree=>node_sel_mode_multiple
          no_html_header      = gc_x.

      CREATE OBJECT go_grid2
        EXPORTING
          i_parent = go_right.


      "# Define ALV event             (SET HANDLER)
*    MODULE define_0100_event_process.

*    GET REFERENCE OF go_event INTO DATA(lr_event).
*    CALL METHOD go_control->define_0100_event_process(
*      CHANGING
*        co_event = lr_event
*        co_grid1 = go_grid2
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

      CREATE OBJECT go_event
        EXPORTING
          i_gubn = 'SCR0100_TREE1'.

      SET HANDLER go_event->handle_node_double_click
              FOR go_tree1.

      CALL METHOD go_tree1->get_registered_events( IMPORTING events = DATA(lt_event) ).
      APPEND VALUE #( eventid = cl_gui_column_tree=>eventid_node_double_click ) TO lt_event.
      CALL METHOD go_tree1->set_registered_events( EXPORTING events = lt_event ).

      CREATE OBJECT go_event
        EXPORTING
          i_gubn = 'SCR0100_GRID2'.
      .

      "GRID1
      SET HANDLER go_event->handle_double_click        "hotspot Triggered
              FOR go_grid2.

*  HANDLE_DATA_CHANGED events are excuted when ALV Grid data changed
      CALL METHOD go_grid2->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.       "By Enter

      CALL METHOD go_grid2->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.    "By Changed



      SET HANDLER go_event->handle_toolbar
              FOR go_grid2.
*   SET HANDLER go_event->handle_user_command
*           FOR go_tree1.
*   SET HANDLER go_event->handle_onf4
*           FOR go_tree1.
* *  SET HANDLER go_event->handle_data_changed_finished
* *          FOR go_tree1.
*   SET HANDLER go_event->handle_data_changed
*           FOR go_tree1.
**
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
  METHOD pbo_init_tree_dragdrop.
    IF gv_first IS INITIAL.
      CREATE OBJECT go_drag.
      DATA(lv_effect) = cl_dragdrop=>move.
      CALL METHOD go_drag->add
        EXPORTING
          flavor     = 'moving'        "이 Drag&Drop의 명칭지정
          dragsrc    = gc_x            "Drag( Source ) 인가?
          droptarget = space           "Drop( Target ) 인가?
          effect     = lv_effect.      "Drag&Drop의 동작방식은?

* The favourite folder shall allow to drop objects with
* flavor 'favorite'. No dragging is possible.
      CREATE OBJECT go_drop.
      lv_effect = cl_dragdrop=>copy.
      CALL METHOD go_drop->add
        EXPORTING
          flavor     = 'moving'                                    "#EC NOTEXT
          dragsrc    = space          "Drag( Source ) 인가?
          droptarget = gc_x           "Drop( Target ) 인가?
          effect     = lv_effect.

    ENDIF.
  ENDMETHOD.
  METHOD pbo_init_tree_value.
    IF gv_first IS INITIAL.
*.............> Define Field category attributes
**               -----------------------
      CALL METHOD lcl_scr0100=>tree1_fcat_build(
        EXPORTING
          it_list1 = gt_tree1
        CHANGING
          ct_fcat1 = gt_fcat1
      ).

*.............> Define ALV Standard button attributes
*               -----------------------
      gt_exld1          =  lcl_module=>get_excl_buttons( ).
      gs_layo1          =  lcl_module=>get_layout( it_tab  = gt_tree1 ).
      gs_vari1-report   =  lcl_module=>get_variant( i_name = 'go_tree1' ).
*
*    IF gv_prg_mode <> gc_prg_mode_display.
*      PERFORM fc_set_style_base    USING   gt_fcat2
*                                CHANGING  gt_style_append.
*    ENDIF.



    ENDIF.
  ENDMETHOD.
  METHOD pbo_init_tree_display.
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
          it_outtab           = gt_tree1 "반드시 테이블은 Global 변수로 선언되어있어야 하며, 빈상태어야 한다.
          it_fieldcatalog     = gt_fcat1.  "FieldCatalog 1줄이라도 채워주어야 한다.



*      FCODE  , ICON            , TEXT , quickinfo , UFCODE  , is_disabled
*      APPEND , ICON_INSERT_ROW , 추가   , 추가
*      DELETE , ICON_DELETE_ROW , 삭제   , 삭제
*      |
*      GITA   , ICON_DELETE_ROW , 버튼들  , 버튼들
*      GITA1  ,                  , 버튼1  , 버튼1       , GITA
*      GITA2  ,                  , 버튼2  , 버튼2       , GITA
*      GITA3  ,                  , 버튼3  , 버튼3       , GITA


      DATA : lt_toolbar     TYPE lcl_tree_assist=>mty_t_toolbar,
             lt_contextmenu TYPE lcl_tree_assist=>mty_t_toolbar.
      DATA : ls_treeinfo TYPE lcl_tree_assist=>mty_s_treeinfo,
             lt_treeicon TYPE lcl_tree_assist=>mty_t_treeicon.
      DATA :lt_expand TYPE lvc_t_nkey.

      ls_treeinfo-fmkey = 'GUID'.
      ls_treeinfo-fname = 'NAME'.
      ls_treeinfo-ficon = 'TYPE'.
      ls_treeinfo-fukey = 'PARENT_GUID'.
      ls_treeinfo-fnode = 'NODE_KEY'.
      ls_treeinfo-fsort = 'ZORDER'.

      lt_toolbar = VALUE #(
            ( fcode = 'APPEND'  icon = icon_insert_row text ='추가'   quickinfo ='추가' ufcode = '' )
            ( fcode = 'MODIFY'  icon = icon_write_file     text ='수정'   quickinfo ='수정' ufcode = '' )
            ( fcode = 'DELETE'  icon = icon_delete_row text ='삭제'   quickinfo ='삭제' ufcode = '' )
            ( fcode = '-' )
            ( fcode = 'REFRESH'   icon = icon_refresh         text ='Refresh'    quickinfo ='Refresh'   )
            ).

      lt_contextmenu = VALUE #(
            ( fcode = 'APPEND'  icon = icon_insert_row text ='추가'   quickinfo ='추가' ufcode = '' )
            ( fcode = 'MODIFY'  icon = icon_write_file     text ='수정'   quickinfo ='수정' ufcode = '' )
            ( fcode = 'DELETE'  icon = icon_delete_row text ='삭제'   quickinfo ='삭제' ufcode = '' )
            ).


      lt_treeicon = VALUE #(
            ( gubn = 'N'  n_image = icon_closed_folder  exp_image = icon_open_folder     isfolder = gc_x )
            ( gubn = 'T'  n_image = icon_database_table exp_image = icon_database_table  isfolder = space )
            ).

      DATA lt_tree1 TYPE gty_t_tree.
      CALL METHOD lcl_model=>get_nodes(
        EXPORTING
          i_usr    = sy-uname
        CHANGING
          ct_nodes = lt_tree1
      ).

      GET REFERENCE OF gt_tree1 INTO DATA(lr_tree1).
      CREATE OBJECT go_tree_assist1
        EXPORTING
          is_treeinfo      = ls_treeinfo
          it_tree          = lr_tree1
          io_tree          = go_tree1
          i_dragdrop       = gc_x
          i_dragdrop_fcode = 'DRAGDROP1'
          it_treeicon      = lt_treeicon
          it_toolbar       = lt_toolbar
          it_contextmenu   = lt_contextmenu.


      CALL METHOD go_tree_assist1->draw
        EXPORTING
          it_source = lt_tree1
        CHANGING
          ct_expand = lt_expand.



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
