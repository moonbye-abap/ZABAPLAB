*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC05
*&---------------------------------------------------------------------*



CLASS lcl_scr0100 IMPLEMENTATION.
  METHOD tree1_event_node_double_click.
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

    CLEAR :ls_fcat.
    ls_fcat-fieldname = 'VMODE'.
    ls_fcat-datatype  = 'CHAR'.
    ls_fcat-inttype   = 'C'.
    ls_fcat-intlen    = '1'.
*    ls_fcat-decimals  = xdetails-decimals.
    APPEND ls_fcat TO gt_fcat2.

    ls_fcat-fieldname = 'VIDX'.
    ls_fcat-ref_table = 'SYST'.
    ls_fcat-ref_field = 'TABIX'.
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
    DELETE gt_fcat2 WHERE fieldname = 'VIDX'.
    CALL METHOD grid2_fcat_build( CHANGING ct_fcat = gt_fcat2 ).
    gt_exld2          =  lcl_module=>get_excl_buttons( ).
    gs_layo2          =  lcl_module=>get_layout( it_tab  = <gt_list2> ).
    gs_vari2-report   =  lcl_module=>get_variant( i_name = 'GO_GRID2' ).

    gs_acc_table = lcl_model=>acctable_select_single( i_uname = sy-uname i_name = ls_tree1-name ).
    IF gs_acc_table-name = 'ZOBJECTBOOK'.
      CLEAR : gs_acc_table-zcreate ,gs_acc_table-zupdate, gs_acc_table-zdelete.
    ENDIF.
    IF gs_acc_table-zread = gc_x.
      lcl_model=>seltable_select( EXPORTING i_name = ls_tree1-name CHANGING ct_table = <gt_list2> ).
    ENDIF.
    gs_layo2-col_opt   = space.
    CALL METHOD grid2_set_style( EXPORTING it_fcat = gt_fcat2 CHANGING ct_list = <gt_list2> ).
    gs_layo2-grid_title = | { ls_tree1-name } ({ ls_tree1-description }) / { lines( <gt_list2> ) }|.
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


  ENDMETHOD.
  METHOD grid2_event_user_command.
    FIELD-SYMBOLS : <ls_list2> TYPE any,
                    <lt_list2> TYPE STANDARD TABLE.
    DATA : ls_list2 TYPE REF TO data,
           lt_list2 TYPE REF TO data.
    CREATE DATA ls_list2 LIKE LINE OF <gt_list2>.
    ASSIGN ls_list2->* TO <ls_list2>.
    CREATE DATA lt_list2 LIKE STANDARD TABLE OF <ls_list2>.
    ASSIGN lt_list2->* TO <lt_list2>.

    DATA : lv_change TYPE c.
    ASSIGN COMPONENT 'VMODE' OF STRUCTURE <ls_list2> TO FIELD-SYMBOL(<lv_vmode>).
    IF sy-subrc = 0.
      <lv_vmode> = gc_alv_mode_append.
    ENDIF.
    CASE i_ucomm.
      WHEN 'SEL_INSERT'.
        DATA(lv_pos) = lines( <gt_list2> ) + 1.
        CALL METHOD lcl_module=>ask_question_input( EXPORTING i_question = '몇줄을 추가할까요?' IMPORTING e_input = DATA(lv_lines) ).
        DO lv_lines TIMES.
*          PERFORM fc_set_style CHANGING ls_list2.
          APPEND <ls_list2> TO <lt_list2>.
        ENDDO.

        CALL METHOD grid2_set_style( EXPORTING it_fcat = gt_fcat2 CHANGING ct_list = <lt_list2> ).
        INSERT LINES OF <lt_list2> INTO TABLE <gt_list2>.
        CALL METHOD lcl_module=>refresh_alv( io_alv = go_grid2 ).
        CALL METHOD lcl_module=>set_position( io_alv = go_grid2 i_row = lv_pos i_fieldname = 'WERKS' ).

      WHEN 'SEL_EDIT'.
        CALL METHOD go_grid2->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index) ).
        CHECK lt_index IS NOT INITIAL.
        LOOP AT lt_index INTO DATA(ls_index).
          READ TABLE <gt_list2> ASSIGNING <ls_list2> INDEX ls_index-index.
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'VMODE' OF STRUCTURE <ls_list2> TO <lv_vmode>.
            IF sy-subrc = 0.
              IF <lv_vmode> = gc_alv_mode_append.
                CONTINUE.
              ENDIF.
              <lv_vmode> = gc_alv_mode_modify.
              CALL METHOD grid2_set_style( EXPORTING it_fcat = gt_fcat2 CHANGING cs_list = <ls_list2> ).
              lv_change = gc_x.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_change = gc_x.
          CALL METHOD lcl_module=>refresh_alv( io_alv = go_grid2 ).
        ENDIF.
*        BREAK-POINT.
      WHEN 'SEL_DELETE'.
        CALL METHOD go_grid2->get_selected_rows( IMPORTING et_index_rows = lt_index ).
        CHECK lt_index IS NOT INITIAL.
        DATA(lv_ans) = lcl_module=>ask_question( i_question = '정말로 삭제하시겠습니까?' ).
        CHECK lv_ans = '1'.
        LOOP AT lt_index INTO ls_index.
          READ TABLE <gt_list2> ASSIGNING <ls_list2> INDEX ls_index-index.
          CHECK sy-subrc = 0.
          APPEND <ls_list2> TO <lt_list2>.
        ENDLOOP.
        CALL METHOD lcl_model=>seltable_delete( EXPORTING it_table = <lt_list2> IMPORTING e_err_chk = gv_err_chk e_err_msg = gv_err_msg ).
        IF gv_err_chk IS INITIAL.
          LOOP AT lt_index INTO ls_index.
            READ TABLE <gt_list2> ASSIGNING <ls_list2> INDEX ls_index-index.
            ASSIGN COMPONENT 'VMODE' OF STRUCTURE <ls_list2> TO <lv_vmode>.
            <lv_vmode> = gc_alv_mode_delete.
          ENDLOOP.
          DATA : lv_where TYPE string.
          lv_where = | VMODE = '{ gc_alv_mode_delete }' |.
          DELETE <gt_list2> WHERE (lv_where).
          lcl_module=>refresh_alv( EXPORTING io_alv = go_grid2 ).
          MESSAGE s000 WITH TEXT-s02.
        ELSE.
          MESSAGE s000 WITH gv_err_msg DISPLAY LIKE gc_e.
        ENDIF.
      WHEN 'SEL_SAVE'.
        CALL METHOD lcl_model=>seltable_save( EXPORTING it_table = <gt_list2> IMPORTING e_err_chk = gv_err_chk e_err_msg = gv_err_msg ).
        IF gv_err_chk IS INITIAL.
          CLEAR <ls_list2>.
          lv_where = | VMODE > ' ' |.
          MODIFY <gt_list2> FROM <ls_list2> TRANSPORTING ('VMODE') WHERE (lv_where).
          CALL METHOD grid2_set_style( EXPORTING it_fcat = gt_fcat2 CHANGING ct_list = <gt_list2> ).
          lcl_module=>refresh_alv( EXPORTING io_alv = go_grid2 ).
          MESSAGE s000 WITH TEXT-s01.
        ELSE.
          MESSAGE s000 WITH gv_err_msg DISPLAY LIKE gc_e.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD grid2_set_style.
    DATA : lt_style_normal TYPE lvc_t_styl,
           lt_style_insert TYPE lvc_t_styl,
           lt_style_modify TYPE lvc_t_styl,
           ls_style        LIKE LINE OF lt_style_normal.

    lt_style_normal = CORRESPONDING #( it_fcat ).
    ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
    MODIFY lt_style_normal FROM ls_style TRANSPORTING style WHERE fieldname > ' '.

    LOOP AT it_fcat INTO DATA(ls_fcat) WHERE key = gc_x.
      INSERT VALUE #( fieldname = ls_fcat-fieldname style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE lt_style_modify .
    ENDLOOP.
    IF ct_list IS NOT INITIAL.
      LOOP AT ct_list ASSIGNING FIELD-SYMBOL(<ls_list>) .
        ASSIGN COMPONENT 'VCELLTAB' OF STRUCTURE <ls_list> TO FIELD-SYMBOL(<lt_style>).
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT 'VMODE' OF STRUCTURE <ls_list> TO FIELD-SYMBOL(<lv_vmode>).
        CHECK sy-subrc = 0.
        CASE <lv_vmode>.
          WHEN gc_alv_mode_append.
            <lt_style> = lt_style_insert.
          WHEN space.
            <lt_style> = lt_style_normal.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    IF cs_list IS NOT INITIAL.
      ASSIGN COMPONENT 'VCELLTAB' OF STRUCTURE cs_list TO <lt_style>.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'VMODE' OF STRUCTURE cs_list TO <lv_vmode>.
      CHECK sy-subrc = 0.
      CASE <lv_vmode>.
        WHEN gc_alv_mode_append.
          <lt_style> = lt_style_insert.
        WHEN gc_alv_mode_modify.
          <lt_style> = lt_style_modify.
        WHEN space.
          <lt_style> = lt_style_normal.
      ENDCASE.

    ENDIF.
  ENDMETHOD.
  METHOD grid2_event_toolbar.

    DATA : ls_toolbar TYPE stb_button.
    ls_toolbar-butn_type = 3.  "Seperate line
    APPEND ls_toolbar TO p_object->mt_toolbar.
    IF gs_acc_table-zcreate IS NOT INITIAL.
      CLEAR ls_toolbar. "Important syntax
      ls_toolbar-function = 'SEL_INSERT'.
      ls_toolbar-icon      = icon_insert_row.
      ls_toolbar-text         = '추가'.
      APPEND ls_toolbar TO p_object->mt_toolbar.
    ENDIF.
    IF gs_acc_table-zupdate IS NOT INITIAL.
      CLEAR ls_toolbar. "Important syntax
      ls_toolbar-function = 'SEL_EDIT'.
      ls_toolbar-icon      = icon_edit_file.
      ls_toolbar-text         = '수정'.
      APPEND ls_toolbar TO p_object->mt_toolbar.
    ENDIF.
    IF gs_acc_table-zdelete IS NOT INITIAL.
      CLEAR ls_toolbar.
      ls_toolbar-function = 'SEL_DELETE'.
      ls_toolbar-icon      = icon_delete_row.
      ls_toolbar-text         = '삭제'.
      APPEND ls_toolbar TO p_object->mt_toolbar.
    ENDIF.

    IF gs_acc_table-zcreate IS NOT INITIAL OR gs_acc_table-zupdate IS NOT INITIAL..
      CLEAR ls_toolbar.
      ls_toolbar-function = 'SEL_SAVE'.
      ls_toolbar-icon      = icon_system_save.
      ls_toolbar-text         = '저장'.
      APPEND ls_toolbar TO p_object->mt_toolbar.
    ENDIF.

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
  METHOD grid2_fcat_build.
    DATA : ls_fcat TYPE lvc_s_fcat.


    LOOP AT ct_fcat INTO ls_fcat.
      IF ls_fcat-fieldname = 'VMODE'.
        ls_fcat-col_pos  = -1.
        ls_fcat-outputlen = 5.
        ls_fcat-edit = ' '.
        CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'MODE' CHANGING cs_fcat = ls_fcat ).
      ELSE.
        ls_fcat-edit   = gc_x.
      ENDIF.
      MODIFY ct_fcat FROM ls_fcat TRANSPORTING edit col_pos scrtext_s scrtext_m scrtext_l tooltip coltext.
    ENDLOOP.
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
        IF sy-tcode = 'ZOBJECT_BOOKMARKA'.
          CALL METHOD lcl_model=>acctable_select( CHANGING ct_acc_table = gt_list3 ).
          CALL SELECTION-SCREEN 2000 STARTING AT 10 10 ENDING AT 120 20.
        ENDIF.
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


    ENDCASE.
  ENDMETHOD.
  METHOD pbo_init_create_container.
    IF gv_first IS INITIAL.
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

*  HANDLE_DATA_CHANGED events are excuted when ALV Grid data changed
      CALL METHOD go_grid2->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.       "By Enter

      CALL METHOD go_grid2->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.    "By Changed


      SET HANDLER go_event->handle_toolbar
              FOR go_grid2.
      SET HANDLER go_event->handle_user_command
              FOR go_grid2.


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
      CALL METHOD lcl_model=>tree_select(
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
