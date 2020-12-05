*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC02
*&---------------------------------------------------------------------*

CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    CONSTANTS : mc_e TYPE c VALUE 'E',
                mc_x TYPE c VALUE 'X'.
    TYPES:
      BEGIN OF mty_s_treeinfo,
        fmkey TYPE fieldname,
        fukey TYPE fieldname,
        fname TYPE fieldname,
        fsort TYPE fieldname,
        fnode TYPE fieldname,
      END OF mty_s_treeinfo .
    METHODS :
*    CALL METHOD go_control->fcat_0100_build(
*      EXPORTING
*        it_list1 = gt_list1
*      CHANGING
*        ct_fcat1 = gt_fcat1
*    ).
      tree_draw
        IMPORTING
          !is_fieldinfo TYPE mty_s_treeinfo
        CHANGING
          !co_tree      TYPE REF TO cl_gui_alv_tree
          !ct_data      TYPE STANDARD TABLE
          !ct_expand    TYPE lvc_t_nkey ,
      tree_draw_image
        IMPORTING
          !it_data        TYPE STANDARD TABLE
          !is_data        TYPE any
          !is_fieldinfo   TYPE mty_s_treeinfo
        CHANGING
          !cs_node_layout TYPE lvc_s_layn
          !c_isfolder     TYPE char01           ,
      tree_draw_recursive
        IMPORTING
          !is_fieldinfo TYPE mty_s_treeinfo
          !is_upper     TYPE any
        EXPORTING
          !e_err_chk    TYPE char01
          !e_err_msg    TYPE char100
        CHANGING
          !co_tree      TYPE REF TO cl_gui_alv_tree
          !ct_data      TYPE STANDARD TABLE
          !ct_expand    TYPE STANDARD TABLE ,
      grid_0100_display
        CHANGING
          co_tree1 TYPE REF TO cl_gui_alv_tree
          ct_fcat1 TYPE lvc_t_fcat
          ct_list1 TYPE gty_t_tree
          ct_tree1 TYPE gty_t_tree
        ,

      fcat_0100_build
        IMPORTING
          it_list1 TYPE gty_t_tree
        CHANGING
          ct_fcat1 TYPE lvc_t_fcat,

      create_0100_container
        IMPORTING
          i_dynnr  TYPE sy-dynnr
          i_repid  TYPE sy-repid
        CHANGING
          co_tree1 TYPE REF TO cl_gui_alv_tree
          co_grid1 TYPE REF TO cl_gui_alv_grid.

ENDCLASS.
CLASS lcl_controller IMPLEMENTATION.
  METHOD tree_draw_recursive.
    DATA : lr_data        TYPE REF TO data,
           lv_node_text   TYPE lvc_value,
           ls_node_layout TYPE lvc_s_layn,
           lv_idx         TYPE sy-tabix,
           lv_tabix       TYPE sy-tabix,
           lv_err_chk     TYPE char01,
           lv_err_msg     TYPE char100,
           lv_nkey_rtn    TYPE lvc_nkey,
           lv_isfolder    TYPE c.

    FIELD-SYMBOLS : <ls_data>              TYPE any,
                    <ls_from>              TYPE any,

                    <lv_upper>             TYPE any,
                    <lv_upper_upper>       TYPE any,
                    <lv_upper_node>        TYPE any,

                    <lv_current>           TYPE any,
                    <lv_current_self>      TYPE any,
                    <lv_current_node_name> TYPE any,

                    <lv_value>             TYPE any.


    ASSIGN COMPONENT is_fieldinfo-fmkey OF STRUCTURE is_upper TO <lv_upper>.
    ASSIGN COMPONENT is_fieldinfo-fukey OF STRUCTURE is_upper TO <lv_upper_upper>.
    ASSIGN COMPONENT is_fieldinfo-fnode OF STRUCTURE is_upper TO <lv_upper_node>.

    READ TABLE ct_data REFERENCE INTO lr_data WITH KEY (is_fieldinfo-fukey) = <lv_upper>.
    CHECK sy-subrc = 0.
    lv_tabix = sy-tabix.
    ASSIGN lr_data->* TO <ls_from>.

    LOOP AT ct_data REFERENCE INTO lr_data FROM lv_tabix.
      ASSIGN lr_data->* TO <ls_data>.
      lv_idx = sy-tabix.
      ASSIGN COMPONENT is_fieldinfo-fukey OF STRUCTURE <ls_data> TO <lv_current>.
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT is_fieldinfo-fmkey OF STRUCTURE <ls_data> TO <lv_current_self>.
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT is_fieldinfo-fname OF STRUCTURE <ls_data> TO <lv_current_node_name>.
      CHECK sy-subrc = 0.

      IF <lv_upper> <> <lv_current>.
        EXIT.
      ENDIF.

      IF <lv_current_self> = <lv_upper_upper>.
        e_err_chk = mc_e.
        e_err_msg = '순환참조'.
        RETURN.
      ENDIF.

      CALL METHOD tree_draw_image
        EXPORTING
          it_data        = ct_data
          is_data        = <ls_data>
          is_fieldinfo   = is_fieldinfo
        CHANGING
          cs_node_layout = ls_node_layout
          c_isfolder     = lv_isfolder.

      lv_node_text = <lv_current_node_name>.

      CALL METHOD co_tree->add_node
        EXPORTING
          i_relat_node_key     = <lv_upper_node>
          i_relationship       = cl_gui_column_tree=>relat_last_child
          i_node_text          = lv_node_text
          is_node_layout       = ls_node_layout
          is_outtab_line       = <ls_data>
        IMPORTING
          e_new_node_key       = lv_nkey_rtn
        EXCEPTIONS
          relat_node_not_found = 1
          node_not_found       = 2
          OTHERS               = 3.

      ASSIGN COMPONENT is_fieldinfo-fnode OF STRUCTURE <ls_data> TO <lv_value>.
      <lv_value> = lv_nkey_rtn.

      IF lv_isfolder = mc_x.
        APPEND <lv_value> TO ct_expand.
      ENDIF.

      CALL METHOD tree_draw_recursive
        EXPORTING
          is_fieldinfo = is_fieldinfo
          is_upper     = <ls_data>
        IMPORTING
          e_err_chk    = lv_err_chk
          e_err_msg    = lv_err_msg
        CHANGING
          co_tree      = co_tree
          ct_data      = ct_data
          ct_expand    = ct_expand.

      IF lv_err_chk = mc_e.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD tree_draw_image.
    CONSTANTS : lc_data TYPE string VALUE 'IS_DATA-'.
    DATA : lv_fieldname TYPE string,
           lr_data      TYPE REF TO data.
    FIELD-SYMBOLS : <lv_value> TYPE any.
    CONCATENATE lc_data is_fieldinfo-fmkey INTO lv_fieldname.
    ASSIGN (lv_fieldname) TO <lv_value>.

    CHECK sy-subrc = 0.

    READ TABLE it_data REFERENCE INTO lr_data WITH KEY (is_fieldinfo-fukey) = <lv_value>.
    IF sy-subrc = 0.
      cs_node_layout-isfolder = mc_x.
      c_isfolder = mc_x.
    ELSE.
      CLEAR c_isfolder.
    ENDIF.
    cs_node_layout-n_image = icon_closed_folder.
    cs_node_layout-exp_image = icon_open_folder.
  ENDMETHOD.
  METHOD tree_draw.

    DATA : lr_data        TYPE REF TO data,
           lv_node_text   TYPE lvc_value,
           ls_node_layout TYPE lvc_s_layn,
           lv_nkey_rtn    TYPE lvc_nkey,
           lv_isfolder    TYPE c.
    DATA : lv_err_chk TYPE char01,
           lv_err_msg TYPE char100.
    DATA : lr_otab TYPE REF TO data.

    FIELD-SYMBOLS : <ls_data>  TYPE any,
                    <lt_data>  TYPE STANDARD TABLE,
                    <lv_value> TYPE any.

    CREATE DATA lr_data LIKE ct_data.
    ASSIGN lr_data->* TO <lt_data>.
    <lt_data> = ct_data.

    SORT <lt_data> BY (is_fieldinfo-fukey) (is_fieldinfo-fsort).
    READ TABLE <lt_data> REFERENCE INTO lr_data WITH KEY (is_fieldinfo-fukey) = ' ' BINARY SEARCH.
    CHECK sy-subrc = 0.
    ASSIGN lr_data->* TO <ls_data>.

    CALL METHOD tree_draw_image
      EXPORTING
        it_data        = <lt_data>
        is_data        = <ls_data>
        is_fieldinfo   = is_fieldinfo
      CHANGING
        cs_node_layout = ls_node_layout
        c_isfolder     = lv_isfolder.

    ASSIGN COMPONENT is_fieldinfo-fname OF STRUCTURE <ls_data> TO <lv_value>.
    lv_node_text = <lv_value>.

    CALL METHOD co_tree->add_node
      EXPORTING
        i_relat_node_key     = ' '
        i_relationship       = cl_gui_column_tree=>relat_last_child
        i_node_text          = lv_node_text
        is_node_layout       = ls_node_layout
        is_outtab_line       = <ls_data>
      IMPORTING
        e_new_node_key       = lv_nkey_rtn
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3.

    IF lv_isfolder = mc_x.
      ASSIGN COMPONENT is_fieldinfo-fnode OF STRUCTURE <ls_data> TO <lv_value>.
      <lv_value> = lv_nkey_rtn.
      APPEND <lv_value> TO ct_expand.
    ENDIF.

    CALL METHOD tree_draw_recursive
      EXPORTING
        is_fieldinfo = is_fieldinfo
        is_upper     = <ls_data>
      IMPORTING
        e_err_chk    = lv_err_chk
        e_err_msg    = lv_err_msg
      CHANGING
        co_tree      = co_tree
        ct_data      = <lt_data>
        ct_expand    = ct_expand.
    ct_data = <lt_data>.
  ENDMETHOD.
  METHOD grid_0100_display.
    DATA l_hierarchy_header TYPE treev_hhdr. "SET_TABLE_FOR_FIRST_DISPLAY 하면서 Header정보를 넣어주어야 하므로 구조체를 선언한다
    l_hierarchy_header-heading = 'NODE'(300).    "ALV의 [첫번째 Column]을 계층적으로 만들어줄 Header정보
    l_hierarchy_header-tooltip = 'NODE'(400).
    l_hierarchy_header-width = 50.
    l_hierarchy_header-width_pix = ' '.


    DATA : ls_fcat TYPE lvc_s_fcat.
    MODIFY ct_fcat1 FROM ls_fcat TRANSPORTING no_out WHERE fieldname > ' '.
    CALL METHOD go_tree1->set_table_for_first_display   "일반 ALV의 경우 output table에 데이타를 채운후 이 Method를 호출하나
      EXPORTING                                           "ALV Tree의 경우 null인 상태의  output table을 넘겨주워 화면을 display시킨다.
        is_hierarchy_header = l_hierarchy_header          "일반ALV에 [첫번째Column]을 계층적으로 사용하기 위한 Header정보를 받는다.
      CHANGING
        it_outtab           = ct_list1 "반드시 테이블은 Global 변수로 선언되어있어야 하며, 빈상태어야 한다.
        it_fieldcatalog     = ct_fcat1.  "FieldCatalog 1줄이라도 채워주어야 한다.


    DATA : ls_treeinfo TYPE mty_s_treeinfo.
    DATA :lt_expand TYPE lvc_t_nkey.
    ls_treeinfo-fmkey = 'GUID'.
    ls_treeinfo-fname = 'NAME'.
    ls_treeinfo-fukey = 'PARENT_GUID'.
    ls_treeinfo-fnode = 'NODE_KEY'.

    DATA lt_tree1 TYPE gty_t_tree.
    CALL METHOD lcl_model=>get_nodes(
      EXPORTING
        i_usr    = sy-uname
      CHANGING
        ct_nodes = lt_tree1
    ).

    CALL METHOD tree_draw
      EXPORTING
        is_fieldinfo = ls_treeinfo
      CHANGING
        co_tree      = co_tree1
        ct_data      = lt_tree1
        ct_expand    = lt_expand.

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
*
*    DATA : lt_list2 TYPE gty_t_list.
*      PERFORM fc_get_data_2nd CHANGING lt_list2.
  ENDMETHOD.
  METHOD fcat_0100_build.
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


        WHEN 'VMODE'.
          ls_fcat-no_out   = gc_x.
          ls_fcat-col_pos  = 0.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'MODE' CHANGING cs_fcat = ls_fcat ).

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
  METHOD create_0100_container.
    "-----------------------------------------
    " Screen 100에 Container를 생성한다.
    "-----------------------------------------
    DATA : lo_dock  TYPE REF TO cl_gui_docking_container,
           lo_left  TYPE REF TO cl_gui_container,
           lo_right TYPE REF TO cl_gui_container,
           lo_easy  TYPE REF TO cl_gui_easy_splitter_container.
    CREATE OBJECT lo_dock
      EXPORTING
        dynnr     = i_dynnr
        repid     = i_repid
        side      = cl_gui_docking_container=>dock_at_top
        extension = 6000.

    CREATE OBJECT lo_easy
      EXPORTING
        parent        = lo_dock
        orientation   = 1
        sash_position = 30.

    lo_left = lo_easy->top_left_container.
    lo_right = lo_easy->bottom_right_container.


    CREATE OBJECT co_tree1
      EXPORTING
        parent = lo_left.

    CREATE OBJECT co_grid1
      EXPORTING
        i_parent = lo_right.

  ENDMETHOD.
ENDCLASS.
