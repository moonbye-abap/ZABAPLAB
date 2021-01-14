*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC00
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC07
*&---------------------------------------------------------------------*
class lcl_tree_assist definition
*  public
  create public .
  .

public section.

  interfaces YIF_COMMONS .

  aliases MC_A
    for YIF_COMMONS~MC_A .
  aliases MC_ASTR
    for YIF_COMMONS~MC_ASTR .
  aliases MC_BT
    for YIF_COMMONS~MC_BT .
  aliases MC_C
    for YIF_COMMONS~MC_C .
  aliases MC_CHAR
    for YIF_COMMONS~MC_CHAR .
  aliases MC_CP
    for YIF_COMMONS~MC_CP .
  aliases MC_E
    for YIF_COMMONS~MC_E .
  aliases MC_ENTER
    for YIF_COMMONS~MC_ENTER .
  aliases MC_EQ
    for YIF_COMMONS~MC_EQ .
  aliases MC_ESC
    for YIF_COMMONS~MC_ESC .
  aliases MC_GE
    for YIF_COMMONS~MC_GE .
  aliases MC_GT
    for YIF_COMMONS~MC_GT .
  aliases MC_H
    for YIF_COMMONS~MC_H .
  aliases MC_HYPHEN
    for YIF_COMMONS~MC_HYPHEN .
  aliases MC_H_LOWER
    for YIF_COMMONS~MC_H_LOWER .
  aliases MC_I
    for YIF_COMMONS~MC_I .
  aliases MC_ICON
    for YIF_COMMONS~MC_ICON .
  aliases MC_KIND
    for YIF_COMMONS~MC_KIND .
  aliases MC_LE
    for YIF_COMMONS~MC_LE .
  aliases MC_LEFT_BRACE
    for YIF_COMMONS~MC_LEFT_BRACE .
  aliases MC_LT
    for YIF_COMMONS~MC_LT .
  aliases MC_NB
    for YIF_COMMONS~MC_NB .
  aliases MC_NE
    for YIF_COMMONS~MC_NE .
  aliases MC_P
    for YIF_COMMONS~MC_P .
  aliases MC_RIGHT_BRACE
    for YIF_COMMONS~MC_RIGHT_BRACE .
  aliases MC_S
    for YIF_COMMONS~MC_S .
  aliases MC_TEXT
    for YIF_COMMONS~MC_TEXT .
  aliases MC_TILDE
    for YIF_COMMONS~MC_TILDE .
  aliases MC_VALUE
    for YIF_COMMONS~MC_VALUE .
  aliases MC_X
    for YIF_COMMONS~MC_X .

  types:
    BEGIN OF mty_s_dragdropbag,
              mkey  TYPE char50,
              umkey TYPE char50,
            END OF mty_s_dragdropbag .
  types:
    mty_t_dragdropbag TYPE STANDARD TABLE OF mty_s_dragdropbag WITH KEY mkey .
  types:
    BEGIN OF mty_s_eventdragdrop,
              tree TYPE REF TO cl_gui_alv_tree,
              bags TYPE mty_t_dragdropbag,
            END OF mty_s_eventdragdrop .
  types:
    mty_t_eventdragdrop TYPE STANDARD TABLE OF mty_s_eventdragdrop WITH KEY tree .
  types:
    BEGIN OF mty_s_toolbar ,
        fcode       TYPE sy-ucomm,
        icon        TYPE icon_d,
        text        TYPE text40,
        quickinfo   TYPE iconquick,
        ufcode      TYPE sy-ucomm,
        is_disabled TYPE c LENGTH 1,
        reftoolbar  TYPE REF TO cl_gui_toolbar,
      END OF mty_s_toolbar .
  types:
    mty_t_toolbar TYPE STANDARD TABLE OF mty_s_toolbar .
  types:
    BEGIN OF mty_s_treeinfo,
        ficon TYPE fieldname,
        fmkey TYPE fieldname,
        fukey TYPE fieldname,
        fname TYPE fieldname,
        fsort TYPE fieldname,
        fnode TYPE fieldname,
      END OF mty_s_treeinfo .
  types:
    BEGIN OF mty_s_treeicon,
        gubn      TYPE fieldname,
        n_image   TYPE icon_d,
        exp_image TYPE icon_d,
        isfolder  TYPE c LENGTH 1,
      END OF mty_s_treeicon .
  types:
    mty_t_treeicon TYPE STANDARD TABLE OF mty_s_treeicon .

  data MT_TOOLBAR type MTY_T_TOOLBAR .
  data MT_CONTEXTMENU type MTY_T_TOOLBAR .
  data MO_DRAG type ref to CL_DRAGDROP .
  data MO_DROP type ref to CL_DRAGDROP .
  data MT_DRAGDROP_NODES type LVC_T_NKEY .
  class-data MT_DRAGDROP_NODES_CHANGE type MTY_T_EVENTDRAGDROP .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_TREE type ref to CL_GUI_ALV_TREE .
  data MT_TREE type ref to DATA .
  data MS_TREEINFO type MTY_S_TREEINFO .
  data MT_TREEICON type MTY_T_TREEICON .
  data M_DRAGDROP type CHAR01 .
  data M_DRAGDROP_FCODE type SY-UCOMM .

  methods CONSTRUCTOR
    importing
      !IO_TREE type ref to CL_GUI_ALV_TREE optional
      !IT_TREE type ref to DATA
      !IS_TREEINFO type MTY_S_TREEINFO
      !IT_TREEICON type MTY_T_TREEICON optional
      !IT_TOOLBAR type MTY_T_TOOLBAR optional
      !IT_CONTEXTMENU type MTY_T_TOOLBAR optional
      !I_DRAGDROP type CHAR1 default ' '
      !I_DRAGDROP_FCODE type SY-UCOMM optional .
  methods RECALC_BOTTOMUP
    changing
      !CT_TREE type STANDARD TABLE .
  methods DRAW
    importing
      !IT_SOURCE type STANDARD TABLE
    changing
      !CT_EXPAND type LVC_T_NKEY .
  methods REDRAW
    importing
      !IT_SOURCE type STANDARD TABLE optional
    changing
      !CT_EXPAND type LVC_T_NKEY .
  methods DELETE_SUBTREE
    importing
      !IT_NODE type LVC_T_NKEY
    exporting
      !ET_TREE_DELETED type STANDARD TABLE .
  methods GET_NODE_TOPDOWN
    importing
      !IS_TREE type ANY
    exporting
      !E_ERR_CHK type CHAR01
      !E_ERR_MSG type CHAR100
    changing
      !CT_RETURN type STANDARD TABLE .
  methods GET_SELECTED_NODES
    returning
      value(RT_NODE) type LVC_T_NKEY .
  methods ADD_ONE_NODE_CHILD
    importing
      !I_RELAT_NODE_KEY type ANY
      !I_NODE_TEXT type ANY
      !IS_OUTTAB_LINE type ANY
    exporting
      !E_NEW_NODE_KEY type LVC_NKEY .
  class-methods GET_FCAT
    importing
      !IS_FCAT type LVC_S_FCAT optional
      !IV_DDIC_OBJECT type ANY optional
      !IS_DATA type ANY optional
      !IT_DATA type ANY TABLE optional
      !IOR_DREF type DATA optional
    returning
      value(RT_FCAT) type LVC_T_FCAT
    exceptions
      NO_STRUCTURE
      INVALID_TYPE .
  methods HANDLE_TOOLBAR_SELECTED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods HANDLE_TOOLBAR_DROPDOWN
    for event DROPDOWN_CLICKED of CL_GUI_TOOLBAR
    importing
      !FCODE
      !POSX
      !POSY .
  methods HANDLE_NODE_CM_REQ
    for event NODE_CONTEXT_MENU_REQUEST of CL_GUI_ALV_TREE
    importing
      !NODE_KEY
      !MENU .
  methods HANDLE_ITEM_CM_REQ
    for event ITEM_CONTEXT_MENU_REQUEST of CL_GUI_ALV_TREE
    importing
      !NODE_KEY
      !MENU .
  methods HANDLE_NODE_CM_SEL
    for event NODE_CONTEXT_MENU_SELECTED of CL_GUI_ALV_TREE
    importing
      !NODE_KEY
      !FCODE
      !SENDER .
  methods HANDLE_ITEM_CM_SEL
    for event ITEM_CONTEXT_MENU_SELECTED of CL_GUI_ALV_TREE
    importing
      !FIELDNAME
      !NODE_KEY
      !FCODE .
  methods HANDLE_DRAG_MULTIPLE
    for event ON_DRAG_MULTIPLE of CL_GUI_ALV_TREE
    importing
      !SENDER
      !NODE_KEY_TABLE
      !FIELDNAME
      !DRAG_DROP_OBJECT .
  methods HANDLE_DROP
    for event ON_DROP of CL_GUI_ALV_TREE
    importing
      !SENDER
      !NODE_KEY
      !DRAG_DROP_OBJECT .
  PROTECTED SECTION.
private section.

  data M_REDRAW type CHAR01 .

  methods DRAW_RECURSIVE
    importing
      !IS_UPPER type ANY
    exporting
      !E_ERR_CHK type CHAR01
      !E_ERR_MSG type CHAR100
    changing
      !CT_SOURCE type STANDARD TABLE
      !CT_EXPAND type STANDARD TABLE .
  methods DRAW_IMAGE
    importing
      !IT_SOURCE type STANDARD TABLE
      !IS_SOURCE type ANY
    changing
      !CS_NODE_LAYOUT type LVC_S_LAYN
      !C_ISFOLDER type CHAR01 .
  methods ADD_TOOLBAR .
  methods ADD_CONTEXTMENU .
  methods ADD_NODE
    importing
      !I_RELAT_NODE_KEY type LVC_NKEY
      !I_RELATIONSHIP type INT4
      !IS_OUTTAB_LINE type ANY optional
      !IS_NODE_LAYOUT type LVC_S_LAYN optional
      !IT_ITEM_LAYOUT type LVC_T_LAYI optional
      !I_NODE_TEXT type LVC_VALUE optional
    exporting
      !E_NEW_NODE_KEY type LVC_NKEY
    exceptions
      RELAT_NODE_NOT_FOUND
      NODE_NOT_FOUND .
ENDCLASS.



CLASS lcl_tree_assist IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method lcl_tree_assist->ADD_CONTEXTMENU
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_contextmenu.
*--------------------------------------------------------------------------------
* Tree에 Context Menu를 장착해 준다.
*    추가된 Context Menu를 선택하면 Screen PAI로 FCODE를 넘겨준다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.09
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* mt_toolbar
*    ---------------------------------------------------------------
*      FCODE  , ICON            , TEXT , quickinfo , UFCODE    , is_disabled, reftoolbar
*    ---------------------------------------------------------------
*      APPEND , ICON_INSERT_ROW , 추가   , 추가
*      DELETE , ICON_DELETE_ROW , 삭제   , 삭제
*--------------------------------------------------------------------------------
*  CALL METHOD lcl_tree_assist=>add_contextmenu( )
*--------------------------------------------------------------------------------
    CHECK mt_toolbar IS NOT INITIAL.

    DATA : lo_event TYPE REF TO lcl_tree_assist.
    CREATE OBJECT lo_event
      EXPORTING
        io_tree     = mo_tree
        it_tree     = mt_tree
        is_treeinfo = ms_treeinfo.

    SET HANDLER lo_event->handle_node_cm_req        "Context Menu Request
            FOR mo_tree.

    SET HANDLER lo_event->handle_item_cm_req        "Context Menu Request
            FOR mo_tree.

    SET HANDLER lo_event->handle_node_cm_sel        "Context Menu Request
            FOR mo_tree.

    SET HANDLER lo_event->handle_item_cm_sel        "Context Menu Request
            FOR mo_tree.

    lo_event->mt_contextmenu = mt_contextmenu.

*    mo_event01 ?= lo_event.
    CALL METHOD mo_tree->get_registered_events
      IMPORTING
        events = DATA(lt_event).

    READ TABLE lt_event TRANSPORTING NO FIELDS WITH KEY eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
    IF sy-subrc <> 0.
      APPEND VALUE #( eventid = cl_gui_column_tree=>eventid_node_context_menu_req ) TO lt_event.
    ENDIF.

    READ TABLE lt_event TRANSPORTING NO FIELDS WITH KEY eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
    IF sy-subrc <> 0.
      APPEND VALUE #( eventid = cl_gui_column_tree=>eventid_item_context_menu_req ) TO lt_event.
    ENDIF.



    "go_tree1의 추가된 event set정보를 등록해 준다.
    CALL METHOD mo_tree->set_registered_events
      EXPORTING
        events                    = lt_event
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.
    IF sy-subrc <> 0.
      "go_tree1의 추가된 event set정보를 등록해 준다.
      DELETE lt_event WHERE eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
      CALL METHOD mo_tree->set_registered_events
        EXPORTING
          events                    = lt_event
        EXCEPTIONS
          cntl_error                = 1
          cntl_system_error         = 2
          illegal_event_combination = 3.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method lcl_tree_assist->ADD_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RELAT_NODE_KEY               TYPE        LVC_NKEY
* | [--->] I_RELATIONSHIP                 TYPE        INT4
* | [--->] IS_OUTTAB_LINE                 TYPE        ANY(optional)
* | [--->] IS_NODE_LAYOUT                 TYPE        LVC_S_LAYN(optional)
* | [--->] IT_ITEM_LAYOUT                 TYPE        LVC_T_LAYI(optional)
* | [--->] I_NODE_TEXT                    TYPE        LVC_VALUE(optional)
* | [<---] E_NEW_NODE_KEY                 TYPE        LVC_NKEY
* | [EXC!] RELAT_NODE_NOT_FOUND
* | [EXC!] NODE_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_node.
*--------------------------------------------------------------------------------
* Tree에 add_node method를 수행한다.
*    Drag&Drop설정된 경우 각각의 Handler를 적재하여 add_node를 수행한다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
*  CALL METHOD add_node
*    EXPORTING
*      i_relat_node_key     = <lv_upper_node>
*      i_relationship       = cl_gui_column_tree=>relat_last_child
*      i_node_text          = lv_node_text
*      is_node_layout       = ls_node_layout
*      is_outtab_line       = <ls_data>
*    IMPORTING
*      e_new_node_key       = lv_nkey_rtn
*    EXCEPTIONS
*      relat_node_not_found = 1
*      node_not_found       = 2
*      OTHERS               = 3.
*--------------------------------------------------------------------------------

    DATA : ls_node_layout TYPE lvc_s_layn.
    ls_node_layout = is_node_layout.
    IF m_dragdrop IS NOT INITIAL.
      ASSIGN COMPONENT ms_treeinfo-ficon OF STRUCTURE is_outtab_line TO FIELD-SYMBOL(<lv_gubn>).
      IF sy-subrc = 0.
        READ TABLE mt_treeicon INTO DATA(ls_treeicon) WITH KEY gubn = <lv_gubn>.
        IF sy-subrc = 0.
          CASE ls_treeicon-isfolder .
            WHEN mc_x.
              CALL METHOD mo_drop->get_handle
                IMPORTING
                  handle = DATA(lv_handle).
            WHEN space.
              CALL METHOD mo_drag->get_handle
                IMPORTING
                  handle = lv_handle.
          ENDCASE.
          ls_node_layout-dragdropid = lv_handle.
        ENDIF.
      ENDIF.


    ENDIF.
    CALL METHOD mo_tree->add_node
      EXPORTING
        i_relat_node_key     = i_relat_node_key
        i_relationship       = i_relationship
        i_node_text          = i_node_text
        is_node_layout       = ls_node_layout
        is_outtab_line       = is_outtab_line
      IMPORTING
        e_new_node_key       = e_new_node_key
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->ADD_ONE_NODE_CHILD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RELAT_NODE_KEY               TYPE        ANY
* | [--->] I_NODE_TEXT                    TYPE        ANY
* | [--->] IS_OUTTAB_LINE                 TYPE        ANY
* | [<---] E_NEW_NODE_KEY                 TYPE        LVC_NKEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_one_node_child.
*--------------------------------------------------------------------------------
* Tree에 단 1개의 Node를 추가해 준다.
*    추가된 Node는 화면상으로 펼쳐지고, 추가된 Node가 focus(selected)를 갖는다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* i_relat_node_key : 상위 Node
* i_node_text : 신규 Node의 text
* is_outtab_line : 신규 Node의 ALV List정보
* e_new_node_key : 신규 추가된 고유 node Key
*--------------------------------------------------------------------------------
*        CALL METHOD lcl_tree_assist=>add_one_node_child
*          EXPORTING
*            i_relat_node_key = gs_tree_add-node
*            i_node_text      = gs_tree_add-name
*            ls_node_layout   = ls_node_layout
*            is_outtab_line   = ls_list1
*          IMPORTING
*            e_new_node_key   = DATA(lv_nkey_rtn).
*--------------------------------------------------------------------------------

    FIELD-SYMBOLS : <mt_tree> type STANDARD TABLE.
    DATA : lr_data        TYPE REF TO data,
           ls_node_layout TYPE lvc_s_layn.
    DATA : lt_node TYPE lvc_t_nkey,
           lv_idx  TYPE sy-tabix.
    ASSIGN mt_tree->* to <mt_tree>.
    ASSIGN COMPONENT ms_treeinfo-ficon OF STRUCTURE is_outtab_line TO FIELD-SYMBOL(<lv_gubn>).
    IF sy-subrc = 0.
      READ TABLE mt_treeicon INTO DATA(ls_treeicon) WITH KEY gubn = <lv_gubn>.
      IF sy-subrc = 0.
        ls_node_layout = CORRESPONDING #( ls_treeicon ).

      ENDIF.
    ENDIF.
    CALL METHOD add_node
      EXPORTING
        i_relat_node_key     = CONV #( i_relat_node_key )
        i_relationship       = cl_gui_column_tree=>relat_last_child
        i_node_text          = CONV #( i_node_text )
        is_node_layout       = ls_node_layout
        is_outtab_line       = is_outtab_line
      IMPORTING
        e_new_node_key       = DATA(lv_nkey_rtn)
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3.

    IF sy-subrc = 0.
      e_new_node_key = lv_nkey_rtn.
      lv_idx = lines( <mt_tree> ).

      READ TABLE <mt_tree> REFERENCE INTO lr_data INDEX lv_idx.
      ASSIGN lr_data->* TO FIELD-SYMBOL(<ls_data>).
      ASSIGN COMPONENT ms_treeinfo-fnode OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_node>).
      <lv_node> = lv_nkey_rtn.
*      data(ls_data) = <mt_tree>[ lv_idx ].
      IF ls_node_layout-isfolder = mc_x.
        lt_node = VALUE #( ( lv_nkey_rtn ) ).
      ELSE.
        lt_node = VALUE #( ( i_relat_node_key ) ).
      ENDIF.
      mo_tree->frontend_update( ).

      CALL METHOD mo_tree->expand_nodes
        EXPORTING
          it_node_key             = lt_node
        EXCEPTIONS
          failed                  = 1
          cntl_system_error       = 2
          error_in_node_key_table = 3
          dp_error                = 4
          node_not_found          = 5
          OTHERS                  = 6.

      lt_node = VALUE #( ( lv_nkey_rtn ) ).
      CALL METHOD mo_tree->set_selected_nodes
        EXPORTING
          it_selected_nodes = lt_node.

    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method lcl_tree_assist->ADD_TOOLBAR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_toolbar.
*--------------------------------------------------------------------------------
* Tree에 Toolbar를 장착해 준다.
*    추가된 Toolbar를 선택하면 Screen PAI로 FCODE를 넘겨준다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.09
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* mt_toolbar
*    fcode에 [ - ]만 입력하면 Seperate 버튼을 삽입한다.
*    Screen(100)에 Tree가 2개 이상인 경우 FCODE가 중복되면 절대로 안된다.
*    reftoolbar는 자동으로 toolbar의 referece가 채워진다.
*    ---------------------------------------------------------------
*      FCODE  , ICON            , TEXT , quickinfo , UFCODE    , is_disabled, reftoolbar
*    ---------------------------------------------------------------
*      APPEND , ICON_INSERT_ROW , 추가   , 추가
*      DELETE , ICON_DELETE_ROW , 삭제   , 삭제
*      -
*      GITA   , ICON_DELETE_ROW , 버튼들  , 버튼들
*      GITA1  ,                  , 버튼1  , 버튼1       , GITA
*      GITA2  ,                  , 버튼2  , 버튼2       , GITA    , 'X'
*      GITA3  ,                  , 버튼3  , 버튼3       , GITA
*--------------------------------------------------------------------------------
*      CALL METHOD lcl_tree_assist=>add_toolbar( )
*--------------------------------------------------------------------------------
    CHECK mo_tree IS NOT INITIAL.
    DATA : lo_event TYPE REF TO lcl_tree_assist.
    CREATE OBJECT lo_event
      EXPORTING
        io_tree     = mo_tree
        it_tree     = mt_tree
        is_treeinfo = ms_treeinfo.

    DATA : lv_butn_type TYPE tb_btype,
           ls_toolbars  LIKE LINE OF mt_toolbar
           .
    IF mo_toolbar IS INITIAL.
      CALL METHOD mo_tree->get_toolbar_object
        IMPORTING
          er_toolbar = mo_toolbar.
    ENDIF.

    LOOP AT mt_toolbar ASSIGNING FIELD-SYMBOL(<ls_toolbar>) WHERE ufcode IS INITIAL.
      CASE <ls_toolbar>-fcode.
        WHEN mc_hyphen.
          lv_butn_type = cntb_btype_sep.
        WHEN OTHERS.
          READ TABLE mt_toolbar TRANSPORTING NO FIELDS WITH KEY ufcode = <ls_toolbar>-fcode.
          IF sy-subrc = 0.
            lv_butn_type = cntb_btype_menu.
          ELSE.
            lv_butn_type = cntb_btype_button.
          ENDIF.
      ENDCASE.
      CALL METHOD mo_toolbar->add_button
        EXPORTING
          fcode       = <ls_toolbar>-fcode
          icon        = <ls_toolbar>-icon
          "Button Type은 Domain값을 통해확인할수 있다.[ 3 : 구분선임]
          butn_type   = lv_butn_type
          text        = <ls_toolbar>-text
          quickinfo   = <ls_toolbar>-quickinfo
          is_disabled = <ls_toolbar>-is_disabled
          "Tree를 그려준다.
        .
    ENDLOOP.

    "Event
    DATA(lt_toolbars) = mt_toolbar.
    DELETE lt_toolbars WHERE fcode = mc_hyphen.
    GET REFERENCE OF mo_toolbar INTO DATA(lv_ref).
    ls_toolbars-reftoolbar = lv_ref->*.
    MODIFY lt_toolbars FROM ls_toolbars TRANSPORTING reftoolbar WHERE fcode > ' '.

    lo_event->mt_toolbar = lt_toolbars.

    SET HANDLER lo_event->handle_toolbar_dropdown FOR mo_toolbar.
    SET HANDLER lo_event->handle_toolbar_selected FOR mo_toolbar.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_TREE                        TYPE REF TO CL_GUI_ALV_TREE(optional)
* | [--->] IT_TREE                        TYPE REF TO DATA
* | [--->] IS_TREEINFO                    TYPE        MTY_S_TREEINFO
* | [--->] IT_TREEICON                    TYPE        MTY_T_TREEICON(optional)
* | [--->] IT_TOOLBAR                     TYPE        MTY_T_TOOLBAR(optional)
* | [--->] IT_CONTEXTMENU                 TYPE        MTY_T_TOOLBAR(optional)
* | [--->] I_DRAGDROP                     TYPE        CHAR1 (default =' ')
* | [--->] I_DRAGDROP_FCODE               TYPE        SY-UCOMM(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
*    ---------------------------------------------------------------
*    is_treeinfo  ( add_new의 outtab_line의 핵심구조 )
*    ---------------------------------------------------------------
*      fmkey : FieldName( Main Key )
*      fname : FieldName( 계층구조상에 보여져야 할 이름 )
*      ficon : FieldName( 계층구조상에 보여져야 하는 ICON의 모양을 결정하는 정보 ) => mt_treeicon (연동)
*      fukey : FieldName( Upper Main Key )
*      fnode : FieldName( 계층구조상에 add_node로 추가된 이후 리턴된 Node번호를 가져야 하는 필드 )
*    ---------------------------------------------------------------

*    ---------------------------------------------------------------
*    it_treeicon  is_treeinfo-ficon 의 값에 따라 보여져야 하는 ICON정보.
*    ---------------------------------------------------------------
*      GUBN   , N_IMAGE            , EXP_IMAGE               ,isfolder
*    ---------------------------------------------------------------
*      N      , icon_closed_folder , icon_open_folder        , X [인 항목은 Drop대상이 된다.)
*      T      , icon_database_table , icon_database_table    , space [인 항목은 Drag대상이 된다. )
*    ---------------------------------------------------------------

*    ---------------------------------------------------------------
*    mt_toolbar [ Toolbar ]세팅을 원하는 경우.
*    ---------------------------------------------------------------
*      FCODE  , ICON            , TEXT , quickinfo , UFCODE    , is_disabled, reftoolbar
*    ---------------------------------------------------------------
*      APPEND , ICON_INSERT_ROW , 추가   , 추가
*      DELETE , ICON_DELETE_ROW , 삭제   , 삭제
*      -
*      GITA   , ICON_DELETE_ROW , 버튼들  , 버튼들
*      GITA1  ,                  , 버튼1  , 버튼1       , GITA
*      GITA2  ,                  , 버튼2  , 버튼2       , GITA    , 'X'
*      GITA3  ,                  , 버튼3  , 버튼3       , GITA
*    --------------------------------------------------------------------------------

*    --------------------------------------------------------------------------------
*     mt_contextmenu  [Context Menu구성을 원하는 경우]
*    ---------------------------------------------------------------
*      FCODE  , ICON            , TEXT , quickinfo , UFCODE    , is_disabled, reftoolbar
*    ---------------------------------------------------------------
*      APPEND , ICON_INSERT_ROW , 추가   , 추가
*      DELETE , ICON_DELETE_ROW , 삭제   , 삭제
*    --------------------------------------------------------------------------------

    mo_tree ?= io_tree.
    mt_tree = it_tree.
    ms_treeinfo = is_treeinfo.
    mt_treeicon = it_treeicon.
    mt_toolbar  = it_toolbar.
    mt_contextmenu = it_contextmenu.
    m_dragdrop = i_dragdrop.
    m_dragdrop_fcode = i_dragdrop_fcode.

    IF mt_toolbar IS NOT INITIAL.
      CALL METHOD add_toolbar( ).
    ENDIF.
    IF mt_contextmenu IS NOT INITIAL.
      CALL METHOD add_contextmenu( ).
    ENDIF.

    IF m_dragdrop IS NOT INITIAL.
      DATA : lo_event TYPE REF TO lcl_tree_assist.
      CREATE OBJECT mo_drag.
      DATA(lv_effect) = cl_dragdrop=>move.
      CALL METHOD mo_drag->add
        EXPORTING
          flavor     = 'movewithtree'                                    "#EC NOTEXT
          dragsrc    = mc_x
          droptarget = space
          effect     = lv_effect.

      CREATE OBJECT mo_drop.
      CALL METHOD mo_drop->add
        EXPORTING
          flavor     = 'movewithtree'                                    "#EC NOTEXT
          dragsrc    = space
          droptarget = mc_x
          effect     = lv_effect.

      CREATE OBJECT lo_event
        EXPORTING
          is_treeinfo = ms_treeinfo
          i_dragdrop_fcode = m_dragdrop_fcode
          it_tree     = mt_tree
          io_tree     = mo_tree.
      SET HANDLER lo_event->handle_drop FOR mo_tree.
      SET HANDLER lo_event->handle_drag_multiple FOR mo_tree.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->DELETE_SUBTREE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_NODE                        TYPE        LVC_T_NKEY
* | [<---] ET_TREE_DELETED                TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delete_subtree.
*--------------------------------------------------------------------------------
* Tree에 주어진 Node를 삭제한다. (Sub Node포함)
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.11
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* it_node : 삭제할 Node를 입력받는다.
* et_tree_deleted : 삭제된 전체 Data를 리턴해 준다.
*--------------------------------------------------------------------------------
*        CALL METHOD lcl_tree_assist=>tree_delete_subtree
*          EXPORTING
*            it_node         = lt_node
*          IMPORTING
*            et_node_deleted = DATA(lt_node_deleted).
*--------------------------------------------------------------------------------

    FIELD-SYMBOLS : <mt_tree> type STANDARD TABLE.
    DATA : lt_subs TYPE lvc_t_nkey.
    ASSIGN mt_tree->* to <mt_tree>.
    LOOP AT it_node INTO DATA(ls_node).
      CALL METHOD mo_tree->get_subtree
        EXPORTING
          i_node_key       = ls_node
        IMPORTING
          et_subtree_nodes = DATA(lt_sub).
      INSERT LINES OF lt_sub INTO TABLE lt_subs.
    ENDLOOP.
    SORT lt_subs.
    DELETE ADJACENT DUPLICATES FROM lt_subs.
    LOOP AT lt_subs INTO DATA(ls_subs).
      READ TABLE <mt_tree> ASSIGNING FIELD-SYMBOL(<ls_data>) WITH KEY (ms_treeinfo-fnode) = ls_subs BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <ls_data> TO et_tree_deleted.
      ENDIF.
    ENDLOOP.

    LOOP AT it_node INTO ls_node.
      CALL METHOD mo_tree->delete_subtree
        EXPORTING
          i_node_key                = ls_node
          i_update_parents_expander = space
          i_update_parents_folder   = space
        EXCEPTIONS
          node_key_not_in_model     = 1
          OTHERS                    = 2.
      IF sy-subrc <> 0.
*         Implement suitable error handling here
      ENDIF.
    ENDLOOP.
    CALL METHOD mo_tree->frontend_update( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->DRAW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SOURCE                      TYPE        STANDARD TABLE
* | [<-->] CT_EXPAND                      TYPE        LVC_T_NKEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD draw.
*-----------------------------------------------------------------------------------
* Tree를 그려주는 Method이다.
*-----------------------------------------------------------------------------------
* 작성일 : 2021.01.11
* 작성자 : moonbye
*-----------------------------------------------------------------------------------
* it_source : 화면상에 보여줄 정보를 가지고 있는 Source용 itab이다.
* ct_expand : ROOT를 기준으로 전체를 펼친다고 가정할때 펼쳐야 하는 항목의 NODE정보이다.
*-----------------------------------------------------------------------------------
*  CALL METHOD lcl_tree_assist=>Draw
*    Exporting
*      it_source     = lt_list2   [set_table_for_first_display]의 gt_list와 다른 Local itab을 지정하는것이 맞다.
*    Changing
*      ct_expand    = gt_expand
*      .
*-----------------------------------------------------------------------------------


    DATA : lr_data        TYPE REF TO data,
           lv_node_text   TYPE lvc_value,
           ls_node_layout TYPE lvc_s_layn,
           lv_nkey_rtn    TYPE lvc_nkey,
           lv_idx         TYPE sy-tabix,
           lv_isfolder    TYPE c.
    DATA : lv_err_chk TYPE char01,
           lv_err_msg TYPE char100.
    DATA : lr_new     TYPE REF TO data.

    FIELD-SYMBOLS : <mt_tree>  TYPE STANDARD TABLE,
                    <ls_data>  TYPE any,
                    <ls_new>   TYPE any,
                    <lt_data>  TYPE STANDARD TABLE,
                    <lv_value> TYPE any.

    ASSIGN mt_tree->* TO <mt_tree>.
    CREATE DATA lr_data LIKE it_source.
    ASSIGN lr_data->* TO <lt_data>.
    <lt_data> = it_source.
    SORT <lt_data> BY (ms_treeinfo-fmkey).

    IF m_redraw IS NOT INITIAL.
      CALL METHOD mo_tree->delete_all_nodes( ).
      m_redraw = ' '.
    ENDIF.
    SORT <lt_data> BY (ms_treeinfo-fukey) (ms_treeinfo-fsort).
    READ TABLE <lt_data> REFERENCE INTO lr_data WITH KEY (ms_treeinfo-fukey) = ' ' BINARY SEARCH.
    CHECK sy-subrc = 0.
    ASSIGN lr_data->* TO <ls_data>.
    ASSIGN COMPONENT ms_treeinfo-fnode OF STRUCTURE <ls_data> to <lv_value>.
    clear <lv_value>.
    CALL METHOD draw_image
      EXPORTING
        it_source      = <lt_data>
        is_source      = <ls_data>
      CHANGING
        cs_node_layout = ls_node_layout
        c_isfolder     = lv_isfolder.

    ASSIGN COMPONENT ms_treeinfo-fname OF STRUCTURE <ls_data> TO <lv_value>.
    lv_node_text = <lv_value>.

    CALL METHOD add_node
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

    lv_idx = lines( <mt_tree> ).
    READ TABLE <mt_tree> REFERENCE INTO lr_new INDEX lv_idx.
    ASSIGN lr_new->* TO <ls_new>.
    ASSIGN COMPONENT ms_treeinfo-fnode OF STRUCTURE <ls_new> TO <lv_value>.
    <lv_value> = lv_nkey_rtn.

    IF lv_isfolder = mc_x.
      ASSIGN COMPONENT ms_treeinfo-fnode OF STRUCTURE <ls_data> TO <lv_value>.
      <lv_value> = lv_nkey_rtn.
      APPEND <lv_value> TO ct_expand.
    ENDIF.

    CALL METHOD draw_recursive
      EXPORTING
        is_upper  = <ls_data>
*        it_source = <lt_data>
      IMPORTING
        e_err_chk = lv_err_chk
        e_err_msg = lv_err_msg
      CHANGING
        ct_source = <lt_data>
        ct_expand = ct_expand.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method lcl_tree_assist->DRAW_IMAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SOURCE                      TYPE        STANDARD TABLE
* | [--->] IS_SOURCE                      TYPE        ANY
* | [<-->] CS_NODE_LAYOUT                 TYPE        LVC_S_LAYN
* | [<-->] C_ISFOLDER                     TYPE        CHAR01
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD draw_image.
*--------------------------------------------------------------------------------
* draw의 SubRoutine으로 해당 Node의 Image를 결정한다.
* cl_gui_alv_tree->add_node에 사용되는 [node layout]을 결정해 준다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* it_source : 화면상에 보여줄 전체정보를 가지고 있는 ITAB이다.
* is_source : 현재 진행중인 Structure이다.
* ms_treeinfo :필드정보가 들어가 있다.
* CS_NODE_LAYOUT : cl_gui_alv_tree->add_node에 넘어갸야 할 자원이다.
* C_ISFOLDER : is_source를 엄마로 모시는 ITEM이 존재하는 경우 해당 항목이 [X]로 리턴된다.
*--------------------------------------------------------------------------------
*    CALL METHOD draw_image
*      EXPORTING
*        it_source        = <lt_data>
*        is_source        = <ls_data>
*      CHANGING
*        cs_node_layout = ls_node_layout
*        c_isfolder     = lv_isfolder.
*--------------------------------------------------------------------------------

    CONSTANTS : lc_data TYPE string VALUE 'is_source-'.
    DATA : lv_fieldname TYPE string,
           lr_data      TYPE REF TO data.
    FIELD-SYMBOLS : <lv_value> TYPE any.
    CONCATENATE lc_data ms_treeinfo-fmkey INTO lv_fieldname.
    ASSIGN (lv_fieldname) TO <lv_value>.

    CHECK sy-subrc = 0.

    READ TABLE it_source REFERENCE INTO lr_data WITH KEY (ms_treeinfo-fukey) = <lv_value>.
    IF sy-subrc = 0.
      cs_node_layout-isfolder = mc_x.
      c_isfolder = mc_x.
    ELSE.
      CLEAR c_isfolder.
    ENDIF.
    cs_node_layout-n_image = icon_closed_folder.
    cs_node_layout-exp_image = icon_open_folder.

    IF mt_treeicon IS NOT INITIAL.
      ASSIGN COMPONENT ms_treeinfo-ficon OF STRUCTURE is_source TO FIELD-SYMBOL(<lv_gubn>).
      IF sy-subrc = 0.
        READ TABLE mt_treeicon INTO DATA(ls_fieldicon) WITH KEY gubn = <lv_gubn>.
        IF sy-subrc = 0.
          cs_node_layout-n_image   = ls_fieldicon-n_image.
          cs_node_layout-exp_image = ls_fieldicon-exp_image.
          cs_node_layout-isfolder  = ls_fieldicon-isfolder.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method lcl_tree_assist->DRAW_RECURSIVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_UPPER                       TYPE        ANY
* | [<---] E_ERR_CHK                      TYPE        CHAR01
* | [<---] E_ERR_MSG                      TYPE        CHAR100
* | [<-->] CT_SOURCE                      TYPE        STANDARD TABLE
* | [<-->] CT_EXPAND                      TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD draw_recursive.
*--------------------------------------------------------------------------------
* drawe의 SubRoutine으로 해당 Node를 정전개[Top-Down]으로 풀어준다.(재귀호출)
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* IS_UPPER     :정전개를 풀어야 하는 [상위정보]이다.
* E_ERR_CHK    :순환참조등의 오류가 발생한 경우 [E]값을 가진다.
* E_ERR_MSG    :오류멘트이다.
* ct_source : CL_GUI_ALV_TREE->ADD_NODE가 호출된 후 고유 NODE KEY를 넣어주어 수정된다.
* ct_expand : ROOT를 기준으로 전체를 펼친다고 가정할때 펼쳐야 하는 항목의 NODE정보이다.
*--------------------------------------------------------------------------------
*    CALL METHOD draw_recursive
*      EXPORTING
*        is_upper     = <ls_data>
*      IMPORTING
*        e_err_chk    = lv_err_chk
*        e_err_msg    = lv_err_msg
*      CHANGING
*        ct_source      = <lt_data>
*        ct_expand    = ct_expand.
*--------------------------------------------------------------------------------

    DATA : lr_data        TYPE REF TO data,
           lr_new         TYPE REF TO data,
           lv_node_text   TYPE lvc_value,
           ls_node_layout TYPE lvc_s_layn,
           lv_idx         TYPE sy-tabix,
           lv_tabix       TYPE sy-tabix,
           lv_err_chk     TYPE char01,
           lv_err_msg     TYPE char100,
           lv_nkey_rtn    TYPE lvc_nkey,
           lv_isfolder    TYPE c.

    FIELD-SYMBOLS : <mt_tree>              TYPE STANDARD TABLE,
                    <ls_data>              TYPE any,
                    <ls_new>               TYPE any,
                    <ls_from>              TYPE any,

                    <lv_upper>             TYPE any,
                    <lv_upper_upper>       TYPE any,
                    <lv_upper_node>        TYPE any,

                    <lv_current>           TYPE any,
                    <lv_current_self>      TYPE any,
                    <lv_current_node_name> TYPE any,

                    <lv_value>             TYPE any.

    ASSIGN mt_tree->* TO <mt_tree>.
    ASSIGN COMPONENT ms_treeinfo-fmkey OF STRUCTURE is_upper TO <lv_upper>.
    ASSIGN COMPONENT ms_treeinfo-fukey OF STRUCTURE is_upper TO <lv_upper_upper>.
    ASSIGN COMPONENT ms_treeinfo-fnode OF STRUCTURE is_upper TO <lv_upper_node>.

    READ TABLE ct_source REFERENCE INTO lr_data WITH KEY (ms_treeinfo-fukey) = <lv_upper>.
    CHECK sy-subrc = 0.
    lv_tabix = sy-tabix.
    ASSIGN lr_data->* TO <ls_from>.

    LOOP AT ct_source REFERENCE INTO lr_data FROM lv_tabix.
      ASSIGN lr_data->* TO <ls_data>.
      lv_idx = sy-tabix.

      ASSIGN COMPONENT ms_treeinfo-fukey OF STRUCTURE <ls_data> TO <lv_current>.
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT ms_treeinfo-fmkey OF STRUCTURE <ls_data> TO <lv_current_self>.
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT ms_treeinfo-fname OF STRUCTURE <ls_data> TO <lv_current_node_name>.
      CHECK sy-subrc = 0.

      IF <lv_upper> <> <lv_current>.
        EXIT.
      ENDIF.

      IF <lv_current_self> = <lv_upper_upper>.
        e_err_chk = mc_e.
        e_err_msg = '순환참조'.
        RETURN.
      ENDIF.

      CALL METHOD draw_image
        EXPORTING
          it_source      = ct_source
          is_source      = <ls_data>
        CHANGING
          cs_node_layout = ls_node_layout
          c_isfolder     = lv_isfolder.

      lv_node_text = <lv_current_node_name>.

      CALL METHOD add_node
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

      lv_idx = lines( <mt_tree> ).
      READ TABLE <mt_tree> REFERENCE INTO lr_new INDEX lv_idx.
      ASSIGN lr_new->* TO <ls_new>.
      ASSIGN COMPONENT ms_treeinfo-fnode OF STRUCTURE <ls_new> TO <lv_value>.
      <lv_value> = lv_nkey_rtn.

      ASSIGN COMPONENT ms_treeinfo-fnode OF STRUCTURE <ls_data> TO <lv_value>.
      <lv_value> = lv_nkey_rtn.

      IF lv_isfolder = mc_x.
        APPEND <lv_value> TO ct_expand.
      ENDIF.

      CALL METHOD draw_recursive
        EXPORTING
          is_upper  = <ls_data>
*          it_source = it_source
        IMPORTING
          e_err_chk = lv_err_chk
          e_err_msg = lv_err_msg
        CHANGING
          ct_source = ct_source
          ct_expand = ct_expand.

      IF lv_err_chk = mc_e.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_tree_assist=>GET_FCAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_FCAT                        TYPE        LVC_S_FCAT(optional)
* | [--->] IV_DDIC_OBJECT                 TYPE        ANY(optional)
* | [--->] IS_DATA                        TYPE        ANY(optional)
* | [--->] IT_DATA                        TYPE        ANY TABLE(optional)
* | [--->] IOR_DREF                       TYPE        DATA(optional)
* | [<-()] RT_FCAT                        TYPE        LVC_T_FCAT
* | [EXC!] NO_STRUCTURE
* | [EXC!] INVALID_TYPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_FCAT.
"--------------------------------------------------------------------------------
* ALV에서 사용하는 FieldCatalog를 빠르게 가져올 때 사용한다.
"--------------------------------------------------------------------------------
* 작성일 : 2018.04.01
* 옴겨온자. : moonbye
"--------------------------------------------------------------------------------
* is_fcat         :
* iv_ddic_object  : ABAP Dictionary의 Object를 지정해 준다. 'SFLIGHT'
* is_data         :
* it_data         : itab을 지정해 준다. gt_list
* ior_dref        : Data Type으로 지정된 itab을 설정할 수 있다.
* rt_fcat         : FieldCatalog를 리턴해 준다.
"--------------------------------------------------------------------------------
* lt_fcat =  ME=>get_fcat( it_data = gt_list )
* => gt_list 내용의 FieldCatalog를 리턴해라.
"--------------------------------------------------------------------------------

   DATA:
      ls_fcat        TYPE lvc_s_fcat,
      lo_structdescr TYPE REF TO cl_abap_structdescr,
      lo_typedescr   TYPE REF TO cl_abap_typedescr,
      ltr_data       TYPE REF TO data,
      lsr_data       TYPE REF TO data,
      lt_dfies       TYPE ddfields,
      lsr_dfies      TYPE REF TO dfies.


    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE,
                   <ls_data> TYPE any,
                   <lv_ele>  TYPE any.



* Full structure input

    IF is_fcat IS NOT INITIAL.

      APPEND is_fcat TO rt_fcat.



* DDIC struct of table name

    ELSEIF iv_ddic_object IS NOT INITIAL.

      CREATE DATA lsr_data TYPE (iv_ddic_object).

      rt_fcat = get_fcat( ior_dref = lsr_data ).



* Fill Table name

      ls_fcat-tabname = iv_ddic_object.

      MODIFY rt_fcat FROM ls_fcat

             TRANSPORTING tabname WHERE tabname = space.



* Any data structure

    ELSEIF is_data IS SUPPLIED.

      GET REFERENCE OF is_data INTO lsr_data.

      rt_fcat = get_fcat( ior_dref = lsr_data ).



* Any data table

    ELSEIF it_data IS SUPPLIED.

      GET REFERENCE OF it_data INTO ltr_data.

      rt_fcat = get_fcat( ior_dref = ltr_data ).



* Data reference

    ELSEIF ior_dref IS BOUND.

* Get reference type

      lo_typedescr ?=

                cl_abap_typedescr=>describe_by_data_ref( ior_dref ).

*    CASE lo_typedescr->kind.

*      WHEN cl_abap_typedescr=>kind_elem.    " field

*

*      WHEN cl_abap_typedescr=>kind_struct.  " structure

*        lo_structdescr ?=

*                 cl_abap_structdescr=>describe_by_data_ref( ior_dref ).

*        lor_struc = ior_dref.

*

*      WHEN cl_abap_typedescr=>kind_table.   " table

*        ASSIGN ior_dref->* TO <lt_data>.

*        CREATE DATA lor_struc LIKE LINE OF <lt_data>.

*        lo_structdescr ?=

*                cl_abap_structdescr=>describe_by_data_ref( lor_struc ).

*    ENDCASE.

      CASE lo_typedescr->type_kind.

        WHEN cl_abap_typedescr=>typekind_struct1 OR

             cl_abap_typedescr=>typekind_struct2.  " structure

          lo_structdescr ?=

                    cl_abap_structdescr=>describe_by_data_ref( ior_dref ).

          lsr_data = ior_dref.



        WHEN cl_abap_typedescr=>typekind_table.   " table

          ASSIGN ior_dref->* TO <lt_data>.

          CREATE DATA lsr_data LIKE LINE OF <lt_data>.

          lo_structdescr ?=

                   cl_abap_structdescr=>describe_by_data_ref( lsr_data ).

      ENDCASE.



** Get fields in table

      lt_dfies = cl_salv_data_descr=>read_structdescr( lo_structdescr ).



* Build Field catalog

      LOOP AT lt_dfies REFERENCE INTO lsr_dfies.

        ls_fcat-fieldname = lsr_dfies->fieldname.

        ls_fcat-ref_field = lsr_dfies->reffield.

        ls_fcat-ref_table = lsr_dfies->reftable.



* Set technical - by fieldname

* This are the fields from include structure ZCLS_ALV_OUTPUT

* or fields that were declared as reference

        IF lsr_dfies->datatype  = 'REF'        OR "reference

           ls_fcat-fieldname = 'T_COLOR'    OR

           ls_fcat-fieldname = 'COLOR'      OR "pointer: layout-info_fname

           ls_fcat-fieldname = 'T_STYLE'    OR

           ls_fcat-fieldname = 'S_DROPDOWN' OR

           ls_fcat-fieldname = 'EDIT'       OR

           ls_fcat-fieldname = 'NODE_KEY'   OR

           ls_fcat-fieldname = 'RELATKEY'.

          ls_fcat-tech = abap_true.



        ELSE.

          IF lsr_dfies->precfield IS NOT INITIAL.

            IF lsr_dfies->datatype = 'CURR'.

              ls_fcat-cfieldname = lsr_dfies->precfield.

            ELSEIF lsr_dfies->datatype = 'QUAN'.

              ls_fcat-qfieldname = lsr_dfies->precfield.

            ENDIF.

          ENDIF.



          ls_fcat-outputlen = lsr_dfies->outputlen.

          ls_fcat-dd_outlen = lsr_dfies->outputlen.



          IF lsr_dfies->convexit IS NOT INITIAL.

            CONCATENATE '==' lsr_dfies->convexit INTO ls_fcat-edit_mask.

          ENDIF.



          ls_fcat-rollname   = lsr_dfies->rollname.

          ls_fcat-f4availabl = lsr_dfies->f4availabl.

          ls_fcat-mac        = lsr_dfies->mac.

          ls_fcat-rollname   = lsr_dfies->rollname.

          ls_fcat-domname    = lsr_dfies->domname.

          ls_fcat-intlen     = lsr_dfies->leng.

          ls_fcat-intlen     = lsr_dfies->intlen.

          ls_fcat-decimals   = lsr_dfies->decimals.

          ls_fcat-datatype   = lsr_dfies->datatype.

          ls_fcat-inttype    = lsr_dfies->inttype.

          ls_fcat-scrtext_s  = lsr_dfies->scrtext_s.

          ls_fcat-scrtext_m  = lsr_dfies->scrtext_m.

          ls_fcat-scrtext_l  = lsr_dfies->scrtext_l.

          IF lsr_dfies->datatype = 'CURR' OR

             lsr_dfies->datatype = 'QUAN'.

            ls_fcat-no_sign = abap_false.

          ELSE.

            ls_fcat-no_sign    = lsr_dfies->sign.

          ENDIF.

          ls_fcat-lowercase  = lsr_dfies->lowercase.

          ls_fcat-f4availabl = lsr_dfies->f4availabl.

          ls_fcat-checktable = lsr_dfies->checktable.

          ls_fcat-key        = lsr_dfies->keyflag.



* Set No-Sum field

          IF ls_fcat-datatype <> 'CURR' AND

             ls_fcat-datatype <> 'QUAN'.

            ls_fcat-no_sum  = abap_true.

          ENDIF.



* Set technical - by rollname

          IF ls_fcat-rollname  = 'MANDT'               OR

             ls_fcat-rollname  = '/BSAR/DE_TSTMP_FROM' OR

             ls_fcat-rollname  = '/BSAR/DE_TSTMP_TO'   OR

             ls_fcat-rollname  = '/BSAR/DE_CRETSTMP'   OR

             ls_fcat-rollname  = '/BSAR/DE_CHATSTMP'.

            ls_fcat-tech = abap_true.

          ENDIF.



* Set checkbox

          IF ls_fcat-domname = 'CHECKBOX' OR

             ls_fcat-domname = 'XFELD'.

            ls_fcat-checkbox = abap_true.

          ENDIF.



* Set Icon

          IF ls_fcat-domname = 'ICON'.

            ls_fcat-icon = abap_true.

            ls_fcat-just = 'C'.  "Center

          ENDIF.



        ENDIF.



* Save column as record

        APPEND ls_fcat TO rt_fcat. CLEAR ls_fcat.

      ENDLOOP.

      IF sy-subrc = 1.

        RAISE no_structure.

      ENDIF.



** Build fcat -- An other way to build, but without F4 shlp info

*  data:

*    lo_descr_elem  TYPE REF TO cl_abap_elemdescr,

*    ls_comp        TYPE abap_compdescr,

*    lv_type        TYPE dom_ref,

*    lv_name        TYPE ddobjname,

*    lv_tabix       TYPE sytabix,

*    ls_dd04v       TYPE dd04v,



*    CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.

** Get reference in structure format

*      ASSIGN lor_struc->* TO <ls_data>.

*

*      LOOP AT lo_structdescr->components INTO ls_comp.

*        CLEAR ls_fcat.

*        ls_fcat-col_pos   = lv_tabix = sy-tabix.

*        ls_fcat-fieldname = ls_comp-name.

*        ls_fcat-inttype   = ls_comp-type_kind.

*        ls_fcat-intlen    = ls_comp-length.

*        ls_fcat-decimals  = ls_comp-decimals.

*

** Set technical - by fieldname

*        IF ls_fcat-fieldname = 'T_COLOR'    OR

*           ls_fcat-fieldname = 'T_STYLE'    OR

*           ls_fcat-fieldname = 'S_DROPDOWN' OR

*           ls_fcat-fieldname = 'EDIT'       OR

*           ls_fcat-fieldname = 'OIF_MVC'    OR   "Ref to implem. class

*           ls_fcat-fieldname = 'OR_DREF'.        "Ref to data record

*          ls_fcat-tech = abap_true.

*

*        ELSE.

*          ASSIGN COMPONENT ls_comp-name OF STRUCTURE <ls_data>

*                                                  TO <lv_ele>.

*

*        lo_typedescr = cl_abap_elemdescr=>describe_by_data( <lv_ele> ).

*          CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.

*            lo_descr_elem ?= lo_typedescr.

*            lv_type = lo_descr_elem->absolute_name.

** type defintion like : \TYPE=....

*            SHIFT lv_type BY 6 PLACES.

*            CALL FUNCTION 'DDIF_DTEL_GET'

*              EXPORTING

*                name     = lv_type

*                state    = 'A'

*                langu    = sy-langu

*              IMPORTING

*                dd04v_wa = ls_dd04v.

*

*            MOVE-CORRESPONDING ls_dd04v TO ls_fcat.

*            IF ls_fcat-reptext IS INITIAL.

*              ls_fcat-reptext   =

*              ls_fcat-scrtext_s =

*              ls_fcat-scrtext_m =

*              ls_fcat-scrtext_l = ls_comp-name.

*            ENDIF.

*          ENDCATCH.

*          IF sy-subrc = 1.

*            RAISE invalid_type.

*          ENDIF.

*

** Set technical - by rollname

*          IF ls_fcat-rollname  = 'MANDT'               OR

*             ls_fcat-rollname  = '/BSAR/DE_TSTMP_FROM' OR

*             ls_fcat-rollname  = '/BSAR/DE_TSTMP_TO'   OR

*             ls_fcat-rollname  = '/BSAR/DE_CRETSTMP'   OR

*             ls_fcat-rollname  = '/BSAR/DE_CHATSTMP'.

*            ls_fcat-tech = abap_true.

*          ENDIF.

*        ENDIF.

*

** Set checkbox

*        IF lv_type = 'CHECKBOX' OR lv_type = 'XFELD'.

*          ls_fcat-checkbox = abap_true.

*        ENDIF.

*

** Save record

*        APPEND ls_fcat TO rt_fcat.

*      ENDLOOP.

*    ENDCATCH.

*    IF sy-subrc = 1.

*      RAISE no_structure.

*    ENDIF.



    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->GET_NODE_TOPDOWN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_TREE                        TYPE        ANY
* | [<---] E_ERR_CHK                      TYPE        CHAR01
* | [<---] E_ERR_MSG                      TYPE        CHAR100
* | [<-->] CT_RETURN                      TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_node_topdown.
*--------------------------------------------------------------------------------
* 계층형 itab[<mt_tree>]를 받아서 특정위치[is_tree ]의 밑에 붙어있는 모든 Node를 ct_return로 전달해 준다.
*--------------------------------------------------------------------------------
* 작성일 : 2020.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* is_tree : TOP의 정보이다.
* ct_return : is_tree[TOP]을 기준으로 [자식]을 가지고 있는 [NODE]정보만을 여기에 넣어준다.
*--------------------------------------------------------------------------------
*  call method lcl_tree_assist=>GET_NODE_TOPDOWN
*    EXPORTING
*      is_tree      = ls_list2
*    IMPORTING
*      e_err_chk    = lv_err
*      e_err_msg    = lv_msg
*    Changing
*      ct_return      = lt_list2.
*--------------------------------------------------------------------------------

    DATA : lr_data    TYPE REF TO data,
           lr_data1   TYPE REF TO data,
           lv_tabix   TYPE sy-tabix,
           lv_err_chk TYPE char01,
           lv_err_msg TYPE char100.

    FIELD-SYMBOLS : <mt_tree>              type STANDARD TABLE,
                    <ls_data>              TYPE any,
                    <ls_from>              TYPE any,
                    <lv_upper>             TYPE any,
                    <lv_upper_upper>       TYPE any,
                    <lv_upper_node>        TYPE any,
                    <ls_target>            TYPE any,
                    <lv_current>           TYPE any,
                    <lv_current_self>      TYPE any,
                    <lv_current_node_name> TYPE any.
    ASSIGN mt_tree->* to <mt_tree>.
    CREATE DATA lr_data1 LIKE LINE OF ct_return.
    ASSIGN lr_data1->* TO <ls_target>.

    ASSIGN COMPONENT ms_treeinfo-fmkey OF STRUCTURE is_tree TO <lv_upper>.
    ASSIGN COMPONENT ms_treeinfo-fukey OF STRUCTURE is_tree TO <lv_upper_upper>.
    ASSIGN COMPONENT ms_treeinfo-fnode OF STRUCTURE is_tree TO <lv_upper_node>.

    MOVE-CORRESPONDING is_tree TO <ls_target>.
    APPEND <ls_target> TO ct_return.

    READ TABLE <mt_tree> REFERENCE INTO lr_data WITH KEY (ms_treeinfo-fukey) = <lv_upper>.
    CHECK sy-subrc = 0.
    lv_tabix = sy-tabix.
    ASSIGN lr_data->* TO <ls_from>.

    LOOP AT <mt_tree> REFERENCE INTO lr_data FROM lv_tabix.
      ASSIGN lr_data->* TO <ls_data>.
      ASSIGN COMPONENT ms_treeinfo-fukey OF STRUCTURE <ls_data> TO <lv_current>.
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT ms_treeinfo-fmkey OF STRUCTURE <ls_data> TO <lv_current_self>.
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT ms_treeinfo-fname OF STRUCTURE <ls_data> TO <lv_current_node_name>.
      CHECK sy-subrc = 0.

      IF <lv_upper> <> <lv_current>.
        EXIT.
      ENDIF.

      IF <lv_current_self> = <lv_upper_upper>.
        e_err_chk = mc_e.
        e_err_msg = '순환참조'.
        RETURN.
      ENDIF.
      MOVE-CORRESPONDING <ls_data> TO <ls_target>.
      APPEND <ls_target> TO ct_return.

      CALL METHOD get_node_topdown
        EXPORTING
          is_tree   = <ls_data>
        IMPORTING
          e_err_chk = lv_err_chk
          e_err_msg = lv_err_msg
        CHANGING
          ct_return = ct_return.
      IF lv_err_chk = mc_e.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->GET_SELECTED_NODES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_NODE                        TYPE        LVC_T_NKEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_selected_nodes.
*--------------------------------------------------------------------------------
* tree로부터 선택되어 있는 node정보를 읽어들인다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.10
* 작성자 : moonbye
*--------------------------------------------------------------------------------
*  mo_tree : cl_gui_alv_tree
*--------------------------------------------------------------------------------
*data(lt_node) = lcl_tree_assist=>get_selected_nodes(  ).
*--------------------------------------------------------------------------------
    CALL METHOD mo_tree->get_selected_nodes
      CHANGING
        ct_selected_nodes = rt_node.
    IF rt_node IS INITIAL.
      CALL METHOD mo_tree->get_selected_item
        IMPORTING
          e_selected_node   = DATA(lv_node)
        EXCEPTIONS
          no_item_selection = 0
          cntl_system_error = 1
          failed            = 2.
      .
      IF lv_node IS NOT INITIAL.
        APPEND lv_node TO rt_node.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->HANDLE_DRAG_MULTIPLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SENDER                         LIKE
* | [--->] NODE_KEY_TABLE                 LIKE
* | [--->] FIELDNAME                      LIKE
* | [--->] DRAG_DROP_OBJECT               LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_drag_multiple.
*--------------------------------------------------------------------------------
* Event Hanlder ( cl_gui_toolbar-ON_DRAG_MULTIPLE )
* Drag&Drop의 Drag Event 전달자를 넣어준다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
        DATA:
          l_node_key    TYPE lvc_nkey,
          l_sflight     TYPE sflight,
          l_node_text   TYPE lvc_value,
          l_node_layout TYPE lvc_s_layn.
    mt_dragdrop_nodes = node_key_table.
    drag_drop_object->object = me. "object가 담기지 않으면 Drop이 호출되지 않는다.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->HANDLE_DROP
* +-------------------------------------------------------------------------------------------------+
* | [--->] SENDER                         LIKE
* | [--->] NODE_KEY                       LIKE
* | [--->] DRAG_DROP_OBJECT               LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_drop.
*--------------------------------------------------------------------------------
* Event Hanlder ( cl_gui_toolbar-ON_DROP )
* Drag&Drop의 Drop Event 를 수행한다. IsFolder = ' ' 인 항목을 isFolder='X'에 Child로 옴겨진다.
*  작업이 완료된 이후에는 어떠한 Node의 엄마가 변경되었는지를 PAI EVENT로 확인할 수 있도록 해준다.
*  ( mt_dragdrop_nodes_change )
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------


    DATA : ls_nodes                 TYPE mty_s_dragdropbag,
           ls_dragdrop_nodes_change TYPE mty_s_eventdragdrop.
    FIELD-SYMBOLS : <lt_tree> TYPE STANDARD TABLE.
    ASSIGN mt_tree->* TO <lt_tree>.
    READ TABLE <lt_tree> ASSIGNING FIELD-SYMBOL(<ls_target>)
      WITH KEY (ms_treeinfo-fnode) = node_key BINARY SEARCH.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT ms_treeinfo-fmkey OF STRUCTURE <ls_target> TO FIELD-SYMBOL(<lv_umkey>).
    CHECK sy-subrc = 0.

    DELETE mt_dragdrop_nodes_change WHERE tree = mo_tree.

    LOOP AT mt_dragdrop_nodes INTO DATA(lv_node).
      READ TABLE <lt_tree> ASSIGNING FIELD-SYMBOL(<ls_source>)
        WITH KEY (ms_treeinfo-fnode) = lv_node BINARY SEARCH.
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT ms_treeinfo-fmkey OF STRUCTURE <ls_source> TO FIELD-SYMBOL(<lv_mkey>).
      CHECK sy-subrc = 0.
      CALL METHOD sender->move_node
        EXPORTING
          i_node_key         = lv_node
          i_relatkey         = node_key
          i_relatship        = cl_gui_column_tree=>relat_last_child
        EXCEPTIONS
          node_not_found     = 0
          relative_not_found = 1.
      IF sy-subrc = 0.
        ls_nodes-mkey   = <lv_mkey>.
        ls_nodes-umkey  = <lv_umkey>.
        APPEND ls_nodes TO ls_dragdrop_nodes_change-bags.
      ENDIF.
    ENDLOOP.
    CHECK ls_dragdrop_nodes_change-bags IS NOT INITIAL.
    ls_dragdrop_nodes_change-tree = mo_tree.
    APPEND ls_dragdrop_nodes_change TO mt_dragdrop_nodes_change.
    sender->frontend_update( ).
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = m_dragdrop_fcode
      EXCEPTIONS
        function_not_supported = 1.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->HANDLE_ITEM_CM_REQ
* +-------------------------------------------------------------------------------------------------+
* | [--->] NODE_KEY                       LIKE
* | [--->] MENU                           LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HANDLE_ITEM_CM_REQ.
*--------------------------------------------------------------------------------
* Event Hanlder ( cl_gui_toolbar-ITEM_CONTEXT_MENU_REQUEST )
* ADD_CONTEXTMENU를 통해 등록된 정보를 item에대한 Context Menu수행시 호출된다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* In this case the standard menu is cleared.
    CALL METHOD menu->clear.
* The next line defines one line of the context menu.
    LOOP AT mt_contextmenu INTO DATA(ls_contextmenu).
      CALL METHOD menu->add_function
        EXPORTING
          fcode    = ls_contextmenu-fcode
          text     = ls_contextmenu-text
          icon     = ls_contextmenu-icon
          disabled = ls_contextmenu-is_disabled.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->HANDLE_ITEM_CM_SEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELDNAME                      LIKE
* | [--->] NODE_KEY                       LIKE
* | [--->] FCODE                          LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HANDLE_ITEM_CM_SEL.
*--------------------------------------------------------------------------------
* Event Hanlder ( cl_gui_toolbar-ITEM_CONTEXT_MENU_SELECTED )
* Item에대한 Context Menu가 선택되면 호출된다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = fcode
      EXCEPTIONS
        function_not_supported = 1.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->HANDLE_NODE_CM_REQ
* +-------------------------------------------------------------------------------------------------+
* | [--->] NODE_KEY                       LIKE
* | [--->] MENU                           LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HANDLE_NODE_CM_REQ.
*--------------------------------------------------------------------------------
* Event Hanlder ( cl_gui_toolbar-NODE_CONTEXT_MENU_REQUEST )
* ADD_CONTEXTMENU를 통해 등록된 정보를 Node에대한 Context Menu수행시 호출된다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
* In this case the standard menu is cleared.
    CALL METHOD menu->clear.
* The next line defines one line of the context menu.
    LOOP AT mt_contextmenu INTO DATA(ls_contextmenu).
      CALL METHOD menu->add_function
        EXPORTING
          fcode    = ls_contextmenu-fcode
          text     = ls_contextmenu-text
          icon     = ls_contextmenu-icon
          disabled = ls_contextmenu-is_disabled.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->HANDLE_NODE_CM_SEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] NODE_KEY                       LIKE
* | [--->] FCODE                          LIKE
* | [--->] SENDER                         LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HANDLE_NODE_CM_SEL.
*--------------------------------------------------------------------------------
* Event Hanlder ( cl_gui_toolbar-NODE_CONTEXT_MENU_SELECTED )
* Node에대한 Context Menu가 선택되면 호출된다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*--------------------------------------------------------------------------------
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = fcode
      EXCEPTIONS
        function_not_supported = 1.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->HANDLE_TOOLBAR_DROPDOWN
* +-------------------------------------------------------------------------------------------------+
* | [--->] FCODE                          LIKE
* | [--->] POSX                           LIKE
* | [--->] POSY                           LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_toolbar_dropdown.
*--------------------------------------------------------------------------------
* Event Hanlder ( cl_gui_toolbar-DROPDOWN_CLICKED )
* ADD_TOOLBAR를 통해 등록된 버튼들 중 Sub Button이 존재하는 경우 이 버튼을 실시간으로 보여준다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.09
* 작성자 : moonbye
*--------------------------------------------------------------------------------

    DATA : lo_ctmenu TYPE REF TO cl_ctmenu.
    CREATE OBJECT lo_ctmenu.

    READ TABLE mt_toolbar INTO DATA(ls_first) WITH KEY ufcode = fcode.
    CHECK sy-subrc = 0.
    LOOP AT mt_toolbar INTO DATA(ls_fcode) FROM sy-tabix.
      IF ls_fcode-ufcode <> fcode.
        EXIT.
      ENDIF.
      CALL METHOD lo_ctmenu->add_function
        EXPORTING
          fcode    = ls_fcode-fcode
          text     = ls_fcode-text
          disabled = ls_fcode-is_disabled
          icon     = ls_fcode-icon.

    ENDLOOP.

    CALL METHOD ls_first-reftoolbar->track_context_menu
      EXPORTING
        context_menu = lo_ctmenu
        posx         = posx
        posy         = posy.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->HANDLE_TOOLBAR_SELECTED
* +-------------------------------------------------------------------------------------------------+
* | [--->] FCODE                          LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_toolbar_selected.
*--------------------------------------------------------------------------------
* Event Hanlder ( cl_gui_toolbar-FUNCTION_SELECTED )
* ADD_TOOLBAR를 통해 등록된 버튼들 중 Sub Button이 눌려지면 SCREEN의 PAI로 전달해 준다.
*--------------------------------------------------------------------------------
* 작성일 : 2021.01.09
* 작성자 : moonbye
*--------------------------------------------------------------------------------

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = fcode
      EXCEPTIONS
        function_not_supported = 1.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->RECALC_BOTTOMUP
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_TREE                        TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD recalc_bottomup.
*-----------------------------------------------------------------------------------
* 계층형 itab[ct_tree]를 받아서 수치형 데이타를 누적하여 돌려준다.
*-----------------------------------------------------------------------------------
* 작성일 : 2021.01.14
* 작성자 : moonbye
*-----------------------------------------------------------------------------------
* ct_tree : 자료가 들어있는 항목이다.
*           (수치형 데이타들은 Level에 따라 자식의 값의 누적데이타를 가진상태로 돌려 받는다. )
*-----------------------------------------------------------------------------------
*  Ex. 컴퓨터 100                  =>       컴퓨터 300
*         모니터 100                          모니터 100
*         본체   100                          본체  100
*-----------------------------------------------------------------------------------
*  call method lcl_tree_assist=>TREE_RECALC_BOTTOMUP
*    changing
*      ct_tree      = lt_list2.
*-----------------------------------------------------------------------------------

    DATA : lr_data  TYPE REF TO data,
           lr_data1 TYPE REF TO data.
    DATA : lt_fcat TYPE lvc_t_fcat,
           ls_fcat LIKE LINE OF lt_fcat.
    FIELD-SYMBOLS : <mt_tree>      TYPE STANDARD TABLE,
                    <ls_data>      TYPE any,
                    <ls_udata>     TYPE any,
                    <ls_current>   TYPE any,
                    <lv_key>       TYPE any,
                    <lv_ukey>      TYPE any,
                    <lt_data_key>  TYPE STANDARD TABLE,
                    <lt_data_ukey> TYPE STANDARD TABLE,
                    <lv_value>     TYPE any,
                    <lv_uvalue>    TYPE any.
    ASSIGN mt_tree->* TO <mt_tree>.
    lt_fcat = get_fcat( it_data = ct_tree ).
    DELETE lt_fcat WHERE NOT ( inttype = mc_p OR inttype = mc_i ).
    "<lt_data_key> : ct_tree를 main key로 정렬한 itab
    CREATE DATA lr_data   LIKE ct_tree.
    ASSIGN lr_data->*   TO <lt_data_key>.
    <lt_data_key> = ct_tree.
    SORT <lt_data_key> BY (ms_treeinfo-fmkey).

    "<lt_data_ukey> : ct_tree를 Upper key로 정렬한 itab
    CREATE DATA lr_data   LIKE ct_tree.
    ASSIGN lr_data->*   TO <lt_data_ukey>.
    <lt_data_ukey> = ct_tree.
    SORT <lt_data_ukey> BY (ms_treeinfo-fukey).

    "<ls_current> : 현재 말아올라가는 놈의 누적수치.
    CREATE DATA lr_data LIKE LINE OF ct_tree.
    ASSIGN lr_data->* TO <ls_current>.

    LOOP AT ct_tree REFERENCE INTO lr_data.
      ASSIGN lr_data->* TO <ls_data>.
      ASSIGN COMPONENT ms_treeinfo-fmkey OF STRUCTURE <ls_data> TO <lv_key>.
      ASSIGN COMPONENT ms_treeinfo-fukey OF STRUCTURE <ls_data> TO <lv_ukey>.

      "나를 엄마로 모시는 항목이 있는가?
      "(내가 끝단인지를 확인한다.)
      READ TABLE <lt_data_ukey> REFERENCE INTO lr_data1 WITH KEY (ms_treeinfo-fukey) = <lv_key> BINARY SEARCH.
      CHECK sy-subrc <> 0.

      "끝단인 경우 말아서 올린다.
      MOVE-CORRESPONDING <ls_data> TO <ls_current>.
      DO 100 TIMES.
        IF <lv_ukey> IS INITIAL.
          EXIT.
        ENDIF.
        READ TABLE <lt_data_key> REFERENCE INTO lr_data1 WITH KEY (ms_treeinfo-fmkey) = <lv_ukey> BINARY SEARCH.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        ASSIGN lr_data1->* TO <ls_udata>.
        ASSIGN COMPONENT ms_treeinfo-fukey OF STRUCTURE <ls_udata> TO <lv_ukey>.

        "숫자형 field를 합산하여 말아올린다.
        LOOP AT lt_fcat INTO ls_fcat.
          ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <ls_current> TO <lv_value>.
          ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <ls_udata> TO <lv_uvalue>.
          <lv_uvalue> = <lv_uvalue> + <lv_value>.
        ENDLOOP.
        MOVE-CORRESPONDING <ls_udata> TO <ls_current>.
      ENDDO.
    ENDLOOP.

    "최종 결과를 ct_tree에 반영해 준다.
    LOOP AT ct_tree REFERENCE INTO lr_data.
      ASSIGN lr_data->* TO <ls_data>.
      ASSIGN COMPONENT ms_treeinfo-fmkey OF STRUCTURE <ls_data> TO <lv_key>.
      READ TABLE <lt_data_key> REFERENCE INTO lr_data1 WITH KEY (ms_treeinfo-fmkey) = <lv_key> BINARY SEARCH.
      CHECK sy-subrc = 0.
      ASSIGN lr_data1->* TO <ls_udata>.
      MOVE-CORRESPONDING <ls_udata> TO <ls_data>.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_tree_assist->REDRAW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SOURCE                      TYPE        STANDARD TABLE(optional)
* | [<-->] CT_EXPAND                      TYPE        LVC_T_NKEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD redraw.
*-----------------------------------------------------------------------------------
* Tree를 기존에 가지고 있는 정보 또는 주어진 정보를 바탕으로 다시그려준다.
*-----------------------------------------------------------------------------------
* 작성일 : 2021.01.11
* 작성자 : moonbye
*-----------------------------------------------------------------------------------
* it_source(Optional) : Source로 사용할 itab 없으면 기존 정보를 가지고 다시 그린다.
* ct_expand : ROOT를 기준으로 전체를 펼친다고 가정할때 펼쳐야 하는 항목의 NODE정보이다.
*-----------------------------------------------------------------------------------
*  CALL METHOD lcl_tree_assist=>ReDraw
*    Exporting
*      it_source     = lt_list2   [set_table_for_first_display]의 gt_list와 다른 Local itab을 지정하는것이 맞다.
*    Changing
*      ct_expand    = gt_expand
*      .
*-----------------------------------------------------------------------------------

    FIELD-SYMBOLS : <lt_source> TYPE STANDARD TABLE.
    IF it_source IS INITIAL.
      ASSIGN mt_tree->* TO <lt_source>.
    ELSE.
      ASSIGN ('IT_SOURCE') TO <lt_source>.
    ENDIF.
    m_redraw = mc_x.
    CALL METHOD draw
      EXPORTING
        it_source = <lt_source>
      CHANGING
        ct_expand = ct_expand.

  ENDMETHOD.
ENDCLASS.
