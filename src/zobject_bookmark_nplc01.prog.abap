*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC01
*&---------------------------------------------------------------------*

CLASS lcl_module     IMPLEMENTATION.




  METHOD get_excl_buttons.
*  APPEND cl_gui_alv_grid=>mc_fc_auf                     TO rt_excl.                 "Drilldown Totals Level
*  APPEND cl_gui_alv_grid=>mc_fc_average                 TO rt_excl.                 "Calculate Average
    APPEND cl_gui_alv_grid=>mc_fc_back_classic            TO rt_excl.                 "Back
    APPEND cl_gui_alv_grid=>mc_fc_call_abc                TO rt_excl.                 "ABC Analysis
    APPEND cl_gui_alv_grid=>mc_fc_call_chain              TO rt_excl.                 "Call Sequence
    APPEND cl_gui_alv_grid=>mc_fc_call_crbatch            TO rt_excl.                 "Seagate Info Batch Processing
    APPEND cl_gui_alv_grid=>mc_fc_call_crweb              TO rt_excl.                 "Seagate Info Web Processing
    APPEND cl_gui_alv_grid=>mc_fc_call_lineitems          TO rt_excl.                 "Line Items
    APPEND cl_gui_alv_grid=>mc_fc_call_master_data        TO rt_excl.                 "Master Data
    APPEND cl_gui_alv_grid=>mc_fc_call_more               TO rt_excl.                 "More Calls
    APPEND cl_gui_alv_grid=>mc_fc_call_report             TO rt_excl.                 "Call Report
    APPEND cl_gui_alv_grid=>mc_fc_call_xint               TO rt_excl.                 "Additional Functions of SAPQuery
*  APPEND cl_gui_alv_grid=>mc_fc_call_xml_export         TO rt_excl.                 "StarOffice
*  APPEND cl_gui_alv_grid=>mc_fc_call_xxl                TO rt_excl.                 "XXL
    APPEND cl_gui_alv_grid=>mc_fc_check                   TO rt_excl.                 "Check Entries
    APPEND cl_gui_alv_grid=>mc_fc_col_invisible           TO rt_excl.                 "Columns Invisible
    APPEND cl_gui_alv_grid=>mc_fc_col_optimize            TO rt_excl.                 "Optimize Columns
* APPEND cl_gui_alv_grid=>mc_fc_count                   TO rt_excl.                 "Counter
*  APPEND cl_gui_alv_grid=>mc_fc_current_variant         TO rt_excl.                 "Current Variant
*  APPEND cl_gui_alv_grid=>mc_fc_data_save               TO rt_excl.                 "Save Data
*  APPEND cl_gui_alv_grid=>mc_fc_delete_filter           TO rt_excl.                 "Delete Filter
*  APPEND cl_gui_alv_grid=>mc_fc_deselect_all            TO rt_excl.                 "Deselect All Rows
*  APPEND cl_gui_alv_grid=>mc_fc_detail                  TO rt_excl.                 "Choose Detail
*  APPEND cl_gui_alv_grid=>mc_fc_excl_all                TO rt_excl.                 "Exclude all Grid Functions
    APPEND cl_gui_alv_grid=>mc_fc_expcrdata               TO rt_excl.                 "CrystalReportsTM (Export with Data)
    APPEND cl_gui_alv_grid=>mc_fc_expcrdesig              TO rt_excl.                 "Crystal Reports (TM) ( Start Designer)
    APPEND cl_gui_alv_grid=>mc_fc_expcrtempl              TO rt_excl.                 "Crystal ( Templ ).
    APPEND cl_gui_alv_grid=>mc_fc_expmdb                  TO rt_excl.                 "MicrosoftTM Database File
    APPEND cl_gui_alv_grid=>mc_fc_extend                  TO rt_excl.                 "Additional Query Functions
    APPEND cl_gui_alv_grid=>mc_fc_f4                      TO rt_excl.                 "F4
*  APPEND cl_gui_alv_grid=>mc_fc_filter                  TO rt_excl.                 "Filters
*  APPEND cl_gui_alv_grid=>mc_fc_find                    TO rt_excl.                 "Find
*  APPEND cl_gui_alv_grid=>mc_fc_find_more               TO rt_excl.                 "Search
    APPEND cl_gui_alv_grid=>mc_fc_fix_columns             TO rt_excl.                 "Freeze to Column
    APPEND cl_gui_alv_grid=>mc_fc_graph                   TO rt_excl.                 "Graphic
    APPEND cl_gui_alv_grid=>mc_fc_help                    TO rt_excl.                 "Help
*  APPEND cl_gui_alv_grid=>mc_fc_html                    TO rt_excl.                 "HTML Download
    APPEND cl_gui_alv_grid=>mc_fc_info                    TO rt_excl.                 "Information
*  APPEND cl_gui_alv_grid=>mc_fc_load_variant            TO rt_excl.                 "Read Variant
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row          TO rt_excl.                 "Local: Append Row
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy                TO rt_excl.                 "Local: Copy
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row            TO rt_excl.                 "Local: Copy Row
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut                 TO rt_excl.                 "Local: Cut
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row          TO rt_excl.                 "Local: Delete Row
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row          TO rt_excl.                 "Local: Insert Row
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row            TO rt_excl.                 "Local: Move Row
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste               TO rt_excl.                 "Local: Paste
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row       TO rt_excl.                 "Locally: Paste new Row
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo                TO rt_excl.                 "Undo
*  APPEND cl_gui_alv_grid=>mc_fc_maintain_variant        TO rt_excl.                 "Maintain Variants
    APPEND cl_gui_alv_grid=>mc_fc_maximum                 TO rt_excl.                 "Maximum
    APPEND cl_gui_alv_grid=>mc_fc_minimum                 TO rt_excl.                 "Minimum
*  APPEND cl_gui_alv_grid=>mc_fc_pc_file                 TO rt_excl.                 "Export Local File
*  APPEND cl_gui_alv_grid=>mc_fc_print                   TO rt_excl.                 "Print
*  APPEND cl_gui_alv_grid=>mc_fc_print_back              TO rt_excl.                 "Print Backend
*  APPEND cl_gui_alv_grid=>mc_fc_print_prev              TO rt_excl.                 "Print Preview
    APPEND cl_gui_alv_grid=>mc_fc_refresh                 TO rt_excl.                 "Refresh
    APPEND cl_gui_alv_grid=>mc_fc_reprep                  TO rt_excl.                 "Report/Report Interface
*  APPEND cl_gui_alv_grid=>mc_fc_save_variant            TO rt_excl.                 "Save Variant
*  APPEND cl_gui_alv_grid=>mc_fc_select_all              TO rt_excl.                 "Select All Rows
    APPEND cl_gui_alv_grid=>mc_fc_send                    TO rt_excl.                 "Send
    APPEND cl_gui_alv_grid=>mc_fc_separator               TO rt_excl.                 "Separator
*  APPEND cl_gui_alv_grid=>mc_fc_sort                    TO rt_excl.                 "Sort
*  APPEND cl_gui_alv_grid=>mc_fc_sort_asc                TO rt_excl.                 "Sort in Ascending Order
*  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc                TO rt_excl.                 "Sort in Descending Order
*  APPEND cl_gui_alv_grid=>mc_fc_subtot                  TO rt_excl.                 "Subtotals
*  APPEND cl_gui_alv_grid=>mc_fc_sum                     TO rt_excl.                 "Total
*  APPEND cl_gui_alv_grid=>mc_fc_to_office               TO rt_excl.                 "Export Office
    APPEND cl_gui_alv_grid=>mc_fc_to_rep_tree             TO rt_excl.                 "Export Reporting Tree
    APPEND cl_gui_alv_grid=>mc_fc_unfix_columns           TO rt_excl.                 "Unfreeze Columns
*  APPEND cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard   TO rt_excl.                 "Generate URL for RFC Call
*  APPEND cl_gui_alv_grid=>mc_fc_variant_admin           TO rt_excl.                 "Function Code
*  APPEND cl_gui_alv_grid=>mc_fc_view_crystal            TO rt_excl.                 "Crystal Preview Inplace
*  APPEND cl_gui_alv_grid=>mc_fc_view_excel              TO rt_excl.                 "Excel Inplace
*  APPEND cl_gui_alv_grid=>mc_fc_view_grid               TO rt_excl.                 "Grid Control
*  APPEND cl_gui_alv_grid=>mc_fc_view_lotus              TO rt_excl.                 "Lotus Inplace
*  APPEND cl_gui_alv_grid=>mc_fc_views                   TO rt_excl.                 "View Change
    APPEND cl_gui_alv_grid=>mc_fc_word_processor          TO rt_excl.                 "Word Processing
  ENDMETHOD.
  method COMMIT.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
     EXPORTING
       WAIT          = i_wait
*     IMPORTING
*       RETURN        =
              .

  endmethod.
  method ASK_QUESTION.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question               = i_question
       IMPORTING
         ANSWER                      = r_ans
       EXCEPTIONS
         TEXT_NOT_FOUND              = 1
         OTHERS                      = 2
                .

  endmethod.
  METHOD ask_question_input.
    DATA : lv_answer TYPE spop-varvalue1.
    DATA : lv_ans    type c.

    CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
      EXPORTING
        textline1   = i_question
*       TEXTLINE2   = ' '
*       TEXTLINE3   = ' '
        titel       = sy-cprog
        valuelength = 50
      IMPORTING
        answer      = lv_ans
        value1      = lv_answer
         EXCEPTIONS
       TITEL_TOO_LONG       = 1
       OTHERS      = 2
      .
    e_input = lv_answer.

  ENDMETHOD.
  METHOD disp_f4_alv.

"--------------------------------------------------------------------------------
* lt_f4의 내용으로 [f4]를 화면에 뿌려준다.
* Return되는 필드[SPRSL]의 결과 내용을 ALV ITAB의 [P_FIELDNAME]으로 채워준다.
"--------------------------------------------------------------------------------
    CONSTANTS : lc_exit type fieldname VALUE 'CONVERSION_EXIT_',
                lc_exit1 type fieldname VALUE '_INPUT',
                lc_exit2 type fieldname VALUE '=='.
    DATA : lt_result   TYPE fico_typ_tab_retval,
           lv_scrfield TYPE help_info-dynprofld,
           lv_fname    type fieldname,
           ls_modi     TYPE lvc_s_modi,
           lt_fcat     type lvc_t_fcat,
           ls_fcat     LIKE LINE OF lt_fcat,
           ls_result   LIKE LINE OF lt_result.
    FIELD-SYMBOLS : <lv_value> TYPE any.
    FIELD-SYMBOLS : <ls_data> type any,
                                <lt_tab> TYPE lvc_t_modi.

  ASSIGN io_data->m_data->* TO <lt_tab>.

   lt_fcat = get_fcat( it_data = it_data ).

   READ TABLE lt_fcat into ls_fcat with key fieldname = i_retfield.
   CHECK sy-subrc = 0.
    lv_scrfield = i_scrfield.

*------------------------------------------------
* 1개만 대상인 경우 바로 리턴해 주는 로직을 추가해 준다.(Start)
*------------------------------------------------
* 구형코드
*---------------
*    CALL METHOD disp_f4(
*      EXPORTING
*        it_data    = it_data
*        i_retfield = i_retfield
*        i_scrfield = lv_scrfield
*        i_display  = i_display
*      IMPORTING
*        et_result  = lt_result
*                     ).

    IF lines( it_data ) > 1.
      CALL METHOD disp_f4(
        EXPORTING
          it_data    = it_data
          i_retfield = i_retfield
          i_scrfield = lv_scrfield
          i_display  = i_display
        IMPORTING
          et_result  = lt_result
*        CHANGING
*          ct_fcat    = lt_fcats
                       ).
    ELSEIF lines( it_data ) = 1.
     READ TABLE it_data ASSIGNING <ls_data> INDEX 1.
      ASSIGN COMPONENT i_retfield OF STRUCTURE <ls_data> TO <lv_value>.
      IF sy-subrc = 0.
        ls_result-fieldval = <lv_value>.
        APPEND ls_result TO lt_result.
      ENDIF.
    ENDIF.
*------------------------------------------------
* 1개만 대상인 경우 바로 리턴해 주는 로직을 추가해 준다. (End)
*------------------------------------------------


    CHECK lt_result IS NOT INITIAL.
    READ TABLE lt_result INTO ls_result INDEX 1.
    CHECK sy-subrc = 0.

    if ls_fcat-edit_mask is not INITIAL.
      lv_fname = ls_fcat-edit_mask.
      REPLACE all OCCURRENCES OF lc_exit2 in lv_fname WITH ''.
      CONCATENATE lc_exit lv_fname lc_exit1 INTO lv_fname.
      try.
          CALL FUNCTION lv_fname
            EXPORTING
              input         = ls_result-fieldval
           IMPORTING
             OUTPUT        = ls_modi-value
                .
          Catch CX_SY_DYN_CALL_ILLEGAL_FUNC.
            return.
      endtry.

    else.
      ls_modi-value       = ls_result-fieldval.
    endif.


    ls_modi-row_id      = is_row_no-row_id.
    ls_modi-fieldname   = i_scrfield.
    APPEND ls_modi to <lt_tab>.
    io_data->m_event_handled = mc_x.

  ENDMETHOD.
  METHOD disp_f4.

    DATA : lt_fields    TYPE STANDARD TABLE OF dynpread,
           lt_f4_return TYPE STANDARD TABLE OF ddshretval,
           lv_prog      TYPE sy-cprog,
           lv_prog_old  TYPE sy-cprog,
           ls_fields    LIKE LINE OF lt_fields.
    FIELD-SYMBOLS : <ls_data>  TYPE any, <lv_field> TYPE any.

    CHECK it_data IS NOT INITIAL.

    lv_prog_old = sy-cprog.
    IF i_prog IS NOT INITIAL.
      sy-cprog = i_prog.
    ENDIF.
    IF lines( it_data ) > 1.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = i_retfield                     "Value_tab 의 Internal Table에서 사용자가 선택후 되돌려줄 Field Name.
          dynpprog        = sy-cprog                       "이 부분을 변수로 처리하게 되면 [F4]화면이 올라온 후 [focus]가 이동하지 않는 현상을 접하게 된다.
          dynpnr          = sy-dynnr
          display         = i_display                      "화면을 비활성창으로 열것인가?
          dynprofield     = i_scrfield            "RETFIELD 에 의해 선택된 값이 화면상에 들어올 개체이름. -Low/-High가 들어가면 데이타가 안올라 올수 있다.
          value_org       = 'S'                      "Return종류 C [Cell], S[Structure]
        TABLES
          value_tab       = it_data                 "화면에 표시될 itab (내 마음대로 정의한 itab사용가능)
          return_tab      = lt_f4_return    "POP후 선택된 값이 들어올 테이블. (이것을 생략하면, 비동기로 처리되어 원하는 않게 동작할 수 있다.)
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      sy-cprog = lv_prog_old.
      et_result = lt_f4_return.
    ELSE.
      READ TABLE it_data ASSIGNING <ls_data> INDEX 1.
      ASSIGN COMPONENT i_retfield OF STRUCTURE <ls_data> TO <lv_field>.
      CHECK sy-subrc = 0.
      ls_fields-fieldname   = i_scrfield.
      ls_fields-fieldvalue  = <lv_field>.
      APPEND ls_fields TO lt_fields.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = lv_prog
          dynumb               = sy-dynnr
        TABLES
          dynpfields           = lt_fields
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          undefind_error       = 7
          OTHERS               = 8.
      APPEND VALUE #( fieldval = <lv_field> retfield = i_scrfield ) TO et_result.
    ENDIF.

  ENDMETHOD.
  METHOD set_position.

    DATA : ls_row_id    TYPE lvc_s_row,
           ls_column_id TYPE lvc_s_col,
           ls_row_no    TYPE lvc_s_roid.

    ls_row_id-index     = i_row.
    ls_column_id-fieldname  = i_fieldname.

    CALL METHOD io_alv->set_current_cell_via_id
      EXPORTING
        is_row_id    = ls_row_id           " Row No
        is_column_id = ls_column_id   " Column No
        is_row_no    = ls_row_no.        " Row No.

  ENDMETHOD.
  METHOD write_scr_field.

    DATA : lt_data TYPE STANDARD TABLE OF dynpread.
    lt_data = CORRESPONDING #( it_fields MAPPING fieldname = retfield fieldvalue = fieldval ).
    lt_data = VALUE #(  BASE lt_data
                    ( fieldname = i_fieldname fieldvalue = i_fieldvalue  )
                  ).
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = i_prog
        dynumb     = i_dynnr
      TABLES
        dynpfields = lt_data
*     EXCEPTIONS
*       INVALID_ABAPWORKAREA       = 1
*       INVALID_DYNPROFIELD        = 2
*       INVALID_DYNPRONAME         = 3
*       INVALID_DYNPRONUMMER       = 4
*       INVALID_REQUEST            = 5
*       NO_FIELDDESCRIPTION        = 6
*       UNDEFIND_ERROR             = 7
*       OTHERS     = 8
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
  METHOD refresh_alv.

    DATA : ls_stbl TYPE lvc_s_stbl,
           lv_soft TYPE char01.
    lv_soft = ls_stbl-row = ls_stbl-col  = mc_x.
    IF i_normal IS NOT INITIAL.
      CLEAR : lv_soft, ls_stbl.
    ENDIF.
    CALL METHOD io_alv->refresh_table_display
      EXPORTING
        is_stable      = ls_stbl
        i_soft_refresh = lv_soft
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.
  ENDMETHOD.
  method READ_SCR_FIELD.
    DATA : lt_dyread TYPE STANDARD TABLE OF dynpread,
           lv_prog   type sy-cprog,
           ls_dyread LIKE LINE OF lt_dyread.

    if lv_prog is INITIAL.
      lv_prog = sy-cprog.
    else.
      lv_prog = i_prog.
    endif.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = lv_prog
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
        request            = 'A'
      TABLES
        dynpfields         = lt_dyread
     EXCEPTIONS
         INVALID_ABAPWORKAREA
         INVALID_DYNPROFIELD
         INVALID_DYNPRONAME
         INVALID_DYNPRONUMMER
         INVALID_REQUEST
         NO_FIELDDESCRIPTION
         INVALID_PARAMETER
         UNDEFIND_ERROR
         DOUBLE_CONVERSION
         STEPL_NOT_FOUND
         .
    SORT lt_dyread BY fieldname.
    READ TABLE lt_dyread INTO ls_dyread WITH KEY fieldname = i_fieldname BINARY SEARCH.
    IF sy-subrc = 0.
      r_value =  ls_dyread-fieldvalue.
    ENDIF.
  endmethod.
  METHOD get_layout.

    CONSTANTS : lc_col  TYPE char5 VALUE '*COL*',
                lc_styl TYPE char6 VALUE '*CELL*'.
    DATA : lr_dref TYPE REF TO data.
    DATA : lo_typedescr   TYPE REF TO cl_abap_structdescr.
    DATA : ls_comp  TYPE LINE OF abap_compdescr_tab.
    CREATE DATA : lr_dref LIKE LINE OF it_tab.

    lo_typedescr ?=  cl_abap_typedescr=>describe_by_data_ref( lr_dref ).
    LOOP AT lo_typedescr->components INTO ls_comp.
      CHECK ls_comp-type_kind = mc_h_lower.  "table type

      IF ls_comp-name CP lc_col.
        rs_layout-ctab_fname = ls_comp-name.
      ENDIF.
      IF ls_comp-name CP lc_styl.
        rs_layout-stylefname = ls_comp-name.
      ENDIF.
    ENDLOOP.

*Provide the fieldname of the celltab field by using field
* STYLEFNAME of the layout structure.
* =======================================
*  IF rs_layout-stylefname IS INITIAL.
*    IF i_incl_style_tab IS NOT INITIAL.
*      rs_layout-stylefname = mv_celltab.         "Stylefname
*    ENDIF.
*  ENDIF.

* General display options
* =======================================
*  IF rs_layout-cwidth_opt IS INITIAL.
*    IF i_no_col_opt IS INITIAL.
*      rs_layout-cwidth_opt = mc_x.          "ALV Column Optimization
*    ENDIF.
*  ENDIF.

    IF rs_layout-smalltitle IS INITIAL.
      rs_layout-smalltitle = mc_x.          "ALV Title Size Small
    ENDIF.

    rs_layout-zebra      = mc_x.          "ALV Line Zebra

*rs_layout-box_fname  = 'BOX'.
* ICON.
* =======================================
*  rs_layout-excp_fname = g_lights_name.      "ALV ICON

* Grid customizing
* Title between grid control and toolbar(70# ##)
* =======================================
*  IF rs_layout-grid_title IS INITIAL.
*    IF i_grid_title IS NOT INITIAL.
*      rs_layout-grid_title = i_grid_title.    "ALV ##: ### ###
*    ENDIF.
*  ENDIF.

*Column headers are hidden
* =======================================
* rs_layout-no_headers = mc_x.            "ALV No Header

* Columns are displayed without horizontal grid lines
* =======================================
* rs_layout-no_hgridln = mc_x.            "ALV No Grid Line

* Cells are not merged when a column is sorted
* =======================================
* rs_layout-no_merging = mc_x.            "ALV No Merge

* the button at the beginning of a row is hidden in selection modes
* cell selection ( SEL_MODE = 'D' ) and column/row selection
* CHECK BOX ### #### NO_ROMARK = X
* ( SEL_MODE = 'A' ).
** =======================================
    IF rs_layout-no_rowmark IS INITIAL.
      rs_layout-no_rowmark = space.             "(or 'X') ALV No Row Mark
    ENDIF.

*Selection modes for SEL_MODE
* 'A' : Column and row selection
* 'B' : Simple selection, list box
* 'C' : Multiple selection, list box
* 'D' : Cell selection
* =======================================
    IF rs_layout-sel_mode IS INITIAL.
      rs_layout-sel_mode = mc_a.                 "ALV Column and row selection
    ENDIF.

*If this field is set, the toolbar is hidden.
* =======================================
*  IF rs_layout-no_toolbar IS INITIAL.
*    IF i_no_toolbar IS NOT INITIAL.
*      rs_layout-no_toolbar = mc_x.            "ALV No Toolbar
*    ENDIF.
*  ENDIF.

*columns are displayed without vertical grid lines
* =======================================
* rs_layout-no_vgridln = mc_x.            "ALV Vertical No line

* Set status of all cells to editable using the layout structure.
* =======================================
*  IF rs_layout-edit IS INITIAL.
*    rs_layout-edit = i_edit.
*  ENDIF.



*Delete Key # Function Key Disabled Setting
* =====================================================
    IF rs_layout-no_rowins IS INITIAL.
      rs_layout-no_rowins = mc_x.
    ENDIF.
  ENDMETHOD.
  METHOD get_variant.
    CONCATENATE sy-cprog i_name INTO r_variant.
  ENDMETHOD.
  METHOD set_fcat_name.
    cs_fcat-scrtext_s = cs_fcat-scrtext_m = cs_fcat-scrtext_l = cs_fcat-tooltip = cs_fcat-coltext = i_name.
  ENDMETHOD.
  method GET_UUIDX16.
    r_rtn = cl_system_uuid=>CREATE_UUID_X16_STATIC( ).
  endmethod.
    method SET_F4.

    data : ls_fcat LIKE LINE OF it_fcat.
    data : lt_f4_list type LVC_T_F4,
           ls_f4      type lvc_s_f4.
    ls_f4-register = mc_x.
    loop at it_fcat into ls_fcat where f4availabl = mc_x.
      ls_f4-fieldname  = ls_fcat-fieldname.
      insert ls_f4 into TABLE lt_f4_list.
    ENDLOOP.

    CALL METHOD io_alv->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4  = lt_f4_list
      .
  endmethod.
  METHOD get_fcat.
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




    ENDIF.
  ENDMETHOD.

  METHOD get_fields.
    DATA : lr_dref TYPE REF TO data.
    CREATE DATA lr_dref TYPE (i_struc).
    DATA : ls_comp   TYPE abap_compdescr,
           ls_fields LIKE LINE OF rt_fields.

    DATA : lo_typedescr   TYPE REF TO cl_abap_structdescr.
    lo_typedescr ?=  cl_abap_typedescr=>describe_by_data_ref( lr_dref ).

    LOOP AT lo_typedescr->components INTO ls_comp.
      CONCATENATE i_prefix ls_comp-name INTO ls_fields.
      APPEND ls_fields TO rt_fields.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
