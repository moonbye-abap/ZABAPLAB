*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC01
*&---------------------------------------------------------------------*

CLASS lcl_module     IMPLEMENTATION.
  METHOD tree_add_contextmenu.
    CHECK it_fcodes IS NOT INITIAL.

    DATA : lo_event TYPE REF TO lcl_module.
    CREATE OBJECT lo_event.

    SET HANDLER lo_event->handle_tree_node_cm_req        "Context Menu Request
            FOR co_tree.

    SET HANDLER lo_event->handle_tree_item_cm_req        "Context Menu Request
            FOR co_tree.

    SET HANDLER lo_event->handle_tree_node_cm_sel        "Context Menu Request
            FOR co_tree.

    SET HANDLER lo_event->handle_tree_item_cm_sel        "Context Menu Request
            FOR co_tree.

    lo_event->mt_contextmenu = it_fcodes.

*    mo_event01 ?= lo_event.
    CALL METHOD co_tree->get_registered_events
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
    CALL METHOD co_tree->set_registered_events
      EXPORTING
        events                    = lt_event
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.
    IF sy-subrc <> 0.
      "go_tree1의 추가된 event set정보를 등록해 준다.
      DELETE lt_event WHERE eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
      CALL METHOD co_tree->set_registered_events
        EXPORTING
          events                    = lt_event
        EXCEPTIONS
          cntl_error                = 1
          cntl_system_error         = 2
          illegal_event_combination = 3.
    ENDIF.
  ENDMETHOD.
  METHOD tree_add_toolbar.

    CHECK co_tree IS NOT INITIAL.
    DATA : lo_event TYPE REF TO lcl_module.
    CREATE OBJECT lo_event.

    DATA : lv_butn_type TYPE tb_btype,
           ls_toolbars  LIKE LINE OF it_fcodes
           .
    IF co_toolbar IS INITIAL.
      CALL METHOD co_tree->get_toolbar_object
        IMPORTING
          er_toolbar = co_toolbar.
    ENDIF.

    LOOP AT it_fcodes ASSIGNING FIELD-SYMBOL(<ls_fcodes>) WHERE ufcode IS INITIAL.
      CASE <ls_fcodes>-fcode.
        WHEN mc_hyphen.
          lv_butn_type = cntb_btype_sep.
        WHEN OTHERS.
          READ TABLE it_fcodes TRANSPORTING NO FIELDS WITH KEY ufcode = <ls_fcodes>-fcode.
          IF sy-subrc = 0.
            lv_butn_type = cntb_btype_menu.
          ELSE.
            lv_butn_type = cntb_btype_button.
          ENDIF.
      ENDCASE.
      CALL METHOD co_toolbar->add_button
        EXPORTING
          fcode       = <ls_fcodes>-fcode
          icon        = <ls_fcodes>-icon
          "Button Type은 Domain값을 통해확인할수 있다.[ 3 : 구분선임]
          butn_type   = lv_butn_type
          text        = <ls_fcodes>-text
          quickinfo   = <ls_fcodes>-quickinfo
          is_disabled = <ls_fcodes>-is_disabled
          "Tree를 그려준다.
        .
    ENDLOOP.

    "Event
    DATA(lt_toolbars) = it_fcodes.
    DELETE lt_toolbars WHERE fcode = mc_hyphen.
    GET REFERENCE OF co_toolbar INTO DATA(lv_ref).
    ls_toolbars-reftoolbar = lv_ref->*.
    MODIFY lt_toolbars FROM ls_toolbars TRANSPORTING reftoolbar WHERE fcode > ' '.

    lo_event->mt_toolbar = lt_toolbars.

    SET HANDLER lo_event->handle_toolbar_dropdown FOR co_toolbar.
    SET HANDLER lo_event->handle_toolbar_selected FOR co_toolbar.
  ENDMETHOD.
  METHOD handle_toolbar_dropdown.
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
  METHOD handle_toolbar_selected.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = fcode
      EXCEPTIONS
        function_not_supported = 1.
  ENDMETHOD.
  METHOD handle_tree_item_cm_req.
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
  METHOD handle_tree_item_cm_sel.
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = fcode
      EXCEPTIONS
        function_not_supported = 1.
  ENDMETHOD.
  METHOD handle_tree_node_cm_req.

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
  METHOD handle_tree_node_cm_sel.
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = fcode
      EXCEPTIONS
        function_not_supported = 1.
  ENDMETHOD.
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
