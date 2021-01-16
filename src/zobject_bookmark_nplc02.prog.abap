*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC02
*&---------------------------------------------------------------------*

CLASS lcl_scr2000 IMPLEMENTATION.
  METHOD grid3_define_event_process.
* Create Event Handler
*  IF go_event_receiver IS INITIAL.
    CREATE OBJECT go_event
      EXPORTING
        i_gubn = 'SCR2000_GRID3'.
    .
*  ENDIF.
    SET HANDLER go_event->handle_hotspot_click        "hotspot Triggered
            FOR go_grid3.
    SET HANDLER go_event->handle_toolbar
            FOR go_grid3.
    SET HANDLER go_event->handle_user_command
            FOR go_grid3.
    SET HANDLER go_event->handle_onf4
            FOR go_grid3.
*  SET HANDLER go_event_receiver->handle_data_changed_finished
*          FOR go_grid3.
    SET HANDLER go_event->handle_data_changed
            FOR go_grid3.



* HANDLE_DATA_CHANGED events are excuted when ALV Grid data changed
    CALL METHOD go_grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.       "By Enter

    CALL METHOD go_grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.    "By Changed

  ENDMETHOD.
  METHOD grid3_display.
    gs_layo3-col_opt   = space.
    CALL METHOD go_grid3->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo3
        is_variant           = gs_vari3
        it_toolbar_excluding = gt_exld3
        is_print             = gs_prnt3
        i_save               = gc_a
        i_default            = space
      CHANGING
        it_sort              = gt_sort3[]
        it_fieldcatalog      = gt_fcat3[]
        it_outtab            = gt_list3.

    IF gv_prg_mode = gc_prg_mode_display.
      CALL METHOD go_grid3->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
    ENDIF.
  ENDMETHOD.
  METHOD grid3_fcat_build.
    DATA : ls_fcat TYPE lvc_s_fcat.

    FREE : ct_fcat.

    ct_fcat = lcl_module=>get_fcat( it_data = it_list ).
*######################################################################*
*   FIELD Attribute Setting
*######################################################################*
    LOOP AT ct_fcat INTO ls_fcat.
      CASE ls_fcat-fieldname.
        WHEN 'VMODE'.
        WHEN 'USRID'.
          ls_fcat-col_pos  = 10.
          ls_fcat-key      = gc_x.
          ls_fcat-outputlen = 10.
          ls_fcat-edit     = gc_x.
          ls_fcat-f4availabl = gc_x.
          ls_fcat-ref_table = ls_fcat-ref_field = ' '.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'USER ID' CHANGING cs_fcat = ls_fcat ).

        WHEN 'USRID_NM'.
          ls_fcat-col_pos  = 15.
          ls_fcat-outputlen = 10.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'Name' CHANGING cs_fcat = ls_fcat ).

        WHEN 'NAME'.
          ls_fcat-col_pos  = 20.
          ls_fcat-key      = gc_x.
          ls_fcat-outputlen = 10.
          ls_fcat-edit     = gc_x.
          ls_fcat-f4availabl = gc_x.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'TABLE' CHANGING cs_fcat = ls_fcat ).

        WHEN 'DESCRIPTION'.
          ls_fcat-col_pos  = 30.
          ls_fcat-outputlen = 20.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'Description' CHANGING cs_fcat = ls_fcat ).

        WHEN 'ZCREATE'.
          ls_fcat-col_pos  = 40.
          ls_fcat-checkbox = gc_x.
          ls_fcat-outputlen = 7.
          ls_fcat-edit     = gc_x.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'Create' CHANGING cs_fcat = ls_fcat ).

        WHEN 'ZREAD'.
          ls_fcat-col_pos  = 50.
          ls_fcat-checkbox = gc_x.
          ls_fcat-outputlen = 7.
          ls_fcat-edit     = gc_x.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'Read' CHANGING cs_fcat = ls_fcat ).

        WHEN 'ZUPDATE'.
          ls_fcat-col_pos  = 60.
          ls_fcat-checkbox = gc_x.
          ls_fcat-outputlen = 7.
          ls_fcat-edit     = gc_x.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'Update' CHANGING cs_fcat = ls_fcat ).

        WHEN 'ZDELETE'.
          ls_fcat-col_pos  = 70.
          ls_fcat-checkbox = gc_x.
          ls_fcat-outputlen = 7.
          ls_fcat-edit     = gc_x.
          CALL METHOD lcl_module=>set_fcat_name( EXPORTING i_name = 'Delete' CHANGING cs_fcat = ls_fcat ).



**######################################################################*
        WHEN OTHERS.
          ls_fcat-no_out   = gc_x.

**######################################################################*
      ENDCASE.
*
      MODIFY ct_fcat FROM ls_fcat.
    ENDLOOP.
*    ls_fcat-edit = gc_x.
*    MODIFY ct_fcat FROM ls_fcat TRANSPORTING edit WHERE key = ' ' AND no_out = ' '.
    ls_fcat-edit = ' '.
    MODIFY ct_fcat FROM ls_fcat TRANSPORTING edit WHERE fieldname CP '*_NM'.
  ENDMETHOD.
  METHOD grid3_define_value.

    CALL METHOD grid3_fcat_build(
      EXPORTING
        it_list = gt_list3
      CHANGING
        ct_fcat = gt_fcat3 ).
*.............> Define ALV Standard button attributes
*               -----------------------
    CALL METHOD lcl_module=>set_f4( io_alv = go_grid3 it_fcat = gt_fcat3 ).
    gt_exld3          =  lcl_module=>get_excl_buttons( ).
    gs_layo3          =  lcl_module=>get_layout( it_tab  = gt_list3 ).
    gs_vari3-report   =  lcl_module=>get_variant( i_name = 'GO_GRID3' ).

    IF gv_prg_mode <> gc_prg_mode_display.
*      PERFORM fc_set_style_base    USING   gt_fcat2
*                                CHANGING  gt_style_append.
    ENDIF.
  ENDMETHOD.
  METHOD grid3_create_container.
    CREATE OBJECT go_dock3
      EXPORTING
        repid = sy-repid
        dynnr = sy-dynnr
        side  = go_dock3->dock_at_bottom
        ratio = 80
*       extension = 3000.
      .
* # ALV GRID ##
    CREATE OBJECT go_grid3
      EXPORTING
*       i_shellstyle = ctld_ws_thickframe
        i_parent = go_dock3.
  ENDMETHOD.

  METHOD pov_onf4_usrid.
    DATA : lv_value TYPE char50.
    DATA(lv_rtn) = ycl_commons=>read_scr_field( i_fieldname = 'P_USRID' ).
    IF lv_rtn IS INITIAL.
      lv_value = '%'.
    ELSE.
      lv_value = lv_rtn.
    ENDIF.
    SELECT bname AS usrid,
           name_first,
           name_last,
           name_text
      INTO TABLE @DATA(lt_f4_user)
      FROM v_usr_name
     WHERE bname LIKE @lv_value.

    CALL METHOD ycl_commons=>disp_f4_and_pai(
        it_data    = lt_f4_user
        i_retfield = 'USRID'
        i_scrfield = 'P_USRID'
*       i_display  = 'X'
      ).


  ENDMETHOD.
  METHOD pov_onf4_table.
    DATA : lv_value TYPE char50.
    DATA(lv_rtn) = ycl_commons=>read_scr_field( i_fieldname = 'P_TABLE' ).
    IF lv_rtn IS INITIAL.
      lv_value = '%'.
    ELSE.
      lv_value = lv_rtn.
    ENDIF.

    SELECT  a~tabname ,
            b~tabclass,
            a~ddtext
      INTO TABLE @DATA(lt_f4_table)
      FROM dd02t AS a INNER JOIN dd02l AS b
        ON a~tabname  = b~tabname
       AND a~as4local = b~as4local
       AND a~as4vers  = b~as4vers
     WHERE a~ddlanguage = @sy-langu
       AND a~tabname LIKE @lv_value
       AND b~tabclass IN ( 'TRANSP' , 'VIEW' )
      .


    CALL METHOD ycl_commons=>disp_f4_and_pai(
        it_data    = lt_f4_table
        i_retfield = 'TABNAME'
        i_scrfield = 'P_TABLE'
*       i_display  = 'X'
      ).


  ENDMETHOD.
  METHOD pbo.
    SET PF-STATUS 'PF_0100'.
    IF go_dock3 IS INITIAL.
*   # create container object
      CALL METHOD grid3_create_container( ).

*   # Define ALV event             (SET HANDLER)
      CALL METHOD grid3_define_event_process( ).

*   # Define ALV Detail attributes (Field category,LAYOUT,SORT, etc)
      CALL METHOD grid3_define_value( ).
*
*   # ALV Grid Display             (Display Screen)
      CALL METHOD grid3_display( ).
    ELSE.
      lcl_module=>refresh_alv( EXPORTING io_alv = go_grid3 ).
    ENDIF.
  ENDMETHOD.
  METHOD pai.
    DATA : lt_filter TYPE lvc_t_filt.
    "사용자가 입력한 조건으로 FILTER를 실행한다.
    IF p_usrid IS NOT INITIAL.
      lt_filter = VALUE #(
            ( fieldname = 'USRID' sign = 'I' option = 'CP' low = p_usrid )
            ).
    ENDIF.
    IF p_table IS NOT INITIAL.
      lt_filter = VALUE #( BASE  lt_filter
            ( fieldname = 'NAME' sign = 'I' option = 'CP' low = P_TABLE )
            ).

    ENDIF.
    CALL METHOD go_grid3->set_filter_criteria
      EXPORTING
        it_filter = lt_filter
*      EXCEPTIONS
*       no_fieldcatalog_available = 1
*       others    = 2
      .
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.
    lcl_module=>refresh_alv( EXPORTING io_alv = go_grid3 i_normal = gc_x ).
  ENDMETHOD.
  METHOD grid3_event_user_command.
    DATA : lv_ans TYPE c.

    DATA : ls_list3 LIKE LINE OF gt_list3,
           lv_line  TYPE char50,
           lv_pos   TYPE sy-tabix,
           lv_lines TYPE sy-tabix.
    CASE i_ucomm.
      WHEN 'INS_LINE'.
        lv_lines = 1.
        lv_pos   = lines( gt_list3 ) + 1.
        DO lv_lines TIMES.
          ls_list3-vmode = gc_alv_mode_append.
          ls_list3-type  = gc_m.
*          PERFORM fc_set_style CHANGING ls_list3.
          APPEND ls_list3 TO gt_list3.
        ENDDO.
        CALL METHOD lcl_module=>refresh_alv( io_alv = go_grid3 ).
        CALL METHOD lcl_module=>set_position( io_alv = go_grid3 i_row = lv_pos i_fieldname = 'USRID' ).
      WHEN 'DEL_LINE'.
        lv_ans = lcl_module=>ask_question( EXPORTING i_question = '삭제 하시겠습니까??'  ).
        CHECK lv_ans = '1'.
        DATA : lt_row TYPE lvc_t_row,
               ls_row LIKE LINE OF lt_row.
        CALL METHOD go_grid3->get_selected_rows
          IMPORTING
            et_index_rows = lt_row.
        LOOP AT lt_row INTO ls_row.
          READ TABLE gt_list3 INTO ls_list3 INDEX ls_row-index.
          CHECK sy-subrc = 0.
          IF ls_list3-vmode <> gc_alv_mode_append.
*            ls_list3-zdel    = gc_x.
            APPEND ls_list3 TO gt_list3_delete.
          ENDIF.
          ls_list3-vmode = gc_alv_mode_delete.
          MODIFY gt_list3 FROM ls_list3 INDEX ls_row-index TRANSPORTING vmode .
        ENDLOOP.
        DELETE gt_list3 WHERE vmode = gc_alv_mode_delete.
        CALL METHOD lcl_module=>refresh_alv( io_alv = go_grid3 ).
        CALL METHOD lcl_model=>acctable_delete( EXPORTING it_acc_table = gt_list3_delete ).
        CLEAR :  gt_list3_delete.
      WHEN 'SAVE'.
        lv_ans = lcl_module=>ask_question( i_question = '저장 하시겠습니까?' ).
        CHECK lv_ans = gc_1.
        CALL METHOD lcl_model=>acctable_save( IMPORTING e_err_chk = gv_err_chk CHANGING ct_acc_table = gt_list3 ).
        CHECK gv_err_chk = 0.
        MESSAGE s000 WITH '저장하였습니다.'.
        lcl_module=>refresh_alv( EXPORTING io_alv = go_grid3 ).
    ENDCASE.
  ENDMETHOD.
  METHOD grid3_event_data_changed.
*          er_data_changed TYPE REF TO  cl_alv_changed_data_protocol
*          e_onf4          TYPE  char01
*          e_onf4_before   TYPE  char01
*          e_onf4_after    TYPE  char01
*          e_ucomm         TYPE  sy-ucomm,
*
    DATA(lt_temp) = er_data_changed->mt_good_cells.
    DATA : lr_table TYPE RANGE OF dd02t-tabname.
    DATA : lr_usrid TYPE RANGE OF v_usr_name-bname.
    DELETE lt_temp WHERE fieldname <> 'NAME'.
    DELETE lt_temp WHERE value IS INITIAL.
    IF lt_temp IS NOT INITIAL.
      SORT lt_temp BY value.
      DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING value.

      lr_table = CORRESPONDING #( lt_temp MAPPING low = value ).
      SELECT a~tabname, a~ddtext
        INTO TABLE @DATA(lt_table)
        FROM dd02t AS a
       FOR ALL ENTRIES IN @lr_table
      WHERE a~tabname =  @lr_table-low
        .
      SORT lt_table BY tabname.
    ENDIF.

    lt_temp = er_data_changed->mt_good_cells.
    DELETE lt_temp WHERE fieldname <> 'USRID'.
    DELETE lt_temp WHERE value IS INITIAL.
    IF lt_temp IS NOT INITIAL.
      SORT lt_temp BY value.
      DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING value.

      lr_usrid = CORRESPONDING #( lt_temp MAPPING low = value ).
      SELECT bname AS usrid,
         name_text
    INTO TABLE @DATA(lt_usrid)
    FROM v_usr_name
        FOR ALL ENTRIES IN @lr_usrid
   WHERE bname =  @lr_usrid-low
   .
      SORT lt_usrid BY usrid.
    ENDIF.




    LOOP AT  er_data_changed->mt_good_cells INTO DATA(ls_modi).
      READ TABLE gt_list3 INTO DATA(ls_list3) INDEX ls_modi-row_id.
      CASE ls_modi-fieldname.
        WHEN 'USRID'.
          READ TABLE lt_usrid INTO DATA(ls_usrid) WITH KEY usrid = ls_modi-value BINARY SEARCH.
          IF sy-subrc = 0.
            ls_list3-usrid_nm = ls_usrid-name_text.
          ELSE.
            ls_list3-usrid_nm = ' '.
          ENDIF.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_modi-row_id
              i_fieldname = 'USRID_NM'
              i_value     = ls_list3-usrid_nm.
        WHEN 'NAME'.
          READ TABLE lt_table INTO DATA(ls_table) WITH KEY tabname = ls_modi-value BINARY SEARCH.
          IF sy-subrc = 0.
            ls_list3-description = ls_table-ddtext.
          ELSE.
            ls_list3-description = ' '.
          ENDIF.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_modi-row_id
              i_fieldname = 'DESCRIPTION'
              i_value     = ls_list3-description.


**         'A': Abort (Stop sign)
**         'E': Error (red LED)
**         'W': Warning (yellow LED)
**         'I': Information (green LED)
**
*            CALL METHOD pr_data_changed->add_protocol_entry
*              EXPORTING
*                i_msgid     = 'YMES01'
*                i_msgno     = '000'
*                i_msgty     = 'E'
*                i_msgv1     = ls_modi-value
**               i_msgv2     = l_planetype
**               i_msgv3     = text-m05           "exitstiert nicht
*                i_fieldname = ls_modi-fieldname
*                i_row_id    = ls_modi-row_id.
*
**           error_in_data = 'X'.
**           exit. "plane does not exit, so we're finished here!
      ENDCASE.
*
      IF ls_list3-vmode <> gc_alv_mode_append.
        ls_list3-vmode = gc_alv_mode_modify.
      ENDIF.
      MODIFY gt_list3 FROM ls_list3 INDEX ls_modi-row_id TRANSPORTING vmode description usrid_nm.
    ENDLOOP.
  ENDMETHOD.
  METHOD grid3_event_onf4.
*        IMPORTING
*          e_fieldname   TYPE  lvc_fname
*          e_fieldvalue  TYPE  lvc_value
*          es_row_no     TYPE  lvc_s_roid
*          er_event_data TYPE REF TO  cl_alv_event_data
*          et_bad_cells  TYPE  lvc_t_modi
*          e_display     TYPE  char01,
    DATA : lv_value TYPE string.
    CASE e_fieldname.
      WHEN 'USRID'. "사용자 id
        IF e_fieldvalue IS INITIAL.
          lv_value = '%'.
        ELSE.
          lv_value = e_fieldvalue.
        ENDIF.
        SELECT bname AS usrid,
               name_first,
               name_last,
               name_text
          INTO TABLE @DATA(lt_f4_user)
          FROM v_usr_name
         WHERE bname LIKE @lv_value.

        CALL METHOD lcl_module=>disp_f4_alv(
          EXPORTING
            it_data    = lt_f4_user
            i_retfield = 'USRID'
            i_scrfield = e_fieldname
            is_row_no  = es_row_no
            io_data    = er_event_data
            i_display  = e_display
        ).

      WHEN 'NAME'. "테이블
        IF e_fieldvalue IS INITIAL.
          lv_value = '%'.
        ELSE.
          lv_value = e_fieldvalue.
        ENDIF.

        SELECT  a~tabname ,
                b~tabclass,
                a~ddtext
          INTO TABLE @DATA(lt_f4_table)
          FROM dd02t AS a INNER JOIN dd02l AS b
            ON a~tabname  = b~tabname
           AND a~as4local = b~as4local
           AND a~as4vers  = b~as4vers
         WHERE a~ddlanguage = @sy-langu
           AND a~tabname LIKE @lv_value
           AND b~tabclass IN ( 'TRANSP' , 'VIEW' )
          .



        CALL METHOD lcl_module=>disp_f4_alv(
          EXPORTING
            it_data    = lt_f4_table
            i_retfield = 'TABNAME'
            i_scrfield = e_fieldname
            is_row_no  = es_row_no
            io_data    = er_event_data
            i_display  = e_display
        ).

    ENDCASE.

  ENDMETHOD.
  METHOD grid3_event_toolbar.

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
    ls_toolbar-icon      = icon_delete_row.
    ls_toolbar-text         = 'DELETE'.
    APPEND ls_toolbar TO p_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = 'SAVE'.
    ls_toolbar-icon      = icon_system_save.
    ls_toolbar-text         = 'SAVE'.
    APPEND ls_toolbar TO p_object->mt_toolbar.


  ENDMETHOD.
ENDCLASS.
