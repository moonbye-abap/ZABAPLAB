*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR 'TI_0100' WITH gv_title.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_0100  OUTPUT
*&---------------------------------------------------------------------*
*       0100 Screen implementation
*----------------------------------------------------------------------*
MODULE init_0100_create_container OUTPUT.
  "Module에서의 Data선언은 Global로 인식되므로 Data선언은 모두 Class-method에서 수행해야 한다.
  IF gv_first IS INITIAL.
    "# create container object
    CALL METHOD go_control->create_0100_container(
      EXPORTING
        i_dynnr  = sy-dynnr
        i_repid  = sy-repid
      CHANGING
        co_tree1 = go_tree1
        co_grid1 = go_grid1 ).


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
ENDMODULE. " INIT_0100 OUTPUT

MODULE init_0100_event_process OUTPUT.
  IF gv_first IS INITIAL.
    IF go_event IS INITIAL.
      CREATE OBJECT go_event
        EXPORTING
          i_gubn = 'SCR_0100'.
      .
    ENDIF.
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
ENDMODULE. " INIT_0100 OUTPUT
MODULE init_0100_grid_value OUTPUT.
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
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_0100_GRID_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_0100_grid_display OUTPUT.
  IF gv_first IS INITIAL.
    call method go_control->grid_0100_display(
*      EXPORTING
*        is_layout1 = gs_layo1
*        io_grid1   = go_grid1
*        is_variant1 = gs_vari1
*        it_toolbar_excl1 = gt_exld1
*        is_print1  = gs_prnt1
      CHANGING
*        ct_sort = gt_sort1
        co_tree1 = go_tree1
        ct_fcat1 = gt_fcat1
        ct_list1 = gt_list1
        ct_tree1 = gt_tree1

       ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_GV_FIRST OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_gv_first OUTPUT.
  gv_first = 'X'.
ENDMODULE.
