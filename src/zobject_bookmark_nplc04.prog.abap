*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC04
*&---------------------------------------------------------------------*


CLASS lcl_event DEFINITION INHERITING FROM cl_gui_object.
  PUBLIC SECTION.
    DATA : mv_gubn TYPE char10.
    METHODS :
      constructor
        IMPORTING
          i_gubn TYPE char10,
      "ALV Hot Spot Click Event
      handle_toolbar
          FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,

      handle_user_command
          FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,

      handle_onf4
          FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display,

      handle_data_changed_finished
          FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING
          e_modified
          et_good_cells,


      handle_data_changed
          FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm,


      handle_button_click
          FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no,

      handle_hotspot_click
                  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id.
ENDCLASS.
CLASS lcl_event IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( ).
    mv_gubn = i_gubn.
  ENDMETHOD.
*======= Hot Spot Click Implementation
  METHOD handle_hotspot_click.
    CASE mv_gubn.
      WHEN 'SCR_0100'.
*     perform handle_hotspot_click1_1   using e_row_id
*                                             e_column_id.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_user_command.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_user_command1        USING e_ucomm.
    ENDCASE.
  ENDMETHOD.
*======= Toolbar Implementation
  METHOD handle_toolbar.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_toolbar1        USING e_object
*                                            e_interactive.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_data_changed_finished.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_data_changed_finished  USING e_modified
*                                                     et_good_cells.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_data_changed.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_data_changed USING er_data_changed
*                                          e_onf4
*                                          e_onf4_before
*                                          e_onf4_after
*                                          e_ucomm.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1

*======= Toolbar Implementation
  METHOD handle_onf4.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_onf4_1        USING e_fieldname
*                                            e_fieldvalue
*                                            es_row_no
*                                            er_event_data
*                                            et_bad_cells
*                                            e_display.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
*======= Toolbar Implementation
  METHOD handle_button_click.
    CASE mv_gubn.
      WHEN 'SCR_0200'.
*        PERFORM handle_button_click1        USING es_col_id
*                                                  es_row_no.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click1_1
ENDCLASS.
