*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dynpro_pbo OUTPUT.
  cl_bus_abstract_screen=>dynpro_pbo(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.


*MODULE status_0100 OUTPUT.
*  SET PF-STATUS 'PF_0100'.
*  SET TITLEBAR 'TI_0100' WITH gv_title.
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_0100  OUTPUT
*&---------------------------------------------------------------------*
*       0100 Screen implementation
*----------------------------------------------------------------------*


MODULE init_0100_grid_value OUTPUT.
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
    gs_vari1-report   =  lcl_module=>get_variant( i_name = 'go_grid1' ).
*
*    IF gv_prg_mode <> gc_prg_mode_display.
*      PERFORM fc_set_style_base    USING   gt_fcat2
*                                CHANGING  gt_style_append.
*    ENDIF.
  ENDIF.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module DYNPRO_0100_BEGIN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE dynpro_0100_begin OUTPUT.
  cl_bus_abstract_screen=>dynpro_pbo_begin(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.
