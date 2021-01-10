*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dynpro_0100_pbo_begin OUTPUT.
  cl_bus_abstract_screen=>dynpro_pbo_begin(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.                    "screen_0100_pbo OUTPUT
MODULE dynpro_0100_pbo_end OUTPUT.
  cl_bus_abstract_screen=>dynpro_pbo_end(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.
MODULE dynpro_0200_pbo_begin OUTPUT.
  cl_bus_abstract_screen=>dynpro_pbo_begin(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.                    "screen_0200_pbo OUTPUT
MODULE dynpro_0200_pbo_end OUTPUT.
  cl_bus_abstract_screen=>dynpro_pbo_end(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_TREE_ADD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dynpro_0100_pai_begin INPUT.
  cl_bus_abstract_screen=>dynpro_pai_begin(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.
MODULE dynpro_0100_pai_end INPUT.
  cl_bus_abstract_screen=>dynpro_pai_end(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.
MODULE dynpro_0200_pai_begin INPUT.
  cl_bus_abstract_screen=>dynpro_pai_begin(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.
MODULE dynpro_0200_pai_end INPUT.
  cl_bus_abstract_screen=>dynpro_pai_end(
    EXPORTING
      iv_program_name = sy-repid
      iv_dynpro_number = sy-dynnr
  ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ONF4_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE onf4_name INPUT.
  call method go_scr0200->pov_name.
ENDMODULE.
