*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  gv_ok_code1 = sy-ucomm.

  CASE gv_ok_code1.
    WHEN  'CANC' OR 'BACK'.
*      PERFORM free_0100_variables.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
*      PERFORM free_0100_variables.
      LEAVE TO CURRENT TRANSACTION.

  ENDCASE.
  clear gv_ok_code1.
ENDMODULE. " USER_COMMAND_0100 INPUT
