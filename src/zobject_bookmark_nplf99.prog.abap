*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLF99
*&---------------------------------------------------------------------*

FORM bus_screen_create
     USING    iv_program_name TYPE bus_screen-program_name
              iv_dynpro_number TYPE bus_screen-dynpro_number
     CHANGING ev_screen        TYPE any.                    "#EC CALLED

* Instantiate local screen classes.
  CASE iv_dynpro_number.

*   The screens of lcl_application
    WHEN '0100'.
      CREATE OBJECT ev_screen TYPE lcl_scr0100
        EXPORTING
          iv_program_name  = iv_program_name
          iv_dynpro_number = iv_dynpro_number.


    WHEN '0200'.
      CREATE OBJECT ev_screen TYPE lcl_scr0200
        EXPORTING
          iv_program_name  = iv_program_name
          iv_dynpro_number = iv_dynpro_number.

  ENDCASE.

ENDFORM.
