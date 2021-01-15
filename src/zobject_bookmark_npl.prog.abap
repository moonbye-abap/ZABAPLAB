*&---------------------------------------------------------------------*
*& Report ZOBJECT_BOOKMARK_NPL
*&---------------------------------------------------------------------*
*& 작성자 : Moonbye (바른목소리)
*& 내용 :
*&  1) SAP가이드에 따라 Form을 사용하지 않고 개발함 ( FORM : Obsolete Syntax  )
*&  2) MVC Design Pattern을 적용.
*&  3) BUS Screen framework 을 적용.
*&  4) lcl_tree_assist ( Moonbye Tree Assistant V0.1 )를 사용하여 ALV_TREE를 쉽게 제어하도록 적용.
*&---------------------------------------------------------------------*
REPORT zobject_bookmark_npl MESSAGE-ID ymes01.

INCLUDE zobject_bookmark_npltop.
INCLUDE zobject_bookmark_nplc00.  "lcl_tree_assist ( Moonbye Tree Assistant V0.1 )
INCLUDE zobject_bookmark_nplc01.  "lcl_module ( 공통으로 사용할 수 있는 각종 method들이 들어있다. )
INCLUDE zobject_bookmark_nplc02.  "lcl_scr2000  ( Screen2000 Controller )
INCLUDE zobject_bookmark_nplc03.  "lcl_model ( MVC Model )
INCLUDE zobject_bookmark_nplc04.  "lcl_event ( Event Handler )
INCLUDE zobject_bookmark_nplc05.  "lcl_scr0100 ( BUS Screen framework )
INCLUDE zobject_bookmark_nplc06.  "lcl_scr0200 ( BUS Screen framework )
INCLUDE zobject_bookmark_npli01.  "PAI
INCLUDE zobject_bookmark_nplo01.  "PBO
INCLUDE zobject_bookmark_nplf99.



AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.
    WHEN 2000. "관리자 화면
      CALL METHOD lcl_scr2000=>pbo.

  ENDCASE.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 2000.
      CALL METHOD lcl_scr2000=>pai.
  ENDCASE.

START-OF-SELECTION.

*  PERFORM fc_get_authority.
*  PERFORM fc_get_data.


  CALL METHOD lcl_model=>get_acc_table( EXPORTING i_usr = sy-uname CHANGING ct_acc_table = gt_acc_table ).

  cl_bus_abstract_screen=>get_screen(
    EXPORTING
      iv_program_name  = sy-repid
      iv_dynpro_number = '0100'
    IMPORTING
      ev_screen        = go_scr0100
  ).

  IF go_scr0100 IS BOUND.
    go_scr0100->show( ).
  ENDIF.
