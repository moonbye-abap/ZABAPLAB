*&---------------------------------------------------------------------*
*& Report ZOBJECT_BOOKMARK_NPL
*&---------------------------------------------------------------------*
*& 작성자 : Moonbye (바른목소리)
*& 내용 :
*&  1) SAP가이드에 따라 Form을 사용하지 않고 개발함 ( FORM : Obsolete Syntax  )
*&  2) MVC Design Pattern을 적용.
*&---------------------------------------------------------------------*
REPORT zobject_bookmark_npl.

INCLUDE zobject_bookmark_npltop.
INCLUDE zobject_bookmark_nplc01.  "lcl_module
INCLUDE zobject_bookmark_nplc03.  "lcl_model
INCLUDE zobject_bookmark_nplc02.  "lcl_controller
INCLUDE zobject_bookmark_nplc04.  "lcl_event
INCLUDE zobject_bookmark_npli01.
INCLUDE zobject_bookmark_nplo01.
*INCLUDE zobject_bookmark_nplf01.
*INCLUDE zobject_bookmark_nplf99.


START-OF-SELECTION.

*  PERFORM fc_get_authority.
*  PERFORM fc_get_data.
  CREATE OBJECT go_module.
  CREATE OBJECT go_control.
  CREATE OBJECT go_model.


  CALL METHOD go_model->get_acc_table( EXPORTING i_usr = sy-uname CHANGING ct_acc_table = gt_acc_table ).

  CALL SCREEN 0100.
