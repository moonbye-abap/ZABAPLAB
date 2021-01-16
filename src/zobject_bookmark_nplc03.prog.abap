*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC03
*&---------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.
  METHOD tree_select.
    "-----------------------------------------
    " Master Table에서 사용자 ID에 해당하는 Node 정보를 읽어들인다.
    "-----------------------------------------
    DATA : lo_module TYPE REF TO lcl_module.
    CREATE OBJECT lo_module.
    DATA(lt_select) = lo_module->get_fields( i_struc = 'ZOBJECTBOOK' ).

*    SELECT *
*      INTO TABLE @DATA(lt_1)
*      FROM zobjectbook.
*    DELETE zobjectbook FROM TABLE lt_1.

    SELECT (lt_select)
      INTO CORRESPONDING FIELDS OF TABLE ct_nodes
      FROM zobjectbook
     WHERE type IN ( 'N','T' )  "Node , Table
       AND usrid = i_usr
      .

    IF ct_nodes IS INITIAL.
      APPEND VALUE #( guid = lcl_module=>get_uuidx16( ) usrid = sy-uname type = 'N' name = 'Root' description = 'Root' )
            TO ct_nodes.

      CALL METHOD tree_save( ct_nodes ).
    ENDIF.

  ENDMETHOD.
  METHOD acctable_save.
    DATA : lt_zobjectbook TYPE STANDARD TABLE OF zobjectbook,
           ls_zobjectbook LIKE LINE OF lt_zobjectbook,
           lv_idx         TYPE sy-tabix,
           lt_list3       TYPE gty_t_list3,
           ls_list3       LIKE LINE OF gt_list3.


    LOOP AT ct_acc_table INTO DATA(ls_list) WHERE vmode > ' '.
      lv_idx = sy-tabix.
      ls_zobjectbook = CORRESPONDING #( ls_list ).
      CASE ls_list-vmode.
        WHEN gc_alv_mode_append.
          ls_zobjectbook-guid = lcl_module=>get_uuidx16( ).
      ENDCASE.
      APPEND ls_zobjectbook TO lt_zobjectbook.

      "DB갱신 성공시 변경되어야 할 내역을 넣어준다.
      ls_list-vidx  = lv_idx.
      ls_list-guid  = ls_zobjectbook-guid.
      APPEND ls_list TO lt_list3.
    ENDLOOP.
    MODIFY zobjectbook FROM TABLE lt_zobjectbook.
    IF sy-subrc = 0.
      LOOP AT lt_list3 INTO ls_list3.
        ls_list = ct_acc_table[ ls_list3-vidx ].
        ls_list-guid = ls_list3-guid.
        CLEAR : ls_list-vmode.
        MODIFY ct_acc_table FROM ls_list INDEX ls_list3-vidx TRANSPORTING vmode guid.
      ENDLOOP.
      lcl_module=>commit( ).
    ELSE.
      ROLLBACK WORK.
      e_err_chk = gc_e.
    ENDIF.
  ENDMETHOD.
  METHOD acctable_delete.
    DATA : lt_zobjectbook TYPE STANDARD TABLE OF zobjectbook.
    lt_zobjectbook = CORRESPONDING #( it_acc_table ).
    DELETE zobjectbook FROM TABLE lt_zobjectbook.
    lcl_module=>commit( ).
  ENDMETHOD.
  METHOD acctable_select_single.
    SELECT SINGLE usrid
      INTO @DATA(lv_usrid)
      FROM zobjectbook
     WHERE usrid = @i_uname
       AND type  = 'M'
       .
    IF sy-subrc = 0.
      SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF @r_rtn
      FROM zobjectbook
     WHERE usrid = @i_uname
       AND type  = 'M'
       AND name  = @i_name.
    ELSE.
      r_rtn-zcreate = r_rtn-zread = r_rtn-zupdate = r_rtn-zdelete = gc_x.
      r_rtn-name = i_name.
    ENDIF.
  ENDMETHOD.
  METHOD seltable_delete.
    FIELD-SYMBOLS : <lt_list> TYPE STANDARD TABLE,
                    <ls_list> TYPE any.
    DATA : lt_list TYPE REF TO data,
           ls_list TYPE REF TO data.
    CREATE DATA lt_list TYPE STANDARD TABLE OF (gs_acc_table-name).
    CREATE DATA ls_list TYPE (gs_acc_table-name).
    ASSIGN lt_list->* TO <lt_list>.
    ASSIGN ls_list->* TO <ls_list>.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_table>).
      ASSIGN COMPONENT 'VMODE' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<lv_vmode>).
      CHECK sy-subrc = 0.
      CHECK <lv_vmode> <> gc_alv_mode_append.

      <ls_list> = CORRESPONDING #( <ls_table> ).
      APPEND <ls_list> TO <lt_list>.
    ENDLOOP.
    DELETE (gs_acc_table-name) FROM TABLE <lt_list>.
    IF sy-subrc = 0.
      lcl_module=>commit( ).
    ELSE.
      e_err_chk = gc_x.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.
  METHOD seltable_select.
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE ct_table
   FROM (i_name).
  ENDMETHOD.

  METHOD acctable_select.
    "-----------------------------------------
    " Master Table에서 사용자 ID에 해당하는 접근가능 Table 정보를 읽어들인다.
    "-----------------------------------------
    DATA : lo_module TYPE REF TO lcl_module.
    CREATE OBJECT lo_module.
    DATA(lt_select) = lo_module->get_fields( i_struc = 'ZOBJECTBOOK' ).

    CLEAR : ct_acc_table.
    SELECT  a~guid,
            a~type,
            a~usrid,
            b~name_text AS usrid_nm,
            a~name,
            a~description,
            a~zcreate,
            a~zread,
            a~zupdate,
            a~zdelete
      INTO CORRESPONDING FIELDS OF TABLE @ct_acc_table
      FROM zobjectbook AS a INNER JOIN v_usr_name AS b
        ON a~usrid = b~bname
     WHERE a~type = 'M'  "Master
      ORDER BY a~usrid , a~name
      .


  ENDMETHOD.
  METHOD tree_save.
    DATA(lv_idx) = lines( it_tree ).
    DATA(ls_tree) = it_tree[ lv_idx ].
    DATA : lt_zobjectbook TYPE STANDARD TABLE OF zobjectbook,
           ls_zobjectbook LIKE LINE OF lt_zobjectbook.

    ls_zobjectbook = CORRESPONDING #( ls_tree ).
    APPEND ls_zobjectbook TO lt_zobjectbook.

    MODIFY zobjectbook FROM TABLE lt_zobjectbook.
    lcl_module=>commit( ).

  ENDMETHOD.
  METHOD tree_update.
    DATA : lt_zobjectbook TYPE STANDARD TABLE OF zobjectbook,
           ls_zobjectbook LIKE LINE OF lt_zobjectbook.

    ls_zobjectbook = CORRESPONDING #( is_tree ).
    APPEND ls_zobjectbook TO lt_zobjectbook.

    MODIFY zobjectbook FROM TABLE lt_zobjectbook.
    lcl_module=>commit( ).

  ENDMETHOD.
ENDCLASS.
