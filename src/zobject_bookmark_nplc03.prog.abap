*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLC03
*&---------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.
  METHOD get_nodes.
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

      CALL METHOD save_tree( ct_nodes ).
    ENDIF.

  ENDMETHOD.
  METHOD get_acc_table.
    "-----------------------------------------
    " Master Table에서 사용자 ID에 해당하는 접근가능 Table 정보를 읽어들인다.
    "-----------------------------------------
    DATA : lo_module TYPE REF TO lcl_module.
    CREATE OBJECT lo_module.
    DATA(lt_select) = lo_module->get_fields( i_struc = 'ZOBJECTBOOK' ).

    SELECT (lt_select)
      INTO CORRESPONDING FIELDS OF TABLE ct_acc_table
      FROM zobjectbook
     WHERE type = 'M'  "Master
       AND usrid = i_usr
      .

  ENDMETHOD.
  METHOD save_tree.
    DATA(lv_idx) = lines( it_tree ).
    DATA(ls_tree) = it_tree[ lv_idx ].
    DATA : lt_zobjectbook TYPE STANDARD TABLE OF zobjectbook,
           ls_zobjectbook LIKE LINE OF lt_zobjectbook.

    ls_zobjectbook = CORRESPONDING #( ls_tree ).
    APPEND ls_zobjectbook TO lt_zobjectbook.

    MODIFY zobjectbook FROM TABLE lt_zobjectbook.
    lcl_module=>commit( ).

  ENDMETHOD.
  METHOD update_tree.
    DATA : lt_zobjectbook TYPE STANDARD TABLE OF zobjectbook,
           ls_zobjectbook LIKE LINE OF lt_zobjectbook.

    ls_zobjectbook = CORRESPONDING #( is_tree ).
    APPEND ls_zobjectbook TO lt_zobjectbook.

    MODIFY zobjectbook FROM TABLE lt_zobjectbook.
    lcl_module=>commit( ).

  ENDMETHOD.
ENDCLASS.
