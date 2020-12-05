*&---------------------------------------------------------------------*
*& Include          ZOBJECT_BOOKMARK_NPLTOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Global [Constant]
*&---------------------------------------------------------------------*
CONSTANTS :
  gc_0 TYPE c VALUE '0',
  gc_1 TYPE c VALUE '1',
  gc_2 TYPE c VALUE '2',
  gc_3 TYPE c VALUE '3',
  gc_4 TYPE c VALUE '4',
  gc_5 TYPE c VALUE '5',
  gc_6 TYPE c VALUE '6',
  gc_7 TYPE c VALUE '7',
  gc_8 TYPE c VALUE '8',
  gc_9 TYPE c VALUE '9',

  gc_a TYPE c VALUE 'A',
  gc_b TYPE c VALUE 'B',
  gc_c TYPE c VALUE 'C',
  gc_d TYPE c VALUE 'D',
  gc_e TYPE c VALUE 'E',
  gc_f TYPE c VALUE 'F',
  gc_g TYPE c VALUE 'G',
  gc_h TYPE c VALUE 'H',
  gc_i TYPE c VALUE 'I',
  gc_j TYPE c VALUE 'J',
  gc_k TYPE c VALUE 'K',
  gc_l TYPE c VALUE 'L',
  gc_m TYPE c VALUE 'M',
  gc_n TYPE c VALUE 'N',
  gc_o TYPE c VALUE 'O',
  gc_p TYPE c VALUE 'P',
  gc_q TYPE c VALUE 'Q',
  gc_r TYPE c VALUE 'R',
  gc_s TYPE c VALUE 'S',
  gc_t TYPE c VALUE 'T',
  gc_u TYPE c VALUE 'U',
  gc_v TYPE c VALUE 'V',
  gc_w TYPE c VALUE 'W',
  gc_x TYPE c VALUE 'X',
  gc_y TYPE c VALUE 'Y',
  gc_z TYPE c VALUE 'Z'.

DATA : gv_prg_mode TYPE c.
CONSTANTS :
  "Program Mode [Admin]
  gc_prg_mode_admin   TYPE c VALUE 'A',
  gc_prg_mode_user    TYPE c VALUE 'U',
  gc_prg_mode_display TYPE c VALUE 'D'.

CONSTANTS :
  "ALV Row Mode [Modified, Append]
  gc_alv_mode_modify TYPE c VALUE 'M',
  gc_alv_mode_append TYPE c VALUE 'A',
  gc_alv_mode_delete TYPE c VALUE 'D'.


CLASS : lcl_controller DEFINITION DEFERRED,  "MVC ( Controller )
        lcl_model      DEFINITION DEFERRED,  "MVC ( Model )

        lcl_event      DEFINITION DEFERRED,  "Event Receiver
        lcl_module     DEFINITION DEFERRED.  "Common Module
DATA : go_control TYPE REF TO lcl_controller,
       go_model   TYPE REF TO lcl_model,
       go_module  TYPE REF TO lcl_module,

       go_event   TYPE REF TO lcl_event.

*&---------------------------------------------------------------------*
* Global [TYPES]
*&---------------------------------------------------------------------*
types:
  BEGIN OF GTY_S_INCL_COMMON ,
        VCOLOR     type LVC_T_SCOL,
        VCELLTAB   TYPE lvc_t_styl,
        VMODE      TYPE char1,
       END OF gty_s_incl_common .

TYPES: BEGIN OF gty_s_TREE.
    include type zobjectbook.
    include type gty_s_incl_common.
TYPES : spras_nm TYPE fieldname,
        node_key type lvc_nkey,
        END OF gty_s_tree,
        gty_t_tree TYPE STANDARD TABLE OF gty_s_tree.

TYPES : gty_t_acc_table TYPE STANDARD TABLE OF zobjectbook.


DATA : gt_acc_table TYPE gty_t_acc_table.


*&---------------------------------------------------------------------*
* Global [Variables]
*&---------------------------------------------------------------------*
DATA : gv_ok_code1 TYPE sy-ucomm,
       gv_title    TYPE c LENGTH 20,
       gv_first    type c,
       gv_changed1 TYPE c.

*DATA : go_cont_tree1 TYPE REF TO cl_gui_container,
*       go_cont_grid1 TYPE REF TO cl_gui_container.

DATA : gt_list1 TYPE gty_t_tree,
       gt_tree1 type gty_t_tree.

DATA : gt_list1_delete TYPE gty_t_acc_table.
DATA : gt_style_append TYPE lvc_t_styl.

DATA : go_tree1 TYPE REF TO cl_gui_alv_tree,
       go_grid1 TYPE REF TO cl_gui_alv_grid,
       gt_fcat1 TYPE lvc_t_fcat,
       gt_exld1 TYPE ui_functions,
       gs_layo1 TYPE lvc_s_layo,
       gs_vari1 TYPE disvariant,
       gs_prnt1 TYPE lvc_s_prnt,
       gt_sort1 TYPE lvc_t_sort.
