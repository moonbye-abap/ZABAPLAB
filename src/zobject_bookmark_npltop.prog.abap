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


CLASS : lcl_scr2000       DEFINITION DEFERRED,  "Screen 2000 controller
        lcl_model         DEFINITION DEFERRED,  "MVC ( Model )
        lcl_tree_assist   DEFINITION DEFERRED,  "MVC ( Model )
        lcl_scr0100    DEFINITION DEFERRED,  "BUS Screen framework( 0100 )
        lcl_scr0200    DEFINITION DEFERRED,  "BUS Screen framework( 0100 )
        lcl_event      DEFINITION DEFERRED,  "Event Receiver
        lcl_module     DEFINITION DEFERRED.  "Common Module
DATA : go_tree_assist1 TYPE REF TO lcl_tree_assist,
       go_scr0100      TYPE REF TO lcl_scr0100,
       go_scr0200      TYPE REF TO lcl_scr0200,

       go_event        TYPE REF TO lcl_event.

*&---------------------------------------------------------------------*
* Global [TYPES]
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_s_incl_common ,
    vcolor   TYPE lvc_t_scol,
    vcelltab TYPE lvc_t_styl,
    vmode    TYPE char1,
  END OF gty_s_incl_common .

TYPES: BEGIN OF gty_s_tree.
         INCLUDE TYPE zobjectbook.
         INCLUDE TYPE gty_s_incl_common.
         TYPES : spras_nm TYPE fieldname,
         node_key TYPE lvc_nkey,
       END OF gty_s_tree,
       gty_t_tree TYPE STANDARD TABLE OF gty_s_tree
        WITH UNIQUE HASHED KEY id   COMPONENTS guid
        WITH NON-UNIQUE SORTED KEY node COMPONENTS node_key
       .

TYPES : gty_t_acc_table TYPE STANDARD TABLE OF zobjectbook.

TYPES : BEGIN OF gty_s_gubn,
          code TYPE c LENGTH 1,
          text TYPE text100,
        END OF gty_s_gubn.

DATA : gt_acc_table TYPE gty_t_acc_table.
DATA : go_dock  TYPE REF TO cl_gui_docking_container,
       go_left  TYPE REF TO cl_gui_container,
       go_right TYPE REF TO cl_gui_container,
       go_easy  TYPE REF TO cl_gui_easy_splitter_container.

*&---------------------------------------------------------------------*
* Global [Variables]
*&---------------------------------------------------------------------*
DATA : gv_ok_code1 TYPE sy-ucomm,
       gv_title    TYPE c LENGTH 20,
       gv_first    TYPE c,
       gv_changed1 TYPE c.

*DATA : go_cont_tree1 TYPE REF TO cl_gui_container,
*       go_cont_grid1 TYPE REF TO cl_gui_container.

FIELD-SYMBOLS : <gt_list2> TYPE STANDARD TABLE.
DATA : gt_tree1 TYPE gty_t_tree.

DATA : gt_list1_delete TYPE gty_t_acc_table.
DATA : gt_style_append TYPE lvc_t_styl.

DATA : go_tree1   TYPE REF TO cl_gui_alv_tree,

       "Drag & Drop용으로 선언한다.
       go_drag    TYPE REF TO cl_dragdrop,
       go_drop    TYPE REF TO cl_dragdrop,

       go_toolbar TYPE REF TO cl_gui_toolbar,
       gt_fcat1   TYPE lvc_t_fcat,
       gt_exld1   TYPE ui_functions,
       gs_layo1   TYPE lvc_s_layo,
       gs_vari1   TYPE disvariant,
       gs_prnt1   TYPE lvc_s_prnt,
       gt_sort1   TYPE lvc_t_sort.


DATA : go_grid2 TYPE REF TO cl_gui_alv_grid,
       gt_fcat2 TYPE lvc_t_fcat,
       gt_exld2 TYPE ui_functions,
       gs_layo2 TYPE lvc_s_layo,
       gs_vari2 TYPE disvariant,
       gs_prnt2 TYPE lvc_s_prnt,
       gt_sort2 TYPE lvc_t_sort.


TYPES : BEGIN OF gty_s_tree_add ,
          ismodify    TYPE c,
          node        TYPE lvc_nkey,
          guid        TYPE zobjectbook-guid,
          type        TYPE zobjectbook-type,
          name        TYPE zobjectbook-name,
          description TYPE zobjectbook-description,
          zorder      TYPE zobjectbook-zorder,
        END OF gty_s_tree_add.
DATA : gs_tree_add TYPE gty_s_tree_add .


TYPES : BEGIN OF gty_s_list3.
          INCLUDE TYPE zobjectbook.
          INCLUDE TYPE ycl_commons=>gty_s_incl_common.
 TYPES : usrid_nm type char30,
        END OF gty_s_list3,
        gty_t_list3 TYPE STANDARD TABLE OF gty_s_list3.

DATA : go_dock3  TYPE REF TO cl_gui_docking_container.
DATA : go_grid3 TYPE REF TO cl_gui_alv_grid,
       gt_fcat3 TYPE lvc_t_fcat,
       gt_exld3 TYPE ui_functions,
       gs_layo3 TYPE lvc_s_layo,
       gs_vari3 TYPE disvariant,
       gs_prnt3 TYPE lvc_s_prnt,
       gt_sort3 TYPE lvc_t_sort.
DATA : gt_list3 TYPE gty_t_list3.
SELECTION-SCREEN BEGIN OF SCREEN 2000.
SELECTION-SCREEN END OF SCREEN 2000.

*&---------------------------------------------------------------------*
* Class Definition
*&---------------------------------------------------------------------*

CLASS lcl_module     DEFINITION.
  PUBLIC SECTION.
    CONSTANTS mc_astr TYPE c VALUE '*' ##NO_TEXT.
    CONSTANTS mc_i TYPE c VALUE 'I' ##NO_TEXT.
    CONSTANTS mc_eq TYPE fieldname VALUE 'EQ' ##NO_TEXT.
    CONSTANTS mc_ne TYPE fieldname VALUE 'NE' ##NO_TEXT.
    CONSTANTS mc_gt TYPE fieldname VALUE 'GT' ##NO_TEXT.
    CONSTANTS mc_ge TYPE fieldname VALUE 'GE' ##NO_TEXT.
    CONSTANTS mc_lt TYPE fieldname VALUE 'LT' ##NO_TEXT.
    CONSTANTS mc_le TYPE fieldname VALUE 'LE' ##NO_TEXT.
    CONSTANTS mc_bt TYPE fieldname VALUE 'BT' ##NO_TEXT.
    CONSTANTS mc_nb TYPE fieldname VALUE 'NB' ##NO_TEXT.
    CONSTANTS mc_cp TYPE fieldname VALUE 'CP' ##NO_TEXT.
    CONSTANTS mc_hyphen TYPE c LENGTH 1 VALUE '-' ##NO_TEXT.
    CONSTANTS mc_s TYPE c VALUE 'S' ##NO_TEXT.
    CONSTANTS mc_left_brace TYPE c VALUE '(' ##NO_TEXT.
    CONSTANTS mc_right_brace TYPE c VALUE ')' ##NO_TEXT.
    CONSTANTS mc_tilde TYPE c VALUE '~' ##NO_TEXT.
    CONSTANTS mc_text TYPE char04 VALUE 'TEXT' ##NO_TEXT.
    CONSTANTS mc_char TYPE char04 VALUE 'CHAR' ##NO_TEXT.
    CONSTANTS mc_c TYPE c VALUE 'C' ##NO_TEXT.
    CONSTANTS mc_icon TYPE char04 VALUE 'ICON' ##NO_TEXT.
    CONSTANTS mc_value TYPE char05 VALUE 'VALUE' ##NO_TEXT.
    CONSTANTS mc_kind TYPE char04 VALUE 'KIND' ##NO_TEXT.
    CONSTANTS mc_esc TYPE c VALUE '"' ##NO_TEXT.
    CONSTANTS mc_enter TYPE char3 VALUE '/00' ##NO_TEXT.
    CONSTANTS mc_x TYPE c VALUE 'X' ##NO_TEXT.
    CONSTANTS mc_a TYPE c VALUE 'A' ##NO_TEXT.
    CONSTANTS mc_h TYPE c VALUE 'H' ##NO_TEXT.
    CONSTANTS mc_h_lower TYPE c VALUE 'h' ##NO_TEXT.
    CONSTANTS mc_e TYPE c VALUE 'E' ##NO_TEXT.
    CONSTANTS mc_p TYPE c VALUE 'P' ##NO_TEXT.



    TYPES:
      BEGIN OF gty_s_incl_common ,
        vcolor   TYPE lvc_t_scol,
        vcelltab TYPE lvc_t_styl,
        vmode    TYPE char1,
      END OF gty_s_incl_common .


    CLASS-METHODS set_position
      IMPORTING
        !i_row       TYPE sy-tabix
        !i_fieldname TYPE fieldname
        !io_alv      TYPE REF TO cl_gui_alv_grid .

    CLASS-METHODS write_scr_field
      IMPORTING
        !i_fieldname  TYPE any
        !i_fieldvalue TYPE any
        !i_prog       TYPE sy-cprog DEFAULT sy-cprog
        !i_dynnr      TYPE sy-dynnr DEFAULT sy-dynnr
        !it_fields    TYPE fico_typ_tab_retval OPTIONAL .
    CLASS-METHODS refresh_alv
      IMPORTING
        !io_alv   TYPE REF TO cl_gui_alv_grid
        !i_normal TYPE char01 OPTIONAL .
    CLASS-METHODS read_scr_field
      IMPORTING
        !i_fieldname   TYPE any
        !i_prog        TYPE sy-cprog OPTIONAL
      RETURNING
        VALUE(r_value) TYPE char100 .
    CLASS-METHODS disp_f4_alv
      IMPORTING
        !it_data    TYPE STANDARD TABLE
        !i_retfield TYPE dfies-fieldname
        !i_scrfield TYPE lvc_fname
        !i_display  TYPE c DEFAULT ' '
        !is_row_no  TYPE lvc_s_roid
        !io_data    TYPE REF TO cl_alv_event_data .

    CLASS-METHODS disp_f4
      IMPORTING
        !i_retfield TYPE dfies-fieldname
        !i_scrfield TYPE help_info-dynprofld
        !it_data    TYPE STANDARD TABLE
        !i_prog     TYPE sy-cprog OPTIONAL
        !i_display  TYPE c DEFAULT ' '
      EXPORTING
        !et_result  TYPE fico_typ_tab_retval .
    CLASS-METHODS ask_question
      IMPORTING
        !i_question  TYPE c
      RETURNING
        VALUE(r_ans) TYPE char01 .
    CLASS-METHODS commit
      IMPORTING
        !i_wait TYPE bapita-wait DEFAULT 'X' .

    CLASS-METHODS get_excl_buttons
      RETURNING
        VALUE(rt_excl) TYPE ui_functions .
    CLASS-METHODS get_layout
      IMPORTING
        !it_tab          TYPE STANDARD TABLE
      RETURNING
        VALUE(rs_layout) TYPE lvc_s_layo .
    CLASS-METHODS get_variant
      IMPORTING
        !i_name          TYPE c
      RETURNING
        VALUE(r_variant) TYPE repid .
    CLASS-METHODS set_fcat_name
      IMPORTING
        !i_name  TYPE c
      CHANGING
        !cs_fcat TYPE lvc_s_fcat .
    CLASS-METHODS set_f4
      IMPORTING
        !io_alv  TYPE REF TO cl_gui_alv_grid
        !it_fcat TYPE lvc_t_fcat .

    CLASS-METHODS get_fcat
      IMPORTING
        !is_fcat        TYPE lvc_s_fcat OPTIONAL
        !iv_ddic_object TYPE any OPTIONAL
        !is_data        TYPE any OPTIONAL
        !it_data        TYPE ANY TABLE OPTIONAL
        !ior_dref       TYPE data OPTIONAL
      RETURNING
        VALUE(rt_fcat)  TYPE lvc_t_fcat
      EXCEPTIONS
        no_structure
        invalid_type .
    CLASS-METHODS get_uuidx16
      RETURNING
        VALUE(r_rtn) TYPE sysuuid_x16 .

    CLASS-METHODS get_fields
      IMPORTING
        !i_struc         TYPE fieldname
        !i_prefix        TYPE c OPTIONAL
      RETURNING
        VALUE(rt_fields) TYPE rstline .



ENDCLASS.

CLASS lcl_scr2000 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS :

      pbo,
      grid3_event_data_changed
        IMPORTING
          er_data_changed TYPE REF TO	cl_alv_changed_data_protocol
          e_onf4          TYPE  char01
          e_onf4_before   TYPE  char01
          e_onf4_after    TYPE  char01
          e_ucomm         TYPE  sy-ucomm,

      grid3_event_onf4
        IMPORTING
          e_fieldname   TYPE  lvc_fname
          e_fieldvalue  TYPE  lvc_value
          es_row_no     TYPE  lvc_s_roid
          er_event_data TYPE REF TO	cl_alv_event_data
          et_bad_cells  TYPE  lvc_t_modi
          e_display     TYPE  char01,

      grid3_event_toolbar
        IMPORTING
          p_object      TYPE REF TO cl_alv_event_toolbar_set
          p_interactive TYPE char01
        ,
      grid3_event_user_command
        IMPORTING
          i_ucomm TYPE sy-ucomm
        ,
      pai.
  PRIVATE SECTION.
    CLASS-METHODS :
      grid3_fcat_build
        IMPORTING
          it_list TYPE gty_t_list3
        CHANGING
          ct_fcat TYPE lvc_t_fcat,
      grid3_display,
      grid3_define_value,
      grid3_define_event_process,
      grid3_create_container.

ENDCLASS.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS :
      save_tree
        IMPORTING
          it_tree TYPE gty_t_tree,
      update_tree
        IMPORTING
          is_tree TYPE gty_s_tree,
      get_acc_table
        IMPORTING i_usr        TYPE sy-uname
        CHANGING  ct_acc_table TYPE gty_t_acc_table,
      get_nodes
        IMPORTING i_usr    TYPE sy-uname
        CHANGING  ct_nodes TYPE gty_t_tree.

ENDCLASS.



CLASS lcl_event DEFINITION INHERITING FROM cl_gui_object.
  PUBLIC SECTION.
    DATA : mv_gubn TYPE char20.
    METHODS :

      constructor
        IMPORTING
          i_gubn TYPE char20,
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



      handle_double_click
                  FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row
                  e_column
                  es_row_no ,


      handle_node_double_click
                  FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key,

      handle_hotspot_click
                  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id.



  PRIVATE SECTION.

ENDCLASS.

"Scree9000을 처리하기 위한 BUS Class를 선언한다.
CLASS lcl_scr0100 DEFINITION
      INHERITING FROM cl_bus_abstract_main_screen .
  PUBLIC SECTION.
    " The constructor method can be used for initializations.
    METHODS constructor
      IMPORTING
        VALUE(iv_program_name)  TYPE bus_screen-program_name
        VALUE(iv_dynpro_number) TYPE bus_screen-dynpro_number.

    " This method is the central handling for all PAI events in this screen.
    METHODS handle_pai FOR EVENT process_after_input OF cl_bus_abstract_main_screen
      IMPORTING iv_function_code.

    METHODS pbo_begin REDEFINITION.

    CLASS-METHODS :
      grid2_node_double_click
        IMPORTING
          node_key TYPE lvc_nkey

        ,

      grid2_event_toolbar
        IMPORTING
          p_object      TYPE REF TO cl_alv_event_toolbar_set
          p_interactive TYPE char01
        ,

      tree1_fcat_build
        IMPORTING
          it_list1 TYPE gty_t_tree
        CHANGING
          ct_fcat1 TYPE lvc_t_fcat.

  PROTECTED SECTION.
    "DynPro 명령 ( Call Screen xxxx)
    METHODS call_screen REDEFINITION.

    "DynPro 명령 ( Call Screen xxxx Starting At xx yy Ending At xx yy )
    METHODS call_screen_starting_at REDEFINITION.

  PRIVATE SECTION.
    METHODS pbo_init_create_container.
    METHODS pbo_init_event_process.
    METHODS pbo_init_tree_value.
    METHODS pbo_init_tree_dragdrop.
    METHODS pbo_init_tree_display.
ENDCLASS.

"Scree0200을 처리하기 위한 BUS Class를 선언한다.
CLASS lcl_scr0200 DEFINITION
      INHERITING FROM cl_bus_abstract_main_screen .
  PUBLIC SECTION.
    " The constructor method can be used for initializations.
    METHODS constructor
      IMPORTING
        VALUE(iv_program_name)  TYPE bus_screen-program_name
        VALUE(iv_dynpro_number) TYPE bus_screen-dynpro_number.

    " This method is the central handling for all PAI events in this screen.
    METHODS handle_pai FOR EVENT process_after_input OF cl_bus_abstract_main_screen
      IMPORTING iv_function_code.

    METHODS pbo_begin REDEFINITION.

    METHODS pov_name.

  PROTECTED SECTION.
    "DynPro 명령 ( Call Screen xxxx)
    METHODS call_screen REDEFINITION.

    "DynPro 명령 ( Call Screen xxxx Starting At xx yy Ending At xx yy )
    METHODS call_screen_starting_at REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.
