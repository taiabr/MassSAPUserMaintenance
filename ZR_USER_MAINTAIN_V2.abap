*&---------------------------------------------------------------------*
*& Report ZR_USER_MAINTAIN_V2
*&---------------------------------------------------------------------*
*&   Author: Taiã Pryor
*&---------------------------------------------------------------------*
REPORT zr_user_maintain_v2.

**********************************************************************
*Selection screen texts:
*P_CREATE	Criação
*P_FILE	Arquivo (.csv)
*P_HEAD	Cabeçalho
*P_LOCK	Bloquear
*P_ROLE	Inserir Perfis
*P_UNLOCK	Desbloquear
*P_UPDATE	Atualização
*S_LOGTYP  Tipos a serem exibidos no log
*S_NAME	Usuário
*S_NOUPD  Usuários desconsiderados
**********************************************************************

**********************************************************************
*** Log
DATA:
  gv_logmsg TYPE string,
  gs_logret TYPE bapiret2.

DEFINE log_msg.  "&1 - tipo / &2 &3 - mensagem
  CLEAR gv_logmsg.
  CONCATENATE space &1 space '|' space &2 space &3
    INTO gv_logmsg RESPECTING BLANKS.
  CASE sy-batch.
    WHEN abap_true.
      MESSAGE gv_logmsg TYPE 'S'.
    WHEN abap_false.
      WRITE:/ gv_logmsg.
  ENDCASE.
END-OF-DEFINITION.

DEFINE log_bapiret2. "&1 - tabela tipo BAPIRET2
  CLEAR gs_logret.
  LOOP AT &1 INTO gs_logret WHERE type IN s_logtyp.
    IF gs_logret-message IS INITIAL.
      MESSAGE ID gs_logret-id TYPE 'S' NUMBER gs_logret-number
        WITH gs_logret-message_v1 gs_logret-message_v2
             gs_logret-message_v3 gs_logret-message_v4
        INTO gs_logret-message.
    ENDIF.
    log_msg gs_logret-type gs_logret-message ''.
  ENDLOOP.
END-OF-DEFINITION.

**********************************************************************
*** Estruturas dos arquivos
TYPES:
  BEGIN OF ty_csv_user,
    id_usuario   TYPE string,
    sobrenome	   TYPE string,
    nome         TYPE string,
    linguagem	   TYPE string,
    funcao       TYPE string,
    departamento TYPE string,
    telefone     TYPE string,
    extensao     TYPE string,
    email	       TYPE string,
    responsavel	 TYPE string,
    descricao	   TYPE string,
    tipo_usuario TYPE string,
    senha	       TYPE string,
  END OF ty_csv_user,
  BEGIN OF ty_csv_role,
    id_usuario TYPE string,
    rolename   TYPE string,
    validfrom  TYPE string,
    validto    TYPE string,
  END OF ty_csv_role.

**********************************************************************
*** Variaveis globais
DATA:
  gt_usertab TYPE STANDARD TABLE OF bapibname,
  gt_csvuser TYPE STANDARD TABLE OF ty_csv_user,
  gt_csvrole TYPE STANDARD TABLE OF ty_csv_role.

FIELD-SYMBOLS:
  <fs_user>   LIKE LINE OF gt_usertab[].

**********************************************************************
*** Tela de selecao
TABLES: sscrfields, usr01, bapiret2.
SELECTION-SCREEN FUNCTION KEY 1.  "botao para download do template

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
"Bloqueio
PARAMETERS:
  p_lock   RADIOBUTTON GROUP g1,                  "Bloqueio
  p_unlock RADIOBUTTON GROUP g1 DEFAULT 'X'.      "Desbloqueio
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
SELECT-OPTIONS:
  s_name FOR usr01-bname NO INTERVALS.            "Usuario
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:
  s_noupd FOR usr01-bname.                        "Usuario q nao podem ser processados
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN SKIP.
"Atualizacao
PARAMETERS:
  p_create RADIOBUTTON GROUP g1,                  "Criacao
  p_update RADIOBUTTON GROUP g1,                  "Atualizacao
  p_role   RADIOBUTTON GROUP g1.                  "Perfil
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME.
PARAMETERS:
  p_file TYPE string LOWER CASE,                  "Arquivo CSV
  p_head AS CHECKBOX DEFAULT 'X'.                 "Cabecalho
SELECTION-SCREEN END OF BLOCK b4.
"Log
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME.
SELECT-OPTIONS:
  s_logtyp FOR bapiret2-type NO INTERVALS.           "Tipos de msg a serem exibidos no log
SELECTION-SCREEN END OF BLOCK b5.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'FC01'.
    "Download template
    PERFORM f_template.

  ELSE.
    "Validacoes de tela
    CASE abap_true.
      WHEN p_lock OR p_unlock.
        IF s_name IS INITIAL AND s_name[] IS INITIAL.
          MESSAGE 'Usuário é obrigatório' TYPE 'E'.
        ENDIF.

      WHEN p_create OR p_update OR p_role.
        IF p_file IS INITIAL.
          MESSAGE 'Arquivo é obrigatório' TYPE 'E'.
        ENDIF.
    ENDCASE.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA:
    lt_file TYPE filetable,
    ls_file LIKE LINE OF lt_file[],
    lv_rc   TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      default_extension       = '*.CSV*'
      multiselection          = abap_false
    CHANGING
      file_table              = lt_file[]
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_file[] INTO ls_file INDEX 1.
  IF sy-subrc IS INITIAL.
    p_file = ls_file-filename.
  ENDIF.

**********************************************************************
*** Inicializacao
LOAD-OF-PROGRAM.
  "Botao para download do template
  DATA ls_dyntxt TYPE smp_dyntxt.
  ls_dyntxt-text = ls_dyntxt-icon_text = 'Template'.
  ls_dyntxt-icon_id = '@0U@'.
  sscrfields-functxt_01 = ls_dyntxt.

  "Seta tipos de msg default
  s_logtyp-sign   = 'I'.
  s_logtyp-option = 'EQ'.
  s_logtyp-low    = 'S'.
  APPEND s_logtyp.
  s_logtyp-low    = 'E'.
  APPEND s_logtyp.
  s_logtyp-low    = 'A'.
  APPEND s_logtyp.
  s_logtyp-low    = 'X'.
  APPEND s_logtyp.

  "Usuario que nao podem ser alterados
  s_noupd-sign   = 'I'.
  s_noupd-option = 'EQ'.
  s_noupd-low    = sy-uname.
  APPEND s_noupd.
  s_noupd-low    = 'BASIS'.
  APPEND s_noupd.
  s_noupd-low    = 'SAP*'.
  APPEND s_noupd.
  s_noupd-low    = 'DDIC'.
  APPEND s_noupd.
  s_noupd-low    = '_SAP_FIORI'.
  APPEND s_noupd.
  s_noupd-low    = '_SAP_TEC_USR'.
  APPEND s_noupd.
  s_noupd-low    = 'SDAGENT'.
  APPEND s_noupd.
  s_noupd-low    = 'BATCH_USER'.
  APPEND s_noupd.
  s_noupd-low    = 'RFC_USER'.
  APPEND s_noupd.

START-OF-SELECTION.
**********************************************************************
*** Leitura dos dados
  CLEAR gt_usertab[].

  CASE abap_true.
    WHEN p_lock OR p_unlock.
      "Le usuarios
      SELECT bname AS bapibname
        FROM usr02
        INTO CORRESPONDING FIELDS OF TABLE gt_usertab[]
        WHERE bname IN s_name[].

    WHEN p_create OR p_update.
      "Le arquivo
      PERFORM f_import_csv USING 'USER' p_file p_head.

    WHEN p_role.
      "Le arquivo
      PERFORM f_import_csv USING 'ROLE' p_file p_head.

  ENDCASE.
  DELETE gt_usertab[] WHERE bapibname IS INITIAL.

END-OF-SELECTION.
  IF gt_usertab[] IS INITIAL.
    MESSAGE 'Nenhum usuário para processamento' TYPE 'E'.
  ENDIF.

**********************************************************************
*** Processamento
  "Confirma processamento
  DATA lv_answer TYPE c.
  PERFORM f_confirm CHANGING lv_answer.

  IF lv_answer = '1'. "Sim

    LOOP AT gt_usertab[] ASSIGNING <fs_user>.
      IF <fs_user>-bapibname IN s_noupd[].
        log_msg 'I' '> Usuário desconsiderado:' <fs_user>-bapibname.
        CONTINUE.
      ENDIF.

      log_msg 'I' '> Processando usuário' <fs_user>-bapibname.
      CASE abap_true.
        WHEN p_lock.   PERFORM f_lock     USING <fs_user>-bapibname.
        WHEN p_unlock. PERFORM f_unlock   USING <fs_user>-bapibname.
        WHEN p_create. PERFORM f_create   USING <fs_user>-bapibname.
        WHEN p_update. PERFORM f_update   USING <fs_user>-bapibname.
        WHEN p_role.   PERFORM f_addroles USING <fs_user>-bapibname.
      ENDCASE.
    ENDLOOP.

  ENDIF.

*&---------------------------------------------------------------------*
*& FORM f_import_csv
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_import_csv USING p_type TYPE string
                        p_file TYPE string
                        p_head TYPE abap_bool.

  DATA:
    lt_csvdata TYPE STANDARD TABLE OF string.

  CLEAR gt_usertab[].

  "Importa arquivo
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = p_file
      filetype                = 'ASC'
    CHANGING
      data_tab                = lt_csvdata[]
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  "Remove cabecalho
  IF p_head = abap_true.
    DELETE lt_csvdata[] INDEX 1.
  ENDIF.

  "Remove linhas vazias
  DELETE lt_csvdata[] WHERE table_line IS INITIAL.

  "Move dados para tabela interna
  CASE p_type.
    WHEN 'USER'. PERFORM f_parse_csv_user USING lt_csvdata[].
    WHEN 'ROLE'. PERFORM f_parse_csv_role USING lt_csvdata[].
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_parse_csv_user
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_parse_csv_user USING p_csvdata TYPE stringtab.

  DATA:
    lt_csvflds TYPE STANDARD TABLE OF string,
    ls_datalin LIKE LINE OF gt_csvuser[],
    ls_usernam LIKE LINE OF gt_usertab[].

  FIELD-SYMBOLS:
    <fs_csvline> LIKE LINE OF p_csvdata[],
    <fs_csvval>  LIKE LINE OF lt_csvflds[],
    <fs_sapval>  TYPE any.

  CLEAR gt_csvuser[].

  LOOP AT p_csvdata[] ASSIGNING <fs_csvline>.
    CLEAR: lt_csvflds[], ls_datalin, ls_usernam.

    "Recupera valores da linha do csv
    SPLIT <fs_csvline> AT ';' INTO TABLE lt_csvflds[].
    CHECK sy-subrc IS INITIAL.

    "Preenche linha interna com valores do csv
    LOOP AT lt_csvflds[] ASSIGNING <fs_csvval>.
      ASSIGN COMPONENT sy-tabix OF STRUCTURE ls_datalin TO <fs_sapval>.
      IF sy-subrc IS INITIAL.
        <fs_sapval> = <fs_csvval>.
      ENDIF.
    ENDLOOP.

    IF ls_datalin-id_usuario IS NOT INITIAL.
      "Salva dados do usuario
      APPEND ls_datalin TO gt_csvuser[].

      "Salva usuario para processamento
      ls_usernam-bapibname = ls_datalin-id_usuario.
      APPEND ls_usernam TO gt_usertab[].
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_parse_csv_role
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_parse_csv_role USING p_csvdata TYPE stringtab.

  DATA:
    lt_csvflds TYPE STANDARD TABLE OF string,
    ls_datalin LIKE LINE OF gt_csvrole[],
    ls_usernam LIKE LINE OF gt_usertab[].

  FIELD-SYMBOLS:
    <fs_csvline> LIKE LINE OF p_csvdata[],
    <fs_csvval>  LIKE LINE OF lt_csvflds[],
    <fs_sapval>  TYPE any.

  CLEAR gt_csvrole[].

  LOOP AT p_csvdata[] ASSIGNING <fs_csvline>.
    CLEAR: lt_csvflds[], ls_datalin, ls_usernam.

    "Recupera valores da linha do csv
    SPLIT <fs_csvline> AT ';' INTO TABLE lt_csvflds[].
    CHECK sy-subrc IS INITIAL.

    "Preenche linha interna com valores do csv
    LOOP AT lt_csvflds[] ASSIGNING <fs_csvval>.
      ASSIGN COMPONENT sy-tabix OF STRUCTURE ls_datalin TO <fs_sapval>.
      IF sy-subrc IS INITIAL.
        <fs_sapval> = <fs_csvval>.
      ENDIF.
    ENDLOOP.

    IF ls_datalin-id_usuario IS NOT INITIAL.
      "Salva dados do perfil
      APPEND ls_datalin TO gt_csvrole[].

      "Salva usuario para processamento
      ls_usernam-bapibname = ls_datalin-id_usuario.
      APPEND ls_usernam TO gt_usertab[].
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CONFIRM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_confirm  CHANGING p_answer.

  CLEAR p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'ATENÇÃO!!!'
      text_question         = 'Deseja realmente processar?'
      default_button        = '2'
      display_cancel_button = ' '
    IMPORTING
      answer                = p_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_USER_TYPE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_check_user_type  USING    p_usrname
                        CHANGING p_valid.

  CLEAR p_valid.

  "Valida se eh usuario de dialogo
  SELECT COUNT(*)
    FROM usr02
    WHERE bname = p_usrname
      AND ustyp = 'A'. "Dialog

  CASE sy-subrc.
    WHEN 0.      p_valid = abap_true.
    WHEN OTHERS. p_valid = abap_false.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_lock
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_lock USING p_name TYPE xubname.

  DATA:
    lt_return TYPE STANDARD TABLE OF bapiret2,
    lv_valid  TYPE abap_bool.

  "Valida tipo do usuario
  PERFORM f_check_user_type USING p_name CHANGING lv_valid.
  IF lv_valid = abap_false.
    log_msg 'E' '> Tipo de usuario inválido: ' p_name.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_USER_LOCK'
    EXPORTING
      username = p_name
    TABLES
      return   = lt_return[].

  "Insere retorno no log
  log_bapiret2 lt_return[].

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_unlock
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_unlock USING p_name TYPE xubname.

  DATA:
    lt_return TYPE STANDARD TABLE OF bapiret2,
    lv_valid  TYPE abap_bool.

  "Valida tipo do usuario.
  PERFORM f_check_user_type USING p_name CHANGING lv_valid.
  IF lv_valid = abap_false.
    log_msg 'E' '> Tipo de usuario inválido: ' p_name.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_USER_UNLOCK'
    EXPORTING
      username = p_name
    TABLES
      return   = lt_return[].

  "Insere retorno no log
  log_bapiret2 lt_return[].

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_create
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_create USING p_name TYPE xubname.

  DATA:
    lt_return    TYPE STANDARD TABLE OF bapiret2,
    ls_logondata TYPE bapilogond,
    ls_password  TYPE bapipwd,
    ls_address   TYPE bapiaddr3,
    ls_descript  TYPE bapiusdesc,
    lv_username  TYPE bapibname-bapibname.

  "Recupera dados do usuario
  READ TABLE gt_csvuser[] ASSIGNING FIELD-SYMBOL(<fs_data>)
    WITH KEY id_usuario = p_name.
  CHECK sy-subrc IS INITIAL.

  "Preenche dados do usuario
  lv_username = <fs_data>-id_usuario.

  "Tipo do usuario
  ls_logondata-ustyp = <fs_data>-tipo_usuario.

  "Senha
  ls_password-bapipwd = <fs_data>-senha.

  "Endereco
  ls_address-firstname  = <fs_data>-nome.
  ls_address-lastname   = <fs_data>-sobrenome.
  ls_address-langu      = <fs_data>-linguagem.
  ls_address-function   = <fs_data>-funcao.
  ls_address-department = <fs_data>-departamento.
  ls_address-tel1_numbr = <fs_data>-telefone.
  ls_address-tel1_ext   = <fs_data>-extensao.
  ls_address-e_mail     = <fs_data>-email.

  "Descricao
  ls_descript-techdesc    = <fs_data>-descricao.
  ls_descript-responsible = <fs_data>-responsavel.

  "Cria usuario
  CALL FUNCTION 'BAPI_USER_CREATE1'
    EXPORTING
      username    = lv_username
      logondata   = ls_logondata
      password    = ls_password
      address     = ls_address
      description = ls_descript
    TABLES
      return      = lt_return[].

  "Insere retorno no log
  log_bapiret2 lt_return[].

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_update
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_update USING p_name TYPE xubname.

  DATA:
    lt_return     TYPE STANDARD TABLE OF bapiret2,
    ls_logondata  TYPE bapilogond,
    ls_logondatax TYPE bapilogonx,
    ls_address    TYPE bapiaddr3,
    ls_addressx   TYPE bapiaddr3x,
    ls_descript   TYPE bapiusdesc,
    ls_descriptx  TYPE bapiusdescx,
    ls_password   TYPE bapipwd,
    ls_passwordx  TYPE bapipwdx.

  "Recupera dados do usuario
  READ TABLE gt_csvuser[] ASSIGNING FIELD-SYMBOL(<fs_data>)
    WITH KEY id_usuario = p_name.
  CHECK sy-subrc IS INITIAL.

  "Tipo usuario
  IF <fs_data>-tipo_usuario IS NOT INITIAL.
    ls_logondata-ustyp  = <fs_data>-tipo_usuario.
    ls_logondatax-ustyp = abap_true.
  ENDIF.

  "Senha
  IF <fs_data>-senha IS NOT INITIAL.
    ls_password-bapipwd  = <fs_data>-senha.
    ls_passwordx-bapipwd = abap_true.
  ENDIF.

  "Responsavel
  IF <fs_data>-responsavel IS NOT INITIAL.
    ls_descript-responsible  = <fs_data>-responsavel.
    ls_descriptx-responsible = abap_true.
  ENDIF.

  "Descricao
  IF <fs_data>-descricao IS NOT INITIAL.
    ls_descript-techdesc  = <fs_data>-descricao.
    ls_descriptx-techdesc = abap_true.
  ENDIF.

  "Nome
  IF <fs_data>-nome IS NOT INITIAL.
    ls_address-firstname  = <fs_data>-nome.
    ls_addressx-firstname = abap_true.
  ENDIF.

  "Sobrenome
  IF <fs_data>-sobrenome IS NOT INITIAL.
    ls_address-lastname  = <fs_data>-sobrenome.
    ls_addressx-lastname = abap_true.
  ENDIF.

  "Idioma
  IF <fs_data>-linguagem IS NOT INITIAL.
    ls_address-langu  = <fs_data>-linguagem.
    ls_addressx-langu = abap_true.
  ENDIF.

  "Funcao
  IF <fs_data>-funcao IS NOT INITIAL.
    ls_address-function  = <fs_data>-funcao.
    ls_addressx-function = abap_true.
  ENDIF.

  "Departamento
  IF <fs_data>-departamento IS NOT INITIAL.
    ls_address-department  = <fs_data>-departamento.
    ls_addressx-department = abap_true.
  ENDIF.

  "Telefone
  IF <fs_data>-telefone IS NOT INITIAL.
    ls_address-tel1_numbr  = <fs_data>-telefone.
    ls_addressx-tel1_numbr = abap_true.
  ENDIF.

  "Extensao do telefone
  IF <fs_data>-extensao IS NOT INITIAL.
    ls_address-tel1_ext  = <fs_data>-extensao.
    ls_addressx-tel1_ext = abap_true.
  ENDIF.

  "Email
  IF <fs_data>-email IS NOT INITIAL.
    ls_address-e_mail  = <fs_data>-email.
    ls_addressx-e_mail = abap_true.
  ENDIF.

  "Atualiza dados do usuario
  CALL FUNCTION 'BAPI_USER_CHANGE'
    EXPORTING
      username     = p_name
      logondata    = ls_logondata
      logondatax   = ls_logondatax
      address      = ls_address
      addressx     = ls_addressx
      password     = ls_password
      passwordx    = ls_passwordx
      description  = ls_descript
      descriptionx = ls_descriptx
    TABLES
      return       = lt_return[].

  "Insere retorno no log
  log_bapiret2 lt_return[].

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_addroles
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_addroles USING p_name TYPE xubname.

  DATA:
    lt_return   TYPE STANDARD TABLE OF bapiret2,
    lt_rolesold TYPE STANDARD TABLE OF bapiagr,
    lt_rolesnew TYPE STANDARD TABLE OF bapiagr,
    lv_username TYPE bapibname-bapibname.

  FIELD-SYMBOLS:
    <fs_return> LIKE LINE OF lt_return[].

  "Valida se existem dados a serem inseridos
  READ TABLE gt_csvrole[] TRANSPORTING NO FIELDS
    WITH KEY id_usuario = p_name.
  CHECK sy-subrc IS INITIAL.

  "Preenche usuario
  lv_username = p_name.

  "Recupera roles existentes
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username       = lv_username
    TABLES
      activitygroups = lt_rolesold[]
      return         = lt_return[].

  LOOP AT lt_return[] ASSIGNING FIELD-SYMBOL(<fs_error>) WHERE type CA 'EAX'.
  ENDLOOP.
  IF <fs_error> IS ASSIGNED.
    "Insere retorno no log
    log_bapiret2 lt_return[].
    RETURN.
  ENDIF.

  "Insere roles existentes
  APPEND LINES OF lt_rolesold[] TO lt_rolesnew[].

  "Recupera perfis do usuario
  LOOP AT gt_csvrole[] ASSIGNING FIELD-SYMBOL(<fs_csvrole>) WHERE id_usuario = lv_username.
    READ TABLE lt_rolesnew[] ASSIGNING FIELD-SYMBOL(<fs_newrole>)
      WITH KEY agr_name = <fs_csvrole>-rolename.
    IF sy-subrc IS INITIAL.
      "Atualiza validade
      <fs_newrole>-from_dat = <fs_csvrole>-validfrom.
      <fs_newrole>-to_dat   = <fs_csvrole>-validto.
    ELSE.
      "Adiciona role
      APPEND INITIAL LINE TO lt_rolesnew[] ASSIGNING <fs_newrole>.
      <fs_newrole>-agr_name = <fs_csvrole>-rolename.
      <fs_newrole>-from_dat = <fs_csvrole>-validfrom.
      <fs_newrole>-to_dat   = <fs_csvrole>-validto.
    ENDIF.
  ENDLOOP.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CLEAR lt_return[].

  "Insere novas roles
  CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
    EXPORTING
      username       = lv_username
    TABLES
      activitygroups = lt_rolesnew[]
      return         = lt_return[].

  "Insere retorno no log
  log_bapiret2 lt_return[].

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_template
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_template.

  DATA:
    lt_filetabl TYPE STANDARD TABLE OF string,
    lv_fileline TYPE string,
    lv_filename TYPE string,
    lv_namedflt TYPE string,
    lv_filepath TYPE string,
    lv_fullpath TYPE string,
    lv_usaction TYPE i,
    lv_answer   TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = 'Selecione template desejado'
      text_button_1  = 'Usuário'
      text_button_2  = 'Perfil'
    IMPORTING
      answer         = lv_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.
  CHECK lv_answer <> 'A'.

  CASE lv_answer.
    WHEN '1'. lv_namedflt = 'Template Usuarios.csv'.
    WHEN '2'. lv_namedflt = 'Template Perfil.csv'.
  ENDCASE.

  "Seleciona diretorio
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      default_extension         = '*.CSV*'
      default_file_name         = lv_namedflt
    CHANGING
      filename                  = lv_filename
      path                      = lv_filepath
      fullpath                  = lv_fullpath
      user_action               = lv_usaction
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF lv_usaction <> 0.
    MESSAGE 'Cancelado pelo usuario' TYPE 'S' DISPLAY LIKE 'W'.
    EXIT.
  ENDIF.

  "Insere cabecalho
  CASE lv_answer.
    WHEN '1'. "Usuario
      CONCATENATE 'ID USUARIO'
              ';' 'SOBRENOME'
              ';' 'NOME'
              ';' 'LINGUAGEM'
              ';' 'FUNCAO'
              ';' 'DEPARTAMENTO'
              ';' 'TELEFONE'
              ';' 'EXTENSAO'
              ';' 'EMAIL'
              ';' 'RESPONSAVEL'
              ';' 'DESCRICAO'
              ';' 'TIPO USUARIO'
              ';' 'SENHA'
             INTO lv_fileline.

    WHEN '2'. "Perfil
      CONCATENATE 'ID USUARIO'
              ';' 'ROLENAME'
              ';' 'VALID FROM (formato: YYYYMMDD)'
              ';' 'VALID TO (formato: YYYYMMDD)'
             INTO lv_fileline.

  ENDCASE.

  APPEND lv_fileline TO lt_filetabl[].

  "Insere linha vazia
  APPEND INITIAL LINE TO lt_filetabl[].

  "Realiza download do arquivo
  cl_gui_frontend_services=>gui_download(
    EXPORTING
      filename                  = lv_filename
    CHANGING
      data_tab                  = lt_filetabl[]
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Abre arquivo
  cl_gui_frontend_services=>execute(
    EXPORTING
      document               = lv_filename
      synchronous            = ' '
    EXCEPTIONS
      cntl_error             = 1
      error_no_gui           = 2
      bad_parameter          = 3
      file_not_found         = 4
      path_not_found         = 5
      file_extension_unknown = 6
      error_execute_failed   = 7
      synchronous_failed     = 8
      not_supported_by_gui   = 9
      OTHERS                 = 10
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.