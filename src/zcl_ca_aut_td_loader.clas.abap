"! <p class="shorttext synchronized" lang="en">CA-TBX: Loader for test doubles</p>
CLASS zcl_ca_aut_td_loader DEFINITION PUBLIC
                                      FINAL
                                      CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Load test double from MIME database (TA SMW0 - Binary data)</p>
      "!
      "! @parameter object_id    | <p class="shorttext synchronized" lang="en">Object name</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Test double data in binary format</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      load_mime_object_from_db
        IMPORTING
          object_id       TYPE w3objid
        RETURNING
          VALUE(result) TYPE solix_tab
        RAISING
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Convert delimited values into structured table</p>
      "!
      "! @parameter binary_data          | <p class="shorttext synchronized" lang="en">Binary and delimited test double data (use CL_BCS_CONVERT)</p>
      "! @parameter code_page          | <p class="shorttext synchronized" lang="en">Codepage</p>
      "! @parameter ignore_first_line | <p class="shorttext synchronized" lang="en">X = First line will not be converted</p>
      "! @parameter test_data              | <p class="shorttext synchronized" lang="en">Table with formated data</p>
      "! @raising   zcx_ca_aut_td_loader | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      "! @raising   zcx_ca_conv          | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      convert_mime_into_table
        IMPORTING
          binary_data          TYPE xstring
          code_page          TYPE cpcodepage DEFAULT '4110'
          ignore_first_line TYPE abap_bool  DEFAULT abap_false
        EXPORTING
          test_data              TYPE STANDARD TABLE
        RAISING
          zcx_ca_aut_td_loader
          zcx_ca_conv.
protected section.
private section.
ENDCLASS.



CLASS ZCL_CA_AUT_TD_LOADER IMPLEMENTATION.


  METHOD convert_mime_into_table.
    "-----------------------------------------------------------------*
    "   Convert delimited values into structured table
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _data_in_character            TYPE string,
      _data_in_char_splitted_at_crlf            TYPE TABLE OF string,
      lt_found_delimiter TYPE match_result_tab,
      ls_found_delimiter LIKE LINE OF lt_found_delimiter,
      lv_start_position  TYPE i,
      lv_length          TYPE i,
      lv_current_value   TYPE string,
      lv_match_offset    TYPE i,
      lv_match_length    TYPE i,
      lv_index           TYPE i.

    FIELD-SYMBOLS:
      <test_data_row> TYPE any,
      <lv_data> TYPE any.

    TRY.
        "Convert into character string
        _data_in_character = cl_bcs_convert=>xstring_to_string( iv_xstr = binary_data
                                                                iv_cp   = code_page ).

        "split string by lines
        SPLIT _data_in_character AT cl_abap_char_utilities=>cr_lf INTO TABLE _data_in_char_splitted_at_crlf.

        LOOP AT _data_in_char_splitted_at_crlf INTO _data_in_character.
          IF sy-tabix          EQ 1          AND      "ignore first line?
             ignore_first_line EQ abap_true.
            CONTINUE.
          ENDIF.

          IF _data_in_character IS INITIAL.
            CONTINUE.
          ENDIF.

          lv_index = 0.
          APPEND INITIAL LINE TO test_data ASSIGNING <test_data_row>.

          "Regex which returns a table with the positions of the delimiters (;)
          FIND ALL OCCURRENCES OF REGEX ';(?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))'
                                                               IN _data_in_character RESULTS lt_found_delimiter.
          IF sy-subrc NE 0.
            FIND ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab
                                                               IN _data_in_character RESULTS lt_found_delimiter.
          ENDIF.

          lv_start_position = 0.

          "Loop at lt_found_delimiter and extract one value of this row in each loop
          LOOP AT lt_found_delimiter INTO ls_found_delimiter.
            "Length of the current value
            lv_length = ls_found_delimiter-offset - lv_start_position.
            "Get the current value by scrape out the text between start and ;
            lv_current_value = substring( val = _data_in_character
                                          off = lv_start_position
                                          len = lv_length ).
            "Save the start positon for the next iteration
            lv_start_position = lv_start_position + lv_length + 1.
            "in the text are escaped by another ", so it looks like "". To de-escape we replace "" by "
            REPLACE ALL OCCURRENCES OF '""' IN lv_current_value WITH '"'.
            "If value is in double quotes, extract the text between the quotes
            FIND REGEX '[^"].*[^"]' IN lv_current_value MATCH OFFSET lv_match_offset
                                                        MATCH LENGTH lv_match_length.
            IF sy-subrc EQ 0.
              "Strips the double quotes, if the text is in double quotes
              lv_current_value = substring( val = lv_current_value
                                            off = lv_match_offset
                                            len = lv_match_length ).
            ENDIF.
            lv_index =  lv_index + 1.
            ASSIGN COMPONENT lv_index OF STRUCTURE <test_data_row> TO <lv_data>.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.

            zcl_ca_conv=>external_2_internal(
                                        EXPORTING
                                          external_value = lv_current_value
                                        IMPORTING
                                          internal_value = <lv_data> ).
          ENDLOOP.

          "Now we have to process the value after the last ;
          "Length of the current value
          lv_length = strlen( _data_in_character ) - lv_start_position.
          "Get the current value by scrape out the text after ;
          lv_current_value = substring( val = _data_in_character
                                        off = lv_start_position
                                        len = lv_length ).
          "in the text are escaped by another ", so it looks like "". To de-escape we replace "" by "
          REPLACE ALL OCCURRENCES OF '""' IN lv_current_value WITH '"'.
          "If value is in double quotes, extract the text between the quotes
          FIND REGEX '[^"].*[^"]' IN lv_current_value MATCH OFFSET lv_match_offset
                                                      MATCH LENGTH lv_match_length.
          IF sy-subrc EQ 0.
            "Strips the double quotes, if the text is in double quotes
            lv_current_value = substring( val = lv_current_value
                                          off = lv_match_offset
                                          len = lv_match_length ).
          ENDIF.
          lv_index = lv_index + 1.
          ASSIGN COMPONENT lv_index OF STRUCTURE <test_data_row> TO <lv_data>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          zcl_ca_conv=>external_2_internal(
                                      EXPORTING
                                        external_value = lv_current_value
                                      IMPORTING
                                        internal_value = <lv_data> ).
        ENDLOOP.

      CATCH cx_sy_conversion_error
            cx_sy_range_out_of_bounds INTO DATA(lx_catched).
        DATA(lx_error) =
             CAST zcx_ca_aut_td_loader(
                    zcx_ca_error=>create_exception(
                             iv_excp_cls = 'ZCX_CA_AUT_TD_LOADER'
                             iv_class    = CONV #( sy-repid )
                             iv_method   = 'CONVERT_MIME_INTO_TABLE'
                             ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "convert_mime_into_table


  METHOD load_mime_object_from_db.
    "-----------------------------------------------------------------*
    "   Load test double from MIME database (TA SMW0 - Binary data)
    "-----------------------------------------------------------------*
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = VALUE wwwdatatab( relid = 'MI'
                                              objid = object_id )
      TABLES
        mime              = result
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 3 ##no_text.
    IF sy-subrc NE 0.
      DATA(lx_error) =
           CAST zcx_ca_dbacc(
                      zcx_ca_error=>create_exception(
                                       iv_excp_cls = 'ZCX_CA_DBACC'
                                       iv_function = 'WWWDATA_IMPORT'
                                       iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "load_mime_object_from_db
ENDCLASS.
