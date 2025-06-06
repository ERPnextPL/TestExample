CLASS zcl_morse_translator DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS encode
      IMPORTING iv_text TYPE string
      RETURNING VALUE(rv_text) TYPE string.
    METHODS decode
      IMPORTING iv_text TYPE string
      RETURNING VALUE(rv_text) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF t_morse,
             char TYPE c LENGTH 1,
             code TYPE string,
           END OF t_morse.
    CLASS-DATA lt_by_char TYPE HASHED TABLE OF t_morse WITH UNIQUE KEY char.
    CLASS-DATA lt_by_code TYPE HASHED TABLE OF t_morse WITH UNIQUE KEY code.
    CLASS-METHODS class_constructor.
ENDCLASS.

CLASS zcl_morse_translator IMPLEMENTATION.
  METHOD class_constructor.
    DATA ls_morse TYPE t_morse.
    ls_morse-char = 'A'. ls_morse-code = '.-'.    INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'B'. ls_morse-code = '-...'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'C'. ls_morse-code = '-.-.'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'D'. ls_morse-code = '-..'.   INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'E'. ls_morse-code = '.'.     INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'F'. ls_morse-code = '..-.'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'G'. ls_morse-code = '--.'.   INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'H'. ls_morse-code = '....'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'I'. ls_morse-code = '..'.    INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'J'. ls_morse-code = '.---'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'K'. ls_morse-code = '-.-'.   INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'L'. ls_morse-code = '.-..'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'M'. ls_morse-code = '--'.    INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'N'. ls_morse-code = '-.'.    INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'O'. ls_morse-code = '---'.   INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'P'. ls_morse-code = '.--.'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'Q'. ls_morse-code = '--.-'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'R'. ls_morse-code = '.-.'.   INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'S'. ls_morse-code = '...'.   INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'T'. ls_morse-code = '-'.     INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'U'. ls_morse-code = '..-'.   INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'V'. ls_morse-code = '...-'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'W'. ls_morse-code = '.--'.   INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'X'. ls_morse-code = '-..-'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'Y'. ls_morse-code = '-.--'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = 'Z'. ls_morse-code = '--..'.  INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '0'. ls_morse-code = '-----'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '1'. ls_morse-code = '.----'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '2'. ls_morse-code = '..---'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '3'. ls_morse-code = '...--'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '4'. ls_morse-code = '....-'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '5'. ls_morse-code = '.....'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '6'. ls_morse-code = '-....'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '7'. ls_morse-code = '--...'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '8'. ls_morse-code = '---..'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
    ls_morse-char = '9'. ls_morse-code = '----.'. INSERT ls_morse INTO TABLE lt_by_char. INSERT ls_morse INTO TABLE lt_by_code.
  ENDMETHOD.

  METHOD encode.
    DATA: lv_char TYPE c LENGTH 1.
    rv_text = ''.
    DO strlen( iv_text ) TIMES.
      lv_char = to_upper( iv_text+sy-index(1) ).
      IF lv_char = ' '.
        CONCATENATE rv_text '/' INTO rv_text SEPARATED BY space.
      ELSE.
        READ TABLE lt_by_char INTO DATA(ls_morse) WITH KEY char = lv_char.
        IF sy-subrc = 0.
          CONCATENATE rv_text ls_morse-code INTO rv_text SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD decode.
    DATA: lt_tokens TYPE STANDARD TABLE OF string,
          lv_token  TYPE string.
    SPLIT iv_text AT space INTO TABLE lt_tokens.
    CLEAR rv_text.
    LOOP AT lt_tokens INTO lv_token.
      IF lv_token = '/'.
        CONCATENATE rv_text ' ' INTO rv_text.
      ELSE.
        READ TABLE lt_by_code INTO DATA(ls_morse) WITH KEY code = lv_token.
        IF sy-subrc = 0.
          CONCATENATE rv_text ls_morse-char INTO rv_text.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_morse_translator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA sut TYPE REF TO zcl_morse_translator.
    METHODS setup.
    METHODS encode_test FOR TESTING.
    METHODS decode_test FOR TESTING.
ENDCLASS.

CLASS ltcl_morse_translator IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT sut.
  ENDMETHOD.

  METHOD encode_test.
    DATA(lv_result) = sut->encode( 'SOS' ).
    cl_abap_unit_assert=>assert_equals( exp = '... --- ...' act = lv_result ).
  ENDMETHOD.

  METHOD decode_test.
    DATA(lv_result) = sut->decode( '... --- ...' ).
    cl_abap_unit_assert=>assert_equals( exp = 'SOS' act = lv_result ).
  ENDMETHOD.
ENDCLASS.
