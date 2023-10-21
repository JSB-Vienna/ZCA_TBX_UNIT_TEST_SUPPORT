"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Exception while loading test double data</p>
class ZCX_CA_AUT_TD_LOADER definition
  public
  inheriting from ZCX_CA_PARAM
  create public .

public section.

  constants:
    BEGIN OF zcx_ca_aut_td_loader,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '088',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_aut_td_loader .
    "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_AUT_TD_LOADER type SEOCLSNAME value 'ZCX_CA_AUT_TD_LOADER' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYMSGTY optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional
      !MV_SEVERITY type T_SEVERITY optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_CA_AUT_TD_LOADER IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
MV_SEVERITY = MV_SEVERITY
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_AUT_TD_LOADER .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
