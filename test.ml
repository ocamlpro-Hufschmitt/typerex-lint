open Std_utils
open Location
open! Asttypes
open Parsetree
open Lexing
open Longident
open Ast_element
let () = let open! Match in ()
module A =
  struct
    type transition =
      (Element.bool*
        (Match.t -> Ast_element.t -> (t* Match.t) Std_utils.List.t))
    and state =
      {
      mutable transitions: transition Std_utils.List.t;
      mutable final: Element.bool;}
    and t =
      | Trash
      | Final
      | Attribute of (state* state)
      | Extension of (state* state)
      | Attributes of list
      | Payload of payload
      | Core_type of core_type
      | Core_type_desc of core_type_desc
      | Package_type of (state* state)
      | Row_field of row_field
      | Pattern of pattern
      | Pattern_desc of pattern_desc
      | Expression of expression
      | Expression_desc of expression_desc
      | Case of case
      | Value_description of value_description
      | Type_declaration of type_declaration
      | Type_kind of type_kind
      | Label_declaration of label_declaration
      | Constructor_declaration of constructor_declaration
      | Type_extension of type_extension
      | Extension_constructor of extension_constructor
      | Extension_constructor_kind of extension_constructor_kind
      | Class_type of class_type
      | Class_type_desc of class_type_desc
      | Class_signature of class_signature
      | Class_type_field of class_type_field
      | Class_type_field_desc of class_type_field_desc
      | Class_description of class_infos
      | Class_type_declaration of class_infos
      | Class_expr of class_expr
      | Class_expr_desc of class_expr_desc
      | Class_structure of class_structure
      | Class_field of class_field
      | Class_field_desc of class_field_desc
      | Class_field_kind of class_field_kind
      | Class_declaration of class_infos
      | Module_type of module_type
      | Module_type_desc of module_type_desc
      | Signature of list
      | Signature_item of signature_item
      | Signature_item_desc of signature_item_desc
      | Module_declaration of module_declaration
      | Module_type_declaration of module_type_declaration
      | Open_description of open_description
      | Include_description of include_infos
      | Include_declaration of include_infos
      | With_constraint of with_constraint
      | Module_expr of module_expr
      | Module_expr_desc of module_expr_desc
      | Structure of list
      | Structure_item of structure_item
      | Structure_item_desc of structure_item_desc
      | Value_binding of value_binding
      | Module_binding of module_binding
      | Unit of unit
      | Bool of bool
      | Int of int
      | Char of char
      | String of string
      | Int32 of int32
      | Int64 of int64
      | Nativeint of nativeint
      | Lexing__position of lexing__position
      | Location__t of location__t
      | Longident__t of longident__t
      | Constant of constant
      | Rec_flag of rec_flag
      | Direction_flag of direction_flag
      | Private_flag of private_flag
      | Mutable_flag of mutable_flag
      | Virtual_flag of virtual_flag
      | Override_flag of override_flag
      | Closed_flag of closed_flag
      | Label of label
      | Variance of variance
      | Attribute_list of list
      | Case_list of list
      | Class_declaration_list of list
      | Class_description_list of list
      | Class_expr_class_infos of class_infos
      | Class_field_kind_mutable_flag_string_loc_tuple_3 of (state* state*
      state)
      | Class_field_kind_private_flag_string_loc_tuple_3 of (state* state*
      state)
      | Class_field_list of list
      | Class_type_class_infos of class_infos
      | Class_type_declaration_list of list
      | Class_type_field_list of list
      | Constructor_declaration_list of list
      | Core_type_Longident__t_loc_tuple_2 of (state* state)
      | Core_type_Longident__t_loc_tuple_2_list of list
      | Core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2 of
      (state* state)
      | Core_type_attributes_string_tuple_3 of (state* state* state)
      | Core_type_attributes_string_tuple_3_list of list
      | Core_type_core_type_tuple_2 of (state* state)
      | Core_type_list of list
      | Core_type_option of option
      | Core_type_virtual_flag_mutable_flag_string_tuple_4 of (state* state*
      state* state)
      | Core_type_virtual_flag_private_flag_string_tuple_4 of (state* state*
      state* state)
      | Expression_Longident__t_loc_tuple_2 of (state* state)
      | Expression_Longident__t_loc_tuple_2_list of list
      | Expression_label_tuple_2 of (state* state)
      | Expression_label_tuple_2_list of list
      | Expression_list of list
      | Expression_option of option
      | Expression_string_loc_tuple_2 of (state* state)
      | Expression_string_loc_tuple_2_list of list
      | Extension_constructor_list of list
      | Label_declaration_list of list
      | Label_list of list
      | Label_list_option of option
      | Location__t_core_type_core_type_tuple_3 of (state* state* state)
      | Location__t_core_type_core_type_tuple_3_list of list
      | Longident__t_loc of loc
      | Module_binding_list of list
      | Module_declaration_list of list
      | Module_expr_include_infos of include_infos
      | Module_type_include_infos of include_infos
      | Module_type_option of option
      | Pattern_Longident__t_loc_tuple_2 of (state* state)
      | Pattern_Longident__t_loc_tuple_2_list of list
      | Pattern_list of list
      | Pattern_option of option
      | Payload_string_loc_tuple_2 of (state* state)
      | Row_field_list of list
      | Signature_item_list of list
      | String_list of list
      | String_loc of loc
      | String_option of option
      | Structure_item_list of list
      | Type_declaration_list of list
      | Value_binding_list of list
      | Variance_core_type_tuple_2 of (state* state)
      | Variance_core_type_tuple_2_list of list
      | With_constraint_list of list
    and attribute = (state* state)
    and extension = (state* state)
    and attributes = state
    and payload =
      | PStr of state
      | PTyp of state
      | PPat of state* state
    and core_type =
      {
      ptyp_desc: state;
      ptyp_loc: state;
      ptyp_attributes: state;}
    and core_type_desc =
      | Ptyp_any
      | Ptyp_var of state
      | Ptyp_arrow of state* state* state
      | Ptyp_tuple of state
      | Ptyp_constr of state* state
      | Ptyp_object of state* state
      | Ptyp_class of state* state
      | Ptyp_alias of state* state
      | Ptyp_variant of state* state* state
      | Ptyp_poly of state* state
      | Ptyp_package of state
      | Ptyp_extension of state
    and package_type = (state* state)
    and row_field =
      | Rtag of state* state* state* state
      | Rinherit of state
    and pattern = {
      ppat_desc: state;
      ppat_loc: state;
      ppat_attributes: state;}
    and pattern_desc =
      | Ppat_any
      | Ppat_var of state
      | Ppat_alias of state* state
      | Ppat_constant of state
      | Ppat_interval of state* state
      | Ppat_tuple of state
      | Ppat_construct of state* state
      | Ppat_variant of state* state
      | Ppat_record of state* state
      | Ppat_array of state
      | Ppat_or of state* state
      | Ppat_constraint of state* state
      | Ppat_type of state
      | Ppat_lazy of state
      | Ppat_unpack of state
      | Ppat_exception of state
      | Ppat_extension of state
    and expression =
      {
      pexp_desc: state;
      pexp_loc: state;
      pexp_attributes: state;}
    and expression_desc =
      | Pexp_ident of state
      | Pexp_constant of state
      | Pexp_let of state* state* state
      | Pexp_function of state
      | Pexp_fun of state* state* state* state
      | Pexp_apply of state* state
      | Pexp_match of state* state
      | Pexp_try of state* state
      | Pexp_tuple of state
      | Pexp_construct of state* state
      | Pexp_variant of state* state
      | Pexp_record of state* state
      | Pexp_field of state* state
      | Pexp_setfield of state* state* state
      | Pexp_array of state
      | Pexp_ifthenelse of state* state* state
      | Pexp_sequence of state* state
      | Pexp_while of state* state
      | Pexp_for of state* state* state* state* state
      | Pexp_constraint of state* state
      | Pexp_coerce of state* state* state
      | Pexp_send of state* state
      | Pexp_new of state
      | Pexp_setinstvar of state* state
      | Pexp_override of state
      | Pexp_letmodule of state* state* state
      | Pexp_assert of state
      | Pexp_lazy of state
      | Pexp_poly of state* state
      | Pexp_object of state
      | Pexp_newtype of state* state
      | Pexp_pack of state
      | Pexp_open of state* state* state
      | Pexp_extension of state
    and case = {
      pc_lhs: state;
      pc_guard: state;
      pc_rhs: state;}
    and value_description =
      {
      pval_name: state;
      pval_type: state;
      pval_prim: state;
      pval_attributes: state;
      pval_loc: state;}
    and type_declaration =
      {
      ptype_name: state;
      ptype_params: state;
      ptype_cstrs: state;
      ptype_kind: state;
      ptype_private: state;
      ptype_manifest: state;
      ptype_attributes: state;
      ptype_loc: state;}
    and type_kind =
      | Ptype_abstract
      | Ptype_variant of state
      | Ptype_record of state
      | Ptype_open
    and label_declaration =
      {
      pld_name: state;
      pld_mutable: state;
      pld_type: state;
      pld_loc: state;
      pld_attributes: state;}
    and constructor_declaration =
      {
      pcd_name: state;
      pcd_args: state;
      pcd_res: state;
      pcd_loc: state;
      pcd_attributes: state;}
    and type_extension =
      {
      ptyext_path: state;
      ptyext_params: state;
      ptyext_constructors: state;
      ptyext_private: state;
      ptyext_attributes: state;}
    and extension_constructor =
      {
      pext_name: state;
      pext_kind: state;
      pext_loc: state;
      pext_attributes: state;}
    and extension_constructor_kind =
      | Pext_decl of state* state
      | Pext_rebind of state
    and class_type =
      {
      pcty_desc: state;
      pcty_loc: state;
      pcty_attributes: state;}
    and class_type_desc =
      | Pcty_constr of state* state
      | Pcty_signature of state
      | Pcty_arrow of state* state* state
      | Pcty_extension of state
    and class_signature = {
      pcsig_self: state;
      pcsig_fields: state;}
    and class_type_field =
      {
      pctf_desc: state;
      pctf_loc: state;
      pctf_attributes: state;}
    and class_type_field_desc =
      | Pctf_inherit of state
      | Pctf_val of state
      | Pctf_method of state
      | Pctf_constraint of state
      | Pctf_attribute of state
      | Pctf_extension of state
    and class_infos =
      {
      pci_virt: state;
      pci_params: state;
      pci_name: state;
      pci_expr: state;
      pci_loc: state;
      pci_attributes: state;}
    and class_description = state
    and class_type_declaration = state
    and class_expr = {
      pcl_desc: state;
      pcl_loc: state;
      pcl_attributes: state;}
    and class_expr_desc =
      | Pcl_constr of state* state
      | Pcl_structure of state
      | Pcl_fun of state* state* state* state
      | Pcl_apply of state* state
      | Pcl_let of state* state* state
      | Pcl_constraint of state* state
      | Pcl_extension of state
    and class_structure = {
      pcstr_self: state;
      pcstr_fields: state;}
    and class_field = {
      pcf_desc: state;
      pcf_loc: state;
      pcf_attributes: state;}
    and class_field_desc =
      | Pcf_inherit of state* state* state
      | Pcf_val of state
      | Pcf_method of state
      | Pcf_constraint of state
      | Pcf_initializer of state
      | Pcf_attribute of state
      | Pcf_extension of state
    and class_field_kind =
      | Cfk_virtual of state
      | Cfk_concrete of state* state
    and class_declaration = state
    and module_type =
      {
      pmty_desc: state;
      pmty_loc: state;
      pmty_attributes: state;}
    and module_type_desc =
      | Pmty_ident of state
      | Pmty_signature of state
      | Pmty_functor of state* state* state
      | Pmty_with of state* state
      | Pmty_typeof of state
      | Pmty_extension of state
      | Pmty_alias of state
    and signature = state
    and signature_item = {
      psig_desc: state;
      psig_loc: state;}
    and signature_item_desc =
      | Psig_value of state
      | Psig_type of state
      | Psig_typext of state
      | Psig_exception of state
      | Psig_module of state
      | Psig_recmodule of state
      | Psig_modtype of state
      | Psig_open of state
      | Psig_include of state
      | Psig_class of state
      | Psig_class_type of state
      | Psig_attribute of state
      | Psig_extension of state* state
    and module_declaration =
      {
      pmd_name: state;
      pmd_type: state;
      pmd_attributes: state;
      pmd_loc: state;}
    and module_type_declaration =
      {
      pmtd_name: state;
      pmtd_type: state;
      pmtd_attributes: state;
      pmtd_loc: state;}
    and open_description =
      {
      popen_lid: state;
      popen_override: state;
      popen_loc: state;
      popen_attributes: state;}
    and include_infos =
      {
      pincl_mod: state;
      pincl_loc: state;
      pincl_attributes: state;}
    and include_description = state
    and include_declaration = state
    and with_constraint =
      | Pwith_type of state* state
      | Pwith_module of state* state
      | Pwith_typesubst of state
      | Pwith_modsubst of state* state
    and module_expr =
      {
      pmod_desc: state;
      pmod_loc: state;
      pmod_attributes: state;}
    and module_expr_desc =
      | Pmod_ident of state
      | Pmod_structure of state
      | Pmod_functor of state* state* state
      | Pmod_apply of state* state
      | Pmod_constraint of state* state
      | Pmod_unpack of state
      | Pmod_extension of state
    and structure = state
    and structure_item = {
      pstr_desc: state;
      pstr_loc: state;}
    and structure_item_desc =
      | Pstr_eval of state* state
      | Pstr_value of state* state
      | Pstr_primitive of state
      | Pstr_type of state
      | Pstr_typext of state
      | Pstr_exception of state
      | Pstr_module of state
      | Pstr_recmodule of state
      | Pstr_modtype of state
      | Pstr_open of state
      | Pstr_class of state
      | Pstr_class_type of state
      | Pstr_include of state
      | Pstr_attribute of state
      | Pstr_extension of state* state
    and value_binding =
      {
      pvb_pat: state;
      pvb_expr: state;
      pvb_attributes: state;
      pvb_loc: state;}
    and module_binding =
      {
      pmb_name: state;
      pmb_expr: state;
      pmb_attributes: state;
      pmb_loc: state;}
    and list =
      | Nil
      | Cons of state* state
    and option =
      | None
      | Some of state
    and unit = state
    and bool = state
    and int = state
    and char = state
    and string = state
    and int32 = state
    and int64 = state
    and nativeint = state
    and lexing__position =
      {
      pos_fname: state;
      pos_lnum: state;
      pos_bol: state;
      pos_cnum: state;}
    and location__t = {
      loc_start: state;
      loc_end: state;
      loc_ghost: state;}
    and longident__t =
      | Lident of state
      | Ldot of state* state
      | Lapply of state* state
    and constant =
      | Const_int of state
      | Const_char of state
      | Const_string of state* state
      | Const_float of state
      | Const_int32 of state
      | Const_int64 of state
      | Const_nativeint of state
    and rec_flag =
      | Nonrecursive
      | Recursive
    and direction_flag =
      | Upto
      | Downto
    and private_flag =
      | Private
      | Public
    and mutable_flag =
      | Immutable
      | Mutable
    and virtual_flag =
      | Virtual
      | Concrete
    and override_flag =
      | Override
      | Fresh
    and closed_flag =
      | Closed
      | Open
    and label = state
    and loc = {
      txt: state;
      loc: state;}
    and variance =
      | Covariant
      | Contravariant
      | Invariant
    and attribute_list = state
    and case_list = state
    and class_declaration_list = state
    and class_description_list = state
    and class_expr_class_infos = state
    and class_field_kind_mutable_flag_string_loc_tuple_3 =
      (state* state* state)
    and class_field_kind_private_flag_string_loc_tuple_3 =
      (state* state* state)
    and class_field_list = state
    and class_type_class_infos = state
    and class_type_declaration_list = state
    and class_type_field_list = state
    and constructor_declaration_list = state
    and core_type_Longident__t_loc_tuple_2 = (state* state)
    and core_type_Longident__t_loc_tuple_2_list = state
    and core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2 =
      (state* state)
    and core_type_attributes_string_tuple_3 = (state* state* state)
    and core_type_attributes_string_tuple_3_list = state
    and core_type_core_type_tuple_2 = (state* state)
    and core_type_list = state
    and core_type_option = state
    and core_type_virtual_flag_mutable_flag_string_tuple_4 =
      (state* state* state* state)
    and core_type_virtual_flag_private_flag_string_tuple_4 =
      (state* state* state* state)
    and expression_Longident__t_loc_tuple_2 = (state* state)
    and expression_Longident__t_loc_tuple_2_list = state
    and expression_label_tuple_2 = (state* state)
    and expression_label_tuple_2_list = state
    and expression_list = state
    and expression_option = state
    and expression_string_loc_tuple_2 = (state* state)
    and expression_string_loc_tuple_2_list = state
    and extension_constructor_list = state
    and label_declaration_list = state
    and label_list = state
    and label_list_option = state
    and location__t_core_type_core_type_tuple_3 = (state* state* state)
    and location__t_core_type_core_type_tuple_3_list = state
    and longident__t_loc = state
    and module_binding_list = state
    and module_declaration_list = state
    and module_expr_include_infos = state
    and module_type_include_infos = state
    and module_type_option = state
    and pattern_Longident__t_loc_tuple_2 = (state* state)
    and pattern_Longident__t_loc_tuple_2_list = state
    and pattern_list = state
    and pattern_option = state
    and payload_string_loc_tuple_2 = (state* state)
    and row_field_list = state
    and signature_item_list = state
    and string_list = state
    and string_loc = state
    and string_option = state
    and structure_item_list = state
    and type_declaration_list = state
    and value_binding_list = state
    and variance_core_type_tuple_2 = (state* state)
    and variance_core_type_tuple_2_list = state
    and with_constraint_list = state
  end
module Match_ =
  struct
    let ignore_meta f m y = List.map (fun elt  -> (elt, m)) (f y)
    let basic_state f =
      { A.transitions = [(false, (ignore_meta f))]; final = false }
    let attribute (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Attribute (_,_) -> [A.Attribute (state_0, state_1)]
         | _ -> [A.Trash])
    and extension (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Extension (_,_) -> [A.Extension (state_0, state_1)]
         | _ -> [A.Trash])
    and attributes_nil =
      basic_state
        (function
         | Element.Attributes [] -> [A.Attributes A.Nil]
         | _ -> [A.Trash])
    and attributes_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Attributes (_::_) ->
             [A.Attributes (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and payload_pStr (state_0 : A.state) =
      basic_state
        (function
         | Element.Payload (PStr _) -> [A.Payload (A.PStr state_0)]
         | _ -> [A.Trash])
    and payload_pTyp (state_0 : A.state) =
      basic_state
        (function
         | Element.Payload (PTyp _) -> [A.Payload (A.PTyp state_0)]
         | _ -> [A.Trash])
    and payload_pPat (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Payload (PPat (_,_)) ->
             [A.Payload (A.PPat (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type (ptyp_desc : A.state) (ptyp_loc : A.state)
      (ptyp_attributes : A.state) =
      basic_state
        (function
         | Element.Core_type
             { ptyp_desc = _; ptyp_loc = _; ptyp_attributes = _ } ->
             [A.Core_type
                {
                  A.ptyp_desc = ptyp_desc;
                  A.ptyp_loc = ptyp_loc;
                  A.ptyp_attributes = ptyp_attributes
                }]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_any =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_any ) ->
             [A.Core_type_desc A.Ptyp_any]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_var (state_0 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_var _) ->
             [A.Core_type_desc (A.Ptyp_var state_0)]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_arrow (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_arrow (_,_,_)) ->
             [A.Core_type_desc (A.Ptyp_arrow (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_tuple (state_0 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_tuple _) ->
             [A.Core_type_desc (A.Ptyp_tuple state_0)]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_constr (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_constr (_,_)) ->
             [A.Core_type_desc (A.Ptyp_constr (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_object (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_object (_,_)) ->
             [A.Core_type_desc (A.Ptyp_object (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_class (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_class (_,_)) ->
             [A.Core_type_desc (A.Ptyp_class (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_alias (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_alias (_,_)) ->
             [A.Core_type_desc (A.Ptyp_alias (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_variant (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_variant (_,_,_)) ->
             [A.Core_type_desc (A.Ptyp_variant (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_poly (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_poly (_,_)) ->
             [A.Core_type_desc (A.Ptyp_poly (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_package (state_0 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_package _) ->
             [A.Core_type_desc (A.Ptyp_package state_0)]
         | _ -> [A.Trash])
    and core_type_desc_ptyp_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Core_type_desc (Ptyp_extension _) ->
             [A.Core_type_desc (A.Ptyp_extension state_0)]
         | _ -> [A.Trash])
    and package_type (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Package_type (_,_) -> [A.Package_type (state_0, state_1)]
         | _ -> [A.Trash])
    and row_field_rtag (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) (state_3 : A.state) =
      basic_state
        (function
         | Element.Row_field (Rtag (_,_,_,_)) ->
             [A.Row_field (A.Rtag (state_0, state_1, state_2, state_3))]
         | _ -> [A.Trash])
    and row_field_rinherit (state_0 : A.state) =
      basic_state
        (function
         | Element.Row_field (Rinherit _) ->
             [A.Row_field (A.Rinherit state_0)]
         | _ -> [A.Trash])
    and pattern (ppat_desc : A.state) (ppat_loc : A.state)
      (ppat_attributes : A.state) =
      basic_state
        (function
         | Element.Pattern
             { ppat_desc = _; ppat_loc = _; ppat_attributes = _ } ->
             [A.Pattern
                {
                  A.ppat_desc = ppat_desc;
                  A.ppat_loc = ppat_loc;
                  A.ppat_attributes = ppat_attributes
                }]
         | _ -> [A.Trash])
    and pattern_desc_ppat_any =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_any ) -> [A.Pattern_desc A.Ppat_any]
         | _ -> [A.Trash])
    and pattern_desc_ppat_var (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_var _) ->
             [A.Pattern_desc (A.Ppat_var state_0)]
         | _ -> [A.Trash])
    and pattern_desc_ppat_alias (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_alias (_,_)) ->
             [A.Pattern_desc (A.Ppat_alias (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_desc_ppat_constant (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_constant _) ->
             [A.Pattern_desc (A.Ppat_constant state_0)]
         | _ -> [A.Trash])
    and pattern_desc_ppat_interval (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_interval (_,_)) ->
             [A.Pattern_desc (A.Ppat_interval (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_desc_ppat_tuple (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_tuple _) ->
             [A.Pattern_desc (A.Ppat_tuple state_0)]
         | _ -> [A.Trash])
    and pattern_desc_ppat_construct (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_construct (_,_)) ->
             [A.Pattern_desc (A.Ppat_construct (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_desc_ppat_variant (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_variant (_,_)) ->
             [A.Pattern_desc (A.Ppat_variant (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_desc_ppat_record (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_record (_,_)) ->
             [A.Pattern_desc (A.Ppat_record (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_desc_ppat_array (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_array _) ->
             [A.Pattern_desc (A.Ppat_array state_0)]
         | _ -> [A.Trash])
    and pattern_desc_ppat_or (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_or (_,_)) ->
             [A.Pattern_desc (A.Ppat_or (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_desc_ppat_constraint (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_constraint (_,_)) ->
             [A.Pattern_desc (A.Ppat_constraint (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_desc_ppat_type (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_type _) ->
             [A.Pattern_desc (A.Ppat_type state_0)]
         | _ -> [A.Trash])
    and pattern_desc_ppat_lazy (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_lazy _) ->
             [A.Pattern_desc (A.Ppat_lazy state_0)]
         | _ -> [A.Trash])
    and pattern_desc_ppat_unpack (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_unpack _) ->
             [A.Pattern_desc (A.Ppat_unpack state_0)]
         | _ -> [A.Trash])
    and pattern_desc_ppat_exception (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_exception _) ->
             [A.Pattern_desc (A.Ppat_exception state_0)]
         | _ -> [A.Trash])
    and pattern_desc_ppat_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_desc (Ppat_extension _) ->
             [A.Pattern_desc (A.Ppat_extension state_0)]
         | _ -> [A.Trash])
    and expression (pexp_desc : A.state) (pexp_loc : A.state)
      (pexp_attributes : A.state) =
      basic_state
        (function
         | Element.Expression
             { pexp_desc = _; pexp_loc = _; pexp_attributes = _ } ->
             [A.Expression
                {
                  A.pexp_desc = pexp_desc;
                  A.pexp_loc = pexp_loc;
                  A.pexp_attributes = pexp_attributes
                }]
         | _ -> [A.Trash])
    and expression_desc_pexp_ident (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_ident _) ->
             [A.Expression_desc (A.Pexp_ident state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_constant (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_constant _) ->
             [A.Expression_desc (A.Pexp_constant state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_let (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_let (_,_,_)) ->
             [A.Expression_desc (A.Pexp_let (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and expression_desc_pexp_function (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_function _) ->
             [A.Expression_desc (A.Pexp_function state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_fun (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) (state_3 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_fun (_,_,_,_)) ->
             [A.Expression_desc
                (A.Pexp_fun (state_0, state_1, state_2, state_3))]
         | _ -> [A.Trash])
    and expression_desc_pexp_apply (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_apply (_,_)) ->
             [A.Expression_desc (A.Pexp_apply (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_match (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_match (_,_)) ->
             [A.Expression_desc (A.Pexp_match (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_try (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_try (_,_)) ->
             [A.Expression_desc (A.Pexp_try (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_tuple (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_tuple _) ->
             [A.Expression_desc (A.Pexp_tuple state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_construct (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_construct (_,_)) ->
             [A.Expression_desc (A.Pexp_construct (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_variant (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.Expression_desc (Pexp_variant (_,_)) ->
             [A.Expression_desc (A.Pexp_variant (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_record (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_record (_,_)) ->
             [A.Expression_desc (A.Pexp_record (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_field (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_field (_,_)) ->
             [A.Expression_desc (A.Pexp_field (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_setfield (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_setfield (_,_,_)) ->
             [A.Expression_desc (A.Pexp_setfield (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and expression_desc_pexp_array (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_array _) ->
             [A.Expression_desc (A.Pexp_array state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_ifthenelse (state_0 : A.state)
      (state_1 : A.state) (state_2 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_ifthenelse (_,_,_)) ->
             [A.Expression_desc
                (A.Pexp_ifthenelse (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and expression_desc_pexp_sequence (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.Expression_desc (Pexp_sequence (_,_)) ->
             [A.Expression_desc (A.Pexp_sequence (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_while (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_while (_,_)) ->
             [A.Expression_desc (A.Pexp_while (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_for (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) (state_3 : A.state) (state_4 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_for (_,_,_,_,_)) ->
             [A.Expression_desc
                (A.Pexp_for (state_0, state_1, state_2, state_3, state_4))]
         | _ -> [A.Trash])
    and expression_desc_pexp_constraint (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_constraint (_,_)) ->
             [A.Expression_desc (A.Pexp_constraint (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_coerce (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_coerce (_,_,_)) ->
             [A.Expression_desc (A.Pexp_coerce (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and expression_desc_pexp_send (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_send (_,_)) ->
             [A.Expression_desc (A.Pexp_send (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_new (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_new _) ->
             [A.Expression_desc (A.Pexp_new state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_setinstvar (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_setinstvar (_,_)) ->
             [A.Expression_desc (A.Pexp_setinstvar (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_override (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_override _) ->
             [A.Expression_desc (A.Pexp_override state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_letmodule (state_0 : A.state)
      (state_1 : A.state) (state_2 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_letmodule (_,_,_)) ->
             [A.Expression_desc
                (A.Pexp_letmodule (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and expression_desc_pexp_assert (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_assert _) ->
             [A.Expression_desc (A.Pexp_assert state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_lazy (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_lazy _) ->
             [A.Expression_desc (A.Pexp_lazy state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_poly (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_poly (_,_)) ->
             [A.Expression_desc (A.Pexp_poly (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_object (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_object _) ->
             [A.Expression_desc (A.Pexp_object state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_newtype (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.Expression_desc (Pexp_newtype (_,_)) ->
             [A.Expression_desc (A.Pexp_newtype (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_desc_pexp_pack (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_pack _) ->
             [A.Expression_desc (A.Pexp_pack state_0)]
         | _ -> [A.Trash])
    and expression_desc_pexp_open (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_open (_,_,_)) ->
             [A.Expression_desc (A.Pexp_open (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and expression_desc_pexp_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_desc (Pexp_extension _) ->
             [A.Expression_desc (A.Pexp_extension state_0)]
         | _ -> [A.Trash])
    and case (pc_lhs : A.state) (pc_guard : A.state) (pc_rhs : A.state) =
      basic_state
        (function
         | Element.Case { pc_lhs = _; pc_guard = _; pc_rhs = _ } ->
             [A.Case
                { A.pc_lhs = pc_lhs; A.pc_guard = pc_guard; A.pc_rhs = pc_rhs
                }]
         | _ -> [A.Trash])
    and value_description (pval_name : A.state) (pval_type : A.state)
      (pval_prim : A.state) (pval_attributes : A.state) (pval_loc : A.state)
      =
      basic_state
        (function
         | Element.Value_description
             { pval_name = _; pval_type = _; pval_prim = _;
               pval_attributes = _; pval_loc = _ }
             ->
             [A.Value_description
                {
                  A.pval_name = pval_name;
                  A.pval_type = pval_type;
                  A.pval_prim = pval_prim;
                  A.pval_attributes = pval_attributes;
                  A.pval_loc = pval_loc
                }]
         | _ -> [A.Trash])
    and type_declaration (ptype_name : A.state) (ptype_params : A.state)
      (ptype_cstrs : A.state) (ptype_kind : A.state)
      (ptype_private : A.state) (ptype_manifest : A.state)
      (ptype_attributes : A.state) (ptype_loc : A.state) =
      basic_state
        (function
         | Element.Type_declaration
             { ptype_name = _; ptype_params = _; ptype_cstrs = _;
               ptype_kind = _; ptype_private = _; ptype_manifest = _;
               ptype_attributes = _; ptype_loc = _ }
             ->
             [A.Type_declaration
                {
                  A.ptype_name = ptype_name;
                  A.ptype_params = ptype_params;
                  A.ptype_cstrs = ptype_cstrs;
                  A.ptype_kind = ptype_kind;
                  A.ptype_private = ptype_private;
                  A.ptype_manifest = ptype_manifest;
                  A.ptype_attributes = ptype_attributes;
                  A.ptype_loc = ptype_loc
                }]
         | _ -> [A.Trash])
    and type_kind_ptype_abstract =
      basic_state
        (function
         | Element.Type_kind (Ptype_abstract ) ->
             [A.Type_kind A.Ptype_abstract]
         | _ -> [A.Trash])
    and type_kind_ptype_variant (state_0 : A.state) =
      basic_state
        (function
         | Element.Type_kind (Ptype_variant _) ->
             [A.Type_kind (A.Ptype_variant state_0)]
         | _ -> [A.Trash])
    and type_kind_ptype_record (state_0 : A.state) =
      basic_state
        (function
         | Element.Type_kind (Ptype_record _) ->
             [A.Type_kind (A.Ptype_record state_0)]
         | _ -> [A.Trash])
    and type_kind_ptype_open =
      basic_state
        (function
         | Element.Type_kind (Ptype_open ) -> [A.Type_kind A.Ptype_open]
         | _ -> [A.Trash])
    and label_declaration (pld_name : A.state) (pld_mutable : A.state)
      (pld_type : A.state) (pld_loc : A.state) (pld_attributes : A.state) =
      basic_state
        (function
         | Element.Label_declaration
             { pld_name = _; pld_mutable = _; pld_type = _; pld_loc = _;
               pld_attributes = _ }
             ->
             [A.Label_declaration
                {
                  A.pld_name = pld_name;
                  A.pld_mutable = pld_mutable;
                  A.pld_type = pld_type;
                  A.pld_loc = pld_loc;
                  A.pld_attributes = pld_attributes
                }]
         | _ -> [A.Trash])
    and constructor_declaration (pcd_name : A.state) (pcd_args : A.state)
      (pcd_res : A.state) (pcd_loc : A.state) (pcd_attributes : A.state) =
      basic_state
        (function
         | Element.Constructor_declaration
             { pcd_name = _; pcd_args = _; pcd_res = _; pcd_loc = _;
               pcd_attributes = _ }
             ->
             [A.Constructor_declaration
                {
                  A.pcd_name = pcd_name;
                  A.pcd_args = pcd_args;
                  A.pcd_res = pcd_res;
                  A.pcd_loc = pcd_loc;
                  A.pcd_attributes = pcd_attributes
                }]
         | _ -> [A.Trash])
    and type_extension (ptyext_path : A.state) (ptyext_params : A.state)
      (ptyext_constructors : A.state) (ptyext_private : A.state)
      (ptyext_attributes : A.state) =
      basic_state
        (function
         | Element.Type_extension
             { ptyext_path = _; ptyext_params = _; ptyext_constructors = _;
               ptyext_private = _; ptyext_attributes = _ }
             ->
             [A.Type_extension
                {
                  A.ptyext_path = ptyext_path;
                  A.ptyext_params = ptyext_params;
                  A.ptyext_constructors = ptyext_constructors;
                  A.ptyext_private = ptyext_private;
                  A.ptyext_attributes = ptyext_attributes
                }]
         | _ -> [A.Trash])
    and extension_constructor (pext_name : A.state) (pext_kind : A.state)
      (pext_loc : A.state) (pext_attributes : A.state) =
      basic_state
        (function
         | Element.Extension_constructor
             { pext_name = _; pext_kind = _; pext_loc = _;
               pext_attributes = _ }
             ->
             [A.Extension_constructor
                {
                  A.pext_name = pext_name;
                  A.pext_kind = pext_kind;
                  A.pext_loc = pext_loc;
                  A.pext_attributes = pext_attributes
                }]
         | _ -> [A.Trash])
    and extension_constructor_kind_pext_decl (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Extension_constructor_kind (Pext_decl (_,_)) ->
             [A.Extension_constructor_kind (A.Pext_decl (state_0, state_1))]
         | _ -> [A.Trash])
    and extension_constructor_kind_pext_rebind (state_0 : A.state) =
      basic_state
        (function
         | Element.Extension_constructor_kind (Pext_rebind _) ->
             [A.Extension_constructor_kind (A.Pext_rebind state_0)]
         | _ -> [A.Trash])
    and class_type (pcty_desc : A.state) (pcty_loc : A.state)
      (pcty_attributes : A.state) =
      basic_state
        (function
         | Element.Class_type
             { pcty_desc = _; pcty_loc = _; pcty_attributes = _ } ->
             [A.Class_type
                {
                  A.pcty_desc = pcty_desc;
                  A.pcty_loc = pcty_loc;
                  A.pcty_attributes = pcty_attributes
                }]
         | _ -> [A.Trash])
    and class_type_desc_pcty_constr (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_type_desc (Pcty_constr (_,_)) ->
             [A.Class_type_desc (A.Pcty_constr (state_0, state_1))]
         | _ -> [A.Trash])
    and class_type_desc_pcty_signature (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_type_desc (Pcty_signature _) ->
             [A.Class_type_desc (A.Pcty_signature state_0)]
         | _ -> [A.Trash])
    and class_type_desc_pcty_arrow (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Class_type_desc (Pcty_arrow (_,_,_)) ->
             [A.Class_type_desc (A.Pcty_arrow (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and class_type_desc_pcty_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_type_desc (Pcty_extension _) ->
             [A.Class_type_desc (A.Pcty_extension state_0)]
         | _ -> [A.Trash])
    and class_signature (pcsig_self : A.state) (pcsig_fields : A.state) =
      basic_state
        (function
         | Element.Class_signature { pcsig_self = _; pcsig_fields = _ } ->
             [A.Class_signature
                { A.pcsig_self = pcsig_self; A.pcsig_fields = pcsig_fields }]
         | _ -> [A.Trash])
    and class_type_field (pctf_desc : A.state) (pctf_loc : A.state)
      (pctf_attributes : A.state) =
      basic_state
        (function
         | Element.Class_type_field
             { pctf_desc = _; pctf_loc = _; pctf_attributes = _ } ->
             [A.Class_type_field
                {
                  A.pctf_desc = pctf_desc;
                  A.pctf_loc = pctf_loc;
                  A.pctf_attributes = pctf_attributes
                }]
         | _ -> [A.Trash])
    and class_type_field_desc_pctf_inherit (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_type_field_desc (Pctf_inherit _) ->
             [A.Class_type_field_desc (A.Pctf_inherit state_0)]
         | _ -> [A.Trash])
    and class_type_field_desc_pctf_val (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_type_field_desc (Pctf_val _) ->
             [A.Class_type_field_desc (A.Pctf_val state_0)]
         | _ -> [A.Trash])
    and class_type_field_desc_pctf_method (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_type_field_desc (Pctf_method _) ->
             [A.Class_type_field_desc (A.Pctf_method state_0)]
         | _ -> [A.Trash])
    and class_type_field_desc_pctf_constraint (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_type_field_desc (Pctf_constraint _) ->
             [A.Class_type_field_desc (A.Pctf_constraint state_0)]
         | _ -> [A.Trash])
    and class_type_field_desc_pctf_attribute (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_type_field_desc (Pctf_attribute _) ->
             [A.Class_type_field_desc (A.Pctf_attribute state_0)]
         | _ -> [A.Trash])
    and class_type_field_desc_pctf_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_type_field_desc (Pctf_extension _) ->
             [A.Class_type_field_desc (A.Pctf_extension state_0)]
         | _ -> [A.Trash])
    and class_description (pci_virt : A.state) (pci_params : A.state)
      (pci_name : A.state) (pci_expr : A.state) (pci_loc : A.state)
      (pci_attributes : A.state) =
      basic_state
        (function
         | Element.Class_description
             { pci_virt = _; pci_params = _; pci_name = _; pci_expr = _;
               pci_loc = _; pci_attributes = _ }
             ->
             [A.Class_description
                {
                  A.pci_virt = pci_virt;
                  A.pci_params = pci_params;
                  A.pci_name = pci_name;
                  A.pci_expr = pci_expr;
                  A.pci_loc = pci_loc;
                  A.pci_attributes = pci_attributes
                }]
         | _ -> [A.Trash])
    and class_type_declaration (pci_virt : A.state) (pci_params : A.state)
      (pci_name : A.state) (pci_expr : A.state) (pci_loc : A.state)
      (pci_attributes : A.state) =
      basic_state
        (function
         | Element.Class_type_declaration
             { pci_virt = _; pci_params = _; pci_name = _; pci_expr = _;
               pci_loc = _; pci_attributes = _ }
             ->
             [A.Class_type_declaration
                {
                  A.pci_virt = pci_virt;
                  A.pci_params = pci_params;
                  A.pci_name = pci_name;
                  A.pci_expr = pci_expr;
                  A.pci_loc = pci_loc;
                  A.pci_attributes = pci_attributes
                }]
         | _ -> [A.Trash])
    and class_expr (pcl_desc : A.state) (pcl_loc : A.state)
      (pcl_attributes : A.state) =
      basic_state
        (function
         | Element.Class_expr
             { pcl_desc = _; pcl_loc = _; pcl_attributes = _ } ->
             [A.Class_expr
                {
                  A.pcl_desc = pcl_desc;
                  A.pcl_loc = pcl_loc;
                  A.pcl_attributes = pcl_attributes
                }]
         | _ -> [A.Trash])
    and class_expr_desc_pcl_constr (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_expr_desc (Pcl_constr (_,_)) ->
             [A.Class_expr_desc (A.Pcl_constr (state_0, state_1))]
         | _ -> [A.Trash])
    and class_expr_desc_pcl_structure (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_expr_desc (Pcl_structure _) ->
             [A.Class_expr_desc (A.Pcl_structure state_0)]
         | _ -> [A.Trash])
    and class_expr_desc_pcl_fun (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) (state_3 : A.state) =
      basic_state
        (function
         | Element.Class_expr_desc (Pcl_fun (_,_,_,_)) ->
             [A.Class_expr_desc
                (A.Pcl_fun (state_0, state_1, state_2, state_3))]
         | _ -> [A.Trash])
    and class_expr_desc_pcl_apply (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_expr_desc (Pcl_apply (_,_)) ->
             [A.Class_expr_desc (A.Pcl_apply (state_0, state_1))]
         | _ -> [A.Trash])
    and class_expr_desc_pcl_let (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Class_expr_desc (Pcl_let (_,_,_)) ->
             [A.Class_expr_desc (A.Pcl_let (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and class_expr_desc_pcl_constraint (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_expr_desc (Pcl_constraint (_,_)) ->
             [A.Class_expr_desc (A.Pcl_constraint (state_0, state_1))]
         | _ -> [A.Trash])
    and class_expr_desc_pcl_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_expr_desc (Pcl_extension _) ->
             [A.Class_expr_desc (A.Pcl_extension state_0)]
         | _ -> [A.Trash])
    and class_structure (pcstr_self : A.state) (pcstr_fields : A.state) =
      basic_state
        (function
         | Element.Class_structure { pcstr_self = _; pcstr_fields = _ } ->
             [A.Class_structure
                { A.pcstr_self = pcstr_self; A.pcstr_fields = pcstr_fields }]
         | _ -> [A.Trash])
    and class_field (pcf_desc : A.state) (pcf_loc : A.state)
      (pcf_attributes : A.state) =
      basic_state
        (function
         | Element.Class_field
             { pcf_desc = _; pcf_loc = _; pcf_attributes = _ } ->
             [A.Class_field
                {
                  A.pcf_desc = pcf_desc;
                  A.pcf_loc = pcf_loc;
                  A.pcf_attributes = pcf_attributes
                }]
         | _ -> [A.Trash])
    and class_field_desc_pcf_inherit (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Class_field_desc (Pcf_inherit (_,_,_)) ->
             [A.Class_field_desc (A.Pcf_inherit (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and class_field_desc_pcf_val (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_field_desc (Pcf_val _) ->
             [A.Class_field_desc (A.Pcf_val state_0)]
         | _ -> [A.Trash])
    and class_field_desc_pcf_method (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_field_desc (Pcf_method _) ->
             [A.Class_field_desc (A.Pcf_method state_0)]
         | _ -> [A.Trash])
    and class_field_desc_pcf_constraint (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_field_desc (Pcf_constraint _) ->
             [A.Class_field_desc (A.Pcf_constraint state_0)]
         | _ -> [A.Trash])
    and class_field_desc_pcf_initializer (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_field_desc (Pcf_initializer _) ->
             [A.Class_field_desc (A.Pcf_initializer state_0)]
         | _ -> [A.Trash])
    and class_field_desc_pcf_attribute (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_field_desc (Pcf_attribute _) ->
             [A.Class_field_desc (A.Pcf_attribute state_0)]
         | _ -> [A.Trash])
    and class_field_desc_pcf_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_field_desc (Pcf_extension _) ->
             [A.Class_field_desc (A.Pcf_extension state_0)]
         | _ -> [A.Trash])
    and class_field_kind_cfk_virtual (state_0 : A.state) =
      basic_state
        (function
         | Element.Class_field_kind (Cfk_virtual _) ->
             [A.Class_field_kind (A.Cfk_virtual state_0)]
         | _ -> [A.Trash])
    and class_field_kind_cfk_concrete (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.Class_field_kind (Cfk_concrete (_,_)) ->
             [A.Class_field_kind (A.Cfk_concrete (state_0, state_1))]
         | _ -> [A.Trash])
    and class_declaration (pci_virt : A.state) (pci_params : A.state)
      (pci_name : A.state) (pci_expr : A.state) (pci_loc : A.state)
      (pci_attributes : A.state) =
      basic_state
        (function
         | Element.Class_declaration
             { pci_virt = _; pci_params = _; pci_name = _; pci_expr = _;
               pci_loc = _; pci_attributes = _ }
             ->
             [A.Class_declaration
                {
                  A.pci_virt = pci_virt;
                  A.pci_params = pci_params;
                  A.pci_name = pci_name;
                  A.pci_expr = pci_expr;
                  A.pci_loc = pci_loc;
                  A.pci_attributes = pci_attributes
                }]
         | _ -> [A.Trash])
    and module_type (pmty_desc : A.state) (pmty_loc : A.state)
      (pmty_attributes : A.state) =
      basic_state
        (function
         | Element.Module_type
             { pmty_desc = _; pmty_loc = _; pmty_attributes = _ } ->
             [A.Module_type
                {
                  A.pmty_desc = pmty_desc;
                  A.pmty_loc = pmty_loc;
                  A.pmty_attributes = pmty_attributes
                }]
         | _ -> [A.Trash])
    and module_type_desc_pmty_ident (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_type_desc (Pmty_ident _) ->
             [A.Module_type_desc (A.Pmty_ident state_0)]
         | _ -> [A.Trash])
    and module_type_desc_pmty_signature (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_type_desc (Pmty_signature _) ->
             [A.Module_type_desc (A.Pmty_signature state_0)]
         | _ -> [A.Trash])
    and module_type_desc_pmty_functor (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Module_type_desc (Pmty_functor (_,_,_)) ->
             [A.Module_type_desc (A.Pmty_functor (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and module_type_desc_pmty_with (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Module_type_desc (Pmty_with (_,_)) ->
             [A.Module_type_desc (A.Pmty_with (state_0, state_1))]
         | _ -> [A.Trash])
    and module_type_desc_pmty_typeof (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_type_desc (Pmty_typeof _) ->
             [A.Module_type_desc (A.Pmty_typeof state_0)]
         | _ -> [A.Trash])
    and module_type_desc_pmty_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_type_desc (Pmty_extension _) ->
             [A.Module_type_desc (A.Pmty_extension state_0)]
         | _ -> [A.Trash])
    and module_type_desc_pmty_alias (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_type_desc (Pmty_alias _) ->
             [A.Module_type_desc (A.Pmty_alias state_0)]
         | _ -> [A.Trash])
    and signature_nil =
      basic_state
        (function
         | Element.Signature [] -> [A.Signature A.Nil]
         | _ -> [A.Trash])
    and signature_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Signature (_::_) ->
             [A.Signature (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and signature_item (psig_desc : A.state) (psig_loc : A.state) =
      basic_state
        (function
         | Element.Signature_item { psig_desc = _; psig_loc = _ } ->
             [A.Signature_item
                { A.psig_desc = psig_desc; A.psig_loc = psig_loc }]
         | _ -> [A.Trash])
    and signature_item_desc_psig_value (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_value _) ->
             [A.Signature_item_desc (A.Psig_value state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_type (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_type _) ->
             [A.Signature_item_desc (A.Psig_type state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_typext (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_typext _) ->
             [A.Signature_item_desc (A.Psig_typext state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_exception (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_exception _) ->
             [A.Signature_item_desc (A.Psig_exception state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_module (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_module _) ->
             [A.Signature_item_desc (A.Psig_module state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_recmodule (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_recmodule _) ->
             [A.Signature_item_desc (A.Psig_recmodule state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_modtype (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_modtype _) ->
             [A.Signature_item_desc (A.Psig_modtype state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_open (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_open _) ->
             [A.Signature_item_desc (A.Psig_open state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_include (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_include _) ->
             [A.Signature_item_desc (A.Psig_include state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_class (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_class _) ->
             [A.Signature_item_desc (A.Psig_class state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_class_type (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_class_type _) ->
             [A.Signature_item_desc (A.Psig_class_type state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_attribute (state_0 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_attribute _) ->
             [A.Signature_item_desc (A.Psig_attribute state_0)]
         | _ -> [A.Trash])
    and signature_item_desc_psig_extension (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Signature_item_desc (Psig_extension (_,_)) ->
             [A.Signature_item_desc (A.Psig_extension (state_0, state_1))]
         | _ -> [A.Trash])
    and module_declaration (pmd_name : A.state) (pmd_type : A.state)
      (pmd_attributes : A.state) (pmd_loc : A.state) =
      basic_state
        (function
         | Element.Module_declaration
             { pmd_name = _; pmd_type = _; pmd_attributes = _; pmd_loc = _ }
             ->
             [A.Module_declaration
                {
                  A.pmd_name = pmd_name;
                  A.pmd_type = pmd_type;
                  A.pmd_attributes = pmd_attributes;
                  A.pmd_loc = pmd_loc
                }]
         | _ -> [A.Trash])
    and module_type_declaration (pmtd_name : A.state) (pmtd_type : A.state)
      (pmtd_attributes : A.state) (pmtd_loc : A.state) =
      basic_state
        (function
         | Element.Module_type_declaration
             { pmtd_name = _; pmtd_type = _; pmtd_attributes = _;
               pmtd_loc = _ }
             ->
             [A.Module_type_declaration
                {
                  A.pmtd_name = pmtd_name;
                  A.pmtd_type = pmtd_type;
                  A.pmtd_attributes = pmtd_attributes;
                  A.pmtd_loc = pmtd_loc
                }]
         | _ -> [A.Trash])
    and open_description (popen_lid : A.state) (popen_override : A.state)
      (popen_loc : A.state) (popen_attributes : A.state) =
      basic_state
        (function
         | Element.Open_description
             { popen_lid = _; popen_override = _; popen_loc = _;
               popen_attributes = _ }
             ->
             [A.Open_description
                {
                  A.popen_lid = popen_lid;
                  A.popen_override = popen_override;
                  A.popen_loc = popen_loc;
                  A.popen_attributes = popen_attributes
                }]
         | _ -> [A.Trash])
    and include_description (pincl_mod : A.state) (pincl_loc : A.state)
      (pincl_attributes : A.state) =
      basic_state
        (function
         | Element.Include_description
             { pincl_mod = _; pincl_loc = _; pincl_attributes = _ } ->
             [A.Include_description
                {
                  A.pincl_mod = pincl_mod;
                  A.pincl_loc = pincl_loc;
                  A.pincl_attributes = pincl_attributes
                }]
         | _ -> [A.Trash])
    and include_declaration (pincl_mod : A.state) (pincl_loc : A.state)
      (pincl_attributes : A.state) =
      basic_state
        (function
         | Element.Include_declaration
             { pincl_mod = _; pincl_loc = _; pincl_attributes = _ } ->
             [A.Include_declaration
                {
                  A.pincl_mod = pincl_mod;
                  A.pincl_loc = pincl_loc;
                  A.pincl_attributes = pincl_attributes
                }]
         | _ -> [A.Trash])
    and with_constraint_pwith_type (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.With_constraint (Pwith_type (_,_)) ->
             [A.With_constraint (A.Pwith_type (state_0, state_1))]
         | _ -> [A.Trash])
    and with_constraint_pwith_module (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.With_constraint (Pwith_module (_,_)) ->
             [A.With_constraint (A.Pwith_module (state_0, state_1))]
         | _ -> [A.Trash])
    and with_constraint_pwith_typesubst (state_0 : A.state) =
      basic_state
        (function
         | Element.With_constraint (Pwith_typesubst _) ->
             [A.With_constraint (A.Pwith_typesubst state_0)]
         | _ -> [A.Trash])
    and with_constraint_pwith_modsubst (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.With_constraint (Pwith_modsubst (_,_)) ->
             [A.With_constraint (A.Pwith_modsubst (state_0, state_1))]
         | _ -> [A.Trash])
    and module_expr (pmod_desc : A.state) (pmod_loc : A.state)
      (pmod_attributes : A.state) =
      basic_state
        (function
         | Element.Module_expr
             { pmod_desc = _; pmod_loc = _; pmod_attributes = _ } ->
             [A.Module_expr
                {
                  A.pmod_desc = pmod_desc;
                  A.pmod_loc = pmod_loc;
                  A.pmod_attributes = pmod_attributes
                }]
         | _ -> [A.Trash])
    and module_expr_desc_pmod_ident (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_expr_desc (Pmod_ident _) ->
             [A.Module_expr_desc (A.Pmod_ident state_0)]
         | _ -> [A.Trash])
    and module_expr_desc_pmod_structure (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_expr_desc (Pmod_structure _) ->
             [A.Module_expr_desc (A.Pmod_structure state_0)]
         | _ -> [A.Trash])
    and module_expr_desc_pmod_functor (state_0 : A.state) (state_1 : A.state)
      (state_2 : A.state) =
      basic_state
        (function
         | Element.Module_expr_desc (Pmod_functor (_,_,_)) ->
             [A.Module_expr_desc (A.Pmod_functor (state_0, state_1, state_2))]
         | _ -> [A.Trash])
    and module_expr_desc_pmod_apply (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Module_expr_desc (Pmod_apply (_,_)) ->
             [A.Module_expr_desc (A.Pmod_apply (state_0, state_1))]
         | _ -> [A.Trash])
    and module_expr_desc_pmod_constraint (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Module_expr_desc (Pmod_constraint (_,_)) ->
             [A.Module_expr_desc (A.Pmod_constraint (state_0, state_1))]
         | _ -> [A.Trash])
    and module_expr_desc_pmod_unpack (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_expr_desc (Pmod_unpack _) ->
             [A.Module_expr_desc (A.Pmod_unpack state_0)]
         | _ -> [A.Trash])
    and module_expr_desc_pmod_extension (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_expr_desc (Pmod_extension _) ->
             [A.Module_expr_desc (A.Pmod_extension state_0)]
         | _ -> [A.Trash])
    and structure_nil =
      basic_state
        (function
         | Element.Structure [] -> [A.Structure A.Nil]
         | _ -> [A.Trash])
    and structure_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Structure (_::_) ->
             [A.Structure (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and structure_item (pstr_desc : A.state) (pstr_loc : A.state) =
      basic_state
        (function
         | Element.Structure_item { pstr_desc = _; pstr_loc = _ } ->
             [A.Structure_item
                { A.pstr_desc = pstr_desc; A.pstr_loc = pstr_loc }]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_eval (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_eval (_,_)) ->
             [A.Structure_item_desc (A.Pstr_eval (state_0, state_1))]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_value (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_value (_,_)) ->
             [A.Structure_item_desc (A.Pstr_value (state_0, state_1))]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_primitive (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_primitive _) ->
             [A.Structure_item_desc (A.Pstr_primitive state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_type (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_type _) ->
             [A.Structure_item_desc (A.Pstr_type state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_typext (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_typext _) ->
             [A.Structure_item_desc (A.Pstr_typext state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_exception (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_exception _) ->
             [A.Structure_item_desc (A.Pstr_exception state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_module (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_module _) ->
             [A.Structure_item_desc (A.Pstr_module state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_recmodule (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_recmodule _) ->
             [A.Structure_item_desc (A.Pstr_recmodule state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_modtype (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_modtype _) ->
             [A.Structure_item_desc (A.Pstr_modtype state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_open (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_open _) ->
             [A.Structure_item_desc (A.Pstr_open state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_class (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_class _) ->
             [A.Structure_item_desc (A.Pstr_class state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_class_type (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_class_type _) ->
             [A.Structure_item_desc (A.Pstr_class_type state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_include (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_include _) ->
             [A.Structure_item_desc (A.Pstr_include state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_attribute (state_0 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_attribute _) ->
             [A.Structure_item_desc (A.Pstr_attribute state_0)]
         | _ -> [A.Trash])
    and structure_item_desc_pstr_extension (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Structure_item_desc (Pstr_extension (_,_)) ->
             [A.Structure_item_desc (A.Pstr_extension (state_0, state_1))]
         | _ -> [A.Trash])
    and value_binding (pvb_pat : A.state) (pvb_expr : A.state)
      (pvb_attributes : A.state) (pvb_loc : A.state) =
      basic_state
        (function
         | Element.Value_binding
             { pvb_pat = _; pvb_expr = _; pvb_attributes = _; pvb_loc = _ }
             ->
             [A.Value_binding
                {
                  A.pvb_pat = pvb_pat;
                  A.pvb_expr = pvb_expr;
                  A.pvb_attributes = pvb_attributes;
                  A.pvb_loc = pvb_loc
                }]
         | _ -> [A.Trash])
    and module_binding (pmb_name : A.state) (pmb_expr : A.state)
      (pmb_attributes : A.state) (pmb_loc : A.state) =
      basic_state
        (function
         | Element.Module_binding
             { pmb_name = _; pmb_expr = _; pmb_attributes = _; pmb_loc = _ }
             ->
             [A.Module_binding
                {
                  A.pmb_name = pmb_name;
                  A.pmb_expr = pmb_expr;
                  A.pmb_attributes = pmb_attributes;
                  A.pmb_loc = pmb_loc
                }]
         | _ -> [A.Trash])
    and unit x =
      basic_state @@
        (function | Element.Unit y when x = y -> [A.Final] | _ -> [A.Trash])
    and bool x =
      basic_state @@
        (function | Element.Bool y when x = y -> [A.Final] | _ -> [A.Trash])
    and int x =
      basic_state @@
        (function | Element.Int y when x = y -> [A.Final] | _ -> [A.Trash])
    and char x =
      basic_state @@
        (function | Element.Char y when x = y -> [A.Final] | _ -> [A.Trash])
    and string x =
      basic_state @@
        (function | Element.String y when x = y -> [A.Final] | _ -> [A.Trash])
    and int32 x =
      basic_state @@
        (function | Element.Int32 y when x = y -> [A.Final] | _ -> [A.Trash])
    and int64 x =
      basic_state @@
        (function | Element.Int64 y when x = y -> [A.Final] | _ -> [A.Trash])
    and nativeint x =
      basic_state @@
        (function
         | Element.Nativeint y when x = y -> [A.Final]
         | _ -> [A.Trash])
    and lexing__position (pos_fname : A.state) (pos_lnum : A.state)
      (pos_bol : A.state) (pos_cnum : A.state) =
      basic_state
        (function
         | Element.Lexing__position
             { pos_fname = _; pos_lnum = _; pos_bol = _; pos_cnum = _ } ->
             [A.Lexing__position
                {
                  A.pos_fname = pos_fname;
                  A.pos_lnum = pos_lnum;
                  A.pos_bol = pos_bol;
                  A.pos_cnum = pos_cnum
                }]
         | _ -> [A.Trash])
    and location__t (loc_start : A.state) (loc_end : A.state)
      (loc_ghost : A.state) =
      basic_state
        (function
         | Element.Location__t { loc_start = _; loc_end = _; loc_ghost = _ }
             ->
             [A.Location__t
                {
                  A.loc_start = loc_start;
                  A.loc_end = loc_end;
                  A.loc_ghost = loc_ghost
                }]
         | _ -> [A.Trash])
    and longident__t_lident (state_0 : A.state) =
      basic_state
        (function
         | Element.Longident__t (Lident _) ->
             [A.Longident__t (A.Lident state_0)]
         | _ -> [A.Trash])
    and longident__t_ldot (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Longident__t (Ldot (_,_)) ->
             [A.Longident__t (A.Ldot (state_0, state_1))]
         | _ -> [A.Trash])
    and longident__t_lapply (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Longident__t (Lapply (_,_)) ->
             [A.Longident__t (A.Lapply (state_0, state_1))]
         | _ -> [A.Trash])
    and constant_const_int (state_0 : A.state) =
      basic_state
        (function
         | Element.Constant (Const_int _) ->
             [A.Constant (A.Const_int state_0)]
         | _ -> [A.Trash])
    and constant_const_char (state_0 : A.state) =
      basic_state
        (function
         | Element.Constant (Const_char _) ->
             [A.Constant (A.Const_char state_0)]
         | _ -> [A.Trash])
    and constant_const_string (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Constant (Const_string (_,_)) ->
             [A.Constant (A.Const_string (state_0, state_1))]
         | _ -> [A.Trash])
    and constant_const_float (state_0 : A.state) =
      basic_state
        (function
         | Element.Constant (Const_float _) ->
             [A.Constant (A.Const_float state_0)]
         | _ -> [A.Trash])
    and constant_const_int32 (state_0 : A.state) =
      basic_state
        (function
         | Element.Constant (Const_int32 _) ->
             [A.Constant (A.Const_int32 state_0)]
         | _ -> [A.Trash])
    and constant_const_int64 (state_0 : A.state) =
      basic_state
        (function
         | Element.Constant (Const_int64 _) ->
             [A.Constant (A.Const_int64 state_0)]
         | _ -> [A.Trash])
    and constant_const_nativeint (state_0 : A.state) =
      basic_state
        (function
         | Element.Constant (Const_nativeint _) ->
             [A.Constant (A.Const_nativeint state_0)]
         | _ -> [A.Trash])
    and rec_flag_nonrecursive =
      basic_state
        (function
         | Element.Rec_flag (Nonrecursive ) -> [A.Rec_flag A.Nonrecursive]
         | _ -> [A.Trash])
    and rec_flag_recursive =
      basic_state
        (function
         | Element.Rec_flag (Recursive ) -> [A.Rec_flag A.Recursive]
         | _ -> [A.Trash])
    and direction_flag_upto =
      basic_state
        (function
         | Element.Direction_flag (Upto ) -> [A.Direction_flag A.Upto]
         | _ -> [A.Trash])
    and direction_flag_downto =
      basic_state
        (function
         | Element.Direction_flag (Downto ) -> [A.Direction_flag A.Downto]
         | _ -> [A.Trash])
    and private_flag_private =
      basic_state
        (function
         | Element.Private_flag (Private ) -> [A.Private_flag A.Private]
         | _ -> [A.Trash])
    and private_flag_public =
      basic_state
        (function
         | Element.Private_flag (Public ) -> [A.Private_flag A.Public]
         | _ -> [A.Trash])
    and mutable_flag_immutable =
      basic_state
        (function
         | Element.Mutable_flag (Immutable ) -> [A.Mutable_flag A.Immutable]
         | _ -> [A.Trash])
    and mutable_flag_mutable =
      basic_state
        (function
         | Element.Mutable_flag (Mutable ) -> [A.Mutable_flag A.Mutable]
         | _ -> [A.Trash])
    and virtual_flag_virtual =
      basic_state
        (function
         | Element.Virtual_flag (Virtual ) -> [A.Virtual_flag A.Virtual]
         | _ -> [A.Trash])
    and virtual_flag_concrete =
      basic_state
        (function
         | Element.Virtual_flag (Concrete ) -> [A.Virtual_flag A.Concrete]
         | _ -> [A.Trash])
    and override_flag_override =
      basic_state
        (function
         | Element.Override_flag (Override ) -> [A.Override_flag A.Override]
         | _ -> [A.Trash])
    and override_flag_fresh =
      basic_state
        (function
         | Element.Override_flag (Fresh ) -> [A.Override_flag A.Fresh]
         | _ -> [A.Trash])
    and closed_flag_closed =
      basic_state
        (function
         | Element.Closed_flag (Closed ) -> [A.Closed_flag A.Closed]
         | _ -> [A.Trash])
    and closed_flag_open =
      basic_state
        (function
         | Element.Closed_flag (Open ) -> [A.Closed_flag A.Open]
         | _ -> [A.Trash])
    and label x =
      basic_state @@
        (function | Element.Label y when x = y -> [A.Final] | _ -> [A.Trash])
    and variance_covariant =
      basic_state
        (function
         | Element.Variance (Covariant ) -> [A.Variance A.Covariant]
         | _ -> [A.Trash])
    and variance_contravariant =
      basic_state
        (function
         | Element.Variance (Contravariant ) -> [A.Variance A.Contravariant]
         | _ -> [A.Trash])
    and variance_invariant =
      basic_state
        (function
         | Element.Variance (Invariant ) -> [A.Variance A.Invariant]
         | _ -> [A.Trash])
    and attribute_list_nil =
      basic_state
        (function
         | Element.Attribute_list [] -> [A.Attribute_list A.Nil]
         | _ -> [A.Trash])
    and attribute_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Attribute_list (_::_) ->
             [A.Attribute_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and case_list_nil =
      basic_state
        (function
         | Element.Case_list [] -> [A.Case_list A.Nil]
         | _ -> [A.Trash])
    and case_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Case_list (_::_) ->
             [A.Case_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and class_declaration_list_nil =
      basic_state
        (function
         | Element.Class_declaration_list [] ->
             [A.Class_declaration_list A.Nil]
         | _ -> [A.Trash])
    and class_declaration_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_declaration_list (_::_) ->
             [A.Class_declaration_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and class_description_list_nil =
      basic_state
        (function
         | Element.Class_description_list [] ->
             [A.Class_description_list A.Nil]
         | _ -> [A.Trash])
    and class_description_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_description_list (_::_) ->
             [A.Class_description_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and class_expr_class_infos (pci_virt : A.state) (pci_params : A.state)
      (pci_name : A.state) (pci_expr : A.state) (pci_loc : A.state)
      (pci_attributes : A.state) =
      basic_state
        (function
         | Element.Class_expr_class_infos
             { pci_virt = _; pci_params = _; pci_name = _; pci_expr = _;
               pci_loc = _; pci_attributes = _ }
             ->
             [A.Class_expr_class_infos
                {
                  A.pci_virt = pci_virt;
                  A.pci_params = pci_params;
                  A.pci_name = pci_name;
                  A.pci_expr = pci_expr;
                  A.pci_loc = pci_loc;
                  A.pci_attributes = pci_attributes
                }]
         | _ -> [A.Trash])
    and class_field_kind_mutable_flag_string_loc_tuple_3 (state_0 : A.state)
      (state_1 : A.state) (state_2 : A.state) =
      basic_state
        (function
         | Element.Class_field_kind_mutable_flag_string_loc_tuple_3 (_,_,_)
             ->
             [A.Class_field_kind_mutable_flag_string_loc_tuple_3
                (state_0, state_1, state_2)]
         | _ -> [A.Trash])
    and class_field_kind_private_flag_string_loc_tuple_3 (state_0 : A.state)
      (state_1 : A.state) (state_2 : A.state) =
      basic_state
        (function
         | Element.Class_field_kind_private_flag_string_loc_tuple_3 (_,_,_)
             ->
             [A.Class_field_kind_private_flag_string_loc_tuple_3
                (state_0, state_1, state_2)]
         | _ -> [A.Trash])
    and class_field_list_nil =
      basic_state
        (function
         | Element.Class_field_list [] -> [A.Class_field_list A.Nil]
         | _ -> [A.Trash])
    and class_field_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_field_list (_::_) ->
             [A.Class_field_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and class_type_class_infos (pci_virt : A.state) (pci_params : A.state)
      (pci_name : A.state) (pci_expr : A.state) (pci_loc : A.state)
      (pci_attributes : A.state) =
      basic_state
        (function
         | Element.Class_type_class_infos
             { pci_virt = _; pci_params = _; pci_name = _; pci_expr = _;
               pci_loc = _; pci_attributes = _ }
             ->
             [A.Class_type_class_infos
                {
                  A.pci_virt = pci_virt;
                  A.pci_params = pci_params;
                  A.pci_name = pci_name;
                  A.pci_expr = pci_expr;
                  A.pci_loc = pci_loc;
                  A.pci_attributes = pci_attributes
                }]
         | _ -> [A.Trash])
    and class_type_declaration_list_nil =
      basic_state
        (function
         | Element.Class_type_declaration_list [] ->
             [A.Class_type_declaration_list A.Nil]
         | _ -> [A.Trash])
    and class_type_declaration_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_type_declaration_list (_::_) ->
             [A.Class_type_declaration_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and class_type_field_list_nil =
      basic_state
        (function
         | Element.Class_type_field_list [] ->
             [A.Class_type_field_list A.Nil]
         | _ -> [A.Trash])
    and class_type_field_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Class_type_field_list (_::_) ->
             [A.Class_type_field_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and constructor_declaration_list_nil =
      basic_state
        (function
         | Element.Constructor_declaration_list [] ->
             [A.Constructor_declaration_list A.Nil]
         | _ -> [A.Trash])
    and constructor_declaration_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Constructor_declaration_list (_::_) ->
             [A.Constructor_declaration_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_Longident__t_loc_tuple_2 (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_Longident__t_loc_tuple_2 (_,_) ->
             [A.Core_type_Longident__t_loc_tuple_2 (state_0, state_1)]
         | _ -> [A.Trash])
    and core_type_Longident__t_loc_tuple_2_list_nil =
      basic_state
        (function
         | Element.Core_type_Longident__t_loc_tuple_2_list [] ->
             [A.Core_type_Longident__t_loc_tuple_2_list A.Nil]
         | _ -> [A.Trash])
    and core_type_Longident__t_loc_tuple_2_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_Longident__t_loc_tuple_2_list (_::_) ->
             [A.Core_type_Longident__t_loc_tuple_2_list
                (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2
      (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2
             (_,_) ->
             [A.Core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2
                (state_0, state_1)]
         | _ -> [A.Trash])
    and core_type_attributes_string_tuple_3 (state_0 : A.state)
      (state_1 : A.state) (state_2 : A.state) =
      basic_state
        (function
         | Element.Core_type_attributes_string_tuple_3 (_,_,_) ->
             [A.Core_type_attributes_string_tuple_3
                (state_0, state_1, state_2)]
         | _ -> [A.Trash])
    and core_type_attributes_string_tuple_3_list_nil =
      basic_state
        (function
         | Element.Core_type_attributes_string_tuple_3_list [] ->
             [A.Core_type_attributes_string_tuple_3_list A.Nil]
         | _ -> [A.Trash])
    and core_type_attributes_string_tuple_3_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_attributes_string_tuple_3_list (_::_) ->
             [A.Core_type_attributes_string_tuple_3_list
                (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_core_type_tuple_2 (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_core_type_tuple_2 (_,_) ->
             [A.Core_type_core_type_tuple_2 (state_0, state_1)]
         | _ -> [A.Trash])
    and core_type_list_nil =
      basic_state
        (function
         | Element.Core_type_list [] -> [A.Core_type_list A.Nil]
         | _ -> [A.Trash])
    and core_type_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Core_type_list (_::_) ->
             [A.Core_type_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and core_type_option_none =
      basic_state
        (function
         | Element.Core_type_option (None ) -> [A.Core_type_option A.None]
         | _ -> [A.Trash])
    and core_type_option_some (state_0 : A.state) =
      basic_state
        (function
         | Element.Core_type_option (Some _) ->
             [A.Core_type_option (A.Some state_0)]
         | _ -> [A.Trash])
    and core_type_virtual_flag_mutable_flag_string_tuple_4
      (state_0 : A.state) (state_1 : A.state) (state_2 : A.state)
      (state_3 : A.state) =
      basic_state
        (function
         | Element.Core_type_virtual_flag_mutable_flag_string_tuple_4
             (_,_,_,_) ->
             [A.Core_type_virtual_flag_mutable_flag_string_tuple_4
                (state_0, state_1, state_2, state_3)]
         | _ -> [A.Trash])
    and core_type_virtual_flag_private_flag_string_tuple_4
      (state_0 : A.state) (state_1 : A.state) (state_2 : A.state)
      (state_3 : A.state) =
      basic_state
        (function
         | Element.Core_type_virtual_flag_private_flag_string_tuple_4
             (_,_,_,_) ->
             [A.Core_type_virtual_flag_private_flag_string_tuple_4
                (state_0, state_1, state_2, state_3)]
         | _ -> [A.Trash])
    and expression_Longident__t_loc_tuple_2 (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_Longident__t_loc_tuple_2 (_,_) ->
             [A.Expression_Longident__t_loc_tuple_2 (state_0, state_1)]
         | _ -> [A.Trash])
    and expression_Longident__t_loc_tuple_2_list_nil =
      basic_state
        (function
         | Element.Expression_Longident__t_loc_tuple_2_list [] ->
             [A.Expression_Longident__t_loc_tuple_2_list A.Nil]
         | _ -> [A.Trash])
    and expression_Longident__t_loc_tuple_2_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_Longident__t_loc_tuple_2_list (_::_) ->
             [A.Expression_Longident__t_loc_tuple_2_list
                (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_label_tuple_2 (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_label_tuple_2 (_,_) ->
             [A.Expression_label_tuple_2 (state_0, state_1)]
         | _ -> [A.Trash])
    and expression_label_tuple_2_list_nil =
      basic_state
        (function
         | Element.Expression_label_tuple_2_list [] ->
             [A.Expression_label_tuple_2_list A.Nil]
         | _ -> [A.Trash])
    and expression_label_tuple_2_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_label_tuple_2_list (_::_) ->
             [A.Expression_label_tuple_2_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_list_nil =
      basic_state
        (function
         | Element.Expression_list [] -> [A.Expression_list A.Nil]
         | _ -> [A.Trash])
    and expression_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_list (_::_) ->
             [A.Expression_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and expression_option_none =
      basic_state
        (function
         | Element.Expression_option (None ) -> [A.Expression_option A.None]
         | _ -> [A.Trash])
    and expression_option_some (state_0 : A.state) =
      basic_state
        (function
         | Element.Expression_option (Some _) ->
             [A.Expression_option (A.Some state_0)]
         | _ -> [A.Trash])
    and expression_string_loc_tuple_2 (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.Expression_string_loc_tuple_2 (_,_) ->
             [A.Expression_string_loc_tuple_2 (state_0, state_1)]
         | _ -> [A.Trash])
    and expression_string_loc_tuple_2_list_nil =
      basic_state
        (function
         | Element.Expression_string_loc_tuple_2_list [] ->
             [A.Expression_string_loc_tuple_2_list A.Nil]
         | _ -> [A.Trash])
    and expression_string_loc_tuple_2_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Expression_string_loc_tuple_2_list (_::_) ->
             [A.Expression_string_loc_tuple_2_list
                (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and extension_constructor_list_nil =
      basic_state
        (function
         | Element.Extension_constructor_list [] ->
             [A.Extension_constructor_list A.Nil]
         | _ -> [A.Trash])
    and extension_constructor_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Extension_constructor_list (_::_) ->
             [A.Extension_constructor_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and label_declaration_list_nil =
      basic_state
        (function
         | Element.Label_declaration_list [] ->
             [A.Label_declaration_list A.Nil]
         | _ -> [A.Trash])
    and label_declaration_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Label_declaration_list (_::_) ->
             [A.Label_declaration_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and label_list_nil =
      basic_state
        (function
         | Element.Label_list [] -> [A.Label_list A.Nil]
         | _ -> [A.Trash])
    and label_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Label_list (_::_) ->
             [A.Label_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and label_list_option_none =
      basic_state
        (function
         | Element.Label_list_option (None ) -> [A.Label_list_option A.None]
         | _ -> [A.Trash])
    and label_list_option_some (state_0 : A.state) =
      basic_state
        (function
         | Element.Label_list_option (Some _) ->
             [A.Label_list_option (A.Some state_0)]
         | _ -> [A.Trash])
    and location__t_core_type_core_type_tuple_3 (state_0 : A.state)
      (state_1 : A.state) (state_2 : A.state) =
      basic_state
        (function
         | Element.Location__t_core_type_core_type_tuple_3 (_,_,_) ->
             [A.Location__t_core_type_core_type_tuple_3
                (state_0, state_1, state_2)]
         | _ -> [A.Trash])
    and location__t_core_type_core_type_tuple_3_list_nil =
      basic_state
        (function
         | Element.Location__t_core_type_core_type_tuple_3_list [] ->
             [A.Location__t_core_type_core_type_tuple_3_list A.Nil]
         | _ -> [A.Trash])
    and location__t_core_type_core_type_tuple_3_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Location__t_core_type_core_type_tuple_3_list (_::_) ->
             [A.Location__t_core_type_core_type_tuple_3_list
                (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and longident__t_loc (txt : A.state) (loc : A.state) =
      basic_state
        (function
         | Element.Longident__t_loc { txt = _; loc = _ } ->
             [A.Longident__t_loc { A.txt = txt; A.loc = loc }]
         | _ -> [A.Trash])
    and module_binding_list_nil =
      basic_state
        (function
         | Element.Module_binding_list [] -> [A.Module_binding_list A.Nil]
         | _ -> [A.Trash])
    and module_binding_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Module_binding_list (_::_) ->
             [A.Module_binding_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and module_declaration_list_nil =
      basic_state
        (function
         | Element.Module_declaration_list [] ->
             [A.Module_declaration_list A.Nil]
         | _ -> [A.Trash])
    and module_declaration_list_cons (state_0 : A.state) (state_1 : A.state)
      =
      basic_state
        (function
         | Element.Module_declaration_list (_::_) ->
             [A.Module_declaration_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and module_expr_include_infos (pincl_mod : A.state) (pincl_loc : A.state)
      (pincl_attributes : A.state) =
      basic_state
        (function
         | Element.Module_expr_include_infos
             { pincl_mod = _; pincl_loc = _; pincl_attributes = _ } ->
             [A.Module_expr_include_infos
                {
                  A.pincl_mod = pincl_mod;
                  A.pincl_loc = pincl_loc;
                  A.pincl_attributes = pincl_attributes
                }]
         | _ -> [A.Trash])
    and module_type_include_infos (pincl_mod : A.state) (pincl_loc : A.state)
      (pincl_attributes : A.state) =
      basic_state
        (function
         | Element.Module_type_include_infos
             { pincl_mod = _; pincl_loc = _; pincl_attributes = _ } ->
             [A.Module_type_include_infos
                {
                  A.pincl_mod = pincl_mod;
                  A.pincl_loc = pincl_loc;
                  A.pincl_attributes = pincl_attributes
                }]
         | _ -> [A.Trash])
    and module_type_option_none =
      basic_state
        (function
         | Element.Module_type_option (None ) ->
             [A.Module_type_option A.None]
         | _ -> [A.Trash])
    and module_type_option_some (state_0 : A.state) =
      basic_state
        (function
         | Element.Module_type_option (Some _) ->
             [A.Module_type_option (A.Some state_0)]
         | _ -> [A.Trash])
    and pattern_Longident__t_loc_tuple_2 (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_Longident__t_loc_tuple_2 (_,_) ->
             [A.Pattern_Longident__t_loc_tuple_2 (state_0, state_1)]
         | _ -> [A.Trash])
    and pattern_Longident__t_loc_tuple_2_list_nil =
      basic_state
        (function
         | Element.Pattern_Longident__t_loc_tuple_2_list [] ->
             [A.Pattern_Longident__t_loc_tuple_2_list A.Nil]
         | _ -> [A.Trash])
    and pattern_Longident__t_loc_tuple_2_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_Longident__t_loc_tuple_2_list (_::_) ->
             [A.Pattern_Longident__t_loc_tuple_2_list
                (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_list_nil =
      basic_state
        (function
         | Element.Pattern_list [] -> [A.Pattern_list A.Nil]
         | _ -> [A.Trash])
    and pattern_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Pattern_list (_::_) ->
             [A.Pattern_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and pattern_option_none =
      basic_state
        (function
         | Element.Pattern_option (None ) -> [A.Pattern_option A.None]
         | _ -> [A.Trash])
    and pattern_option_some (state_0 : A.state) =
      basic_state
        (function
         | Element.Pattern_option (Some _) ->
             [A.Pattern_option (A.Some state_0)]
         | _ -> [A.Trash])
    and payload_string_loc_tuple_2 (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Payload_string_loc_tuple_2 (_,_) ->
             [A.Payload_string_loc_tuple_2 (state_0, state_1)]
         | _ -> [A.Trash])
    and row_field_list_nil =
      basic_state
        (function
         | Element.Row_field_list [] -> [A.Row_field_list A.Nil]
         | _ -> [A.Trash])
    and row_field_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Row_field_list (_::_) ->
             [A.Row_field_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and signature_item_list_nil =
      basic_state
        (function
         | Element.Signature_item_list [] -> [A.Signature_item_list A.Nil]
         | _ -> [A.Trash])
    and signature_item_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Signature_item_list (_::_) ->
             [A.Signature_item_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and string_list_nil =
      basic_state
        (function
         | Element.String_list [] -> [A.String_list A.Nil]
         | _ -> [A.Trash])
    and string_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.String_list (_::_) ->
             [A.String_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and string_loc (txt : A.state) (loc : A.state) =
      basic_state
        (function
         | Element.String_loc { txt = _; loc = _ } ->
             [A.String_loc { A.txt = txt; A.loc = loc }]
         | _ -> [A.Trash])
    and string_option_none =
      basic_state
        (function
         | Element.String_option (None ) -> [A.String_option A.None]
         | _ -> [A.Trash])
    and string_option_some (state_0 : A.state) =
      basic_state
        (function
         | Element.String_option (Some _) ->
             [A.String_option (A.Some state_0)]
         | _ -> [A.Trash])
    and structure_item_list_nil =
      basic_state
        (function
         | Element.Structure_item_list [] -> [A.Structure_item_list A.Nil]
         | _ -> [A.Trash])
    and structure_item_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Structure_item_list (_::_) ->
             [A.Structure_item_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and type_declaration_list_nil =
      basic_state
        (function
         | Element.Type_declaration_list [] ->
             [A.Type_declaration_list A.Nil]
         | _ -> [A.Trash])
    and type_declaration_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Type_declaration_list (_::_) ->
             [A.Type_declaration_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and value_binding_list_nil =
      basic_state
        (function
         | Element.Value_binding_list [] -> [A.Value_binding_list A.Nil]
         | _ -> [A.Trash])
    and value_binding_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Value_binding_list (_::_) ->
             [A.Value_binding_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and variance_core_type_tuple_2 (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.Variance_core_type_tuple_2 (_,_) ->
             [A.Variance_core_type_tuple_2 (state_0, state_1)]
         | _ -> [A.Trash])
    and variance_core_type_tuple_2_list_nil =
      basic_state
        (function
         | Element.Variance_core_type_tuple_2_list [] ->
             [A.Variance_core_type_tuple_2_list A.Nil]
         | _ -> [A.Trash])
    and variance_core_type_tuple_2_list_cons (state_0 : A.state)
      (state_1 : A.state) =
      basic_state
        (function
         | Element.Variance_core_type_tuple_2_list (_::_) ->
             [A.Variance_core_type_tuple_2_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
    and with_constraint_list_nil =
      basic_state
        (function
         | Element.With_constraint_list [] -> [A.With_constraint_list A.Nil]
         | _ -> [A.Trash])
    and with_constraint_list_cons (state_0 : A.state) (state_1 : A.state) =
      basic_state
        (function
         | Element.With_constraint_list (_::_) ->
             [A.With_constraint_list (A.Cons (state_0, state_1))]
         | _ -> [A.Trash])
  end
let final () =
  let open A in
    {
      final = true;
      transitions = [(false, ((fun env  -> fun _  -> [(Final, env)])))]
    }
let trash () =
  let open A in
    {
      final = false;
      transitions = [(false, ((fun env  -> fun _  -> [(Trash, env)])))]
    }
module Match :
  sig
    include module type of Match_
    val wildcard : unit -> A.state
    val metavar_expr : string -> A.state
    val metavar_pat : string -> A.state
  end =
  struct
    include Match_
    module E = Element
    let location__t _ _ _ =
      basic_state @@
        (function | E.Location__t _ -> [A.Final] | _ -> [A.Trash])
    let metavar_pat name =
      let open A in
        {
          final = false;
          transitions =
            [(false,
               ((fun meta  ->
                   fun ast_elt  ->
                     match ast_elt with
                     | E.Pattern pat ->
                         [(Final,
                            {
                              meta with
                              Match.substitutions =
                                (Substitution.add_pattern name pat
                                   meta.Match.substitutions)
                            })]
                     | _ -> [])))]
        }
    let metavar_expr name =
      let open A in
        {
          final = false;
          transitions =
            [(false,
               ((fun meta  ->
                   fun ast_elt  ->
                     match ast_elt with
                     | E.Expression expr ->
                         [(Final,
                            {
                              meta with
                              Match.substitutions =
                                (Substitution.add_expr name expr
                                   meta.Match.substitutions)
                            })]
                     | _ -> [])))]
        }
    let wildcard () =
      let state = let open A in { final = false; transitions = [] } in
      state.A.transitions <-
        [(false,
           (ignore_meta @@
              ((function
                | Element.Attribute (_,_) ->
                    [A.Attribute ((final ()), state);
                    A.Attribute (state, (final ()))]
                | Element.Extension (_,_) ->
                    [A.Extension ((final ()), state);
                    A.Extension (state, (final ()))]
                | Element.Attributes [] -> [A.Attributes A.Nil]
                | Element.Attributes (_::_) ->
                    [A.Attributes (A.Cons ((final ()), state));
                    A.Attributes (A.Cons (state, (final ())))]
                | Element.Payload (PStr _) -> [A.Payload (A.PStr state)]
                | Element.Payload (PTyp _) -> [A.Payload (A.PTyp state)]
                | Element.Payload (PPat (_,_)) ->
                    [A.Payload (A.PPat ((final ()), state));
                    A.Payload (A.PPat (state, (final ())))]
                | Element.Core_type
                    { ptyp_desc = _; ptyp_loc = _; ptyp_attributes = _ } ->
                    [A.Core_type
                       {
                         A.ptyp_desc = (final ());
                         A.ptyp_loc = (final ());
                         A.ptyp_attributes = state
                       };
                    A.Core_type
                      {
                        A.ptyp_desc = (final ());
                        A.ptyp_loc = state;
                        A.ptyp_attributes = (final ())
                      };
                    A.Core_type
                      {
                        A.ptyp_desc = state;
                        A.ptyp_loc = (final ());
                        A.ptyp_attributes = (final ())
                      }]
                | Element.Core_type_desc (Ptyp_any ) ->
                    [A.Core_type_desc A.Ptyp_any]
                | Element.Core_type_desc (Ptyp_var _) ->
                    [A.Core_type_desc (A.Ptyp_var state)]
                | Element.Core_type_desc (Ptyp_arrow (_,_,_)) ->
                    [A.Core_type_desc
                       (A.Ptyp_arrow ((final ()), (final ()), state));
                    A.Core_type_desc
                      (A.Ptyp_arrow ((final ()), state, (final ())));
                    A.Core_type_desc
                      (A.Ptyp_arrow (state, (final ()), (final ())))]
                | Element.Core_type_desc (Ptyp_tuple _) ->
                    [A.Core_type_desc (A.Ptyp_tuple state)]
                | Element.Core_type_desc (Ptyp_constr (_,_)) ->
                    [A.Core_type_desc (A.Ptyp_constr ((final ()), state));
                    A.Core_type_desc (A.Ptyp_constr (state, (final ())))]
                | Element.Core_type_desc (Ptyp_object (_,_)) ->
                    [A.Core_type_desc (A.Ptyp_object ((final ()), state));
                    A.Core_type_desc (A.Ptyp_object (state, (final ())))]
                | Element.Core_type_desc (Ptyp_class (_,_)) ->
                    [A.Core_type_desc (A.Ptyp_class ((final ()), state));
                    A.Core_type_desc (A.Ptyp_class (state, (final ())))]
                | Element.Core_type_desc (Ptyp_alias (_,_)) ->
                    [A.Core_type_desc (A.Ptyp_alias ((final ()), state));
                    A.Core_type_desc (A.Ptyp_alias (state, (final ())))]
                | Element.Core_type_desc (Ptyp_variant (_,_,_)) ->
                    [A.Core_type_desc
                       (A.Ptyp_variant ((final ()), (final ()), state));
                    A.Core_type_desc
                      (A.Ptyp_variant ((final ()), state, (final ())));
                    A.Core_type_desc
                      (A.Ptyp_variant (state, (final ()), (final ())))]
                | Element.Core_type_desc (Ptyp_poly (_,_)) ->
                    [A.Core_type_desc (A.Ptyp_poly ((final ()), state));
                    A.Core_type_desc (A.Ptyp_poly (state, (final ())))]
                | Element.Core_type_desc (Ptyp_package _) ->
                    [A.Core_type_desc (A.Ptyp_package state)]
                | Element.Core_type_desc (Ptyp_extension _) ->
                    [A.Core_type_desc (A.Ptyp_extension state)]
                | Element.Package_type (_,_) ->
                    [A.Package_type ((final ()), state);
                    A.Package_type (state, (final ()))]
                | Element.Row_field (Rtag (_,_,_,_)) ->
                    [A.Row_field
                       (A.Rtag ((final ()), (final ()), (final ()), state));
                    A.Row_field
                      (A.Rtag ((final ()), (final ()), state, (final ())));
                    A.Row_field
                      (A.Rtag ((final ()), state, (final ()), (final ())));
                    A.Row_field
                      (A.Rtag (state, (final ()), (final ()), (final ())))]
                | Element.Row_field (Rinherit _) ->
                    [A.Row_field (A.Rinherit state)]
                | Element.Pattern
                    { ppat_desc = _; ppat_loc = _; ppat_attributes = _ } ->
                    [A.Pattern
                       {
                         A.ppat_desc = (final ());
                         A.ppat_loc = (final ());
                         A.ppat_attributes = state
                       };
                    A.Pattern
                      {
                        A.ppat_desc = (final ());
                        A.ppat_loc = state;
                        A.ppat_attributes = (final ())
                      };
                    A.Pattern
                      {
                        A.ppat_desc = state;
                        A.ppat_loc = (final ());
                        A.ppat_attributes = (final ())
                      }]
                | Element.Pattern_desc (Ppat_any ) ->
                    [A.Pattern_desc A.Ppat_any]
                | Element.Pattern_desc (Ppat_var _) ->
                    [A.Pattern_desc (A.Ppat_var state)]
                | Element.Pattern_desc (Ppat_alias (_,_)) ->
                    [A.Pattern_desc (A.Ppat_alias ((final ()), state));
                    A.Pattern_desc (A.Ppat_alias (state, (final ())))]
                | Element.Pattern_desc (Ppat_constant _) ->
                    [A.Pattern_desc (A.Ppat_constant state)]
                | Element.Pattern_desc (Ppat_interval (_,_)) ->
                    [A.Pattern_desc (A.Ppat_interval ((final ()), state));
                    A.Pattern_desc (A.Ppat_interval (state, (final ())))]
                | Element.Pattern_desc (Ppat_tuple _) ->
                    [A.Pattern_desc (A.Ppat_tuple state)]
                | Element.Pattern_desc (Ppat_construct (_,_)) ->
                    [A.Pattern_desc (A.Ppat_construct ((final ()), state));
                    A.Pattern_desc (A.Ppat_construct (state, (final ())))]
                | Element.Pattern_desc (Ppat_variant (_,_)) ->
                    [A.Pattern_desc (A.Ppat_variant ((final ()), state));
                    A.Pattern_desc (A.Ppat_variant (state, (final ())))]
                | Element.Pattern_desc (Ppat_record (_,_)) ->
                    [A.Pattern_desc (A.Ppat_record ((final ()), state));
                    A.Pattern_desc (A.Ppat_record (state, (final ())))]
                | Element.Pattern_desc (Ppat_array _) ->
                    [A.Pattern_desc (A.Ppat_array state)]
                | Element.Pattern_desc (Ppat_or (_,_)) ->
                    [A.Pattern_desc (A.Ppat_or ((final ()), state));
                    A.Pattern_desc (A.Ppat_or (state, (final ())))]
                | Element.Pattern_desc (Ppat_constraint (_,_)) ->
                    [A.Pattern_desc (A.Ppat_constraint ((final ()), state));
                    A.Pattern_desc (A.Ppat_constraint (state, (final ())))]
                | Element.Pattern_desc (Ppat_type _) ->
                    [A.Pattern_desc (A.Ppat_type state)]
                | Element.Pattern_desc (Ppat_lazy _) ->
                    [A.Pattern_desc (A.Ppat_lazy state)]
                | Element.Pattern_desc (Ppat_unpack _) ->
                    [A.Pattern_desc (A.Ppat_unpack state)]
                | Element.Pattern_desc (Ppat_exception _) ->
                    [A.Pattern_desc (A.Ppat_exception state)]
                | Element.Pattern_desc (Ppat_extension _) ->
                    [A.Pattern_desc (A.Ppat_extension state)]
                | Element.Expression
                    { pexp_desc = _; pexp_loc = _; pexp_attributes = _ } ->
                    [A.Expression
                       {
                         A.pexp_desc = (final ());
                         A.pexp_loc = (final ());
                         A.pexp_attributes = state
                       };
                    A.Expression
                      {
                        A.pexp_desc = (final ());
                        A.pexp_loc = state;
                        A.pexp_attributes = (final ())
                      };
                    A.Expression
                      {
                        A.pexp_desc = state;
                        A.pexp_loc = (final ());
                        A.pexp_attributes = (final ())
                      }]
                | Element.Expression_desc (Pexp_ident _) ->
                    [A.Expression_desc (A.Pexp_ident state)]
                | Element.Expression_desc (Pexp_constant _) ->
                    [A.Expression_desc (A.Pexp_constant state)]
                | Element.Expression_desc (Pexp_let (_,_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_let ((final ()), (final ()), state));
                    A.Expression_desc
                      (A.Pexp_let ((final ()), state, (final ())));
                    A.Expression_desc
                      (A.Pexp_let (state, (final ()), (final ())))]
                | Element.Expression_desc (Pexp_function _) ->
                    [A.Expression_desc (A.Pexp_function state)]
                | Element.Expression_desc (Pexp_fun (_,_,_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_fun
                          ((final ()), (final ()), (final ()), state));
                    A.Expression_desc
                      (A.Pexp_fun ((final ()), (final ()), state, (final ())));
                    A.Expression_desc
                      (A.Pexp_fun ((final ()), state, (final ()), (final ())));
                    A.Expression_desc
                      (A.Pexp_fun (state, (final ()), (final ()), (final ())))]
                | Element.Expression_desc (Pexp_apply (_,_)) ->
                    [A.Expression_desc (A.Pexp_apply ((final ()), state));
                    A.Expression_desc (A.Pexp_apply (state, (final ())))]
                | Element.Expression_desc (Pexp_match (_,_)) ->
                    [A.Expression_desc (A.Pexp_match ((final ()), state));
                    A.Expression_desc (A.Pexp_match (state, (final ())))]
                | Element.Expression_desc (Pexp_try (_,_)) ->
                    [A.Expression_desc (A.Pexp_try ((final ()), state));
                    A.Expression_desc (A.Pexp_try (state, (final ())))]
                | Element.Expression_desc (Pexp_tuple _) ->
                    [A.Expression_desc (A.Pexp_tuple state)]
                | Element.Expression_desc (Pexp_construct (_,_)) ->
                    [A.Expression_desc (A.Pexp_construct ((final ()), state));
                    A.Expression_desc (A.Pexp_construct (state, (final ())))]
                | Element.Expression_desc (Pexp_variant (_,_)) ->
                    [A.Expression_desc (A.Pexp_variant ((final ()), state));
                    A.Expression_desc (A.Pexp_variant (state, (final ())))]
                | Element.Expression_desc (Pexp_record (_,_)) ->
                    [A.Expression_desc (A.Pexp_record ((final ()), state));
                    A.Expression_desc (A.Pexp_record (state, (final ())))]
                | Element.Expression_desc (Pexp_field (_,_)) ->
                    [A.Expression_desc (A.Pexp_field ((final ()), state));
                    A.Expression_desc (A.Pexp_field (state, (final ())))]
                | Element.Expression_desc (Pexp_setfield (_,_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_setfield ((final ()), (final ()), state));
                    A.Expression_desc
                      (A.Pexp_setfield ((final ()), state, (final ())));
                    A.Expression_desc
                      (A.Pexp_setfield (state, (final ()), (final ())))]
                | Element.Expression_desc (Pexp_array _) ->
                    [A.Expression_desc (A.Pexp_array state)]
                | Element.Expression_desc (Pexp_ifthenelse (_,_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_ifthenelse ((final ()), (final ()), state));
                    A.Expression_desc
                      (A.Pexp_ifthenelse ((final ()), state, (final ())));
                    A.Expression_desc
                      (A.Pexp_ifthenelse (state, (final ()), (final ())))]
                | Element.Expression_desc (Pexp_sequence (_,_)) ->
                    [A.Expression_desc (A.Pexp_sequence ((final ()), state));
                    A.Expression_desc (A.Pexp_sequence (state, (final ())))]
                | Element.Expression_desc (Pexp_while (_,_)) ->
                    [A.Expression_desc (A.Pexp_while ((final ()), state));
                    A.Expression_desc (A.Pexp_while (state, (final ())))]
                | Element.Expression_desc (Pexp_for (_,_,_,_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_for
                          ((final ()), (final ()), (final ()), (final ()),
                            state));
                    A.Expression_desc
                      (A.Pexp_for
                         ((final ()), (final ()), (final ()), state,
                           (final ())));
                    A.Expression_desc
                      (A.Pexp_for
                         ((final ()), (final ()), state, (final ()),
                           (final ())));
                    A.Expression_desc
                      (A.Pexp_for
                         ((final ()), state, (final ()), (final ()),
                           (final ())));
                    A.Expression_desc
                      (A.Pexp_for
                         (state, (final ()), (final ()), (final ()),
                           (final ())))]
                | Element.Expression_desc (Pexp_constraint (_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_constraint ((final ()), state));
                    A.Expression_desc (A.Pexp_constraint (state, (final ())))]
                | Element.Expression_desc (Pexp_coerce (_,_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_coerce ((final ()), (final ()), state));
                    A.Expression_desc
                      (A.Pexp_coerce ((final ()), state, (final ())));
                    A.Expression_desc
                      (A.Pexp_coerce (state, (final ()), (final ())))]
                | Element.Expression_desc (Pexp_send (_,_)) ->
                    [A.Expression_desc (A.Pexp_send ((final ()), state));
                    A.Expression_desc (A.Pexp_send (state, (final ())))]
                | Element.Expression_desc (Pexp_new _) ->
                    [A.Expression_desc (A.Pexp_new state)]
                | Element.Expression_desc (Pexp_setinstvar (_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_setinstvar ((final ()), state));
                    A.Expression_desc (A.Pexp_setinstvar (state, (final ())))]
                | Element.Expression_desc (Pexp_override _) ->
                    [A.Expression_desc (A.Pexp_override state)]
                | Element.Expression_desc (Pexp_letmodule (_,_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_letmodule ((final ()), (final ()), state));
                    A.Expression_desc
                      (A.Pexp_letmodule ((final ()), state, (final ())));
                    A.Expression_desc
                      (A.Pexp_letmodule (state, (final ()), (final ())))]
                | Element.Expression_desc (Pexp_assert _) ->
                    [A.Expression_desc (A.Pexp_assert state)]
                | Element.Expression_desc (Pexp_lazy _) ->
                    [A.Expression_desc (A.Pexp_lazy state)]
                | Element.Expression_desc (Pexp_poly (_,_)) ->
                    [A.Expression_desc (A.Pexp_poly ((final ()), state));
                    A.Expression_desc (A.Pexp_poly (state, (final ())))]
                | Element.Expression_desc (Pexp_object _) ->
                    [A.Expression_desc (A.Pexp_object state)]
                | Element.Expression_desc (Pexp_newtype (_,_)) ->
                    [A.Expression_desc (A.Pexp_newtype ((final ()), state));
                    A.Expression_desc (A.Pexp_newtype (state, (final ())))]
                | Element.Expression_desc (Pexp_pack _) ->
                    [A.Expression_desc (A.Pexp_pack state)]
                | Element.Expression_desc (Pexp_open (_,_,_)) ->
                    [A.Expression_desc
                       (A.Pexp_open ((final ()), (final ()), state));
                    A.Expression_desc
                      (A.Pexp_open ((final ()), state, (final ())));
                    A.Expression_desc
                      (A.Pexp_open (state, (final ()), (final ())))]
                | Element.Expression_desc (Pexp_extension _) ->
                    [A.Expression_desc (A.Pexp_extension state)]
                | Element.Case { pc_lhs = _; pc_guard = _; pc_rhs = _ } ->
                    [A.Case
                       {
                         A.pc_lhs = (final ());
                         A.pc_guard = (final ());
                         A.pc_rhs = state
                       };
                    A.Case
                      {
                        A.pc_lhs = (final ());
                        A.pc_guard = state;
                        A.pc_rhs = (final ())
                      };
                    A.Case
                      {
                        A.pc_lhs = state;
                        A.pc_guard = (final ());
                        A.pc_rhs = (final ())
                      }]
                | Element.Value_description
                    { pval_name = _; pval_type = _; pval_prim = _;
                      pval_attributes = _; pval_loc = _ }
                    ->
                    [A.Value_description
                       {
                         A.pval_name = (final ());
                         A.pval_type = (final ());
                         A.pval_prim = (final ());
                         A.pval_attributes = (final ());
                         A.pval_loc = state
                       };
                    A.Value_description
                      {
                        A.pval_name = (final ());
                        A.pval_type = (final ());
                        A.pval_prim = (final ());
                        A.pval_attributes = state;
                        A.pval_loc = (final ())
                      };
                    A.Value_description
                      {
                        A.pval_name = (final ());
                        A.pval_type = (final ());
                        A.pval_prim = state;
                        A.pval_attributes = (final ());
                        A.pval_loc = (final ())
                      };
                    A.Value_description
                      {
                        A.pval_name = (final ());
                        A.pval_type = state;
                        A.pval_prim = (final ());
                        A.pval_attributes = (final ());
                        A.pval_loc = (final ())
                      };
                    A.Value_description
                      {
                        A.pval_name = state;
                        A.pval_type = (final ());
                        A.pval_prim = (final ());
                        A.pval_attributes = (final ());
                        A.pval_loc = (final ())
                      }]
                | Element.Type_declaration
                    { ptype_name = _; ptype_params = _; ptype_cstrs = _;
                      ptype_kind = _; ptype_private = _; ptype_manifest = _;
                      ptype_attributes = _; ptype_loc = _ }
                    ->
                    [A.Type_declaration
                       {
                         A.ptype_name = (final ());
                         A.ptype_params = (final ());
                         A.ptype_cstrs = (final ());
                         A.ptype_kind = (final ());
                         A.ptype_private = (final ());
                         A.ptype_manifest = (final ());
                         A.ptype_attributes = (final ());
                         A.ptype_loc = state
                       };
                    A.Type_declaration
                      {
                        A.ptype_name = (final ());
                        A.ptype_params = (final ());
                        A.ptype_cstrs = (final ());
                        A.ptype_kind = (final ());
                        A.ptype_private = (final ());
                        A.ptype_manifest = (final ());
                        A.ptype_attributes = state;
                        A.ptype_loc = (final ())
                      };
                    A.Type_declaration
                      {
                        A.ptype_name = (final ());
                        A.ptype_params = (final ());
                        A.ptype_cstrs = (final ());
                        A.ptype_kind = (final ());
                        A.ptype_private = (final ());
                        A.ptype_manifest = state;
                        A.ptype_attributes = (final ());
                        A.ptype_loc = (final ())
                      };
                    A.Type_declaration
                      {
                        A.ptype_name = (final ());
                        A.ptype_params = (final ());
                        A.ptype_cstrs = (final ());
                        A.ptype_kind = (final ());
                        A.ptype_private = state;
                        A.ptype_manifest = (final ());
                        A.ptype_attributes = (final ());
                        A.ptype_loc = (final ())
                      };
                    A.Type_declaration
                      {
                        A.ptype_name = (final ());
                        A.ptype_params = (final ());
                        A.ptype_cstrs = (final ());
                        A.ptype_kind = state;
                        A.ptype_private = (final ());
                        A.ptype_manifest = (final ());
                        A.ptype_attributes = (final ());
                        A.ptype_loc = (final ())
                      };
                    A.Type_declaration
                      {
                        A.ptype_name = (final ());
                        A.ptype_params = (final ());
                        A.ptype_cstrs = state;
                        A.ptype_kind = (final ());
                        A.ptype_private = (final ());
                        A.ptype_manifest = (final ());
                        A.ptype_attributes = (final ());
                        A.ptype_loc = (final ())
                      };
                    A.Type_declaration
                      {
                        A.ptype_name = (final ());
                        A.ptype_params = state;
                        A.ptype_cstrs = (final ());
                        A.ptype_kind = (final ());
                        A.ptype_private = (final ());
                        A.ptype_manifest = (final ());
                        A.ptype_attributes = (final ());
                        A.ptype_loc = (final ())
                      };
                    A.Type_declaration
                      {
                        A.ptype_name = state;
                        A.ptype_params = (final ());
                        A.ptype_cstrs = (final ());
                        A.ptype_kind = (final ());
                        A.ptype_private = (final ());
                        A.ptype_manifest = (final ());
                        A.ptype_attributes = (final ());
                        A.ptype_loc = (final ())
                      }]
                | Element.Type_kind (Ptype_abstract ) ->
                    [A.Type_kind A.Ptype_abstract]
                | Element.Type_kind (Ptype_variant _) ->
                    [A.Type_kind (A.Ptype_variant state)]
                | Element.Type_kind (Ptype_record _) ->
                    [A.Type_kind (A.Ptype_record state)]
                | Element.Type_kind (Ptype_open ) ->
                    [A.Type_kind A.Ptype_open]
                | Element.Label_declaration
                    { pld_name = _; pld_mutable = _; pld_type = _;
                      pld_loc = _; pld_attributes = _ }
                    ->
                    [A.Label_declaration
                       {
                         A.pld_name = (final ());
                         A.pld_mutable = (final ());
                         A.pld_type = (final ());
                         A.pld_loc = (final ());
                         A.pld_attributes = state
                       };
                    A.Label_declaration
                      {
                        A.pld_name = (final ());
                        A.pld_mutable = (final ());
                        A.pld_type = (final ());
                        A.pld_loc = state;
                        A.pld_attributes = (final ())
                      };
                    A.Label_declaration
                      {
                        A.pld_name = (final ());
                        A.pld_mutable = (final ());
                        A.pld_type = state;
                        A.pld_loc = (final ());
                        A.pld_attributes = (final ())
                      };
                    A.Label_declaration
                      {
                        A.pld_name = (final ());
                        A.pld_mutable = state;
                        A.pld_type = (final ());
                        A.pld_loc = (final ());
                        A.pld_attributes = (final ())
                      };
                    A.Label_declaration
                      {
                        A.pld_name = state;
                        A.pld_mutable = (final ());
                        A.pld_type = (final ());
                        A.pld_loc = (final ());
                        A.pld_attributes = (final ())
                      }]
                | Element.Constructor_declaration
                    { pcd_name = _; pcd_args = _; pcd_res = _; pcd_loc = _;
                      pcd_attributes = _ }
                    ->
                    [A.Constructor_declaration
                       {
                         A.pcd_name = (final ());
                         A.pcd_args = (final ());
                         A.pcd_res = (final ());
                         A.pcd_loc = (final ());
                         A.pcd_attributes = state
                       };
                    A.Constructor_declaration
                      {
                        A.pcd_name = (final ());
                        A.pcd_args = (final ());
                        A.pcd_res = (final ());
                        A.pcd_loc = state;
                        A.pcd_attributes = (final ())
                      };
                    A.Constructor_declaration
                      {
                        A.pcd_name = (final ());
                        A.pcd_args = (final ());
                        A.pcd_res = state;
                        A.pcd_loc = (final ());
                        A.pcd_attributes = (final ())
                      };
                    A.Constructor_declaration
                      {
                        A.pcd_name = (final ());
                        A.pcd_args = state;
                        A.pcd_res = (final ());
                        A.pcd_loc = (final ());
                        A.pcd_attributes = (final ())
                      };
                    A.Constructor_declaration
                      {
                        A.pcd_name = state;
                        A.pcd_args = (final ());
                        A.pcd_res = (final ());
                        A.pcd_loc = (final ());
                        A.pcd_attributes = (final ())
                      }]
                | Element.Type_extension
                    { ptyext_path = _; ptyext_params = _;
                      ptyext_constructors = _; ptyext_private = _;
                      ptyext_attributes = _ }
                    ->
                    [A.Type_extension
                       {
                         A.ptyext_path = (final ());
                         A.ptyext_params = (final ());
                         A.ptyext_constructors = (final ());
                         A.ptyext_private = (final ());
                         A.ptyext_attributes = state
                       };
                    A.Type_extension
                      {
                        A.ptyext_path = (final ());
                        A.ptyext_params = (final ());
                        A.ptyext_constructors = (final ());
                        A.ptyext_private = state;
                        A.ptyext_attributes = (final ())
                      };
                    A.Type_extension
                      {
                        A.ptyext_path = (final ());
                        A.ptyext_params = (final ());
                        A.ptyext_constructors = state;
                        A.ptyext_private = (final ());
                        A.ptyext_attributes = (final ())
                      };
                    A.Type_extension
                      {
                        A.ptyext_path = (final ());
                        A.ptyext_params = state;
                        A.ptyext_constructors = (final ());
                        A.ptyext_private = (final ());
                        A.ptyext_attributes = (final ())
                      };
                    A.Type_extension
                      {
                        A.ptyext_path = state;
                        A.ptyext_params = (final ());
                        A.ptyext_constructors = (final ());
                        A.ptyext_private = (final ());
                        A.ptyext_attributes = (final ())
                      }]
                | Element.Extension_constructor
                    { pext_name = _; pext_kind = _; pext_loc = _;
                      pext_attributes = _ }
                    ->
                    [A.Extension_constructor
                       {
                         A.pext_name = (final ());
                         A.pext_kind = (final ());
                         A.pext_loc = (final ());
                         A.pext_attributes = state
                       };
                    A.Extension_constructor
                      {
                        A.pext_name = (final ());
                        A.pext_kind = (final ());
                        A.pext_loc = state;
                        A.pext_attributes = (final ())
                      };
                    A.Extension_constructor
                      {
                        A.pext_name = (final ());
                        A.pext_kind = state;
                        A.pext_loc = (final ());
                        A.pext_attributes = (final ())
                      };
                    A.Extension_constructor
                      {
                        A.pext_name = state;
                        A.pext_kind = (final ());
                        A.pext_loc = (final ());
                        A.pext_attributes = (final ())
                      }]
                | Element.Extension_constructor_kind (Pext_decl (_,_)) ->
                    [A.Extension_constructor_kind
                       (A.Pext_decl ((final ()), state));
                    A.Extension_constructor_kind
                      (A.Pext_decl (state, (final ())))]
                | Element.Extension_constructor_kind (Pext_rebind _) ->
                    [A.Extension_constructor_kind (A.Pext_rebind state)]
                | Element.Class_type
                    { pcty_desc = _; pcty_loc = _; pcty_attributes = _ } ->
                    [A.Class_type
                       {
                         A.pcty_desc = (final ());
                         A.pcty_loc = (final ());
                         A.pcty_attributes = state
                       };
                    A.Class_type
                      {
                        A.pcty_desc = (final ());
                        A.pcty_loc = state;
                        A.pcty_attributes = (final ())
                      };
                    A.Class_type
                      {
                        A.pcty_desc = state;
                        A.pcty_loc = (final ());
                        A.pcty_attributes = (final ())
                      }]
                | Element.Class_type_desc (Pcty_constr (_,_)) ->
                    [A.Class_type_desc (A.Pcty_constr ((final ()), state));
                    A.Class_type_desc (A.Pcty_constr (state, (final ())))]
                | Element.Class_type_desc (Pcty_signature _) ->
                    [A.Class_type_desc (A.Pcty_signature state)]
                | Element.Class_type_desc (Pcty_arrow (_,_,_)) ->
                    [A.Class_type_desc
                       (A.Pcty_arrow ((final ()), (final ()), state));
                    A.Class_type_desc
                      (A.Pcty_arrow ((final ()), state, (final ())));
                    A.Class_type_desc
                      (A.Pcty_arrow (state, (final ()), (final ())))]
                | Element.Class_type_desc (Pcty_extension _) ->
                    [A.Class_type_desc (A.Pcty_extension state)]
                | Element.Class_signature
                    { pcsig_self = _; pcsig_fields = _ } ->
                    [A.Class_signature
                       { A.pcsig_self = (final ()); A.pcsig_fields = state };
                    A.Class_signature
                      { A.pcsig_self = state; A.pcsig_fields = (final ()) }]
                | Element.Class_type_field
                    { pctf_desc = _; pctf_loc = _; pctf_attributes = _ } ->
                    [A.Class_type_field
                       {
                         A.pctf_desc = (final ());
                         A.pctf_loc = (final ());
                         A.pctf_attributes = state
                       };
                    A.Class_type_field
                      {
                        A.pctf_desc = (final ());
                        A.pctf_loc = state;
                        A.pctf_attributes = (final ())
                      };
                    A.Class_type_field
                      {
                        A.pctf_desc = state;
                        A.pctf_loc = (final ());
                        A.pctf_attributes = (final ())
                      }]
                | Element.Class_type_field_desc (Pctf_inherit _) ->
                    [A.Class_type_field_desc (A.Pctf_inherit state)]
                | Element.Class_type_field_desc (Pctf_val _) ->
                    [A.Class_type_field_desc (A.Pctf_val state)]
                | Element.Class_type_field_desc (Pctf_method _) ->
                    [A.Class_type_field_desc (A.Pctf_method state)]
                | Element.Class_type_field_desc (Pctf_constraint _) ->
                    [A.Class_type_field_desc (A.Pctf_constraint state)]
                | Element.Class_type_field_desc (Pctf_attribute _) ->
                    [A.Class_type_field_desc (A.Pctf_attribute state)]
                | Element.Class_type_field_desc (Pctf_extension _) ->
                    [A.Class_type_field_desc (A.Pctf_extension state)]
                | Element.Class_description
                    { pci_virt = _; pci_params = _; pci_name = _;
                      pci_expr = _; pci_loc = _; pci_attributes = _ }
                    ->
                    [A.Class_description
                       {
                         A.pci_virt = (final ());
                         A.pci_params = (final ());
                         A.pci_name = (final ());
                         A.pci_expr = (final ());
                         A.pci_loc = (final ());
                         A.pci_attributes = state
                       };
                    A.Class_description
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = state;
                        A.pci_attributes = (final ())
                      };
                    A.Class_description
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = state;
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_description
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = state;
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_description
                      {
                        A.pci_virt = (final ());
                        A.pci_params = state;
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_description
                      {
                        A.pci_virt = state;
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      }]
                | Element.Class_type_declaration
                    { pci_virt = _; pci_params = _; pci_name = _;
                      pci_expr = _; pci_loc = _; pci_attributes = _ }
                    ->
                    [A.Class_type_declaration
                       {
                         A.pci_virt = (final ());
                         A.pci_params = (final ());
                         A.pci_name = (final ());
                         A.pci_expr = (final ());
                         A.pci_loc = (final ());
                         A.pci_attributes = state
                       };
                    A.Class_type_declaration
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = state;
                        A.pci_attributes = (final ())
                      };
                    A.Class_type_declaration
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = state;
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_type_declaration
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = state;
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_type_declaration
                      {
                        A.pci_virt = (final ());
                        A.pci_params = state;
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_type_declaration
                      {
                        A.pci_virt = state;
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      }]
                | Element.Class_expr
                    { pcl_desc = _; pcl_loc = _; pcl_attributes = _ } ->
                    [A.Class_expr
                       {
                         A.pcl_desc = (final ());
                         A.pcl_loc = (final ());
                         A.pcl_attributes = state
                       };
                    A.Class_expr
                      {
                        A.pcl_desc = (final ());
                        A.pcl_loc = state;
                        A.pcl_attributes = (final ())
                      };
                    A.Class_expr
                      {
                        A.pcl_desc = state;
                        A.pcl_loc = (final ());
                        A.pcl_attributes = (final ())
                      }]
                | Element.Class_expr_desc (Pcl_constr (_,_)) ->
                    [A.Class_expr_desc (A.Pcl_constr ((final ()), state));
                    A.Class_expr_desc (A.Pcl_constr (state, (final ())))]
                | Element.Class_expr_desc (Pcl_structure _) ->
                    [A.Class_expr_desc (A.Pcl_structure state)]
                | Element.Class_expr_desc (Pcl_fun (_,_,_,_)) ->
                    [A.Class_expr_desc
                       (A.Pcl_fun ((final ()), (final ()), (final ()), state));
                    A.Class_expr_desc
                      (A.Pcl_fun ((final ()), (final ()), state, (final ())));
                    A.Class_expr_desc
                      (A.Pcl_fun ((final ()), state, (final ()), (final ())));
                    A.Class_expr_desc
                      (A.Pcl_fun (state, (final ()), (final ()), (final ())))]
                | Element.Class_expr_desc (Pcl_apply (_,_)) ->
                    [A.Class_expr_desc (A.Pcl_apply ((final ()), state));
                    A.Class_expr_desc (A.Pcl_apply (state, (final ())))]
                | Element.Class_expr_desc (Pcl_let (_,_,_)) ->
                    [A.Class_expr_desc
                       (A.Pcl_let ((final ()), (final ()), state));
                    A.Class_expr_desc
                      (A.Pcl_let ((final ()), state, (final ())));
                    A.Class_expr_desc
                      (A.Pcl_let (state, (final ()), (final ())))]
                | Element.Class_expr_desc (Pcl_constraint (_,_)) ->
                    [A.Class_expr_desc (A.Pcl_constraint ((final ()), state));
                    A.Class_expr_desc (A.Pcl_constraint (state, (final ())))]
                | Element.Class_expr_desc (Pcl_extension _) ->
                    [A.Class_expr_desc (A.Pcl_extension state)]
                | Element.Class_structure
                    { pcstr_self = _; pcstr_fields = _ } ->
                    [A.Class_structure
                       { A.pcstr_self = (final ()); A.pcstr_fields = state };
                    A.Class_structure
                      { A.pcstr_self = state; A.pcstr_fields = (final ()) }]
                | Element.Class_field
                    { pcf_desc = _; pcf_loc = _; pcf_attributes = _ } ->
                    [A.Class_field
                       {
                         A.pcf_desc = (final ());
                         A.pcf_loc = (final ());
                         A.pcf_attributes = state
                       };
                    A.Class_field
                      {
                        A.pcf_desc = (final ());
                        A.pcf_loc = state;
                        A.pcf_attributes = (final ())
                      };
                    A.Class_field
                      {
                        A.pcf_desc = state;
                        A.pcf_loc = (final ());
                        A.pcf_attributes = (final ())
                      }]
                | Element.Class_field_desc (Pcf_inherit (_,_,_)) ->
                    [A.Class_field_desc
                       (A.Pcf_inherit ((final ()), (final ()), state));
                    A.Class_field_desc
                      (A.Pcf_inherit ((final ()), state, (final ())));
                    A.Class_field_desc
                      (A.Pcf_inherit (state, (final ()), (final ())))]
                | Element.Class_field_desc (Pcf_val _) ->
                    [A.Class_field_desc (A.Pcf_val state)]
                | Element.Class_field_desc (Pcf_method _) ->
                    [A.Class_field_desc (A.Pcf_method state)]
                | Element.Class_field_desc (Pcf_constraint _) ->
                    [A.Class_field_desc (A.Pcf_constraint state)]
                | Element.Class_field_desc (Pcf_initializer _) ->
                    [A.Class_field_desc (A.Pcf_initializer state)]
                | Element.Class_field_desc (Pcf_attribute _) ->
                    [A.Class_field_desc (A.Pcf_attribute state)]
                | Element.Class_field_desc (Pcf_extension _) ->
                    [A.Class_field_desc (A.Pcf_extension state)]
                | Element.Class_field_kind (Cfk_virtual _) ->
                    [A.Class_field_kind (A.Cfk_virtual state)]
                | Element.Class_field_kind (Cfk_concrete (_,_)) ->
                    [A.Class_field_kind (A.Cfk_concrete ((final ()), state));
                    A.Class_field_kind (A.Cfk_concrete (state, (final ())))]
                | Element.Class_declaration
                    { pci_virt = _; pci_params = _; pci_name = _;
                      pci_expr = _; pci_loc = _; pci_attributes = _ }
                    ->
                    [A.Class_declaration
                       {
                         A.pci_virt = (final ());
                         A.pci_params = (final ());
                         A.pci_name = (final ());
                         A.pci_expr = (final ());
                         A.pci_loc = (final ());
                         A.pci_attributes = state
                       };
                    A.Class_declaration
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = state;
                        A.pci_attributes = (final ())
                      };
                    A.Class_declaration
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = state;
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_declaration
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = state;
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_declaration
                      {
                        A.pci_virt = (final ());
                        A.pci_params = state;
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_declaration
                      {
                        A.pci_virt = state;
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      }]
                | Element.Module_type
                    { pmty_desc = _; pmty_loc = _; pmty_attributes = _ } ->
                    [A.Module_type
                       {
                         A.pmty_desc = (final ());
                         A.pmty_loc = (final ());
                         A.pmty_attributes = state
                       };
                    A.Module_type
                      {
                        A.pmty_desc = (final ());
                        A.pmty_loc = state;
                        A.pmty_attributes = (final ())
                      };
                    A.Module_type
                      {
                        A.pmty_desc = state;
                        A.pmty_loc = (final ());
                        A.pmty_attributes = (final ())
                      }]
                | Element.Module_type_desc (Pmty_ident _) ->
                    [A.Module_type_desc (A.Pmty_ident state)]
                | Element.Module_type_desc (Pmty_signature _) ->
                    [A.Module_type_desc (A.Pmty_signature state)]
                | Element.Module_type_desc (Pmty_functor (_,_,_)) ->
                    [A.Module_type_desc
                       (A.Pmty_functor ((final ()), (final ()), state));
                    A.Module_type_desc
                      (A.Pmty_functor ((final ()), state, (final ())));
                    A.Module_type_desc
                      (A.Pmty_functor (state, (final ()), (final ())))]
                | Element.Module_type_desc (Pmty_with (_,_)) ->
                    [A.Module_type_desc (A.Pmty_with ((final ()), state));
                    A.Module_type_desc (A.Pmty_with (state, (final ())))]
                | Element.Module_type_desc (Pmty_typeof _) ->
                    [A.Module_type_desc (A.Pmty_typeof state)]
                | Element.Module_type_desc (Pmty_extension _) ->
                    [A.Module_type_desc (A.Pmty_extension state)]
                | Element.Module_type_desc (Pmty_alias _) ->
                    [A.Module_type_desc (A.Pmty_alias state)]
                | Element.Signature [] -> [A.Signature A.Nil]
                | Element.Signature (_::_) ->
                    [A.Signature (A.Cons ((final ()), state));
                    A.Signature (A.Cons (state, (final ())))]
                | Element.Signature_item { psig_desc = _; psig_loc = _ } ->
                    [A.Signature_item
                       { A.psig_desc = (final ()); A.psig_loc = state };
                    A.Signature_item
                      { A.psig_desc = state; A.psig_loc = (final ()) }]
                | Element.Signature_item_desc (Psig_value _) ->
                    [A.Signature_item_desc (A.Psig_value state)]
                | Element.Signature_item_desc (Psig_type _) ->
                    [A.Signature_item_desc (A.Psig_type state)]
                | Element.Signature_item_desc (Psig_typext _) ->
                    [A.Signature_item_desc (A.Psig_typext state)]
                | Element.Signature_item_desc (Psig_exception _) ->
                    [A.Signature_item_desc (A.Psig_exception state)]
                | Element.Signature_item_desc (Psig_module _) ->
                    [A.Signature_item_desc (A.Psig_module state)]
                | Element.Signature_item_desc (Psig_recmodule _) ->
                    [A.Signature_item_desc (A.Psig_recmodule state)]
                | Element.Signature_item_desc (Psig_modtype _) ->
                    [A.Signature_item_desc (A.Psig_modtype state)]
                | Element.Signature_item_desc (Psig_open _) ->
                    [A.Signature_item_desc (A.Psig_open state)]
                | Element.Signature_item_desc (Psig_include _) ->
                    [A.Signature_item_desc (A.Psig_include state)]
                | Element.Signature_item_desc (Psig_class _) ->
                    [A.Signature_item_desc (A.Psig_class state)]
                | Element.Signature_item_desc (Psig_class_type _) ->
                    [A.Signature_item_desc (A.Psig_class_type state)]
                | Element.Signature_item_desc (Psig_attribute _) ->
                    [A.Signature_item_desc (A.Psig_attribute state)]
                | Element.Signature_item_desc (Psig_extension (_,_)) ->
                    [A.Signature_item_desc
                       (A.Psig_extension ((final ()), state));
                    A.Signature_item_desc
                      (A.Psig_extension (state, (final ())))]
                | Element.Module_declaration
                    { pmd_name = _; pmd_type = _; pmd_attributes = _;
                      pmd_loc = _ }
                    ->
                    [A.Module_declaration
                       {
                         A.pmd_name = (final ());
                         A.pmd_type = (final ());
                         A.pmd_attributes = (final ());
                         A.pmd_loc = state
                       };
                    A.Module_declaration
                      {
                        A.pmd_name = (final ());
                        A.pmd_type = (final ());
                        A.pmd_attributes = state;
                        A.pmd_loc = (final ())
                      };
                    A.Module_declaration
                      {
                        A.pmd_name = (final ());
                        A.pmd_type = state;
                        A.pmd_attributes = (final ());
                        A.pmd_loc = (final ())
                      };
                    A.Module_declaration
                      {
                        A.pmd_name = state;
                        A.pmd_type = (final ());
                        A.pmd_attributes = (final ());
                        A.pmd_loc = (final ())
                      }]
                | Element.Module_type_declaration
                    { pmtd_name = _; pmtd_type = _; pmtd_attributes = _;
                      pmtd_loc = _ }
                    ->
                    [A.Module_type_declaration
                       {
                         A.pmtd_name = (final ());
                         A.pmtd_type = (final ());
                         A.pmtd_attributes = (final ());
                         A.pmtd_loc = state
                       };
                    A.Module_type_declaration
                      {
                        A.pmtd_name = (final ());
                        A.pmtd_type = (final ());
                        A.pmtd_attributes = state;
                        A.pmtd_loc = (final ())
                      };
                    A.Module_type_declaration
                      {
                        A.pmtd_name = (final ());
                        A.pmtd_type = state;
                        A.pmtd_attributes = (final ());
                        A.pmtd_loc = (final ())
                      };
                    A.Module_type_declaration
                      {
                        A.pmtd_name = state;
                        A.pmtd_type = (final ());
                        A.pmtd_attributes = (final ());
                        A.pmtd_loc = (final ())
                      }]
                | Element.Open_description
                    { popen_lid = _; popen_override = _; popen_loc = _;
                      popen_attributes = _ }
                    ->
                    [A.Open_description
                       {
                         A.popen_lid = (final ());
                         A.popen_override = (final ());
                         A.popen_loc = (final ());
                         A.popen_attributes = state
                       };
                    A.Open_description
                      {
                        A.popen_lid = (final ());
                        A.popen_override = (final ());
                        A.popen_loc = state;
                        A.popen_attributes = (final ())
                      };
                    A.Open_description
                      {
                        A.popen_lid = (final ());
                        A.popen_override = state;
                        A.popen_loc = (final ());
                        A.popen_attributes = (final ())
                      };
                    A.Open_description
                      {
                        A.popen_lid = state;
                        A.popen_override = (final ());
                        A.popen_loc = (final ());
                        A.popen_attributes = (final ())
                      }]
                | Element.Include_description
                    { pincl_mod = _; pincl_loc = _; pincl_attributes = _ } ->
                    [A.Include_description
                       {
                         A.pincl_mod = (final ());
                         A.pincl_loc = (final ());
                         A.pincl_attributes = state
                       };
                    A.Include_description
                      {
                        A.pincl_mod = (final ());
                        A.pincl_loc = state;
                        A.pincl_attributes = (final ())
                      };
                    A.Include_description
                      {
                        A.pincl_mod = state;
                        A.pincl_loc = (final ());
                        A.pincl_attributes = (final ())
                      }]
                | Element.Include_declaration
                    { pincl_mod = _; pincl_loc = _; pincl_attributes = _ } ->
                    [A.Include_declaration
                       {
                         A.pincl_mod = (final ());
                         A.pincl_loc = (final ());
                         A.pincl_attributes = state
                       };
                    A.Include_declaration
                      {
                        A.pincl_mod = (final ());
                        A.pincl_loc = state;
                        A.pincl_attributes = (final ())
                      };
                    A.Include_declaration
                      {
                        A.pincl_mod = state;
                        A.pincl_loc = (final ());
                        A.pincl_attributes = (final ())
                      }]
                | Element.With_constraint (Pwith_type (_,_)) ->
                    [A.With_constraint (A.Pwith_type ((final ()), state));
                    A.With_constraint (A.Pwith_type (state, (final ())))]
                | Element.With_constraint (Pwith_module (_,_)) ->
                    [A.With_constraint (A.Pwith_module ((final ()), state));
                    A.With_constraint (A.Pwith_module (state, (final ())))]
                | Element.With_constraint (Pwith_typesubst _) ->
                    [A.With_constraint (A.Pwith_typesubst state)]
                | Element.With_constraint (Pwith_modsubst (_,_)) ->
                    [A.With_constraint (A.Pwith_modsubst ((final ()), state));
                    A.With_constraint (A.Pwith_modsubst (state, (final ())))]
                | Element.Module_expr
                    { pmod_desc = _; pmod_loc = _; pmod_attributes = _ } ->
                    [A.Module_expr
                       {
                         A.pmod_desc = (final ());
                         A.pmod_loc = (final ());
                         A.pmod_attributes = state
                       };
                    A.Module_expr
                      {
                        A.pmod_desc = (final ());
                        A.pmod_loc = state;
                        A.pmod_attributes = (final ())
                      };
                    A.Module_expr
                      {
                        A.pmod_desc = state;
                        A.pmod_loc = (final ());
                        A.pmod_attributes = (final ())
                      }]
                | Element.Module_expr_desc (Pmod_ident _) ->
                    [A.Module_expr_desc (A.Pmod_ident state)]
                | Element.Module_expr_desc (Pmod_structure _) ->
                    [A.Module_expr_desc (A.Pmod_structure state)]
                | Element.Module_expr_desc (Pmod_functor (_,_,_)) ->
                    [A.Module_expr_desc
                       (A.Pmod_functor ((final ()), (final ()), state));
                    A.Module_expr_desc
                      (A.Pmod_functor ((final ()), state, (final ())));
                    A.Module_expr_desc
                      (A.Pmod_functor (state, (final ()), (final ())))]
                | Element.Module_expr_desc (Pmod_apply (_,_)) ->
                    [A.Module_expr_desc (A.Pmod_apply ((final ()), state));
                    A.Module_expr_desc (A.Pmod_apply (state, (final ())))]
                | Element.Module_expr_desc (Pmod_constraint (_,_)) ->
                    [A.Module_expr_desc
                       (A.Pmod_constraint ((final ()), state));
                    A.Module_expr_desc
                      (A.Pmod_constraint (state, (final ())))]
                | Element.Module_expr_desc (Pmod_unpack _) ->
                    [A.Module_expr_desc (A.Pmod_unpack state)]
                | Element.Module_expr_desc (Pmod_extension _) ->
                    [A.Module_expr_desc (A.Pmod_extension state)]
                | Element.Structure [] -> [A.Structure A.Nil]
                | Element.Structure (_::_) ->
                    [A.Structure (A.Cons ((final ()), state));
                    A.Structure (A.Cons (state, (final ())))]
                | Element.Structure_item { pstr_desc = _; pstr_loc = _ } ->
                    [A.Structure_item
                       { A.pstr_desc = (final ()); A.pstr_loc = state };
                    A.Structure_item
                      { A.pstr_desc = state; A.pstr_loc = (final ()) }]
                | Element.Structure_item_desc (Pstr_eval (_,_)) ->
                    [A.Structure_item_desc (A.Pstr_eval ((final ()), state));
                    A.Structure_item_desc (A.Pstr_eval (state, (final ())))]
                | Element.Structure_item_desc (Pstr_value (_,_)) ->
                    [A.Structure_item_desc (A.Pstr_value ((final ()), state));
                    A.Structure_item_desc (A.Pstr_value (state, (final ())))]
                | Element.Structure_item_desc (Pstr_primitive _) ->
                    [A.Structure_item_desc (A.Pstr_primitive state)]
                | Element.Structure_item_desc (Pstr_type _) ->
                    [A.Structure_item_desc (A.Pstr_type state)]
                | Element.Structure_item_desc (Pstr_typext _) ->
                    [A.Structure_item_desc (A.Pstr_typext state)]
                | Element.Structure_item_desc (Pstr_exception _) ->
                    [A.Structure_item_desc (A.Pstr_exception state)]
                | Element.Structure_item_desc (Pstr_module _) ->
                    [A.Structure_item_desc (A.Pstr_module state)]
                | Element.Structure_item_desc (Pstr_recmodule _) ->
                    [A.Structure_item_desc (A.Pstr_recmodule state)]
                | Element.Structure_item_desc (Pstr_modtype _) ->
                    [A.Structure_item_desc (A.Pstr_modtype state)]
                | Element.Structure_item_desc (Pstr_open _) ->
                    [A.Structure_item_desc (A.Pstr_open state)]
                | Element.Structure_item_desc (Pstr_class _) ->
                    [A.Structure_item_desc (A.Pstr_class state)]
                | Element.Structure_item_desc (Pstr_class_type _) ->
                    [A.Structure_item_desc (A.Pstr_class_type state)]
                | Element.Structure_item_desc (Pstr_include _) ->
                    [A.Structure_item_desc (A.Pstr_include state)]
                | Element.Structure_item_desc (Pstr_attribute _) ->
                    [A.Structure_item_desc (A.Pstr_attribute state)]
                | Element.Structure_item_desc (Pstr_extension (_,_)) ->
                    [A.Structure_item_desc
                       (A.Pstr_extension ((final ()), state));
                    A.Structure_item_desc
                      (A.Pstr_extension (state, (final ())))]
                | Element.Value_binding
                    { pvb_pat = _; pvb_expr = _; pvb_attributes = _;
                      pvb_loc = _ }
                    ->
                    [A.Value_binding
                       {
                         A.pvb_pat = (final ());
                         A.pvb_expr = (final ());
                         A.pvb_attributes = (final ());
                         A.pvb_loc = state
                       };
                    A.Value_binding
                      {
                        A.pvb_pat = (final ());
                        A.pvb_expr = (final ());
                        A.pvb_attributes = state;
                        A.pvb_loc = (final ())
                      };
                    A.Value_binding
                      {
                        A.pvb_pat = (final ());
                        A.pvb_expr = state;
                        A.pvb_attributes = (final ());
                        A.pvb_loc = (final ())
                      };
                    A.Value_binding
                      {
                        A.pvb_pat = state;
                        A.pvb_expr = (final ());
                        A.pvb_attributes = (final ());
                        A.pvb_loc = (final ())
                      }]
                | Element.Module_binding
                    { pmb_name = _; pmb_expr = _; pmb_attributes = _;
                      pmb_loc = _ }
                    ->
                    [A.Module_binding
                       {
                         A.pmb_name = (final ());
                         A.pmb_expr = (final ());
                         A.pmb_attributes = (final ());
                         A.pmb_loc = state
                       };
                    A.Module_binding
                      {
                        A.pmb_name = (final ());
                        A.pmb_expr = (final ());
                        A.pmb_attributes = state;
                        A.pmb_loc = (final ())
                      };
                    A.Module_binding
                      {
                        A.pmb_name = (final ());
                        A.pmb_expr = state;
                        A.pmb_attributes = (final ());
                        A.pmb_loc = (final ())
                      };
                    A.Module_binding
                      {
                        A.pmb_name = state;
                        A.pmb_expr = (final ());
                        A.pmb_attributes = (final ());
                        A.pmb_loc = (final ())
                      }]
                | Element.Unit _ -> [A.Trash]
                | Element.Bool _ -> [A.Trash]
                | Element.Int _ -> [A.Trash]
                | Element.Char _ -> [A.Trash]
                | Element.String _ -> [A.Trash]
                | Element.Int32 _ -> [A.Trash]
                | Element.Int64 _ -> [A.Trash]
                | Element.Nativeint _ -> [A.Trash]
                | Element.Lexing__position
                    { pos_fname = _; pos_lnum = _; pos_bol = _; pos_cnum = _
                      }
                    ->
                    [A.Lexing__position
                       {
                         A.pos_fname = (final ());
                         A.pos_lnum = (final ());
                         A.pos_bol = (final ());
                         A.pos_cnum = state
                       };
                    A.Lexing__position
                      {
                        A.pos_fname = (final ());
                        A.pos_lnum = (final ());
                        A.pos_bol = state;
                        A.pos_cnum = (final ())
                      };
                    A.Lexing__position
                      {
                        A.pos_fname = (final ());
                        A.pos_lnum = state;
                        A.pos_bol = (final ());
                        A.pos_cnum = (final ())
                      };
                    A.Lexing__position
                      {
                        A.pos_fname = state;
                        A.pos_lnum = (final ());
                        A.pos_bol = (final ());
                        A.pos_cnum = (final ())
                      }]
                | Element.Location__t
                    { loc_start = _; loc_end = _; loc_ghost = _ } ->
                    [A.Location__t
                       {
                         A.loc_start = (final ());
                         A.loc_end = (final ());
                         A.loc_ghost = state
                       };
                    A.Location__t
                      {
                        A.loc_start = (final ());
                        A.loc_end = state;
                        A.loc_ghost = (final ())
                      };
                    A.Location__t
                      {
                        A.loc_start = state;
                        A.loc_end = (final ());
                        A.loc_ghost = (final ())
                      }]
                | Element.Longident__t (Lident _) ->
                    [A.Longident__t (A.Lident state)]
                | Element.Longident__t (Ldot (_,_)) ->
                    [A.Longident__t (A.Ldot ((final ()), state));
                    A.Longident__t (A.Ldot (state, (final ())))]
                | Element.Longident__t (Lapply (_,_)) ->
                    [A.Longident__t (A.Lapply ((final ()), state));
                    A.Longident__t (A.Lapply (state, (final ())))]
                | Element.Constant (Const_int _) ->
                    [A.Constant (A.Const_int state)]
                | Element.Constant (Const_char _) ->
                    [A.Constant (A.Const_char state)]
                | Element.Constant (Const_string (_,_)) ->
                    [A.Constant (A.Const_string ((final ()), state));
                    A.Constant (A.Const_string (state, (final ())))]
                | Element.Constant (Const_float _) ->
                    [A.Constant (A.Const_float state)]
                | Element.Constant (Const_int32 _) ->
                    [A.Constant (A.Const_int32 state)]
                | Element.Constant (Const_int64 _) ->
                    [A.Constant (A.Const_int64 state)]
                | Element.Constant (Const_nativeint _) ->
                    [A.Constant (A.Const_nativeint state)]
                | Element.Rec_flag (Nonrecursive ) ->
                    [A.Rec_flag A.Nonrecursive]
                | Element.Rec_flag (Recursive ) -> [A.Rec_flag A.Recursive]
                | Element.Direction_flag (Upto ) -> [A.Direction_flag A.Upto]
                | Element.Direction_flag (Downto ) ->
                    [A.Direction_flag A.Downto]
                | Element.Private_flag (Private ) ->
                    [A.Private_flag A.Private]
                | Element.Private_flag (Public ) -> [A.Private_flag A.Public]
                | Element.Mutable_flag (Immutable ) ->
                    [A.Mutable_flag A.Immutable]
                | Element.Mutable_flag (Mutable ) ->
                    [A.Mutable_flag A.Mutable]
                | Element.Virtual_flag (Virtual ) ->
                    [A.Virtual_flag A.Virtual]
                | Element.Virtual_flag (Concrete ) ->
                    [A.Virtual_flag A.Concrete]
                | Element.Override_flag (Override ) ->
                    [A.Override_flag A.Override]
                | Element.Override_flag (Fresh ) -> [A.Override_flag A.Fresh]
                | Element.Closed_flag (Closed ) -> [A.Closed_flag A.Closed]
                | Element.Closed_flag (Open ) -> [A.Closed_flag A.Open]
                | Element.Label _ -> [A.Trash]
                | Element.Variance (Covariant ) -> [A.Variance A.Covariant]
                | Element.Variance (Contravariant ) ->
                    [A.Variance A.Contravariant]
                | Element.Variance (Invariant ) -> [A.Variance A.Invariant]
                | Element.Attribute_list [] -> [A.Attribute_list A.Nil]
                | Element.Attribute_list (_::_) ->
                    [A.Attribute_list (A.Cons ((final ()), state));
                    A.Attribute_list (A.Cons (state, (final ())))]
                | Element.Case_list [] -> [A.Case_list A.Nil]
                | Element.Case_list (_::_) ->
                    [A.Case_list (A.Cons ((final ()), state));
                    A.Case_list (A.Cons (state, (final ())))]
                | Element.Class_declaration_list [] ->
                    [A.Class_declaration_list A.Nil]
                | Element.Class_declaration_list (_::_) ->
                    [A.Class_declaration_list (A.Cons ((final ()), state));
                    A.Class_declaration_list (A.Cons (state, (final ())))]
                | Element.Class_description_list [] ->
                    [A.Class_description_list A.Nil]
                | Element.Class_description_list (_::_) ->
                    [A.Class_description_list (A.Cons ((final ()), state));
                    A.Class_description_list (A.Cons (state, (final ())))]
                | Element.Class_expr_class_infos
                    { pci_virt = _; pci_params = _; pci_name = _;
                      pci_expr = _; pci_loc = _; pci_attributes = _ }
                    ->
                    [A.Class_expr_class_infos
                       {
                         A.pci_virt = (final ());
                         A.pci_params = (final ());
                         A.pci_name = (final ());
                         A.pci_expr = (final ());
                         A.pci_loc = (final ());
                         A.pci_attributes = state
                       };
                    A.Class_expr_class_infos
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = state;
                        A.pci_attributes = (final ())
                      };
                    A.Class_expr_class_infos
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = state;
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_expr_class_infos
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = state;
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_expr_class_infos
                      {
                        A.pci_virt = (final ());
                        A.pci_params = state;
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_expr_class_infos
                      {
                        A.pci_virt = state;
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      }]
                | Element.Class_field_kind_mutable_flag_string_loc_tuple_3
                    (_,_,_) ->
                    [A.Class_field_kind_mutable_flag_string_loc_tuple_3
                       ((final ()), (final ()), state);
                    A.Class_field_kind_mutable_flag_string_loc_tuple_3
                      ((final ()), state, (final ()));
                    A.Class_field_kind_mutable_flag_string_loc_tuple_3
                      (state, (final ()), (final ()))]
                | Element.Class_field_kind_private_flag_string_loc_tuple_3
                    (_,_,_) ->
                    [A.Class_field_kind_private_flag_string_loc_tuple_3
                       ((final ()), (final ()), state);
                    A.Class_field_kind_private_flag_string_loc_tuple_3
                      ((final ()), state, (final ()));
                    A.Class_field_kind_private_flag_string_loc_tuple_3
                      (state, (final ()), (final ()))]
                | Element.Class_field_list [] -> [A.Class_field_list A.Nil]
                | Element.Class_field_list (_::_) ->
                    [A.Class_field_list (A.Cons ((final ()), state));
                    A.Class_field_list (A.Cons (state, (final ())))]
                | Element.Class_type_class_infos
                    { pci_virt = _; pci_params = _; pci_name = _;
                      pci_expr = _; pci_loc = _; pci_attributes = _ }
                    ->
                    [A.Class_type_class_infos
                       {
                         A.pci_virt = (final ());
                         A.pci_params = (final ());
                         A.pci_name = (final ());
                         A.pci_expr = (final ());
                         A.pci_loc = (final ());
                         A.pci_attributes = state
                       };
                    A.Class_type_class_infos
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = state;
                        A.pci_attributes = (final ())
                      };
                    A.Class_type_class_infos
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = state;
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_type_class_infos
                      {
                        A.pci_virt = (final ());
                        A.pci_params = (final ());
                        A.pci_name = state;
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_type_class_infos
                      {
                        A.pci_virt = (final ());
                        A.pci_params = state;
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      };
                    A.Class_type_class_infos
                      {
                        A.pci_virt = state;
                        A.pci_params = (final ());
                        A.pci_name = (final ());
                        A.pci_expr = (final ());
                        A.pci_loc = (final ());
                        A.pci_attributes = (final ())
                      }]
                | Element.Class_type_declaration_list [] ->
                    [A.Class_type_declaration_list A.Nil]
                | Element.Class_type_declaration_list (_::_) ->
                    [A.Class_type_declaration_list
                       (A.Cons ((final ()), state));
                    A.Class_type_declaration_list
                      (A.Cons (state, (final ())))]
                | Element.Class_type_field_list [] ->
                    [A.Class_type_field_list A.Nil]
                | Element.Class_type_field_list (_::_) ->
                    [A.Class_type_field_list (A.Cons ((final ()), state));
                    A.Class_type_field_list (A.Cons (state, (final ())))]
                | Element.Constructor_declaration_list [] ->
                    [A.Constructor_declaration_list A.Nil]
                | Element.Constructor_declaration_list (_::_) ->
                    [A.Constructor_declaration_list
                       (A.Cons ((final ()), state));
                    A.Constructor_declaration_list
                      (A.Cons (state, (final ())))]
                | Element.Core_type_Longident__t_loc_tuple_2 (_,_) ->
                    [A.Core_type_Longident__t_loc_tuple_2 ((final ()), state);
                    A.Core_type_Longident__t_loc_tuple_2 (state, (final ()))]
                | Element.Core_type_Longident__t_loc_tuple_2_list [] ->
                    [A.Core_type_Longident__t_loc_tuple_2_list A.Nil]
                | Element.Core_type_Longident__t_loc_tuple_2_list (_::_) ->
                    [A.Core_type_Longident__t_loc_tuple_2_list
                       (A.Cons ((final ()), state));
                    A.Core_type_Longident__t_loc_tuple_2_list
                      (A.Cons (state, (final ())))]
                | Element.Core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2
                    (_,_) ->
                    [A.Core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2
                       ((final ()), state);
                    A.Core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2
                      (state, (final ()))]
                | Element.Core_type_attributes_string_tuple_3 (_,_,_) ->
                    [A.Core_type_attributes_string_tuple_3
                       ((final ()), (final ()), state);
                    A.Core_type_attributes_string_tuple_3
                      ((final ()), state, (final ()));
                    A.Core_type_attributes_string_tuple_3
                      (state, (final ()), (final ()))]
                | Element.Core_type_attributes_string_tuple_3_list [] ->
                    [A.Core_type_attributes_string_tuple_3_list A.Nil]
                | Element.Core_type_attributes_string_tuple_3_list (_::_) ->
                    [A.Core_type_attributes_string_tuple_3_list
                       (A.Cons ((final ()), state));
                    A.Core_type_attributes_string_tuple_3_list
                      (A.Cons (state, (final ())))]
                | Element.Core_type_core_type_tuple_2 (_,_) ->
                    [A.Core_type_core_type_tuple_2 ((final ()), state);
                    A.Core_type_core_type_tuple_2 (state, (final ()))]
                | Element.Core_type_list [] -> [A.Core_type_list A.Nil]
                | Element.Core_type_list (_::_) ->
                    [A.Core_type_list (A.Cons ((final ()), state));
                    A.Core_type_list (A.Cons (state, (final ())))]
                | Element.Core_type_option (None ) ->
                    [A.Core_type_option A.None]
                | Element.Core_type_option (Some _) ->
                    [A.Core_type_option (A.Some state)]
                | Element.Core_type_virtual_flag_mutable_flag_string_tuple_4
                    (_,_,_,_) ->
                    [A.Core_type_virtual_flag_mutable_flag_string_tuple_4
                       ((final ()), (final ()), (final ()), state);
                    A.Core_type_virtual_flag_mutable_flag_string_tuple_4
                      ((final ()), (final ()), state, (final ()));
                    A.Core_type_virtual_flag_mutable_flag_string_tuple_4
                      ((final ()), state, (final ()), (final ()));
                    A.Core_type_virtual_flag_mutable_flag_string_tuple_4
                      (state, (final ()), (final ()), (final ()))]
                | Element.Core_type_virtual_flag_private_flag_string_tuple_4
                    (_,_,_,_) ->
                    [A.Core_type_virtual_flag_private_flag_string_tuple_4
                       ((final ()), (final ()), (final ()), state);
                    A.Core_type_virtual_flag_private_flag_string_tuple_4
                      ((final ()), (final ()), state, (final ()));
                    A.Core_type_virtual_flag_private_flag_string_tuple_4
                      ((final ()), state, (final ()), (final ()));
                    A.Core_type_virtual_flag_private_flag_string_tuple_4
                      (state, (final ()), (final ()), (final ()))]
                | Element.Expression_Longident__t_loc_tuple_2 (_,_) ->
                    [A.Expression_Longident__t_loc_tuple_2
                       ((final ()), state);
                    A.Expression_Longident__t_loc_tuple_2 (state, (final ()))]
                | Element.Expression_Longident__t_loc_tuple_2_list [] ->
                    [A.Expression_Longident__t_loc_tuple_2_list A.Nil]
                | Element.Expression_Longident__t_loc_tuple_2_list (_::_) ->
                    [A.Expression_Longident__t_loc_tuple_2_list
                       (A.Cons ((final ()), state));
                    A.Expression_Longident__t_loc_tuple_2_list
                      (A.Cons (state, (final ())))]
                | Element.Expression_label_tuple_2 (_,_) ->
                    [A.Expression_label_tuple_2 ((final ()), state);
                    A.Expression_label_tuple_2 (state, (final ()))]
                | Element.Expression_label_tuple_2_list [] ->
                    [A.Expression_label_tuple_2_list A.Nil]
                | Element.Expression_label_tuple_2_list (_::_) ->
                    [A.Expression_label_tuple_2_list
                       (A.Cons ((final ()), state));
                    A.Expression_label_tuple_2_list
                      (A.Cons (state, (final ())))]
                | Element.Expression_list [] -> [A.Expression_list A.Nil]
                | Element.Expression_list (_::_) ->
                    [A.Expression_list (A.Cons ((final ()), state));
                    A.Expression_list (A.Cons (state, (final ())))]
                | Element.Expression_option (None ) ->
                    [A.Expression_option A.None]
                | Element.Expression_option (Some _) ->
                    [A.Expression_option (A.Some state)]
                | Element.Expression_string_loc_tuple_2 (_,_) ->
                    [A.Expression_string_loc_tuple_2 ((final ()), state);
                    A.Expression_string_loc_tuple_2 (state, (final ()))]
                | Element.Expression_string_loc_tuple_2_list [] ->
                    [A.Expression_string_loc_tuple_2_list A.Nil]
                | Element.Expression_string_loc_tuple_2_list (_::_) ->
                    [A.Expression_string_loc_tuple_2_list
                       (A.Cons ((final ()), state));
                    A.Expression_string_loc_tuple_2_list
                      (A.Cons (state, (final ())))]
                | Element.Extension_constructor_list [] ->
                    [A.Extension_constructor_list A.Nil]
                | Element.Extension_constructor_list (_::_) ->
                    [A.Extension_constructor_list
                       (A.Cons ((final ()), state));
                    A.Extension_constructor_list (A.Cons (state, (final ())))]
                | Element.Label_declaration_list [] ->
                    [A.Label_declaration_list A.Nil]
                | Element.Label_declaration_list (_::_) ->
                    [A.Label_declaration_list (A.Cons ((final ()), state));
                    A.Label_declaration_list (A.Cons (state, (final ())))]
                | Element.Label_list [] -> [A.Label_list A.Nil]
                | Element.Label_list (_::_) ->
                    [A.Label_list (A.Cons ((final ()), state));
                    A.Label_list (A.Cons (state, (final ())))]
                | Element.Label_list_option (None ) ->
                    [A.Label_list_option A.None]
                | Element.Label_list_option (Some _) ->
                    [A.Label_list_option (A.Some state)]
                | Element.Location__t_core_type_core_type_tuple_3 (_,_,_) ->
                    [A.Location__t_core_type_core_type_tuple_3
                       ((final ()), (final ()), state);
                    A.Location__t_core_type_core_type_tuple_3
                      ((final ()), state, (final ()));
                    A.Location__t_core_type_core_type_tuple_3
                      (state, (final ()), (final ()))]
                | Element.Location__t_core_type_core_type_tuple_3_list [] ->
                    [A.Location__t_core_type_core_type_tuple_3_list A.Nil]
                | Element.Location__t_core_type_core_type_tuple_3_list (_::_)
                    ->
                    [A.Location__t_core_type_core_type_tuple_3_list
                       (A.Cons ((final ()), state));
                    A.Location__t_core_type_core_type_tuple_3_list
                      (A.Cons (state, (final ())))]
                | Element.Longident__t_loc { txt = _; loc = _ } ->
                    [A.Longident__t_loc { A.txt = (final ()); A.loc = state };
                    A.Longident__t_loc { A.txt = state; A.loc = (final ()) }]
                | Element.Module_binding_list [] ->
                    [A.Module_binding_list A.Nil]
                | Element.Module_binding_list (_::_) ->
                    [A.Module_binding_list (A.Cons ((final ()), state));
                    A.Module_binding_list (A.Cons (state, (final ())))]
                | Element.Module_declaration_list [] ->
                    [A.Module_declaration_list A.Nil]
                | Element.Module_declaration_list (_::_) ->
                    [A.Module_declaration_list (A.Cons ((final ()), state));
                    A.Module_declaration_list (A.Cons (state, (final ())))]
                | Element.Module_expr_include_infos
                    { pincl_mod = _; pincl_loc = _; pincl_attributes = _ } ->
                    [A.Module_expr_include_infos
                       {
                         A.pincl_mod = (final ());
                         A.pincl_loc = (final ());
                         A.pincl_attributes = state
                       };
                    A.Module_expr_include_infos
                      {
                        A.pincl_mod = (final ());
                        A.pincl_loc = state;
                        A.pincl_attributes = (final ())
                      };
                    A.Module_expr_include_infos
                      {
                        A.pincl_mod = state;
                        A.pincl_loc = (final ());
                        A.pincl_attributes = (final ())
                      }]
                | Element.Module_type_include_infos
                    { pincl_mod = _; pincl_loc = _; pincl_attributes = _ } ->
                    [A.Module_type_include_infos
                       {
                         A.pincl_mod = (final ());
                         A.pincl_loc = (final ());
                         A.pincl_attributes = state
                       };
                    A.Module_type_include_infos
                      {
                        A.pincl_mod = (final ());
                        A.pincl_loc = state;
                        A.pincl_attributes = (final ())
                      };
                    A.Module_type_include_infos
                      {
                        A.pincl_mod = state;
                        A.pincl_loc = (final ());
                        A.pincl_attributes = (final ())
                      }]
                | Element.Module_type_option (None ) ->
                    [A.Module_type_option A.None]
                | Element.Module_type_option (Some _) ->
                    [A.Module_type_option (A.Some state)]
                | Element.Pattern_Longident__t_loc_tuple_2 (_,_) ->
                    [A.Pattern_Longident__t_loc_tuple_2 ((final ()), state);
                    A.Pattern_Longident__t_loc_tuple_2 (state, (final ()))]
                | Element.Pattern_Longident__t_loc_tuple_2_list [] ->
                    [A.Pattern_Longident__t_loc_tuple_2_list A.Nil]
                | Element.Pattern_Longident__t_loc_tuple_2_list (_::_) ->
                    [A.Pattern_Longident__t_loc_tuple_2_list
                       (A.Cons ((final ()), state));
                    A.Pattern_Longident__t_loc_tuple_2_list
                      (A.Cons (state, (final ())))]
                | Element.Pattern_list [] -> [A.Pattern_list A.Nil]
                | Element.Pattern_list (_::_) ->
                    [A.Pattern_list (A.Cons ((final ()), state));
                    A.Pattern_list (A.Cons (state, (final ())))]
                | Element.Pattern_option (None ) -> [A.Pattern_option A.None]
                | Element.Pattern_option (Some _) ->
                    [A.Pattern_option (A.Some state)]
                | Element.Payload_string_loc_tuple_2 (_,_) ->
                    [A.Payload_string_loc_tuple_2 ((final ()), state);
                    A.Payload_string_loc_tuple_2 (state, (final ()))]
                | Element.Row_field_list [] -> [A.Row_field_list A.Nil]
                | Element.Row_field_list (_::_) ->
                    [A.Row_field_list (A.Cons ((final ()), state));
                    A.Row_field_list (A.Cons (state, (final ())))]
                | Element.Signature_item_list [] ->
                    [A.Signature_item_list A.Nil]
                | Element.Signature_item_list (_::_) ->
                    [A.Signature_item_list (A.Cons ((final ()), state));
                    A.Signature_item_list (A.Cons (state, (final ())))]
                | Element.String_list [] -> [A.String_list A.Nil]
                | Element.String_list (_::_) ->
                    [A.String_list (A.Cons ((final ()), state));
                    A.String_list (A.Cons (state, (final ())))]
                | Element.String_loc { txt = _; loc = _ } ->
                    [A.String_loc { A.txt = (final ()); A.loc = state };
                    A.String_loc { A.txt = state; A.loc = (final ()) }]
                | Element.String_option (None ) -> [A.String_option A.None]
                | Element.String_option (Some _) ->
                    [A.String_option (A.Some state)]
                | Element.Structure_item_list [] ->
                    [A.Structure_item_list A.Nil]
                | Element.Structure_item_list (_::_) ->
                    [A.Structure_item_list (A.Cons ((final ()), state));
                    A.Structure_item_list (A.Cons (state, (final ())))]
                | Element.Type_declaration_list [] ->
                    [A.Type_declaration_list A.Nil]
                | Element.Type_declaration_list (_::_) ->
                    [A.Type_declaration_list (A.Cons ((final ()), state));
                    A.Type_declaration_list (A.Cons (state, (final ())))]
                | Element.Value_binding_list [] ->
                    [A.Value_binding_list A.Nil]
                | Element.Value_binding_list (_::_) ->
                    [A.Value_binding_list (A.Cons ((final ()), state));
                    A.Value_binding_list (A.Cons (state, (final ())))]
                | Element.Variance_core_type_tuple_2 (_,_) ->
                    [A.Variance_core_type_tuple_2 ((final ()), state);
                    A.Variance_core_type_tuple_2 (state, (final ()))]
                | Element.Variance_core_type_tuple_2_list [] ->
                    [A.Variance_core_type_tuple_2_list A.Nil]
                | Element.Variance_core_type_tuple_2_list (_::_) ->
                    [A.Variance_core_type_tuple_2_list
                       (A.Cons ((final ()), state));
                    A.Variance_core_type_tuple_2_list
                      (A.Cons (state, (final ())))]
                | Element.With_constraint_list [] ->
                    [A.With_constraint_list A.Nil]
                | Element.With_constraint_list (_::_) ->
                    [A.With_constraint_list (A.Cons ((final ()), state));
                    A.With_constraint_list (A.Cons (state, (final ())))]))))];
      state
  end 
let make_report state =
  {
    state with
    A.transitions = (List.map (fun (_,f)  -> (true, f)) state.A.transitions)
  }
let add_transitions_from dest origin =
  dest.A.transitions <- origin.A.transitions @ dest.A.transitions; dest
let has_attr name attributes =
  List.exists (fun (id,_)  -> id.Asttypes.txt = name) attributes
module From =
  struct
    let rec attribute (sub_0,sub_1) =
      Match.attribute (string_loc sub_0) (payload sub_1)
    and extension (sub_0,sub_1) =
      Match.extension (string_loc sub_0) (payload sub_1)
    and attributes =
      function
      | [] -> (Match.attributes_nil : A.state)
      | sub_0::sub_1 ->
          (Match.attributes_cons (attribute sub_0) (attribute_list sub_1) : 
          A.state)
    and payload =
      function
      | PStr sub_0 -> (Match.payload_pStr (structure sub_0) : A.state)
      | PTyp sub_0 -> (Match.payload_pTyp (core_type sub_0) : A.state)
      | PPat (sub_0,sub_1) ->
          (Match.payload_pPat (pattern sub_0) (expression_option sub_1) : 
          A.state)
    and core_type { ptyp_desc; ptyp_loc; ptyp_attributes } =
      Match.core_type (core_type_desc ptyp_desc) (location__t ptyp_loc)
        (attributes ptyp_attributes)
    and core_type_desc =
      function
      | Ptyp_any  -> (Match.core_type_desc_ptyp_any : A.state)
      | Ptyp_var sub_0 ->
          (Match.core_type_desc_ptyp_var (string sub_0) : A.state)
      | Ptyp_arrow (sub_0,sub_1,sub_2) ->
          (Match.core_type_desc_ptyp_arrow (label sub_0) (core_type sub_1)
             (core_type sub_2) : A.state)
      | Ptyp_tuple sub_0 ->
          (Match.core_type_desc_ptyp_tuple (core_type_list sub_0) : A.state)
      | Ptyp_constr (sub_0,sub_1) ->
          (Match.core_type_desc_ptyp_constr (longident__t_loc sub_0)
             (core_type_list sub_1) : A.state)
      | Ptyp_object (sub_0,sub_1) ->
          (Match.core_type_desc_ptyp_object
             (core_type_attributes_string_tuple_3_list sub_0)
             (closed_flag sub_1) : A.state)
      | Ptyp_class (sub_0,sub_1) ->
          (Match.core_type_desc_ptyp_class (longident__t_loc sub_0)
             (core_type_list sub_1) : A.state)
      | Ptyp_alias (sub_0,sub_1) ->
          (Match.core_type_desc_ptyp_alias (core_type sub_0) (string sub_1) : 
          A.state)
      | Ptyp_variant (sub_0,sub_1,sub_2) ->
          (Match.core_type_desc_ptyp_variant (row_field_list sub_0)
             (closed_flag sub_1) (label_list_option sub_2) : A.state)
      | Ptyp_poly (sub_0,sub_1) ->
          (Match.core_type_desc_ptyp_poly (string_list sub_0)
             (core_type sub_1) : A.state)
      | Ptyp_package sub_0 ->
          (Match.core_type_desc_ptyp_package (package_type sub_0) : A.state)
      | Ptyp_extension sub_0 ->
          (Match.core_type_desc_ptyp_extension (extension sub_0) : A.state)
    and package_type (sub_0,sub_1) =
      Match.package_type (longident__t_loc sub_0)
        (core_type_Longident__t_loc_tuple_2_list sub_1)
    and row_field =
      function
      | Rtag (sub_0,sub_1,sub_2,sub_3) ->
          (Match.row_field_rtag (label sub_0) (attributes sub_1) (bool sub_2)
             (core_type_list sub_3) : A.state)
      | Rinherit sub_0 ->
          (Match.row_field_rinherit (core_type sub_0) : A.state)
    and pattern { ppat_desc; ppat_attributes; ppat_loc } =
      match ppat_desc with
      | Ppat_var { txt = id;_} when
          has_attr "__sempatch_metavar" ppat_attributes ->
          Match.metavar_pat id
      | _ ->
          Match.pattern (pattern_desc ppat_desc) (location__t ppat_loc)
            (attributes ppat_attributes)
    and pattern_desc =
      function
      | Ppat_any  -> (Match.pattern_desc_ppat_any : A.state)
      | Ppat_var sub_0 ->
          (Match.pattern_desc_ppat_var (string_loc sub_0) : A.state)
      | Ppat_alias (sub_0,sub_1) ->
          (Match.pattern_desc_ppat_alias (pattern sub_0) (string_loc sub_1) : 
          A.state)
      | Ppat_constant sub_0 ->
          (Match.pattern_desc_ppat_constant (constant sub_0) : A.state)
      | Ppat_interval (sub_0,sub_1) ->
          (Match.pattern_desc_ppat_interval (constant sub_0) (constant sub_1) : 
          A.state)
      | Ppat_tuple sub_0 ->
          (Match.pattern_desc_ppat_tuple (pattern_list sub_0) : A.state)
      | Ppat_construct (sub_0,sub_1) ->
          (Match.pattern_desc_ppat_construct (longident__t_loc sub_0)
             (pattern_option sub_1) : A.state)
      | Ppat_variant (sub_0,sub_1) ->
          (Match.pattern_desc_ppat_variant (label sub_0)
             (pattern_option sub_1) : A.state)
      | Ppat_record (sub_0,sub_1) ->
          (Match.pattern_desc_ppat_record
             (pattern_Longident__t_loc_tuple_2_list sub_0)
             (closed_flag sub_1) : A.state)
      | Ppat_array sub_0 ->
          (Match.pattern_desc_ppat_array (pattern_list sub_0) : A.state)
      | Ppat_or (sub_0,sub_1) ->
          (Match.pattern_desc_ppat_or (pattern sub_0) (pattern sub_1) : 
          A.state)
      | Ppat_constraint (sub_0,sub_1) ->
          (Match.pattern_desc_ppat_constraint (pattern sub_0)
             (core_type sub_1) : A.state)
      | Ppat_type sub_0 ->
          (Match.pattern_desc_ppat_type (longident__t_loc sub_0) : A.state)
      | Ppat_lazy sub_0 ->
          (Match.pattern_desc_ppat_lazy (pattern sub_0) : A.state)
      | Ppat_unpack sub_0 ->
          (Match.pattern_desc_ppat_unpack (string_loc sub_0) : A.state)
      | Ppat_exception sub_0 ->
          (Match.pattern_desc_ppat_exception (pattern sub_0) : A.state)
      | Ppat_extension sub_0 ->
          (Match.pattern_desc_ppat_extension (extension sub_0) : A.state)
    and expression { pexp_desc; pexp_loc; pexp_attributes } =
      match pexp_desc with
      | Pexp_extension
          ({ Asttypes.txt = "__sempatch_report";_},PStr
           ({ pstr_desc = Pstr_eval (e,_);_}::[]))
          -> make_report @@ (expression e)
      | Pexp_extension
          ({ Asttypes.txt = "__sempatch_inside";_},PStr
           ({ pstr_desc = Pstr_eval (e,_);_}::[]))
          -> add_transitions_from (Match.wildcard ()) (expression e)
      | Pexp_extension
          ({ Asttypes.txt = "__sempatch_first";_},PStr
           ({ pstr_desc = Pstr_eval (e,_);_}::[]))
          ->
          (match e.pexp_desc with
           | Pexp_match (e_patch,cases_patch) ->
               let original_state =
                 Match.expression_desc_pexp_match (expression e_patch)
                   (case_list cases_patch) in
               let from_expr_desc =
                 let open A in
                   {
                     final = (original_state.final);
                     transitions =
                       (List.map
                          (fun (update_loc,trans_fun)  ->
                             (update_loc,
                               (fun env  ->
                                  function
                                  | Element.Expression_desc
                                      (Element.Pexp_match (e,cases)) ->
                                      let cases_truncated =
                                        (List.truncate_as cases cases_patch)
                                          |> (Option.value cases) in
                                      (Pprintast.expression
                                         Format.std_formatter
                                         {
                                           e with
                                           Element.pexp_desc =
                                             (Element.Pexp_match
                                                (e, cases_truncated))
                                         };
                                       trans_fun env
                                         (Element.Expression_desc
                                            (Element.Pexp_match
                                               (e, cases_truncated))))
                                  | Element.Expression_desc desc ->
                                      (print_endline "This isn't a match";
                                       [(A.Trash, env)])
                                  | _ -> assert false)))
                          original_state.A.transitions)
                   } in
               Match.expression from_expr_desc (location__t pexp_loc)
                 (attributes pexp_attributes)
           | _ -> expression e)
      | Pexp_ident { txt = Longident.Lident id;_} when
          has_attr "__sempatch_metavar" pexp_attributes ->
          Match.metavar_expr id
      | _ ->
          Match.expression (expression_desc pexp_desc) (location__t pexp_loc)
            (attributes pexp_attributes)
    and expression_desc =
      function
      | Pexp_ident sub_0 ->
          (Match.expression_desc_pexp_ident (longident__t_loc sub_0) : 
          A.state)
      | Pexp_constant sub_0 ->
          (Match.expression_desc_pexp_constant (constant sub_0) : A.state)
      | Pexp_let (sub_0,sub_1,sub_2) ->
          (Match.expression_desc_pexp_let (rec_flag sub_0)
             (value_binding_list sub_1) (expression sub_2) : A.state)
      | Pexp_function sub_0 ->
          (Match.expression_desc_pexp_function (case_list sub_0) : A.state)
      | Pexp_fun (sub_0,sub_1,sub_2,sub_3) ->
          (Match.expression_desc_pexp_fun (label sub_0)
             (expression_option sub_1) (pattern sub_2) (expression sub_3) : 
          A.state)
      | Pexp_apply (sub_0,sub_1) ->
          (Match.expression_desc_pexp_apply (expression sub_0)
             (expression_label_tuple_2_list sub_1) : A.state)
      | Pexp_match (sub_0,sub_1) ->
          (Match.expression_desc_pexp_match (expression sub_0)
             (case_list sub_1) : A.state)
      | Pexp_try (sub_0,sub_1) ->
          (Match.expression_desc_pexp_try (expression sub_0)
             (case_list sub_1) : A.state)
      | Pexp_tuple sub_0 ->
          (Match.expression_desc_pexp_tuple (expression_list sub_0) : 
          A.state)
      | Pexp_construct (sub_0,sub_1) ->
          (Match.expression_desc_pexp_construct (longident__t_loc sub_0)
             (expression_option sub_1) : A.state)
      | Pexp_variant (sub_0,sub_1) ->
          (Match.expression_desc_pexp_variant (label sub_0)
             (expression_option sub_1) : A.state)
      | Pexp_record (sub_0,sub_1) ->
          (Match.expression_desc_pexp_record
             (expression_Longident__t_loc_tuple_2_list sub_0)
             (expression_option sub_1) : A.state)
      | Pexp_field (sub_0,sub_1) ->
          (Match.expression_desc_pexp_field (expression sub_0)
             (longident__t_loc sub_1) : A.state)
      | Pexp_setfield (sub_0,sub_1,sub_2) ->
          (Match.expression_desc_pexp_setfield (expression sub_0)
             (longident__t_loc sub_1) (expression sub_2) : A.state)
      | Pexp_array sub_0 ->
          (Match.expression_desc_pexp_array (expression_list sub_0) : 
          A.state)
      | Pexp_ifthenelse (sub_0,sub_1,sub_2) ->
          (Match.expression_desc_pexp_ifthenelse (expression sub_0)
             (expression sub_1) (expression_option sub_2) : A.state)
      | Pexp_sequence (sub_0,sub_1) ->
          (Match.expression_desc_pexp_sequence (expression sub_0)
             (expression sub_1) : A.state)
      | Pexp_while (sub_0,sub_1) ->
          (Match.expression_desc_pexp_while (expression sub_0)
             (expression sub_1) : A.state)
      | Pexp_for (sub_0,sub_1,sub_2,sub_3,sub_4) ->
          (Match.expression_desc_pexp_for (pattern sub_0) (expression sub_1)
             (expression sub_2) (direction_flag sub_3) (expression sub_4) : 
          A.state)
      | Pexp_constraint (sub_0,sub_1) ->
          (Match.expression_desc_pexp_constraint (expression sub_0)
             (core_type sub_1) : A.state)
      | Pexp_coerce (sub_0,sub_1,sub_2) ->
          (Match.expression_desc_pexp_coerce (expression sub_0)
             (core_type_option sub_1) (core_type sub_2) : A.state)
      | Pexp_send (sub_0,sub_1) ->
          (Match.expression_desc_pexp_send (expression sub_0) (string sub_1) : 
          A.state)
      | Pexp_new sub_0 ->
          (Match.expression_desc_pexp_new (longident__t_loc sub_0) : 
          A.state)
      | Pexp_setinstvar (sub_0,sub_1) ->
          (Match.expression_desc_pexp_setinstvar (string_loc sub_0)
             (expression sub_1) : A.state)
      | Pexp_override sub_0 ->
          (Match.expression_desc_pexp_override
             (expression_string_loc_tuple_2_list sub_0) : A.state)
      | Pexp_letmodule (sub_0,sub_1,sub_2) ->
          (Match.expression_desc_pexp_letmodule (string_loc sub_0)
             (module_expr sub_1) (expression sub_2) : A.state)
      | Pexp_assert sub_0 ->
          (Match.expression_desc_pexp_assert (expression sub_0) : A.state)
      | Pexp_lazy sub_0 ->
          (Match.expression_desc_pexp_lazy (expression sub_0) : A.state)
      | Pexp_poly (sub_0,sub_1) ->
          (Match.expression_desc_pexp_poly (expression sub_0)
             (core_type_option sub_1) : A.state)
      | Pexp_object sub_0 ->
          (Match.expression_desc_pexp_object (class_structure sub_0) : 
          A.state)
      | Pexp_newtype (sub_0,sub_1) ->
          (Match.expression_desc_pexp_newtype (string sub_0)
             (expression sub_1) : A.state)
      | Pexp_pack sub_0 ->
          (Match.expression_desc_pexp_pack (module_expr sub_0) : A.state)
      | Pexp_open (sub_0,sub_1,sub_2) ->
          (Match.expression_desc_pexp_open (override_flag sub_0)
             (longident__t_loc sub_1) (expression sub_2) : A.state)
      | Pexp_extension sub_0 ->
          (Match.expression_desc_pexp_extension (extension sub_0) : A.state)
    and case { pc_lhs; pc_guard; pc_rhs } =
      Match.case (pattern pc_lhs) (expression_option pc_guard)
        (expression pc_rhs)
    and value_description
      { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } =
      Match.value_description (string_loc pval_name) (core_type pval_type)
        (string_list pval_prim) (attributes pval_attributes)
        (location__t pval_loc)
    and type_declaration
      { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
        ptype_manifest; ptype_attributes; ptype_loc }
      =
      Match.type_declaration (string_loc ptype_name)
        (variance_core_type_tuple_2_list ptype_params)
        (location__t_core_type_core_type_tuple_3_list ptype_cstrs)
        (type_kind ptype_kind) (private_flag ptype_private)
        (core_type_option ptype_manifest) (attributes ptype_attributes)
        (location__t ptype_loc)
    and type_kind =
      function
      | Ptype_abstract  -> (Match.type_kind_ptype_abstract : A.state)
      | Ptype_variant sub_0 ->
          (Match.type_kind_ptype_variant (constructor_declaration_list sub_0) : 
          A.state)
      | Ptype_record sub_0 ->
          (Match.type_kind_ptype_record (label_declaration_list sub_0) : 
          A.state)
      | Ptype_open  -> (Match.type_kind_ptype_open : A.state)
    and label_declaration
      { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } =
      Match.label_declaration (string_loc pld_name)
        (mutable_flag pld_mutable) (core_type pld_type) (location__t pld_loc)
        (attributes pld_attributes)
    and constructor_declaration
      { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } =
      Match.constructor_declaration (string_loc pcd_name)
        (core_type_list pcd_args) (core_type_option pcd_res)
        (location__t pcd_loc) (attributes pcd_attributes)
    and type_extension
      { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
        ptyext_attributes }
      =
      Match.type_extension (longident__t_loc ptyext_path)
        (variance_core_type_tuple_2_list ptyext_params)
        (extension_constructor_list ptyext_constructors)
        (private_flag ptyext_private) (attributes ptyext_attributes)
    and extension_constructor
      { pext_name; pext_kind; pext_loc; pext_attributes } =
      Match.extension_constructor (string_loc pext_name)
        (extension_constructor_kind pext_kind) (location__t pext_loc)
        (attributes pext_attributes)
    and extension_constructor_kind =
      function
      | Pext_decl (sub_0,sub_1) ->
          (Match.extension_constructor_kind_pext_decl (core_type_list sub_0)
             (core_type_option sub_1) : A.state)
      | Pext_rebind sub_0 ->
          (Match.extension_constructor_kind_pext_rebind
             (longident__t_loc sub_0) : A.state)
    and class_type { pcty_desc; pcty_loc; pcty_attributes } =
      Match.class_type (class_type_desc pcty_desc) (location__t pcty_loc)
        (attributes pcty_attributes)
    and class_type_desc =
      function
      | Pcty_constr (sub_0,sub_1) ->
          (Match.class_type_desc_pcty_constr (longident__t_loc sub_0)
             (core_type_list sub_1) : A.state)
      | Pcty_signature sub_0 ->
          (Match.class_type_desc_pcty_signature (class_signature sub_0) : 
          A.state)
      | Pcty_arrow (sub_0,sub_1,sub_2) ->
          (Match.class_type_desc_pcty_arrow (label sub_0) (core_type sub_1)
             (class_type sub_2) : A.state)
      | Pcty_extension sub_0 ->
          (Match.class_type_desc_pcty_extension (extension sub_0) : A.state)
    and class_signature { pcsig_self; pcsig_fields } =
      Match.class_signature (core_type pcsig_self)
        (class_type_field_list pcsig_fields)
    and class_type_field { pctf_desc; pctf_loc; pctf_attributes } =
      Match.class_type_field (class_type_field_desc pctf_desc)
        (location__t pctf_loc) (attributes pctf_attributes)
    and class_type_field_desc =
      function
      | Pctf_inherit sub_0 ->
          (Match.class_type_field_desc_pctf_inherit (class_type sub_0) : 
          A.state)
      | Pctf_val sub_0 ->
          (Match.class_type_field_desc_pctf_val
             (core_type_virtual_flag_mutable_flag_string_tuple_4 sub_0) : 
          A.state)
      | Pctf_method sub_0 ->
          (Match.class_type_field_desc_pctf_method
             (core_type_virtual_flag_private_flag_string_tuple_4 sub_0) : 
          A.state)
      | Pctf_constraint sub_0 ->
          (Match.class_type_field_desc_pctf_constraint
             (core_type_core_type_tuple_2 sub_0) : A.state)
      | Pctf_attribute sub_0 ->
          (Match.class_type_field_desc_pctf_attribute (attribute sub_0) : 
          A.state)
      | Pctf_extension sub_0 ->
          (Match.class_type_field_desc_pctf_extension (extension sub_0) : 
          A.state)
    and class_description
      { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } =
      Match.class_description (virtual_flag pci_virt)
        (variance_core_type_tuple_2_list pci_params) (string_loc pci_name)
        (class_type pci_expr) (location__t pci_loc)
        (attributes pci_attributes)
    and class_type_declaration
      { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } =
      Match.class_type_declaration (virtual_flag pci_virt)
        (variance_core_type_tuple_2_list pci_params) (string_loc pci_name)
        (class_type pci_expr) (location__t pci_loc)
        (attributes pci_attributes)
    and class_expr { pcl_desc; pcl_loc; pcl_attributes } =
      Match.class_expr (class_expr_desc pcl_desc) (location__t pcl_loc)
        (attributes pcl_attributes)
    and class_expr_desc =
      function
      | Pcl_constr (sub_0,sub_1) ->
          (Match.class_expr_desc_pcl_constr (longident__t_loc sub_0)
             (core_type_list sub_1) : A.state)
      | Pcl_structure sub_0 ->
          (Match.class_expr_desc_pcl_structure (class_structure sub_0) : 
          A.state)
      | Pcl_fun (sub_0,sub_1,sub_2,sub_3) ->
          (Match.class_expr_desc_pcl_fun (label sub_0)
             (expression_option sub_1) (pattern sub_2) (class_expr sub_3) : 
          A.state)
      | Pcl_apply (sub_0,sub_1) ->
          (Match.class_expr_desc_pcl_apply (class_expr sub_0)
             (expression_label_tuple_2_list sub_1) : A.state)
      | Pcl_let (sub_0,sub_1,sub_2) ->
          (Match.class_expr_desc_pcl_let (rec_flag sub_0)
             (value_binding_list sub_1) (class_expr sub_2) : A.state)
      | Pcl_constraint (sub_0,sub_1) ->
          (Match.class_expr_desc_pcl_constraint (class_expr sub_0)
             (class_type sub_1) : A.state)
      | Pcl_extension sub_0 ->
          (Match.class_expr_desc_pcl_extension (extension sub_0) : A.state)
    and class_structure { pcstr_self; pcstr_fields } =
      Match.class_structure (pattern pcstr_self)
        (class_field_list pcstr_fields)
    and class_field { pcf_desc; pcf_loc; pcf_attributes } =
      Match.class_field (class_field_desc pcf_desc) (location__t pcf_loc)
        (attributes pcf_attributes)
    and class_field_desc =
      function
      | Pcf_inherit (sub_0,sub_1,sub_2) ->
          (Match.class_field_desc_pcf_inherit (override_flag sub_0)
             (class_expr sub_1) (string_option sub_2) : A.state)
      | Pcf_val sub_0 ->
          (Match.class_field_desc_pcf_val
             (class_field_kind_mutable_flag_string_loc_tuple_3 sub_0) : 
          A.state)
      | Pcf_method sub_0 ->
          (Match.class_field_desc_pcf_method
             (class_field_kind_private_flag_string_loc_tuple_3 sub_0) : 
          A.state)
      | Pcf_constraint sub_0 ->
          (Match.class_field_desc_pcf_constraint
             (core_type_core_type_tuple_2 sub_0) : A.state)
      | Pcf_initializer sub_0 ->
          (Match.class_field_desc_pcf_initializer (expression sub_0) : 
          A.state)
      | Pcf_attribute sub_0 ->
          (Match.class_field_desc_pcf_attribute (attribute sub_0) : A.state)
      | Pcf_extension sub_0 ->
          (Match.class_field_desc_pcf_extension (extension sub_0) : A.state)
    and class_field_kind =
      function
      | Cfk_virtual sub_0 ->
          (Match.class_field_kind_cfk_virtual (core_type sub_0) : A.state)
      | Cfk_concrete (sub_0,sub_1) ->
          (Match.class_field_kind_cfk_concrete (override_flag sub_0)
             (expression sub_1) : A.state)
    and class_declaration
      { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } =
      Match.class_declaration (virtual_flag pci_virt)
        (variance_core_type_tuple_2_list pci_params) (string_loc pci_name)
        (class_expr pci_expr) (location__t pci_loc)
        (attributes pci_attributes)
    and module_type { pmty_desc; pmty_loc; pmty_attributes } =
      Match.module_type (module_type_desc pmty_desc) (location__t pmty_loc)
        (attributes pmty_attributes)
    and module_type_desc =
      function
      | Pmty_ident sub_0 ->
          (Match.module_type_desc_pmty_ident (longident__t_loc sub_0) : 
          A.state)
      | Pmty_signature sub_0 ->
          (Match.module_type_desc_pmty_signature (signature sub_0) : 
          A.state)
      | Pmty_functor (sub_0,sub_1,sub_2) ->
          (Match.module_type_desc_pmty_functor (string_loc sub_0)
             (module_type_option sub_1) (module_type sub_2) : A.state)
      | Pmty_with (sub_0,sub_1) ->
          (Match.module_type_desc_pmty_with (module_type sub_0)
             (with_constraint_list sub_1) : A.state)
      | Pmty_typeof sub_0 ->
          (Match.module_type_desc_pmty_typeof (module_expr sub_0) : A.state)
      | Pmty_extension sub_0 ->
          (Match.module_type_desc_pmty_extension (extension sub_0) : 
          A.state)
      | Pmty_alias sub_0 ->
          (Match.module_type_desc_pmty_alias (longident__t_loc sub_0) : 
          A.state)
    and signature =
      function
      | [] -> (Match.signature_nil : A.state)
      | sub_0::sub_1 ->
          (Match.signature_cons (signature_item sub_0)
             (signature_item_list sub_1) : A.state)
    and signature_item { psig_desc; psig_loc } =
      Match.signature_item (signature_item_desc psig_desc)
        (location__t psig_loc)
    and signature_item_desc =
      function
      | Psig_value sub_0 ->
          (Match.signature_item_desc_psig_value (value_description sub_0) : 
          A.state)
      | Psig_type sub_0 ->
          (Match.signature_item_desc_psig_type (type_declaration_list sub_0) : 
          A.state)
      | Psig_typext sub_0 ->
          (Match.signature_item_desc_psig_typext (type_extension sub_0) : 
          A.state)
      | Psig_exception sub_0 ->
          (Match.signature_item_desc_psig_exception
             (extension_constructor sub_0) : A.state)
      | Psig_module sub_0 ->
          (Match.signature_item_desc_psig_module (module_declaration sub_0) : 
          A.state)
      | Psig_recmodule sub_0 ->
          (Match.signature_item_desc_psig_recmodule
             (module_declaration_list sub_0) : A.state)
      | Psig_modtype sub_0 ->
          (Match.signature_item_desc_psig_modtype
             (module_type_declaration sub_0) : A.state)
      | Psig_open sub_0 ->
          (Match.signature_item_desc_psig_open (open_description sub_0) : 
          A.state)
      | Psig_include sub_0 ->
          (Match.signature_item_desc_psig_include (include_description sub_0) : 
          A.state)
      | Psig_class sub_0 ->
          (Match.signature_item_desc_psig_class
             (class_description_list sub_0) : A.state)
      | Psig_class_type sub_0 ->
          (Match.signature_item_desc_psig_class_type
             (class_type_declaration_list sub_0) : A.state)
      | Psig_attribute sub_0 ->
          (Match.signature_item_desc_psig_attribute (attribute sub_0) : 
          A.state)
      | Psig_extension (sub_0,sub_1) ->
          (Match.signature_item_desc_psig_extension (extension sub_0)
             (attributes sub_1) : A.state)
    and module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc } =
      Match.module_declaration (string_loc pmd_name) (module_type pmd_type)
        (attributes pmd_attributes) (location__t pmd_loc)
    and module_type_declaration
      { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } =
      Match.module_type_declaration (string_loc pmtd_name)
        (module_type_option pmtd_type) (attributes pmtd_attributes)
        (location__t pmtd_loc)
    and open_description
      { popen_lid; popen_override; popen_loc; popen_attributes } =
      Match.open_description (longident__t_loc popen_lid)
        (override_flag popen_override) (location__t popen_loc)
        (attributes popen_attributes)
    and include_description { pincl_mod; pincl_loc; pincl_attributes } =
      Match.include_description (module_type pincl_mod)
        (location__t pincl_loc) (attributes pincl_attributes)
    and include_declaration { pincl_mod; pincl_loc; pincl_attributes } =
      Match.include_declaration (module_expr pincl_mod)
        (location__t pincl_loc) (attributes pincl_attributes)
    and with_constraint =
      function
      | Pwith_type (sub_0,sub_1) ->
          (Match.with_constraint_pwith_type (longident__t_loc sub_0)
             (type_declaration sub_1) : A.state)
      | Pwith_module (sub_0,sub_1) ->
          (Match.with_constraint_pwith_module (longident__t_loc sub_0)
             (longident__t_loc sub_1) : A.state)
      | Pwith_typesubst sub_0 ->
          (Match.with_constraint_pwith_typesubst (type_declaration sub_0) : 
          A.state)
      | Pwith_modsubst (sub_0,sub_1) ->
          (Match.with_constraint_pwith_modsubst (string_loc sub_0)
             (longident__t_loc sub_1) : A.state)
    and module_expr { pmod_desc; pmod_loc; pmod_attributes } =
      Match.module_expr (module_expr_desc pmod_desc) (location__t pmod_loc)
        (attributes pmod_attributes)
    and module_expr_desc =
      function
      | Pmod_ident sub_0 ->
          (Match.module_expr_desc_pmod_ident (longident__t_loc sub_0) : 
          A.state)
      | Pmod_structure sub_0 ->
          (Match.module_expr_desc_pmod_structure (structure sub_0) : 
          A.state)
      | Pmod_functor (sub_0,sub_1,sub_2) ->
          (Match.module_expr_desc_pmod_functor (string_loc sub_0)
             (module_type_option sub_1) (module_expr sub_2) : A.state)
      | Pmod_apply (sub_0,sub_1) ->
          (Match.module_expr_desc_pmod_apply (module_expr sub_0)
             (module_expr sub_1) : A.state)
      | Pmod_constraint (sub_0,sub_1) ->
          (Match.module_expr_desc_pmod_constraint (module_expr sub_0)
             (module_type sub_1) : A.state)
      | Pmod_unpack sub_0 ->
          (Match.module_expr_desc_pmod_unpack (expression sub_0) : A.state)
      | Pmod_extension sub_0 ->
          (Match.module_expr_desc_pmod_extension (extension sub_0) : 
          A.state)
    and structure =
      function
      | [] -> (Match.structure_nil : A.state)
      | sub_0::sub_1 ->
          (Match.structure_cons (structure_item sub_0)
             (structure_item_list sub_1) : A.state)
    and structure_item { pstr_desc; pstr_loc } =
      Match.structure_item (structure_item_desc pstr_desc)
        (location__t pstr_loc)
    and structure_item_desc =
      function
      | Pstr_eval (sub_0,sub_1) ->
          (Match.structure_item_desc_pstr_eval (expression sub_0)
             (attributes sub_1) : A.state)
      | Pstr_value (sub_0,sub_1) ->
          (Match.structure_item_desc_pstr_value (rec_flag sub_0)
             (value_binding_list sub_1) : A.state)
      | Pstr_primitive sub_0 ->
          (Match.structure_item_desc_pstr_primitive (value_description sub_0) : 
          A.state)
      | Pstr_type sub_0 ->
          (Match.structure_item_desc_pstr_type (type_declaration_list sub_0) : 
          A.state)
      | Pstr_typext sub_0 ->
          (Match.structure_item_desc_pstr_typext (type_extension sub_0) : 
          A.state)
      | Pstr_exception sub_0 ->
          (Match.structure_item_desc_pstr_exception
             (extension_constructor sub_0) : A.state)
      | Pstr_module sub_0 ->
          (Match.structure_item_desc_pstr_module (module_binding sub_0) : 
          A.state)
      | Pstr_recmodule sub_0 ->
          (Match.structure_item_desc_pstr_recmodule
             (module_binding_list sub_0) : A.state)
      | Pstr_modtype sub_0 ->
          (Match.structure_item_desc_pstr_modtype
             (module_type_declaration sub_0) : A.state)
      | Pstr_open sub_0 ->
          (Match.structure_item_desc_pstr_open (open_description sub_0) : 
          A.state)
      | Pstr_class sub_0 ->
          (Match.structure_item_desc_pstr_class
             (class_declaration_list sub_0) : A.state)
      | Pstr_class_type sub_0 ->
          (Match.structure_item_desc_pstr_class_type
             (class_type_declaration_list sub_0) : A.state)
      | Pstr_include sub_0 ->
          (Match.structure_item_desc_pstr_include (include_declaration sub_0) : 
          A.state)
      | Pstr_attribute sub_0 ->
          (Match.structure_item_desc_pstr_attribute (attribute sub_0) : 
          A.state)
      | Pstr_extension (sub_0,sub_1) ->
          (Match.structure_item_desc_pstr_extension (extension sub_0)
             (attributes sub_1) : A.state)
    and value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } =
      Match.value_binding (pattern pvb_pat) (expression pvb_expr)
        (attributes pvb_attributes) (location__t pvb_loc)
    and module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc } =
      Match.module_binding (string_loc pmb_name) (module_expr pmb_expr)
        (attributes pmb_attributes) (location__t pmb_loc)
    and unit = Match.unit
    and bool = Match.bool
    and int = Match.int
    and char = Match.char
    and string = Match.string
    and int32 = Match.int32
    and int64 = Match.int64
    and nativeint = Match.nativeint
    and lexing__position { pos_fname; pos_lnum; pos_bol; pos_cnum } =
      Match.lexing__position (string pos_fname) (int pos_lnum) (int pos_bol)
        (int pos_cnum)
    and location__t { loc_start; loc_end; loc_ghost } =
      Match.location__t (lexing__position loc_start)
        (lexing__position loc_end) (bool loc_ghost)
    and longident__t =
      function
      | Lident sub_0 -> (Match.longident__t_lident (string sub_0) : A.state)
      | Ldot (sub_0,sub_1) ->
          (Match.longident__t_ldot (longident__t sub_0) (string sub_1) : 
          A.state)
      | Lapply (sub_0,sub_1) ->
          (Match.longident__t_lapply (longident__t sub_0)
             (longident__t sub_1) : A.state)
    and constant =
      function
      | Const_int sub_0 -> (Match.constant_const_int (int sub_0) : A.state)
      | Const_char sub_0 ->
          (Match.constant_const_char (char sub_0) : A.state)
      | Const_string (sub_0,sub_1) ->
          (Match.constant_const_string (string sub_0) (string_option sub_1) : 
          A.state)
      | Const_float sub_0 ->
          (Match.constant_const_float (string sub_0) : A.state)
      | Const_int32 sub_0 ->
          (Match.constant_const_int32 (int32 sub_0) : A.state)
      | Const_int64 sub_0 ->
          (Match.constant_const_int64 (int64 sub_0) : A.state)
      | Const_nativeint sub_0 ->
          (Match.constant_const_nativeint (nativeint sub_0) : A.state)
    and rec_flag =
      function
      | Nonrecursive  -> (Match.rec_flag_nonrecursive : A.state)
      | Recursive  -> (Match.rec_flag_recursive : A.state)
    and direction_flag =
      function
      | Upto  -> (Match.direction_flag_upto : A.state)
      | Downto  -> (Match.direction_flag_downto : A.state)
    and private_flag =
      function
      | Private  -> (Match.private_flag_private : A.state)
      | Public  -> (Match.private_flag_public : A.state)
    and mutable_flag =
      function
      | Immutable  -> (Match.mutable_flag_immutable : A.state)
      | Mutable  -> (Match.mutable_flag_mutable : A.state)
    and virtual_flag =
      function
      | Virtual  -> (Match.virtual_flag_virtual : A.state)
      | Concrete  -> (Match.virtual_flag_concrete : A.state)
    and override_flag =
      function
      | Override  -> (Match.override_flag_override : A.state)
      | Fresh  -> (Match.override_flag_fresh : A.state)
    and closed_flag =
      function
      | Closed  -> (Match.closed_flag_closed : A.state)
      | Open  -> (Match.closed_flag_open : A.state)
    and label = Match.label
    and variance =
      function
      | Covariant  -> (Match.variance_covariant : A.state)
      | Contravariant  -> (Match.variance_contravariant : A.state)
      | Invariant  -> (Match.variance_invariant : A.state)
    and attribute_list =
      function
      | [] -> (Match.attribute_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.attribute_list_cons (attribute sub_0) (attribute_list sub_1) : 
          A.state)
    and case_list =
      function
      | [] -> (Match.case_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.case_list_cons (case sub_0) (case_list sub_1) : A.state)
    and class_declaration_list =
      function
      | [] -> (Match.class_declaration_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.class_declaration_list_cons (class_declaration sub_0)
             (class_declaration_list sub_1) : A.state)
    and class_description_list =
      function
      | [] -> (Match.class_description_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.class_description_list_cons (class_description sub_0)
             (class_description_list sub_1) : A.state)
    and class_expr_class_infos
      { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } =
      Match.class_expr_class_infos (virtual_flag pci_virt)
        (variance_core_type_tuple_2_list pci_params) (string_loc pci_name)
        (class_expr pci_expr) (location__t pci_loc)
        (attributes pci_attributes)
    and class_field_kind_mutable_flag_string_loc_tuple_3 (sub_0,sub_1,sub_2)
      =
      Match.class_field_kind_mutable_flag_string_loc_tuple_3
        (string_loc sub_0) (mutable_flag sub_1) (class_field_kind sub_2)
    and class_field_kind_private_flag_string_loc_tuple_3 (sub_0,sub_1,sub_2)
      =
      Match.class_field_kind_private_flag_string_loc_tuple_3
        (string_loc sub_0) (private_flag sub_1) (class_field_kind sub_2)
    and class_field_list =
      function
      | [] -> (Match.class_field_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.class_field_list_cons (class_field sub_0)
             (class_field_list sub_1) : A.state)
    and class_type_class_infos
      { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } =
      Match.class_type_class_infos (virtual_flag pci_virt)
        (variance_core_type_tuple_2_list pci_params) (string_loc pci_name)
        (class_type pci_expr) (location__t pci_loc)
        (attributes pci_attributes)
    and class_type_declaration_list =
      function
      | [] -> (Match.class_type_declaration_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.class_type_declaration_list_cons
             (class_type_declaration sub_0)
             (class_type_declaration_list sub_1) : A.state)
    and class_type_field_list =
      function
      | [] -> (Match.class_type_field_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.class_type_field_list_cons (class_type_field sub_0)
             (class_type_field_list sub_1) : A.state)
    and constructor_declaration_list =
      function
      | [] -> (Match.constructor_declaration_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.constructor_declaration_list_cons
             (constructor_declaration sub_0)
             (constructor_declaration_list sub_1) : A.state)
    and core_type_Longident__t_loc_tuple_2 (sub_0,sub_1) =
      Match.core_type_Longident__t_loc_tuple_2 (longident__t_loc sub_0)
        (core_type sub_1)
    and core_type_Longident__t_loc_tuple_2_list =
      function
      | [] -> (Match.core_type_Longident__t_loc_tuple_2_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.core_type_Longident__t_loc_tuple_2_list_cons
             (core_type_Longident__t_loc_tuple_2 sub_0)
             (core_type_Longident__t_loc_tuple_2_list sub_1) : A.state)
    and core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2
      (sub_0,sub_1) =
      Match.core_type_Longident__t_loc_tuple_2_list_Longident__t_loc_tuple_2
        (longident__t_loc sub_0)
        (core_type_Longident__t_loc_tuple_2_list sub_1)
    and core_type_attributes_string_tuple_3 (sub_0,sub_1,sub_2) =
      Match.core_type_attributes_string_tuple_3 (string sub_0)
        (attributes sub_1) (core_type sub_2)
    and core_type_attributes_string_tuple_3_list =
      function
      | [] -> (Match.core_type_attributes_string_tuple_3_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.core_type_attributes_string_tuple_3_list_cons
             (core_type_attributes_string_tuple_3 sub_0)
             (core_type_attributes_string_tuple_3_list sub_1) : A.state)
    and core_type_core_type_tuple_2 (sub_0,sub_1) =
      Match.core_type_core_type_tuple_2 (core_type sub_0) (core_type sub_1)
    and core_type_list =
      function
      | [] -> (Match.core_type_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.core_type_list_cons (core_type sub_0) (core_type_list sub_1) : 
          A.state)
    and core_type_option =
      function
      | None  -> (Match.core_type_option_none : A.state)
      | Some sub_0 ->
          (Match.core_type_option_some (core_type sub_0) : A.state)
    and core_type_virtual_flag_mutable_flag_string_tuple_4
      (sub_0,sub_1,sub_2,sub_3) =
      Match.core_type_virtual_flag_mutable_flag_string_tuple_4 (string sub_0)
        (mutable_flag sub_1) (virtual_flag sub_2) (core_type sub_3)
    and core_type_virtual_flag_private_flag_string_tuple_4
      (sub_0,sub_1,sub_2,sub_3) =
      Match.core_type_virtual_flag_private_flag_string_tuple_4 (string sub_0)
        (private_flag sub_1) (virtual_flag sub_2) (core_type sub_3)
    and expression_Longident__t_loc_tuple_2 (sub_0,sub_1) =
      Match.expression_Longident__t_loc_tuple_2 (longident__t_loc sub_0)
        (expression sub_1)
    and expression_Longident__t_loc_tuple_2_list =
      function
      | [] -> (Match.expression_Longident__t_loc_tuple_2_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.expression_Longident__t_loc_tuple_2_list_cons
             (expression_Longident__t_loc_tuple_2 sub_0)
             (expression_Longident__t_loc_tuple_2_list sub_1) : A.state)
    and expression_label_tuple_2 (sub_0,sub_1) =
      Match.expression_label_tuple_2 (label sub_0) (expression sub_1)
    and expression_label_tuple_2_list =
      function
      | [] -> (Match.expression_label_tuple_2_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.expression_label_tuple_2_list_cons
             (expression_label_tuple_2 sub_0)
             (expression_label_tuple_2_list sub_1) : A.state)
    and expression_list =
      function
      | [] -> (Match.expression_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.expression_list_cons (expression sub_0)
             (expression_list sub_1) : A.state)
    and expression_option =
      function
      | None  -> (Match.expression_option_none : A.state)
      | Some sub_0 ->
          (Match.expression_option_some (expression sub_0) : A.state)
    and expression_string_loc_tuple_2 (sub_0,sub_1) =
      Match.expression_string_loc_tuple_2 (string_loc sub_0)
        (expression sub_1)
    and expression_string_loc_tuple_2_list =
      function
      | [] -> (Match.expression_string_loc_tuple_2_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.expression_string_loc_tuple_2_list_cons
             (expression_string_loc_tuple_2 sub_0)
             (expression_string_loc_tuple_2_list sub_1) : A.state)
    and extension_constructor_list =
      function
      | [] -> (Match.extension_constructor_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.extension_constructor_list_cons
             (extension_constructor sub_0) (extension_constructor_list sub_1) : 
          A.state)
    and label_declaration_list =
      function
      | [] -> (Match.label_declaration_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.label_declaration_list_cons (label_declaration sub_0)
             (label_declaration_list sub_1) : A.state)
    and label_list =
      function
      | [] -> (Match.label_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.label_list_cons (label sub_0) (label_list sub_1) : A.state)
    and label_list_option =
      function
      | None  -> (Match.label_list_option_none : A.state)
      | Some sub_0 ->
          (Match.label_list_option_some (label_list sub_0) : A.state)
    and location__t_core_type_core_type_tuple_3 (sub_0,sub_1,sub_2) =
      Match.location__t_core_type_core_type_tuple_3 (core_type sub_0)
        (core_type sub_1) (location__t sub_2)
    and location__t_core_type_core_type_tuple_3_list =
      function
      | [] ->
          (Match.location__t_core_type_core_type_tuple_3_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.location__t_core_type_core_type_tuple_3_list_cons
             (location__t_core_type_core_type_tuple_3 sub_0)
             (location__t_core_type_core_type_tuple_3_list sub_1) : A.state)
    and longident__t_loc { txt; loc } =
      Match.longident__t_loc (longident__t txt) (location__t loc)
    and module_binding_list =
      function
      | [] -> (Match.module_binding_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.module_binding_list_cons (module_binding sub_0)
             (module_binding_list sub_1) : A.state)
    and module_declaration_list =
      function
      | [] -> (Match.module_declaration_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.module_declaration_list_cons (module_declaration sub_0)
             (module_declaration_list sub_1) : A.state)
    and module_expr_include_infos { pincl_mod; pincl_loc; pincl_attributes }
      =
      Match.module_expr_include_infos (module_expr pincl_mod)
        (location__t pincl_loc) (attributes pincl_attributes)
    and module_type_include_infos { pincl_mod; pincl_loc; pincl_attributes }
      =
      Match.module_type_include_infos (module_type pincl_mod)
        (location__t pincl_loc) (attributes pincl_attributes)
    and module_type_option =
      function
      | None  -> (Match.module_type_option_none : A.state)
      | Some sub_0 ->
          (Match.module_type_option_some (module_type sub_0) : A.state)
    and pattern_Longident__t_loc_tuple_2 (sub_0,sub_1) =
      Match.pattern_Longident__t_loc_tuple_2 (longident__t_loc sub_0)
        (pattern sub_1)
    and pattern_Longident__t_loc_tuple_2_list =
      function
      | [] -> (Match.pattern_Longident__t_loc_tuple_2_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.pattern_Longident__t_loc_tuple_2_list_cons
             (pattern_Longident__t_loc_tuple_2 sub_0)
             (pattern_Longident__t_loc_tuple_2_list sub_1) : A.state)
    and pattern_list =
      function
      | [] -> (Match.pattern_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.pattern_list_cons (pattern sub_0) (pattern_list sub_1) : 
          A.state)
    and pattern_option =
      function
      | None  -> (Match.pattern_option_none : A.state)
      | Some sub_0 -> (Match.pattern_option_some (pattern sub_0) : A.state)
    and payload_string_loc_tuple_2 (sub_0,sub_1) =
      Match.payload_string_loc_tuple_2 (string_loc sub_0) (payload sub_1)
    and row_field_list =
      function
      | [] -> (Match.row_field_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.row_field_list_cons (row_field sub_0) (row_field_list sub_1) : 
          A.state)
    and signature_item_list =
      function
      | [] -> (Match.signature_item_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.signature_item_list_cons (signature_item sub_0)
             (signature_item_list sub_1) : A.state)
    and string_list =
      function
      | [] -> (Match.string_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.string_list_cons (string sub_0) (string_list sub_1) : 
          A.state)
    and string_loc { txt; loc } =
      Match.string_loc (string txt) (location__t loc)
    and string_option =
      function
      | None  -> (Match.string_option_none : A.state)
      | Some sub_0 -> (Match.string_option_some (string sub_0) : A.state)
    and structure_item_list =
      function
      | [] -> (Match.structure_item_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.structure_item_list_cons (structure_item sub_0)
             (structure_item_list sub_1) : A.state)
    and type_declaration_list =
      function
      | [] -> (Match.type_declaration_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.type_declaration_list_cons (type_declaration sub_0)
             (type_declaration_list sub_1) : A.state)
    and value_binding_list =
      function
      | [] -> (Match.value_binding_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.value_binding_list_cons (value_binding sub_0)
             (value_binding_list sub_1) : A.state)
    and variance_core_type_tuple_2 (sub_0,sub_1) =
      Match.variance_core_type_tuple_2 (core_type sub_0) (variance sub_1)
    and variance_core_type_tuple_2_list =
      function
      | [] -> (Match.variance_core_type_tuple_2_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.variance_core_type_tuple_2_list_cons
             (variance_core_type_tuple_2 sub_0)
             (variance_core_type_tuple_2_list sub_1) : A.state)
    and with_constraint_list =
      function
      | [] -> (Match.with_constraint_list_nil : A.state)
      | sub_0::sub_1 ->
          (Match.with_constraint_list_cons (with_constraint sub_0)
             (with_constraint_list sub_1) : A.state)
  end
