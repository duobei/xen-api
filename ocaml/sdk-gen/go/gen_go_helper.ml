(* Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

(* Generator of Go bindings from the datamodel *)

open Datamodel_types
open CommonFunctions
open Xapi_stdext_std

type fields = string * field list

let templates_dir = "templates"

let ( // ) = Filename.concat

let special_words =
  [
    "id"
  ; "ip"
  ; "vm"
  ; "api"
  ; "uuid"
  ; "cpu"
  ; "tls"
  ; "https"
  ; "url"
  ; "db"
  ; "xml"
  ; "eof"
  ]

let snake_to_camel (s : string) : string =
  String.split_on_char '_' s
  |> List.map (fun s -> String.split_on_char '-' s)
  |> List.concat
  |> List.map (fun s ->
         match s with
         | s when List.mem s special_words ->
             String.uppercase_ascii s
         | _ ->
             String.capitalize_ascii s
     )
  |> String.concat ""

let render_template template_file json ?(newline = false) () =
  let templ =
    string_of_file (templates_dir // template_file) |> Mustache.of_string
  in
  let renndered = Mustache.render templ json in
  if newline then renndered ^ "\n" else renndered

let generate_file ~rendered ~destdir ~output_file =
  let out_chan = open_out (destdir // output_file) in
  Fun.protect
    (fun () -> output_string out_chan rendered)
    ~finally:(fun () -> close_out out_chan)

let records =
  List.map
    (fun obj ->
      let obj_name = snake_to_camel obj.name ^ "Record" in
      (obj_name, Datamodel_utils.fields_of_obj obj)
    )
    objects

module StringSet = Set.Make (String)

module Json = struct
  type enum = (string * string) list

  module StringMap = Map.Make (String)

  type enums = enum StringMap.t

  let choose_enum _key a _b = Some a

  let merge_maps m maps =
    List.fold_left (fun acc map -> StringMap.union choose_enum acc map) m maps

  let rec get_fp_type ty =
    match ty with
    | SecretString | String ->
        "String"
    | Int ->
        "Int"
    | Float ->
        "Float"
    | Bool ->
        "Bool"
    | DateTime ->
        "Time"
    | Enum (name, _) ->
        "Enum" ^ snake_to_camel name
    | Set ty ->
        get_fp_type ty ^ "Set"
    | Map (ty1, ty2) ->
        let fp_type1 = get_fp_type ty1 in
        let fp_type2 = get_fp_type ty2 in
        fp_type1 ^ "To" ^ fp_type2 ^ "Map"
    | Ref r ->
        snake_to_camel r ^ "Ref"
    | Record r ->
        snake_to_camel r ^ "Record"
    | Option ty ->
        get_fp_type ty

  let rec string_of_ty_with_enums ty : string * enums =
    match ty with
    | SecretString | String ->
        ("string", StringMap.empty)
    | Int ->
        ("int", StringMap.empty)
    | Float ->
        ("float64", StringMap.empty)
    | Bool ->
        ("bool", StringMap.empty)
    | DateTime ->
        ("time.Time", StringMap.empty)
    | Enum (name, kv) ->
        let name = snake_to_camel name in
        (name, StringMap.singleton name kv)
    | Set ty ->
        let name, e = string_of_ty_with_enums ty in
        ("[]" ^ name, e)
    | Map (ty1, ty2) ->
        let s1, e1 = string_of_ty_with_enums ty1 in
        let s2, e2 = string_of_ty_with_enums ty2 in
        let name = "map[" ^ s1 ^ "]" ^ s2 in
        (name, StringMap.union choose_enum e1 e2)
    | Ref r ->
        let name = snake_to_camel r ^ "Ref" in
        (name, StringMap.empty)
    | Record r ->
        let name = snake_to_camel r ^ "Record" in
        (name, StringMap.empty)
    | Option ty ->
        let _, e = string_of_ty_with_enums ty in
        let name = get_fp_type ty in
        ("Option" ^ name, e)

  let of_enum name vs =
    let name = snake_to_camel name in
    let of_value (v, d) =
      `O
        [
          ("value", `String v)
        ; ("doc", `String d)
        ; ("name", `String (name ^ snake_to_camel v))
        ; ("type", `String name)
        ]
    in
    `O [("name", `String name); ("values", `A (List.map of_value vs))]

  let of_field field =
    let concat_and_convert field =
      let concated =
        String.concat "" (List.map snake_to_camel field.full_name)
      in
      match concated with
      | "Uuid" | "Id" ->
          String.uppercase_ascii concated
      | _ ->
          concated
    in
    let ty, _e = string_of_ty_with_enums field.ty in
    `O
      [
        ("name", `String (concat_and_convert field))
      ; ("description", `String (String.trim field.field_description))
      ; ("type", `String ty)
      ]

  let modules_of_type = function
    | DateTime ->
        [`O [("name", `String "time"); ("sname", `Null)]]
    | _ ->
        []

  let modules_of_types types =
    let common = [`O [("name", `String "fmt"); ("sname", `Null)]] in
    let items =
      List.map modules_of_type types |> List.concat |> List.append common
    in
    `O [("import", `Bool true); ("items", `A items)]

  let all_enums objs =
    let enums =
      Datamodel_utils.Types.of_objects objs
      |> List.map (fun ty ->
             let _, e = string_of_ty_with_enums ty in
             e
         )
      |> merge_maps StringMap.empty
    in
    `O
      [
        ( "enums"
        , `A (StringMap.fold (fun k v acc -> of_enum k v :: acc) enums [])
        )
      ]

  let get_event_snapshot name =
    if String.lowercase_ascii name = "event" then
      [
        `O
          [
            ("name", `String "Snapshot")
          ; ( "description"
            , `String
                "The record of the database object that was added, changed or \
                 deleted"
            )
          ; ("type", `String "RecordInterface")
          ]
      ]
    else
      []

  let get_event_session_value = function
    | "event" ->
        [("event", `Bool true); ("session", `Null)]
    | "session" ->
        [("event", `Null); ("session", `Bool true)]
    | _ ->
        [("event", `Null); ("session", `Null)]

  let of_result obj msg =
    match msg.msg_result with
    | None ->
        `Null
    | Some (t, _d) ->
        if obj.name = "event" && String.lowercase_ascii msg.msg_name = "from"
        then
          `O
            [
              ("type", `String "EventBatch")
            ; ("func_partial_type", `String "EventBatch")
            ]
        else
          let t', _ = string_of_ty_with_enums t in
          `O
            [
              ("type", `String t')
            ; ("func_partial_type", `String (get_fp_type t))
            ]

  let group_params params =
    List.fold_left
      (fun groups (param_release, info) ->
        match groups with
        | [] ->
            [(param_release, [info])]
        | group :: tl -> (
          match group with
          | pr, infos when pr = param_release ->
              (pr, info :: infos) :: tl
          | _ ->
              (param_release, [info]) :: groups
        )
      )
      [] params

  let scan_left func init list =
    let rec aux func init list result =
      match list with
      | [] ->
          result
      | x :: xs ->
          let init = func init x in
          aux func init xs (init :: result)
    in
    List.rev @@ aux func init list []

  let combine_groups groups =
    let scans =
      scan_left
        (fun (len, group) (_, infos) -> (len + List.length infos, group @ infos))
        (0, []) groups
    in
    match scans with [] -> [] | [x] -> [x] | _ :: xs -> xs

  let rec get_last = function
    | [] ->
        failwith "empty list"
    | [x] ->
        x
    | _ :: tl ->
        get_last tl

  let of_param_groups class_name params =
    let name_internal name =
      let name = name |> snake_to_camel |> String.uncapitalize_ascii in
      match name with "type" -> "typeKey" | "interface" -> "inter" | _ -> name
    in
    let base_assoc_list (t, name, doc, type_name) =
      [
        ("session", `Bool (name = "session_id"))
      ; ("type", `String t)
      ; ("name", `String name)
      ; ("name_internal", `String (name_internal name))
      ; ("doc", `String doc)
      ; ("func_partial_type", `String type_name)
      ]
    in
    let deal_with_logout name =
      match (class_name, name) with
      | "session", "session_id" ->
          ([("param_ignore", `Bool true); ("session_class", `Bool true)], true)
      | "session", _ ->
          ([("param_ignore", `Bool false); ("session_class", `Bool true)], false)
      | _ ->
          ([("param_ignore", `Null); ("session_class", `Null)], false)
    in
    let get_assoc_list (t, name, doc, type_name) =
      let fields, could_ignore = deal_with_logout name in
      (fields @ base_assoc_list (t, name, doc, type_name), could_ignore)
    in
    let rec add_first = function
      | head :: rest ->
          let assoc_list, could_ignore = get_assoc_list head in
          let assoc_list = ("first", `Bool (not could_ignore)) :: assoc_list in
          let others =
            if could_ignore then
              add_first rest
            else
              List.map
                (fun item ->
                  let assoc_list, _ = get_assoc_list item in
                  `O (("first", `Bool false) :: assoc_list)
                )
                rest
          in
          `O assoc_list :: others
      | [] ->
          []
    in
    let groups =
      params
      |> List.rev_map (fun p ->
             let fp_type = get_fp_type p.param_type in
             let t, _e = string_of_ty_with_enums p.param_type in
             (p.param_release, (t, p.param_name, p.param_doc, fp_type))
         )
      |> group_params
      |> combine_groups
      |> List.map (fun (num, params) -> (num, `A (add_first params)))
    in
    match get_last groups with
    | 0, _ ->
        groups
    | _, last ->
        groups @ [(0, last)]

  let of_error e = `O [("name", `String e.err_name); ("doc", `String e.err_doc)]

  let of_errors = function
    | [] ->
        `Null
    | errors ->
        `A (List.map of_error errors)

  let add_session_info class_name method_name =
    match (class_name, method_name) with
    | "session", "login_with_password"
    | "session", "slave_local_login_with_password" ->
        [
          ("session_class", `Bool true)
        ; ("session_login", `Bool true)
        ; ("session_logout", `Bool false)
        ]
    | "session", "logout" | "session", "local_logout" ->
        [
          ("session_class", `Bool true)
        ; ("session_login", `Bool false)
        ; ("session_logout", `Bool true)
        ]
    | "session", _ ->
        [
          ("session_class", `Bool true)
        ; ("session_login", `Bool false)
        ; ("session_logout", `Bool false)
        ]
    | _ ->
        [
          ("session_class", `Bool false)
        ; ("session_login", `Bool false)
        ; ("session_logout", `Bool false)
        ]

  let desc_of_msg msg ctor_fields =
    let ctor =
      if msg.msg_tag = FromObject Make then
        Printf.sprintf " The constructor args are: %s (* = non-optional)."
          ctor_fields
      else
        ""
    in
    match msg.msg_doc ^ ctor with
    | "" ->
        `Null
    | desc ->
        `String (String.trim desc)

  let ctor_fields_of_obj obj =
    Datamodel_utils.fields_of_obj obj
    |> List.filter (function
         | {qualifier= StaticRO | RW; _} ->
             true
         | _ ->
             false
         )
    |> List.map (fun f ->
           String.concat "_" f.full_name
           ^ if f.default_value = None then "*" else ""
       )
    |> String.concat ", "

  let messages_of_obj obj =
    let ctor_fields = ctor_fields_of_obj obj in
    let method_name_exported obj msg num =
      let method_name = snake_to_camel msg.msg_name in
      let not_ends_with str suffix = not (String.ends_with str ~suffix) in
      let num =
        match (obj.name, msg.msg_name) with
        | "session", method_name
          when not_ends_with method_name "login_with_password" && num > 0 ->
            num - 1
        | _ ->
            num
      in
      match num with 0 -> method_name | _ -> method_name ^ string_of_int num
    in
    let params_in_msg msg =
      if msg.msg_session then
        session_id :: msg.msg_params
      else
        msg.msg_params
    in
    obj.messages
    |> List.rev_map (fun msg ->
           let of_message (num, params) =
             let base_assoc_list =
               [
                 ("method_name", `String msg.msg_name)
               ; ("class_name", `String obj.name)
               ; ("class_name_exported", `String (snake_to_camel obj.name))
               ; ( "method_name_exported"
                 , `String (method_name_exported obj msg num)
                 )
               ; ("description", desc_of_msg msg ctor_fields)
               ; ("result", of_result obj msg)
               ; ("params", params)
               ; ("errors", of_errors msg.msg_errors)
               ; ("has_error", `Bool (msg.msg_errors <> []))
               ; ("async", `Bool msg.msg_async)
               ]
             in
             `O (add_session_info obj.name msg.msg_name @ base_assoc_list)
           in
           params_in_msg msg |> of_param_groups obj.name |> List.map of_message
       )
    |> List.concat
    |> Listext.List.setify

  let of_option ty =
    let name, _ = string_of_ty_with_enums ty in
    `O [("type", `String name); ("func_partial_type", `String (get_fp_type ty))]

  let of_options types =
    types
    |> List.filter_map (fun ty ->
           match ty with Option ty -> Some ty | _ -> None
       )
    |> List.map of_option

  let xenapi objs =
    List.map
      (fun obj ->
        let obj_name = snake_to_camel obj.name in
        let name_internal = String.uncapitalize_ascii obj_name in
        let fields = Datamodel_utils.fields_of_obj obj in
        let types =
          List.map (fun field -> field.ty) fields |> Listext.List.setify
        in
        let modules =
          match obj.messages with [] -> `Null | _ -> modules_of_types types
        in
        let base_assoc_list =
          [
            ("name", `String obj_name)
          ; ("name_internal", `String name_internal)
          ; ("description", `String (String.trim obj.description))
          ; ( "fields"
            , `A (get_event_snapshot obj.name @ List.map of_field fields)
            )
          ; ("modules", modules)
          ; ("messages", `A (messages_of_obj obj))
          ; ("option", `A (of_options types))
          ]
        in
        let assoc_list = base_assoc_list @ get_event_session_value obj.name in
        (String.lowercase_ascii obj.name, `O assoc_list)
      )
      objs

  let of_api_message_or_error info =
    let snake_to_camel (s : string) : string =
      String.split_on_char '_' s
      |> List.map (fun seg ->
             let lower = String.lowercase_ascii seg in
             match lower with
             | s when List.mem s special_words ->
                 String.uppercase_ascii lower
             | _ ->
                 String.capitalize_ascii lower
         )
      |> String.concat ""
    in
    `O [("name", `String (snake_to_camel info)); ("value", `String info)]

  let api_messages =
    List.map (fun (msg, _) -> of_api_message_or_error msg) !Api_messages.msgList

  let api_errors = List.map of_api_message_or_error !Api_errors.errors
end

module Types = struct
  include Datamodel_utils.Types

  let rec decompose = function
    | Set x as y ->
        y :: decompose x
    | Map (a, b) as y ->
        (y :: decompose a) @ decompose b
    | Option x as y ->
        let name, _ = Json.string_of_ty_with_enums y in
        print_endline ("option: " ^ name) ;
        y :: decompose x
    | Record r as y ->
        let name = snake_to_camel r ^ "Record" in
        let types_in_field =
          List.assoc_opt name records
          |> Option.value ~default:[]
          |> List.map (fun field -> decompose field.ty)
          |> List.concat
        in
        y :: types_in_field
    | (SecretString | String | Int | Float | DateTime | Enum _ | Bool | Ref _)
      as x ->
        [x]

  let mesages objects = objects |> List.map (fun x -> x.messages) |> List.concat

  (** All types of params in a list of objects (automatically decomposes) *)
  let of_params objects =
    let param_types =
      mesages objects
      |> List.map (fun x -> x.msg_params)
      |> List.concat
      |> List.map (fun p -> p.param_type)
      |> Listext.List.setify
    in
    List.concat_map decompose param_types |> Listext.List.setify

  (** All types of results in a list of objects (automatically decomposes) *)
  let of_results objects =
    let return_types =
      let aux accu msg =
        match msg.msg_result with None -> accu | Some (ty, _) -> ty :: accu
      in
      mesages objects |> List.fold_left aux [] |> Listext.List.setify
    in
    List.concat_map decompose return_types |> Listext.List.setify
end

module Convert = struct
  type params = {fp_type: string; value_ty: string}

  type params_of_option = {fp_type: string}

  type params_of_set = {fp_type: string; value_ty: string; item_fp_type: string}

  type params_of_record_field = {
      name: string
    ; name_internal: string
    ; name_exported: string
    ; fp_type: string
    ; type_option: bool
  }

  type params_of_record = {
      fp_type: string
    ; value_ty: string
    ; fields: params_of_record_field list
  }

  type params_of_enum_item = {value: string; name: string}

  type params_of_enum = {
      fp_type: string
    ; value_ty: string
    ; items: params_of_enum_item list
  }

  type params_of_map = {
      fp_type: string
    ; value_ty: string
    ; key_ty: string
    ; val_ty: string
  }

  type convert_params =
    | Simple of params
    | Int of params
    | Float of params
    | Time of params
    | Ref of params
    | Option of params_of_option
    | Set of params_of_set
    | Enum of params_of_enum
    | Record of params_of_record
    | Map of params_of_map

  let template_of_convert : convert_params -> string = function
    | Simple _ ->
        "ConvertSimpleType.mustache"
    | Int _ ->
        "ConvertInt.mustache"
    | Float _ ->
        "ConvertFloat.mustache"
    | Time _ ->
        "ConvertTime.mustache"
    | Ref _ ->
        "ConvertRef.mustache"
    | Set _ ->
        "ConvertSet.mustache"
    | Record _ ->
        "ConvertRecord.mustache"
    | Map _ ->
        "ConvertMap.mustache"
    | Enum _ ->
        "ConvertEnum.mustache"
    | Option _ ->
        "ConvertOption.mustache"

  let to_json : convert_params -> Mustache.Json.value = function
    | Simple params | Int params | Float params | Time params | Ref params ->
        `O
          [
            ("func_partial_type", `String params.fp_type)
          ; ("type", `String params.value_ty)
          ]
    | Option params ->
        `O [("func_partial_type", `String params.fp_type)]
    | Set params ->
        `O
          [
            ("func_partial_type", `String params.fp_type)
          ; ("type", `String params.value_ty)
          ; ("item_func_partial_type", `String params.item_fp_type)
          ]
    | Record params ->
        let fields =
          List.rev_map
            (fun (field : params_of_record_field) ->
              `O
                [
                  ("name", `String field.name)
                ; ("name_internal", `String field.name_internal)
                ; ("name_exported", `String field.name_exported)
                ; ("func_partial_type", `String field.fp_type)
                ; ("type_option", `Bool field.type_option)
                ]
            )
            params.fields
        in
        `O
          [
            ("func_partial_type", `String params.fp_type)
          ; ("type", `String params.value_ty)
          ; ("fields", `A fields)
          ]
    | Enum params ->
        let of_value item =
          `O [("value", `String item.value); ("name", `String item.name)]
        in
        `O
          [
            ("type", `String params.value_ty)
          ; ("func_partial_type", `String params.fp_type)
          ; ("items", `A (List.map of_value params.items))
          ]
    | Map params ->
        `O
          [
            ("func_partial_type", `String params.fp_type)
          ; ("type", `String params.value_ty)
          ; ("key_type", `String params.key_ty)
          ; ("value_type", `String params.val_ty)
          ]

  let fields record_name =
    let fields =
      List.assoc_opt record_name records
      |> Option.value ~default:[]
      |> List.rev_map (fun field ->
             ( String.concat "_" field.full_name
             , Json.get_fp_type field.ty
             , match field.ty with Option _ -> true | _ -> false
             )
         )
    in
    if record_name = "EventRecord" then
      ("snapshot", "RecordInterface", false) :: fields
    else
      fields

  let of_ty = function
    | SecretString | String ->
        Simple {fp_type= "String"; value_ty= "string"}
    | Int ->
        Int {fp_type= "Int"; value_ty= "int"}
    | Float ->
        Float {fp_type= "Float"; value_ty= "float64"}
    | Bool ->
        Simple {fp_type= "Bool"; value_ty= "bool"}
    | DateTime ->
        Time {fp_type= "Time"; value_ty= "time.Time"}
    | Enum (name, kv) ->
        let name = snake_to_camel name in
        let items =
          List.map (fun (k, _) -> {value= k; name= name ^ snake_to_camel k}) kv
        in
        Enum {fp_type= "Enum" ^ name; value_ty= name; items}
    | Set ty ->
        let fp_ty = Json.get_fp_type ty in
        let ty, _ = Json.string_of_ty_with_enums ty in
        Set {fp_type= fp_ty ^ "Set"; value_ty= ty; item_fp_type= fp_ty}
    | Map (ty1, ty2) ->
        let s1, _ = Json.string_of_ty_with_enums ty1 in
        let s2, _ = Json.string_of_ty_with_enums ty2 in
        let name = "map[" ^ s1 ^ "]" ^ s2 in
        let k_fp_ty = Json.get_fp_type ty1 in
        let v_fp_ty = Json.get_fp_type ty2 in
        Map
          {
            fp_type= k_fp_ty ^ "To" ^ v_fp_ty ^ "Map"
          ; value_ty= name
          ; key_ty= Json.get_fp_type ty1
          ; val_ty= Json.get_fp_type ty2
          }
    | Ref r ->
        let name = snake_to_camel r ^ "Ref" in
        Ref {fp_type= name; value_ty= name}
    | Record r ->
        let name = snake_to_camel r ^ "Record" in
        let fields =
          List.map
            (fun (name, fp_type, is_option_type) ->
              let camel_name = snake_to_camel name in
              {
                name
              ; name_internal= String.uncapitalize_ascii camel_name
              ; name_exported= camel_name
              ; fp_type
              ; type_option= is_option_type
              }
            )
            (fields name)
        in
        Record {fp_type= name; value_ty= name; fields}
    | Option ty ->
        Option {fp_type= Json.get_fp_type ty}

  let of_serialize params =
    `O [("serialize", `A [to_json params]); ("deserialize", `Null)]

  let of_deserialize params =
    `O [("serialize", `Null); ("deserialize", `A [to_json params])]

  let event_batch : Mustache.Json.t =
    `O
      [
        ( "deserialize"
        , `A
            [
              `O
                [
                  ("func_partial_type", `String "EventBatch")
                ; ("type", `String "EventBatch")
                ; ( "elements"
                  , `A
                      [
                        `O
                          [
                            ("name", `String "token")
                          ; ("name_internal", `String "token")
                          ; ("name_exported", `String "Token")
                          ; ("func_partial_type", `String "String")
                          ]
                      ; `O
                          [
                            ("name", `String "validRefCounts")
                          ; ("name_internal", `String "validRefCounts")
                          ; ("name_exported", `String "ValidRefCounts")
                          ; ("func_partial_type", `String "StringToIntMap")
                          ]
                      ; `O
                          [
                            ("name", `String "events")
                          ; ("name_internal", `String "events")
                          ; ("name_exported", `String "Events")
                          ; ("func_partial_type", `String "EventRecordSet")
                          ]
                      ]
                  )
                ]
            ]
        )
      ]

  let interface : Mustache.Json.t =
    `O
      [
        ( "deserialize"
        , `A
            [
              `O
                [
                  ("func_partial_type", `String "RecordInterface")
                ; ("type", `String "RecordInterface")
                ]
            ]
        )
      ]
end
