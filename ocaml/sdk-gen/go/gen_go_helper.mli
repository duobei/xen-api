(* Copyright (C) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

val ( // ) : string -> string -> string

val snake_to_camel : string -> string

val render_template : string -> Mustache.Json.t -> string

val generate_file : string -> string -> string -> unit

module Json : sig
  val xenapi : Datamodel_types.obj list -> (string * Mustache.Json.t) list

  val api_messages : Mustache.Json.value list

  val api_errors : Mustache.Json.value list
end
