(*
 * Copyright (C) 2013 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type greeting = {
  major: int;
  minor: int;
  micro: int;
  package: string;
}

type event = {
  timestamp: float;
  event: string;
}

type enabled = {
  enabled: bool;
  present: bool;
}

type core = {
	coreId  : int;
	threadId: int;
	socketId: int;
	vcpusCount: int;
	qomPath: string option;
	coreType: string;
}

type device_info = {
	name  : string;
	value : string;
}

type command =
  | Qmp_capabilities
  | Query_commands
  | Query_kvm
  | Query_status
  | Query_hotpluggable_cpus
  | Stop
  | Cont
  | Eject of string * bool option
  | Change of string * string * string option
  | Device_add of device_info list
  | Device_del of string
  | System_powerdown
  | Xen_save_devices_state of string
  | Xen_load_devices_state of string
  | Xen_set_global_dirty_log of bool

type result =
  | Name_list of string list
  | Enabled of enabled
  | Status of string
	| CoreList of core list
  | Unit

type error = {
  cls: string;
  descr: string;
}

type id = string

type message =
  | Greeting of greeting
  | Command of (id option * command)
  | Error of (id option * error)
  | Success of (id option * result)
  | Event of event

let message_of_string x =
  let int = function
  | `Int x -> x
  | _ -> failwith "int" in
  let float = function
  | `Int x -> float_of_int x
  | _ -> failwith "float" in
  let string = function
  | `String x -> x
  | _ -> failwith "string" in
  let assoc = function
  | `Assoc x -> x
  | _ -> failwith "assoc" in
  let bool = function
  | `Bool x -> x
  | _ -> failwith "bool" in
  match Yojson.Safe.from_string x with
  | `Assoc
     [ ("QMP", `Assoc [ ("version", `Assoc [ "qemu", `Assoc version; "package", `String package ]); ("capabilities", _)] )] ->
    Greeting {
      minor = int (List.assoc "minor" version);
      major = int (List.assoc "major" version);
      micro = int (List.assoc "micro" version);
      package = package;
    }
  | `Assoc list when List.mem_assoc "event" list ->
    let event = string (List.assoc "event" list) in
    let timestamp = assoc (List.assoc "timestamp" list) in
    let secs = float (List.assoc "seconds" timestamp) in
    let usecs = float (List.assoc "microseconds" timestamp) in
    let timestamp = secs +. usecs /. 1e6 in
    Event { timestamp; event }
  | `Assoc list when List.mem_assoc "execute" list ->
    let id = if List.mem_assoc "id" list then Some (string (List.assoc "id" list)) else None in
    Command (id, (match string (List.assoc "execute" list) with
      | "qmp_capabilities" -> Qmp_capabilities
      | "stop" -> Stop
      | "cont" -> Cont
      | "system_powerdown" -> System_powerdown
      | "query-commands" -> Query_commands
      | "query-status" -> Query_status
      | "query-hotpluggable-cpus" -> Query_hotpluggable_cpus
      | "query-kvm" -> Query_kvm
      | "eject" ->
            let arguments = assoc (List.assoc "arguments" list) in
            Eject (string (List.assoc "device" arguments),
                   if List.mem_assoc "force" arguments then
                     Some (bool (List.assoc "force" arguments))
                   else
                     None)
      | "change" ->
          let arguments = assoc (List.assoc "arguments" list) in
            Change (string (List.assoc "device" arguments),
                    string (List.assoc "target" arguments),
                    if List.mem_assoc "arg" arguments then
                      Some (string (List.assoc "arg" arguments))
                    else None)
			(**)
      | "device_add" -> 
          let arguments = assoc (List.assoc "arguments" list) in
            Device_add ( List.map 
              (function
                  | (name , `String value) ->
                    {name; value} 
                  | _ -> failwith "assoc #my2->"
               ) arguments
            )
			(**)
      | "device_del" ->  Device_del (string (List.assoc "id" (assoc (List.assoc "arguments" list))))
      | "xen-save-devices-state" -> Xen_save_devices_state (string (List.assoc "filename" (assoc (List.assoc "arguments" list))))
      | "xen-load-devices-state" -> Xen_load_devices_state (string (List.assoc "filename" (assoc (List.assoc "arguments" list))))
      | "xen-set-global-dirty-log" -> Xen_set_global_dirty_log (bool (List.assoc "enable" (assoc (List.assoc "arguments" list))))
      | x -> failwith (Printf.sprintf "unknown command %s" x)
    ))
  | `Assoc list when List.mem_assoc "return" list ->
    let id = if List.mem_assoc "id" list then Some (string (List.assoc "id" list)) else None in
    Success (id, (match List.assoc "return" list with
      | `Assoc [] -> Unit
      | `Assoc list when List.mem_assoc "status" list ->
        Status (string (List.assoc "status" list))
      | `Assoc list when List.mem_assoc "enabled" list ->
        let enabled = bool (List.assoc "enabled" list) in
        let present = bool (List.assoc "present" list) in
        Enabled {enabled; present}
      | `List ((`Assoc pair :: _) as list) when List.mem_assoc "name" pair ->
        Name_list (List.map (function
                             | `Assoc [ "name", `String x ] -> x
                             | _ -> failwith "assoc") list)

     | `List list ->(match  (List.hd list) with
		   | `Assoc list' when
					List.mem_assoc "props" list'
					&& List.mem_assoc "type" list' ->
			    CoreList (
            List.map (
            function
            | `Assoc [ ("props", `Assoc coreinfo); ("vcpus-count", `Int vcpusCount); ("qom-path", `String qomPath); ("type", `String coreType) ] ->
              let coreId = int (List.assoc "core-id" coreinfo)  in
              let threadId = int (List.assoc "thread-id" coreinfo) in
              let socketId = int (List.assoc "socket-id" coreinfo) in
              let qomPath = Some qomPath in
               {coreId; threadId; socketId; vcpusCount; qomPath; coreType}
            | `Assoc [ ("props", `Assoc coreinfo); ("vcpus-count", `Int vcpusCount); ("type", `String coreType) ] ->
              let coreId = int (List.assoc "core-id" coreinfo)  in
              let threadId = int (List.assoc "thread-id" coreinfo) in
              let socketId = int (List.assoc "socket-id" coreinfo) in
              let qomPath = None in
                {coreId; threadId; socketId; vcpusCount; qomPath; coreType}
            | _ -> failwith "assoc"
          ) list)
				| _ -> failwith "assoc")

      | x -> failwith (Printf.sprintf "unknown result %s" (Yojson.Safe.to_string x))
    ))
  | `Assoc list when List.mem_assoc "error" list ->
    let id = if List.mem_assoc "id" list then Some (string (List.assoc "id" list)) else None in
    let error = assoc (List.assoc "error" list) in
    let cls = string (List.assoc "class" error) in
    let descr = string (List.assoc "desc" error) in
    Error (id, {cls; descr})
  | x ->
    failwith (Printf.sprintf "message_of_string %s" (Yojson.Safe.to_string x))

let json_of_message = function
  | Greeting { major; minor; micro; package } ->
    let version = [ "major", `Int major; "minor", `Int minor; "micro", `Int micro ] in
    `Assoc [ ("QMP", `Assoc [ ("version", `Assoc [ "qemu", `Assoc version; "package", `String package ]); ("capabilities", `List []) ])]
  | Command(id, cmd) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let cmd, args = match cmd with
      | Qmp_capabilities -> "qmp_capabilities", []
      | Stop -> "stop", []
      | Cont -> "cont", []
      | System_powerdown -> "system_powerdown", []
      | Query_commands -> "query-commands", []
      | Query_status -> "query-status", []
      | Query_hotpluggable_cpus -> "query-hotpluggable-cpus", []
      | Query_kvm -> "query-kvm", []
      | Eject (device, None) -> "eject", [ "device", `String device ]
      | Eject (device, Some force) -> "eject", [ "device", `String device; "force", `Bool force ]
      | Change (device, target, None) -> "change", [ "device", `String device; "target", `String target ]
      | Change (device, target, Some arg) -> "change", [ "device", `String device; "target", `String target; "arg", `String arg ]
      | Device_add device_info_list ->  "device_add", List.map (fun x -> (x.name, `String x.value)) device_info_list
      | Device_del id -> "device_del", [ "id", `String id]
      | Xen_save_devices_state filename -> "xen-save-devices-state", [ "filename", `String filename]
      | Xen_load_devices_state filename -> "xen-load-devices-state", [ "filename", `String filename]
      | Xen_set_global_dirty_log enable -> "xen-set-global-dirty-log", [ "enable", `Bool enable ]
    in
    let args = match args with [] -> [] | args -> [ "arguments", `Assoc args ] in
    `Assoc (("execute", `String cmd) :: id @ args)
  | Event {timestamp; event} ->
    let usecs, secs = modf timestamp in
    `Assoc [("event", `String event); ("timestamp", `Assoc [ "seconds", `Int (int_of_float secs); "microseconds", `Int (int_of_float (usecs *. 1e6)) ])]
  | Success(id, result) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let result = match result with
      | Unit -> `Assoc []
      | Status s -> `Assoc [ "status", `String s ]
      | Enabled {enabled; present} -> `Assoc [ "enabled", `Bool enabled; "present", `Bool present ]
      | Name_list xs -> `List (List.map (fun x -> `Assoc [ "name", `String x ]) xs)
      | CoreList xs -> `List (List.map (fun x ->
        match x.qomPath with
        | Some c ->
          `Assoc  [
            ("props",
              `Assoc [("core-id", `Int x.coreId); ("thread-id", `Int x.threadId); ("socket-id"), `Int x.socketId]
            );
            ("vcpus-count", `Int x.vcpusCount);
            ("qom-path", `String c);
            ("type", `String x.coreType)
           ]
        | None ->
          `Assoc  [
            ("props",
              `Assoc [("core-id", `Int x.coreId); ("thread-id", `Int x.threadId); ("socket-id"), `Int x.socketId]
            );
            ("vcpus-count", `Int x.vcpusCount);
					  ("type", `String x.coreType)
          ]
          )
				xs) in
    `Assoc (("return", result) :: id)
  | Error(id, e) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let e = `Assoc [ "class", `String e.cls; "desc", `String e.descr; "data", `Assoc [] ] in
    `Assoc (("error", e) :: id)

let string_of_message m = Yojson.Safe.to_string (json_of_message m)

