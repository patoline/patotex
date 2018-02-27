(* SPDX-License-Identifier: GPL-3.0 *)
(** Utility functions *)

open Latex

(** {1 Handling locations in source file} *)
type loc = Location.t

let default_loc = ref Location.none

let with_default_loc loc f =
  let backup_loc = !default_loc in
  default_loc := loc;
  let res = f () in
  default_loc := backup_loc;
  res

let mkloc = Location.mkloc

let txt loc = loc.Location.txt

(** {1 Helper functions for building syntax trees} *)
module DocCls = struct
  let mk ?(_loc = !default_loc) ?(options = []) name = {
    cls_name = name;
    cls_options = options;
    cls_loc = _loc;
  }
end
