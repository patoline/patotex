open Latex

type loc = Location.t

let default_loc = ref Location.none

let with_default_loc loc f =
  let backup_loc = !default_loc in
  default_loc := loc;
  let res = f () in
  default_loc := backup_loc;
  res

let mkloc = Location.mkloc

module DocCls = struct
  let mk ?(loc = !default_loc) ?(options = []) name = {
    cls_name = name;
    cls_options = options;
    cls_loc = loc;
  }
end
