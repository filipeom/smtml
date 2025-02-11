type t =
  { value : Z.t
  ; width : int
  }

let make v m = { value = v; width = m }

let view { value; _ } = value

let numbits { width; _ } = width
