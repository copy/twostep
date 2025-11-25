let __unix_time () =
Int64.of_float @@ Unix.time ()


let counter ?(timestep = 30) ?(drift = 0) ?(timestamp = __unix_time) () =
  let now = timestamp () in
  let add = Int64.of_int drift in
  let ctr =
    Int64.add add @@ Int64.div now @@ Int64.of_int timestep
  in
  Mirage_crypto_pk.Z_extra.to_octets_be ~size:8
  @@ Z.of_int64 ctr
