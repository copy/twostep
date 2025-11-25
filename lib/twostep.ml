type hash = [`Sha1 | `Sha256 | `Sha512]

module Internals = struct
  let counter = Time.counter

  let hmac = Hmac.hmac

  type padding = Helpers.padding

  let pad = Helpers.pad

  let padOnLeft = Helpers.padOnLeft

  let padOnRight = Helpers.padOnRight

  let truncate ~image ~digits =
    let bytes =
      Base.List.map ~f:Base.Char.to_int @@ Base.String.to_list image
    in
    let offset =
      Base.List.nth_exn bytes (Base.List.length bytes - 1) land 0xf
    in
    let fst = (Base.List.nth_exn bytes (offset + 0) land 0x7f) lsl 24 in
    let snd = (Base.List.nth_exn bytes (offset + 1) land 0xff) lsl 16 in
    let trd = (Base.List.nth_exn bytes (offset + 2) land 0xff) lsl 8 in
    let fth = (Base.List.nth_exn bytes (offset + 3) land 0xff) lsl 0 in
    let num = fst lor snd lor trd lor fth mod Base.Int.pow 10 digits in
    Helpers.pad ~basis:digits ~byte:'0' ~direction:Helpers.OnLeft
    @@ Base.Int.to_string num
end

module type ITOTP = sig
  val secret : ?bytes:int -> unit -> string

  val code :
       ?window:int
    -> ?drift:int
    -> ?digits:int
    -> ?hash:hash
    -> secret:string
    -> unit
    -> string

  val verify :
       ?window:int
    -> ?digits:int
    -> ?hash:hash
    -> secret:string
    -> code:string
    -> unit
    -> bool
end

module TOTP : ITOTP = struct
  let secret ?(bytes = 10) () = Secret.generate ~bytes ()

  let code
      ?(window = 30) ?(drift = 0) ?(digits = 6) ?(hash = `Sha1) ~secret () =
    assert (digits = 6 || digits = 8) ;
    let decoded = Base32.decode_exn secret in
    let counter = Time.counter ~timestep:window ~drift () in
    let image = Hmac.hmac ~hash ~secret:decoded counter in
    Internals.truncate ~image ~digits


  let verify
      ?(window = 30) ?(digits = 6) ?(hash = `Sha1) ~secret ~code:number () =
    number = code ~secret ~window ~digits ~hash ~drift:(-1) ()
    || number = code ~secret ~window ~digits ~hash ~drift:0 ()
    || number = code ~secret ~window ~digits ~hash ~drift:1 ()
end

module type IHOTP = sig
  val secret : ?bytes:int -> unit -> string

  val codes :
       ?digits:int
    -> ?hash:hash
    -> ?amount:int
    -> counter:int
    -> secret:string
    -> unit
    -> string list

  val verify :
       ?digits:int
    -> ?hash:hash
    -> ?ahead:int
    -> counter:int
    -> secret:string
    -> codes:string list
    -> unit
    -> bool * int
end

module HOTP : IHOTP = struct
  let secret ?(bytes = 10) () = Secret.generate ~bytes ()

  let code ~digits ~hash ~counter ~secret () =
    let decoded = Base32.decode_exn secret in
    let counter = Int64.of_int counter in
    let counter' =
      Mirage_crypto_pk.Z_extra.to_octets_be ~size:8
      @@ Z.of_int64 counter
    in
    let image = Hmac.hmac ~hash ~secret:decoded counter' in
    Internals.truncate ~image ~digits


  let codes ?(digits = 6) ?(hash = `Sha1) ?(amount = 1) ~counter ~secret () =
    assert (amount >= 1) ;
    let step index = code ~digits ~hash ~counter:(counter + index) ~secret () in
    List.init amount step


  let verify
      ?(digits = 6)
      ?(hash = `Sha1)
      ?(ahead = 0)
      ~counter
      ~secret
      ~codes:numbers
      () =
    assert (ahead >= 0) ;
    assert (List.length numbers >= 1) ;
    let amount = List.length numbers in
    let step index =
      let valid =
        numbers
        = codes ~digits ~hash ~amount ~counter:(counter + index) ~secret ()
      in
      let next = counter + index + amount in
      (valid, next)
    in
    let results = List.init (ahead + 1) step in
    let folding previous current =
      if fst previous
      then previous
      else if fst current
      then current
      else (false, counter)
    in
    let invalid = (false, counter) in
    Base.List.fold_left results ~init:invalid ~f:folding
end
