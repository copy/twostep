module S1 = Digestif.SHA1
module S2 = Digestif.SHA256
module S5 = Digestif.SHA512

let hmac_sha1 ~secret payload =
  S1.to_raw_string @@ S1.hmac_string ~key:secret payload


let hmac_sha256 ~secret payload =
  S2.to_raw_string @@ S2.hmac_string ~key:secret payload


let hmac_sha512 ~secret payload =
  S5.to_raw_string @@ S5.hmac_string ~key:secret payload


let hmac ~hash =
  match hash with
  | `Sha1 ->
      hmac_sha1
  | `Sha256 ->
      hmac_sha256
  | `Sha512 ->
      hmac_sha512
