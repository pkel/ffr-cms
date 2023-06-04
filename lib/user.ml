open Sexplib.Std

type t =
  { name: string (* goes into commit message *)
  ; email: string (* goes into commit message *)
  ; password: string (* argon2encoded *)
  } [@@deriving sexp]

let salt len =
  Mirage_crypto_rng_unix.getrandom len |> Cstruct.to_string

let t ~email ~name ~password:pwd : t =
  let salt_len = 16 in
  let salt = salt salt_len
  and t_cost = 1
  and m_cost = 1 lsl 20
  and parallelism = 4
  and hash_len = 16
  and kind = Argon2.ID
  and version = Argon2.VERSION_13
  in
  let encoded_len =
    Argon2.encoded_len ~salt_len ~t_cost ~m_cost ~kind ~hash_len ~parallelism
  in
  let password =
    Argon2.hash ~t_cost ~m_cost ~parallelism ~pwd ~salt ~hash_len
      ~encoded_len ~version ~kind
    |> Result.map snd
    |> Result.get_ok
  in
  { name ; email ; password }

let nobody = t ~email:"" ~name:"" ~password:(salt 16)

let valid_password user pwd =
  let encoded = user.password in
  match Argon2.verify ~encoded ~pwd ~kind:ID with
  | Ok true -> true
  | _ -> false
  | exception _ -> false

type users = (string (* login/handle *) * t) list [@@deriving sexp]
