(*
* Slacko - Binding to the Slack API
* Copyright (C) 2014-2019 Marek Kubica <marek@xivilization.net>
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 3.0 of the License, or (at your option) any later version,
* with the special exception on linking described in file COPYING.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*)

let int64_pow b n =
  let rec loop b n acc =
    if n = 0 then acc else
    loop Int64.(mul b b) (n lsr 1)
         (if n land 1 = 0 then acc else Int64.(mul b acc)) in
  loop b n 1L

type t = Ptime.t
let pp = Ptime.pp_human ~frac_s:6 ()

let of_string x =
  let d_ps_of_intlit intlit =
    let sec = Int64.of_string intlit in
    let d = Int64.div sec 86_400L in
    let ps = Int64.(mul (rem sec 86_400L) 1_000_000_000_000L) in
    (Int64.to_int d, ps) in
  match
    match String.split_on_char '.' x with
    | [_] ->
        Ptime.Span.of_d_ps (d_ps_of_intlit x)
    | [sec_lit; subsec_lit] ->
      let (d, ps_int) = d_ps_of_intlit sec_lit in
      let ps_frac =
        if String.length subsec_lit <= 12 then
          let scale = int64_pow 10L (12 - String.length subsec_lit) in
          Int64.mul scale (Int64.of_string subsec_lit)
        else
          Int64.of_string (String.sub subsec_lit 0 12) in
      Ptime.Span.of_d_ps (d, Int64.add ps_int ps_frac)
    | _ -> None
  with
  | exception Failure _ -> None
  | None -> None
  | Some span -> Ptime.of_span span

let to_string ts =
  let d, ps = Ptime.Span.to_d_ps (Ptime.diff ts Ptime.epoch) in
  let sec = Int64.(add (mul (of_int d) 86_400L) (div ps 1_000_000_000_000L)) in
  let subsec = Int64.(rem ps 1_000_000_000_000L) in
  Printf.sprintf "%Ld.%06Ld" sec (Int64.div subsec 1_000_000L)

let of_yojson json =
  match
    match json with
    | `Int x -> Ptime.of_span (Ptime.Span.of_int_s x)
    | `Intlit x -> of_string x
    | `String x -> of_string x
    | _ -> None
  with
  | Some ts -> Result.Ok ts
  | None -> Result.Error "Couldn't parse timestamp"

let to_yojson ts = `String (to_string ts)
