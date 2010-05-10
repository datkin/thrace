structure AST = struct
  type pos = int
  type span = pos * pos

  datatype num = Int of LargeInt.int
               | Float of (LargeInt.int * LargeInt.int)
               | Rational of (LargeInt.int * LargeInt.int)

  datatype exp = Id of Id.id * span
               | Num of num * span
               | String of string * span
               | Sexp of exp list * span

  val i2s = LargeInt.toString

  fun numToString (Int int) = i2s int
    | numToString (Float (int, frac)) = (i2s int) ^ "." ^ (i2s frac)
    | numToString (Rational (num, den)) = (i2s num) ^ "/" ^ (i2s den)

  fun toString (Id (id, _)) = Id.name id
    | toString (Num (num, _)) = numToString num
    | toString (String (str, _)) = "\"" ^ (String.toCString str) ^ "\""
    | toString (Sexp (exps, _)) = "(" ^ (String.concatWith " " (map toString exps)) ^ ")"

(*
  fun astToString (ast, limit) =
      let
        val tabWidth = 2
        val tab = String.implode (List.tabulate (tabWidth, fn _ => #" "))

        (* fun indent n = String.concatWith "" (List.tabulate (n, fn _ => tab)) *)

        (* new string, (current string, remaining chars, indentation string) *)
        fun appendStr (str', (str, rem, indent)) =
            (* if the string wont fit on this line but will fit on a new line... *)
            if rem - (size str') - 1 < 0 andalso
               limit - (size indent) - (size str') >= 0 then
              (str ^ "\n" ^ indent ^ str',
               limit - (size indent) - (size str'),
               indent)
            else if rem - (size str') - 1 < 0 then
              (str ^ " " ^ str',
               rem - (size str') - 1,
               indent)
            else


        fun appendNode (node, (str, rem, indent)) =
            case node of
              Id (id, _) => appendStr (id, (str, rem, indent))
            | Num (num, _) => appendStr (numToString num, (str, rem, indent))
            | String (str, _) => appendStr ("\"" ^ (String.toCString str) ^ "\"",
                                            (str, rem, indent))
            | Sexp (exps, _) => ""
      in
        appendNode (ast, ("", limit, 0))
      end
*)
end
