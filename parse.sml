structure Parser =
struct
structure TP = ThraceParseFn(ThraceLexer)

structure StreamPos = AntlrStreamPos
structure Repair = AntlrRepair

datatype source = File of string | StdIn
datatype result = Success of AST.exp list
                | Failure of (ThraceTokens.token Repair.repair) list
type parse_unit = {result: result, source: source, sourcemap: StreamPos.sourcemap}
type compilation_unit = {ast: AST.exp list, source: source, sourcemap: StreamPos.sourcemap}

fun parse (source, instrm) : parse_unit =
    let
      val sm = StreamPos.mkSourcemap ()
      val lex = ThraceLexer.lex sm
      val strm = ThraceLexer.streamifyInstream instrm
      val (maybeAst, strm', errs) = TP.parse lex strm
      val _ = TextIO.closeIn instrm
      val result = case (maybeAst, errs) of
                     (SOME ast, []) => Success ast
                   | _ => Failure errs
    in
      {result = result,
       source = source,
       sourcemap = sm}
    end

fun parseFile filename =
    parse (File filename, TextIO.openIn filename)

fun parseString str =
    parse (StdIn, TextIO.openString str)

fun read () : parse_unit =
    let
      fun loop str =
          let
            val str' = str ^ (valOf (TextIO.inputLine TextIO.stdIn))
            val result = parseString str'
          in
            case #result result of
              Success _ => result
            (* Keep going *if* any/all of the failures was an insert.
             * This heuristic is completely wrong. *)
            | Failure errs =>
              if List.all (fn (_, (Repair.Insert _)) => true
                            | _ => false) errs then
                loop str'
              else
                result
          end
    in
      loop ""
    end

fun repl () : unit =
    (read (); repl ())

fun parseToString {result, source, sourcemap} =
    let
      val sourceStr = case source of
                        File name => name
                      | StdIn => "stdIn"

      val tokenToString = ThraceTokens.toString;

      fun tokensToString tokens = String.concatWith " " (map tokenToString
                                                             tokens)

      fun actionToString action =
          case action of
            Repair.Insert tokens => "Try inserting " ^ (tokensToString tokens)
          | Repair.Delete tokens => "Try deleting " ^ (tokensToString tokens)
          | Repair.Subst {old, new} => "Try substituting " ^ (tokensToString new) ^ " for " ^
                                       (tokensToString old)
          | Repair.FailureAt token => "Unexpected token " ^ (tokenToString token)

      fun posToString pos =
          let val {lineNo, colNo, fileName} = StreamPos.sourceLoc sourcemap pos in
            (Int.toString lineNo) ^ "." ^ (Int.toString colNo)
          end

      fun repairToString (pos, action) =
          sourceStr ^ ":" ^ (posToString pos) ^ " Error: " ^ (actionToString action)

    in
      String.concatWith "\n"
                        (case result of
                           Success ast => map AST.toString ast
                         | Failure errs => map repairToString errs)
    end

fun getAst ({result=Success ast, ...} : parse_unit) = ast
  | getAst _ = raise Fail "Parse failed"

end
