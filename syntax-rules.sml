structure SyntaxRules = struct

datatype const =
         Id of Id.id
       | Num of AST.num

datatype pattern =
         PList of pattern list *
                  ptail
       | PVar of Id.id
       | PConst of const

     and ptail =
         PSeq of pattern *
                 pattern list
       | PRest of pattern
       | PEnd

datatype match =
         MList of match list *
                  mtail
       | MVar of Id.id * AST.exp
       | MConst

     and mtail =
         MSeq of match list *
                 match list
       | MRest of match
       | MEnd

datatype template =
         (* Do we need to support imporper tails? I'm not convinced yet. *)
         TList of titem list
       | TVar of Id.id
       | TConst of const

     and titem =
         TRepeat of template
       | TOnce of template

(* Every time we recur on a repeat, we pass the environment down, but for any N-dimensional identifier, we
 * select one element of from the top dimension and bring it the top (ie, stripping away all the other
 * top level elements, eg:
 * (a: (((1 2 3) (4 5 6)) ((1 2) (4))), b: ((1 2) (3 4)) c: (1 2 3) d: 4), recur on 2, gives
 * (a: ((1 2) (4)), b: (3 4), c: -, d: 4?)
 * So how do we know on what index to stop recurring? *)

(* We need to carry around a binding environment? *)
(* Each variables has some nesting level, so for instance
 * (a ...) would mean a is singly nested,
 * whereas ((a ...) ...) would mean a is doubly nested,
 * and any occurence of a in the pattern needs to be
 * "bound" by _two_ ellipses. *)
fun matchPattern (PConst (Num n), AST.Num (n', _)) = if n = n' then MConst else raise Fail "not num const"
  | matchPattern (PConst (Id id), AST.Id (id', _)) = if id = id' then MConst else raise Fail "not id const"
  | matchPattern (PVar id, ast) = MVar (id, ast)
  | matchPattern (PList (patterns, ptail), AST.Sexp (asts, span)) =
    let
      val limit = length patterns

      fun split (ast, (n, head, tail)) =
          if n < limit then
            (n + 1, head @ [ast], tail)
          else
            (n + 1, head, tail @ [ast])

      val (_, head, tail) = foldl split (0, [], []) asts

      fun matchPatterns (patterns, asts) =
          ListPair.foldlEq (fn (pattern, ast, matches) =>
                               matchPattern (pattern, ast) :: matches)
                           []
                           (patterns, asts)

      fun matchTail (PEnd, []) = MEnd
        | matchTail (PRest pattern, ast) = MRest (matchPattern (pattern, AST.Sexp (ast, span)))
        | matchTail (PSeq (pattern, patterns), asts) =
          let
            val limit = length patterns

            fun split (ast, (n, head, tail)) =
                if n < limit then
                  (n + 1, head, ast :: tail)
                else
                  (n + 1, ast :: head, tail)

            val (_, head, tail) = foldr split (0, [], []) asts
          in
            MSeq (map (fn ast => matchPattern (pattern, ast))
                      head,
                  matchPatterns (patterns, tail))
          end
        | matchTail _ = raise Fail "Could not match tail!"
    in
      MList (matchPatterns (patterns, head),
             matchTail (ptail, tail))
    end
  | matchPattern (_, _) = raise Fail "didn't match!"

(* Testing:
 val pattern = SyntaxRules.parsePattern (hd (Parser.getAst (Parser.read ())), []);
 val ast = hd (Parser.getAst (Parser.read ()));
 val match = SyntaxRules.matchPattern (pattern, ast);
*)

(* TODO: ensure variables only occur once within the pattern *)
fun parsePattern (ast, literals) =
    let
      fun isLiteral id = List.exists (Util.eq id) literals

      fun isDot (AST.Id (id, _)) = id = Id.id "."
        | isDot _ = false

      fun isEllipsis (AST.Id (id, _)) = id = Id.id "..."
        | isEllipsis _ = false

      fun parse ast = parsePattern (ast, literals)

      fun parsePList (ast1 :: ast2 :: rest, (exps, pseq)) =
          if isDot ast1 then
            if rest = nil then
              case pseq of
                PEnd => (exps, PRest (parse ast2))
              | _ => raise Fail ". not allowed after ..."
            else
              raise Fail "Only one pattern allowed after ."
          else if isEllipsis ast2 then
            case pseq of
              PSeq _ => raise Fail "Only one sequence pattern per list"
            | _ => parsePList (rest, (exps, PSeq ((parse ast1), [])))
          else
            let
              val exp1 = parse ast1
              val plist = case pseq of
                            PSeq (seq, exps') => (exps, PSeq (seq, exps' @ [exp1]))
                          | _ => (exps @ [exp1], PEnd)
            in
              parsePList (ast2 :: rest, plist)
            end
        | parsePList (rest, (exps, PSeq (seq, exps'))) =
          (exps, PSeq (seq, exps' @ (map parse rest)))
        | parsePList (rest, (exps, _)) =
          (exps @ (map parse rest), PEnd)

    in
      case ast of
        AST.Id (id, _) => if isLiteral id then
                            PConst (Id id)
                          else
                            PVar id
      | AST.Num (num, _) => PConst (Num num)
      | AST.Sexp (exps, _) => PList (parsePList (exps, ([], PEnd)))
      | AST.String _ => raise Fail "String matches not supported by macros"
    end

(* Tests:
SyntaxRules.parsePattern (hd (Parser.getAst (Parser.read ())), []);

(_ a ... x)
(_ (a . (b ...)))
(_ (a b ...) ...)

*)

(* TODO: integrate this with parsePattern, which will allow for better error messages. *)
fun getBinders (pattern, binders, level) =
    case pattern of
      PVar id => (case Id.IdMap.find (binders, id) of
                   NONE => Id.IdMap.insert (binders, id, level)
                 | SOME _ => raise Fail ("Id already defined: " ^ Id.name id))
    | PList (patterns, ptail) =>
      let
        val collectBinders =
            foldr (fn (pattern, binders') =>
                      getBinders (pattern, binders', level))

        val binders' = collectBinders binders patterns
        val binders'' = case ptail of
                          PSeq (pattern, patterns) => collectBinders (getBinders (pattern,
                                                                                 binders',
                                                                                 level + 1))
                                                                    patterns
                        | PRest pattern => getBinders (pattern, binders', level)
                        | PEnd => binders'
      in
        binders''
      end
    | _ => binders

(* Test:
 val pattern = SyntaxRules.parsePattern (hd (Parser.getAst (Parser.read ())), []);
 (a b (c ...) ((d ...) ...) ...)

 val binders = SyntaxRules.getBinders (pattern, Id.IdMap.empty, 0);
 a: 0, b: 0, c: 1, d: 3
*)

(*
fun parseTemplate (ast, binders, level) =
    ...
*)
end
