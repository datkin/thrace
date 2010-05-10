structure SyntaxRules = struct

datatype const =
         Id of Id.id
       | Num of AST.num

datatype pattern =
         PList of pattern list *
                  pseq *
                  ptail
       | PVar of Id.id
       | PConst of const

     and pseq =
         PSeq of pattern *
                 pattern list
       | PNothing

     and ptail =
         PRest of pattern
       | PEnd

datatype match =
         MList of match list *
                  mseq *
                  mtail
       | MVar of Id.id * AST.exp
       | MConst

     and mseq =
         MSeq of match list *
                 match list
       | MNothing

     and mtail =
         MRest of match
       | MEnd

(* We need to carry around a binding environment? *)
(* Each variables has some nesting level, so for instance
 * (a ...) would mean a is singly nested,
 * whereas ((a ...) ...) would mean a is double ested,
 * and any occurence of a in the pattern needs to be
 * "bound" by _two_ ellipses. *)
fun matchPattern (PConst (Num n), AST.Num (n', _)) = if n = n' then MConst else raise Fail "not num const"
  | matchPattern (PConst (Id id), AST.Id (id', _)) = if id = id' then MConst else raise Fail "not id const"
  | matchPattern (PVar id, ast) = MVar (id, ast)
  (* | matchPattern (PList (plist, pseq, ptail), ast) = *)
  | matchPattern (_, _) = raise Fail "didn't match!"

(* TODO: ensure variables only occur once within the pattern *)
fun parsePattern (ast, literals) =
    let
      fun isLiteral id = List.exists (Util.eq id) literals

      fun isDot (AST.Id (id, _)) = id = Id.id "."
        | isDot _ = false

      fun isEllipses (AST.Id (id, _)) = id = Id.id "..."
        | isEllipses _ = false

      fun parse ast = parsePattern (ast, literals)

      fun parsePList (ast1 :: ast2 :: rest, (exps, pseq, _)) =
          if isDot ast1 then
            if rest = nil then
              (exps, pseq, PRest (parse ast2))
            else
              raise Fail "Only one pattern allowed after ."
          else if isEllipses ast2 then
            case pseq of
              PNothing => parsePList (rest, (exps, PSeq ((parse ast1), []), PEnd))
            | PSeq _ => raise Fail "Only one sequence pattern per list"
          else
            let
              val exp1 = parse ast1
              val plist = case pseq of
                            PNothing => (exps @ [exp1], PNothing, PEnd)
                          | PSeq (seq, exps') => (exps, PSeq (seq, exps' @ [exp1]), PEnd)
            in
              parsePList (ast2 :: rest, plist)
            end
        | parsePList (rest, (exps, PNothing, _)) =
          (exps @ (map parse rest), PNothing, PEnd)
        | parsePList (rest, (exps, PSeq (seq, exps'), _)) =
          (exps, PSeq (seq, exps' @ (map parse rest)), PEnd)

    in
      case ast of
        AST.Id (id, _) => if isLiteral id then
                            PConst (Id id)
                          else
                            PVar id
      | AST.Num (num, _) => PConst (Num num)
      | AST.Sexp (exps, _) => PList (parsePList (exps, ([], PNothing, PEnd)))
      | AST.String _ => raise Fail "String matches not supported by macros"
    end

end
