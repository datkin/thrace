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

(* Every time we recur on a repeat, we pass the environment down, but for any N-dimensional identifier, we
 * select one element of from the top dimension and bring it the top (ie, stripping away all the other
 * top level elements, eg:
 * (a: (((1 2 3) (4 5 6)) ((1 2) (4))), b: ((1 2) (3 4)) c: (1 2 3) d: 4), recur on 2, gives
 * (a: ((1 2) (4)), b: (3 4), c: -, d: 4?) - in otherwords, if c ends up being used in a pattern,
 * then there just weren't enough c's to go around and this is an error on the users part.
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

fun isEllipsis (AST.Id (id, _)) = id = Id.id "..."
  | isEllipsis _ = false

(* TODO: ensure variables only occur once within the pattern *)
fun parsePattern (ast, literals) =
    let
      fun isLiteral id = List.exists (Util.eq id) literals

      fun isDot (AST.Id (id, _)) = id = Id.id "."
        | isDot _ = false

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

datatype template =
         (* Do we need to support imporper tails? I'm not convinced yet. *)
         TList of titem list
       | TVar of Id.id
       | TConst of const

     and titem =
         (* the identifier is a hint for the id that's being looped over *)
         TRepeat of titem * Id.IdSet.set
       | TOnce of template

datatype item_parse = Done
                    | Item of titem * AST.exp list * Id.IdSet.set

fun parseTemplate (AST.Id (id, _), binders, level) =
    (case Id.IdMap.find (binders, id) of
       NONE => (TConst (Id id), Id.IdSet.empty)
     (* An id must appear at or *past* it's level of nesting! *)
     | SOME level' => if level < level' then
                        raise Fail ("Id " ^ (Id.name id) ^ " not nested deeply enough!")
                      else
                        (* The option represents the most deeply nested id appearing in the expr? *)
                        (TVar id, Id.IdSet.singleton id))
  | parseTemplate (AST.Num (num, _), _, _) = (TConst (Num num), Id.IdSet.empty)
  | parseTemplate (AST.Sexp (asts, _), binders, level) =
    let
      (* Parse from right to left? *)
      (* Return an item and the rest of the asts *)
      fun parseTItem ([], _) = Done
        | parseTItem (ast :: rest, level) =
          if isEllipsis ast then
            case parseTItem (rest, level + 1) of
              Done => raise Fail "... must come after a template"
            | Item (item, rest, ids) => Item (TRepeat (item, ids), rest, ids)
          else
            let
              val (template, ids) = parseTemplate (ast, binders, level)
            in
              Item (TOnce template, rest, ids)
            end

      fun parseTList (asts, level, items, ids) =
          case parseTItem (asts, level) of
            Done => (items, ids)
          | Item (item, rest, ids') => parseTList (rest, level, item :: items, Id.IdSet.union (ids, ids'))

      val (items, ids) = parseTList ((rev asts), level, [], Id.IdSet.empty)
    in
      (TList items, ids)
    end
  | parseTemplate (AST.String _, _, _) = raise Fail "Strings not supported in templates"

(* Testing:
 val ast = hd (Parser.getAst (Parser.read ()));
     (a (b ...) (c ...))

 val binders = SyntaxRules.getBinders (pattern, Id.IdMap.empty, 0);

 val ast = hd (Parser.getAst (Parser.read ()));
     ((b c) ... a)

 val template = SyntaxRules.parseTemplate (ast, binders, 0);

 Other templates to try:
   (a ...) w/ ((a ...) (a ...))
           w/ (a a ...) (should fail)
           w/ (foo a ...)
           w/ (foo a ... ...)
*)

(* TODO: is there some way to encode in the type that this should be a "full" tree? *)
datatype binding = Nested of int * binding list (* cache the length *)
                 | Binding of AST.exp
                 | NoBinding

fun bindingToStr NoBinding = "Nothing"
  | bindingToStr (Binding ast) = AST.toString ast
  | bindingToStr (Nested (_, bindings)) = "[" ^ String.concatWith ", " (map bindingToStr bindings) ^ "]"

fun matchToBindings match =
    case match of
      MConst => Id.IdMap.empty
    | MVar (id, ast) => Id.IdMap.singleton (id, Binding ast)
    | MList (matches, mtail) =>
      let
        fun mergeBindings (id, binding1, binding2) =
            raise Fail ("Id " ^ Id.name id ^ " bound more than once?")

        (* Implementing this might be a bit easier if we had a list of
         * the bound vars in a sequence ahead of time, because then we
         * would merely have to add an empty Nested instance for each
         * of them to our *base* binding, and we'd be guaranteed that
         * every union would cause a collision that could resolve. *)
        fun raiseBinding binding = Nested (1, [binding])

        fun mergeRaised (_, Nested (n1, b1), Nested (n2, b2)) = Nested (n1 + n2, b1 @ b2)
          | mergeRaised (id, _, _) = raise Fail ("Cannot merge non-nested bindings for Id " ^ Id.name id)

        fun matchesToBindings matches =
            foldl (fn (match, bindings) =>
                      Id.IdMap.unionWithi mergeBindings
                                          (bindings,
                                           (matchToBindings match)))
                  Id.IdMap.empty
                  matches

        fun mtailToBindings mtail =
            case mtail of
              MEnd => Id.IdMap.empty
            | MRest match => matchToBindings match
            | MSeq (seq, matches) => let
                val raisedBindings = map (Id.IdMap.map raiseBinding)
                                         (map matchToBindings seq)
                val seqBindings = foldl (fn (source, target) =>
                                            Id.IdMap.unionWithi mergeRaised
                                                                (target, source))
                                        Id.IdMap.empty
                                        raisedBindings
                val matchBindings = matchesToBindings matches
              in
                Id.IdMap.unionWithi mergeBindings (seqBindings, matchBindings)
              end

        val bindings = matchesToBindings matches
        val bindings' = mtailToBindings mtail
      in
        Id.IdMap.unionWithi mergeBindings (bindings, bindings')
      end

(* Testing:
 val ast = hd (Parser.getAst (Parser.read ()));
     ((a ... b) ...)

 val pattern = SyntaxRules.parsePattern (ast, []);
 val ast = hd (Parser.getAst (Parser.read ()));
     ((1 2 3) (4 5 6))

 val match = SyntaxRules.matchPattern (pattern, ast);
 val bindings = matchToBindings match;
 val _ = Id.dump (bindings, SyntaxRules.bindingToStr);
*)

end
