structure Parser =
struct
structure TP = ThraceParseFn(ThraceLexer)

fun parse instrm = let
  val sm = AntlrStreamPos.mkSourcemap()
  val lex = ThraceLexer.lex sm
  val strm = ThraceLexer.streamifyInstream instrm
  val (r, strm', errs) = TP.parse lex strm
                         (* AtomMap *)
in
  print (String.concatWith
           "\n"
           (map
              (AntlrRepair.repairToString
                 ThraceTokens.toString
                 sm) errs));
  r
end
end

