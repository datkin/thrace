%name ThraceLexer;

%let digit = [0-9];
%let alpha = [\033\036-\038\042-\043\045-\058\060-\090\092\094\095\097-\126]; (* printable ascii characters that aren't reserved *)
%let alphaExt = [\033\036-\039\042-\043\045-\058\060-\090\092\094\095\097-\126]; (* printable ascii including quote, \039 *)

%defs (
  open ThraceTokens;
  type lex_result = token;
  fun eof () = EOF;

  fun toFloat (str) =
      case String.tokens (fn (chr) => chr = #".") str of
        [integer, fraction] =>
        let
          val digitStr = integer ^ fraction
          val exponent = LargeInt.fromInt (String.size fraction)
        in
          (valOf (LargeInt.fromString digitStr), exponent)
        end
      | _ => raise Fail ("Couldn't create float from " ^ str)

  val currentStr = ref "";

  fun toRat (str) =
      case String.tokens (fn (chr) => chr = #"/") str of
        [numerStr, denomStr] =>
        let
          val numer = valOf (LargeInt.fromString (numerStr))
          val denom = valOf (LargeInt.fromString (denomStr))
        in
          (numer, denom)
        end
      | _ => raise Fail ("Couldn't create rational from " ^ str)

);

%states COMMENT_S STRING_S;

<INITIAL> "(" => (LPAREN);
<INITIAL> ")" => (RPAREN);
<INITIAL> "[" => (LBRACK);
<INITIAL> "]" => (RBRACK);
<INITIAL> "'" => (QUOTE);
<INITIAL> "`" => (QUASIQUOTE);
<INITIAL> "," => (UNQUOTE);
<INITIAL> "#;" => (COMMENT);

<INITIAL> ";" => (YYBEGIN COMMENT_S; continue ());

<COMMENT_S> "\n" => (YYBEGIN INITIAL; continue ());
<COMMENT_S> . => (continue ());

<INITIAL> "\"" => (YYBEGIN STRING_S; continue ());
<STRING_S> {alpha}* => (currentStr := !currentStr ^ yytext; continue ());
<STRING_S> "\"" => (YYBEGIN INITIAL;
                    let val str = STRING (!currentStr) in
                      currentStr := ""; str
                    end);

<INITIAL> "-"? {digit}+ => (INT (valOf (LargeInt.fromString yytext)));
<INITIAL> "-"? {digit}+ "." {digit}+ => (FLOAT (toFloat yytext));
<INITIAL> "-"? {digit}+ "/" {digit}+ => (RATIONAL (toRat yytext));
<INITIAL> {alpha}{alphaExt}* => (ID yytext);
<INITIAL> [\n\t ]+ => (skip ());
<INITIAL>.   => ( (* error *) continue ());
