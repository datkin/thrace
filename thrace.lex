%name ThraceLexer;

%let digit = [0-9];
%let alpha = [\033\036-\038\042-\043\045\047-\058\060-\090\092\094\095\097-\126]; (* printable ascii characters that aren't reserved *)

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

);

%states LINE_COMMENT;

<INITIAL> "(" => (LPAREN);
<INITIAL> ")" => (RPAREN);
<INITIAL> "[" => (LBRACK);
<INITIAL> "]" => (RBRACK);
<INITIAL> "'" => (QUOTE);
<INITIAL> "`" => (QUASIQUOTE);
<INITIAL> "," => (UNQUOTE);
<INITIAL> "#;" => (COMMENT);

<INITIAL> ";" => (YYBEGIN LINE_COMMENT; continue ());

<LINE_COMMENT> "\n" => (YYBEGIN INITIAL; continue ());
<LINE_COMMENT> . => (continue ());

<INITIAL> "-"? {digit}+ => (INT (valOf (LargeInt.fromString yytext)));
<INITIAL> "-"? {digit}+ "." {digit}+ => (FLOAT (toFloat yytext));
<INITIAL> {alpha}+ => (ID yytext);
<INITIAL> [\n\t ] => (continue ());
<INITIAL>.   => ( (* error *) continue ());
