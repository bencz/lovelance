-- A lexical scanner generated by aflex
with text_io; use text_io;
with lovelace_dfa; use lovelace_dfa; 
with lovelace_io; use lovelace_io; 
--# line 1 "lovelace.l"
-- Caracter representable
-- Caracter representable diferent de cometa o doble cometa
--# line 34 "lovelace.l"

with lovelace_tokens, decls.p_atributs, p_semantica;
use  lovelace_tokens, decls.p_atributs, p_semantica;

package p_lexic is
    function yylex return token;
end p_lexic;
