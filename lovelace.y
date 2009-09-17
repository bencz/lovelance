%token pc_and
%token pc_array
%token pc_begin
%token pc_constant
%token pc_else
%token pc_end
%token pc_if
%token pc_in
%token pc_is
%token pc_loop
%token pc_mod
%token pc_new
%token pc_not
%token pc_null
%token pc_of
%token pc_or
%token pc_out
%token pc_procedure
%token pc_range
%token pc_record
%token pc_then
%token pc_type
%token pc_while

%token identificador
%token literal

%token s_parobert
%token s_partancat
%token s_dospunts
%token s_coma
%token s_punticoma
%token s_punt
%token s_rang

%token op_suma
%token op_resta
%token op_multiplicacio
%token op_divisio
%token op_major
%token op_majorigual
%token op_menor
%token op_menorigual
%token op_igual
%token op_diferent
%token op_assignacio

%left pc_or
%left pc_and
%left pc_not

%left op_suma
    op_resta

%left menys_unitari

%left op_multiplicacio
    op_divisio
    pc_mod

%nonassoc op_major
        op_majorigual
        op_menor
        op_menorigual
        op_igual
        op_diferent

%with decls.p_atributs
{
   subtype yystype is decls.p_atributs.t_atribut;
}

%%

PROG:
    DECL_PROG
    {rs_prog($1);}
  ;

DECL_PROG:
    pc_procedure ENCAP pc_is
      DECLS
    pc_begin M_DECL_PROG
      SENTS
    pc_end identificador s_punticoma
    {rs_decl_prog($$, $2, $9);}
  ;

M_DECL_PROG:
     {rs_m_decl_prog;}
  ;

ENCAP:
    identificador
    {rs_encap_id($$,$1);}
  | P_ENCAP s_partancat
    {rs_encap_p_encap($$,$1);}
  ;

P_ENCAP:
    identificador s_parobert ARGUMENT
    {rs_p_encap_id($$, $1, $3);}
  | P_ENCAP s_punticoma ARGUMENT
    {rs_p_encap_rec($$, $1, $3);}
  ;

ARGUMENT:
    identificador s_dospunts MODE identificador
    {rs_argument($$, $1, $3, $4);}
  ;

MODE:
    pc_in
    {rs_mode_in($$);}
  | pc_out
    {rs_mode_out($$);}
  | pc_in pc_out
    {rs_mode_in_out($$);}
  ;

DECLS:
    DECLS DECL
  |
  ;

DECL:
    DECL_PROG
  | DECL_VAR
  | DECL_CONST
  | DECL_TIPUS
  ;

DECL_VAR:
    identificador C_DECL_VAR
    {rs_decl_var($$, $1, $2);}
  ;

C_DECL_VAR:
    s_dospunts identificador s_punticoma
    {rs_c_decl_var($$, $2);}
  | s_coma identificador C_DECL_VAR
    {rs_c_decl_var_rec($$, $3, $2);}
  ;

DECL_CONST:
    identificador s_dospunts pc_constant identificador op_assignacio literal s_punticoma
    {rs_decl_const($1, $4, $6);}
  ;

DECL_TIPUS:
    DECL_RECORD
  | DECL_ARRAY
  | DECL_SUBRANG
  ;

DECL_RECORD:
    P_RECORD pc_end pc_record s_punticoma
    {rs_decl_record($1);}
  ;

P_RECORD:
    P_RECORD identificador s_dospunts identificador s_punticoma
    {rs_p_record_rec($$, $1, $2, $4);}
  | pc_type identificador pc_is pc_record identificador s_dospunts identificador s_punticoma
    {rs_p_record($$, $2, $5, $7);}
  ;

DECL_ARRAY:
    P_ARRAY s_partancat pc_of identificador s_punticoma
    {rs_decl_array($1, $4);}
  ;

P_ARRAY:
    pc_type identificador pc_is pc_array s_parobert identificador
    {rs_p_array($$, $2, $6);}
  | P_ARRAY s_coma identificador
    {rs_p_array_rec($$, $1, $3);}
  ;

DECL_SUBRANG:
    pc_type identificador pc_is pc_new identificador pc_range LIM s_rang LIM s_punticoma
    {rs_decl_subrang($2, $5, $7, $9);}
  ;
  
LIM:
    identificador
    {rs_lim_id($$, $1);}
  | literal
    {rs_lim_lit($$, $1);}
  ;
  
REF:
    identificador
    {rs_ref_id($$, $1);}
  | REF s_punt identificador
    {rs_ref_rec($$, $1, $3);}
  | PRMB_RIND s_partancat
    {rs_ref_prmb_rind($$, $1);}
  ;

PRMB_RIND:
    REF s_parobert E
    {rs_prmb_rind($$, $1, $3);}
  | PRMB_RIND s_coma E
    {rs_prmb_rind_rec($$, $1, $3);}
  ;

E:
    op_resta E %prec menys_unitari
    {rs_e_menys_unitari($$, $2);}
  | s_parobert E s_partancat
    {rs_e_par($$, $2);}
  | literal
    {rs_e_lit($$, $1);}
  | REF
    {rs_e_ref($$, $1);}
  | E op_major E
    {rs_op_major($$, $1, $3);}
  | E op_majorigual E
    {rs_op_major_igual($$, $1, $3);}
  | E op_menor E
    {rs_op_menor($$, $1, $3);}
  | E op_menorigual E
    {rs_op_menor_igual($$, $1, $3);}
  | E op_igual E
    {rs_op_igual($$, $1, $3);}
  | E op_diferent E
    {rs_op_diferent($$, $1, $3);}
  | E op_suma E
    {rs_op_suma($$, $1, $3);}
  | E op_resta E
    {rs_op_resta($$, $1, $3);}
  | E op_multiplicacio E
    {rs_op_mult($$, $1, $3);}
  | E op_divisio E
    {rs_op_div($$, $1, $3);}
  | E pc_and E
    {rs_op_and($$, $1, $3);}
  | E pc_or E
    {rs_op_or($$, $1, $3);}
  | pc_not E
    {rs_op_not($$, $2);}
  | E pc_mod E
    {rs_op_mod($$, $1, $3);}
  ;
  
SENTS:
    SENTS SENT
  | SENT
  ;

SENT:
    SENT_IF
  | SENT_WHILE
  | SENT_ASSIG
  | SENT_CRID
  ;

SENT_IF:
    P_SENT_IF SENTS pc_end pc_if s_punticoma
    {rs_sent_if;}
  | P_SENT_IF SENTS pc_else M_IF SENTS pc_end pc_if s_punticoma
    {rs_sent_if_else($4);}
  ;

P_SENT_IF:
    pc_if E pc_then
    {rs_p_sent_if($2);}
  ;
  
M_IF:
    {rs_m_if($$);}
  ;

SENT_WHILE:
    P_SENT_WHILE SENTS pc_end pc_loop s_punticoma
    {rs_sent_while($1);}
  ;

P_SENT_WHILE:
    pc_while M_WHILE E pc_loop
    {rs_p_sent_while($$, $2, $3);}
  ;

M_WHILE:
    {rs_m_while($$);}
  ;

SENT_ASSIG:
    REF op_assignacio E s_punticoma
    {rs_sent_assig($1, $3);}
  ;

SENT_CRID:
    REF s_punticoma
    {rs_sent_crid($1);}
  ;

%%
package p_sintactica is
    procedure yyparse;
end p_sintactica;

with text_io;
with p_lexic;
with lovelace_tokens;
with lovelace_goto;
with lovelace_shift_reduce;
with p_semantica;

use text_io;
use p_lexic;
use lovelace_tokens;
use lovelace_goto;
use lovelace_shift_reduce;
use p_semantica;

package body p_sintactica is
	procedure yyerror(str : in string) is
	begin
		put_line(str & ": " & yylval.a'img);
		put_line("Linia: " & yylval.linia'img);
		put_line("Columna: " & yylval.columna'img);	
	end yyerror;
##
end p_sintactica;
