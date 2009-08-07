a [aA]
b [bB]
c [cC]
d [dD]
e [eE]
f [fF]
g [gG]
h [hH]
i [iI]
j [jJ]
k [kK]
l [lL]
m [mM]
n [nN]
o [oO]
p [pP]
q [qQ]
r [rR]
s [sS]
t [tT]
u [uU]
v [vV]
w [wW]
x [xX]
y [yY]
z [zZ]
DELIMITADORS [" "\n\b\t\r\f]
DIGIT [0-9]
LLETRA [a-zA-Z]
-- Caracter representable
CRP   [\040-\176]
-- Caracter representable diferent de cometa o doble cometa
CRPNC [\040-\041\043-\046\050-\176]
%%
{DELIMITADORS}			{null;}
--
--Paraules reservades
--
{a}{n}{d}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_and;}
{a}{r}{r}{a}{y}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_array;}
{b}{e}{g}{i}{n}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_begin;}
{c}{o}{n}{s}{t}{a}{n}{t}		{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_constant;}
{e}{l}{s}{e}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_else;}
{e}{n}{d}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_end;}
{i}{f}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_if;}
{i}{n}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_in;}
{i}{s}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_is;}
{l}{o}{o}{p}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_loop;}
{m}{o}{d}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_mod;}
{n}{e}{w}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_new;}
{n}{o}{t}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_not;}
{n}{u}{l}{l}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_null;}
{o}{f}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_of;}
{o}{r}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_or;}
{o}{u}{t}					{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_out;}
{p}{r}{o}{c}{e}{d}{u}{r}{e}	{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_procedure;}
{r}{a}{n}{g}{e}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_range;}
{r}{e}{c}{o}{r}{d}			{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_record;}
{t}{h}{e}{n}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_then;}
{t}{y}{p}{e}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_type;}
{w}{h}{i}{l}{e}				{calc_atom(yylval, yy_line_number, yy_begin_column); return pc_while;}
--
--Identificadors i literals
--
{LLETRA}({LLETRA}|{DIGIT}|_({LLETRA}|{DIGIT}))* 	{calc_id(yylval, yytext, yy_line_number, yy_begin_column); return identificador;}
{DIGIT}({DIGIT})*							{calc_num(yylval, yytext, yy_line_number, yy_begin_column); return literal;}
\"({CRPNC}|\"\"|')*\"							{calc_str(yylval, yytext, yy_line_number, yy_begin_column); return literal;}
'{CRP}'									{calc_car(yylval, yytext, yy_line_number, yy_begin_column); return literal;}
--
--Simbols
--
"("	{calc_atom(yylval, yy_line_number, yy_begin_column); return s_parobert;}
")"	{calc_atom(yylval, yy_line_number, yy_begin_column); return s_partancat;}
":"	{calc_atom(yylval, yy_line_number, yy_begin_column); return s_dospunts;}
","	{calc_atom(yylval, yy_line_number, yy_begin_column); return s_coma;}
";"	{calc_atom(yylval, yy_line_number, yy_begin_column); return s_punticoma;}
"."	{calc_atom(yylval, yy_line_number, yy_begin_column); return s_punt;}
".."	{calc_atom(yylval, yy_line_number, yy_begin_column); return s_rang;}
--
--Operadors
--
"+"	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_suma;}
"-"	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_resta;}
"*"	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_multiplicacio;}
"/"	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_divisio;}
">"	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_major;}
">="	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_majorigual;}
"<"	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_menor;}
"<="	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_menorigual;}
"="	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_igual;}
"/="	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_diferent;}
":="	{calc_atom(yylval, yy_line_number, yy_begin_column); return op_assignacio;}
--
--Comentaris
--
"--"([^\n\r\f])*		{null;}
--
--Altres construccions
--
.				{return error;}
%%
with lovelace_tokens, decls.p_atributs, p_semantica;
use  lovelace_tokens, decls.p_atributs, p_semantica;

package p_lexic is
    function yylex return token;
end p_lexic;

package body p_lexic is
##
end p_lexic;
