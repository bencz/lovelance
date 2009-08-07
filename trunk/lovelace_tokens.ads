with  Decls.p_Atributs;
package Lovelace_Tokens is


   subtype yystype is decls.p_atributs.t_atribut;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, Pc_And, Pc_Array,
         Pc_Begin, Pc_Constant, Pc_Else,
         Pc_End, Pc_If, Pc_In,
         Pc_Is, Pc_Loop, Pc_Mod,
         Pc_New, Pc_Not, Pc_Null,
         Pc_Of, Pc_Or, Pc_Out,
         Pc_Procedure, Pc_Range, Pc_Record,
         Pc_Then, Pc_Type, Pc_While,
         Identificador, Literal, S_Parobert,
         S_Partancat, S_Dospunts, S_Coma,
         S_Punticoma, S_Punt, S_Rang,
         Op_Suma, Op_Resta, Op_Multiplicacio,
         Op_Divisio, Op_Major, Op_Majorigual,
         Op_Menor, Op_Menorigual, Op_Igual,
         Op_Diferent, Op_Assignacio, Menys_Unitari );

    Syntax_Error : exception;

end Lovelace_Tokens;
