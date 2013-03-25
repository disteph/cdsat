%{
  open EqAst
  open Core.Num
  open Core

  let _coeffs_map = fun coeffs ->
    List.fold_left
      (fun map (c, id) ->
         try
           let c' = Core.StringMap.find id map in
             Core.StringMap.add id (c +/ c') map
         with
           | Not_found ->
               Core.StringMap.add id c map)
      Core.StringMap.empty
      coeffs

  let _equation = fun coeffs sign bound ->
    { eq_coeffs = _coeffs_map coeffs ;
      eq_sign   = sign               ;
      eq_bound  = bound              ;
      id        = 0
    }
%}

%token <Big_int.big_int> INTEGER
%token <string> ID
%token PLUS MINUS DIV EOL
%token LE GE LT GT EQ

%nonassoc LE GE EQ
%left     PLUS MINUS
%nonassoc UMINUS
%start    equation

%type <EqAst.equation> equation

%%
equation:
| equation_ EOL { $1 }
;

equation_:
| coeffs eqsign num { _equation $1 $2 $3 }
;

coeffs:
| coeffs PLUS  coeff { $3 :: $1 }
| coeffs MINUS coeff { (minus_num (fst $3), snd $3) :: $1 }
| coeff              { [$1] }
;

coeff:
| ID       { (Num.num_1, $1) }
| num ID   { ($1, $2) }
| MINUS ID { (Num.num_neg_1, $2) }
;

eqsign:
| LT { `Lt }
| GT { `Gt }
| LE { `Le }
| GE { `Ge }
/*| EQ { `Eq }*/
;

num:
| INTEGER             { num_of_big_int $1 }
| INTEGER DIV INTEGER { num_of_ratio (Ratio.create_ratio $1 $3) }
| MINUS num           { minus_num $2 }
;
