%{
#include <libguile.h>
#include <string.h>
#include "y.tab.h"

%}

%%
[a-z]    { yylval = scm_from_int(*yytext); return VARIABLE; }
"\\"      return LAMBDA;
"("       return '(';
")"       return ')';
"."       return ABSTRACTION;
"#"       return MACRO;
"="       return DEFINITION;
[ \t]+    return APPLICATION;
\n        return EVAL;
%%

int yyerror(int c){
  printf("asdf\n");
  return 1;
}

int main(int argc, char** argv){

    scm_init_guile();
    scm_c_primitive_load("src/lambda_exprs.scm");
    yyparse();

    return 0;
}
