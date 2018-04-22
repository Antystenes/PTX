%{
#include <string.h>
#include <libguile.h>
#define YYSTYPE SCM


SCM make_var;

SCM scheme_func(const char* fname) {
    return scm_variable_ref(scm_c_lookup(fname));
}

void printer(SCM expr){
    SCM expr2 = scm_call_1(scheme_func("reduce-expr"), expr);
    SCM pretty_printed = scm_call_1(scheme_func("pretty-display"), expr2);
    // char* result = scm_to_locale_string(pretty_printed);
    // printf("%s\n", result);
    //free(result);
}

%}

%left  APPLICATION
%token LAMBDA
%token ABSTRACTION
%token '('
%token ')'
%token VARIABLE
%token EVAL
%token MACRO
%right DEFINITION;
%%

statements : statement
           | statements statement;

statement : term EVAL { printer($1); }
          | VARIABLE DEFINITION term EVAL { $$ = scm_call_2(scheme_func("define-macro"), $1, $3);}
          ;

term : LAMBDA variables ABSTRACTION term
       { $$ = scm_call_2(scheme_func("make-abstraction"), $2, $4);}
     | VARIABLE { $$ = scm_call_1(scheme_func("make-variable"), $1); }
     | term APPLICATION term
       { $$ = scm_call_2(scheme_func("make-application"), $1, $3);}
     | '(' term ')' { $$ = $2; }
     | MACRO VARIABLE { $$ = scm_call_1(scheme_func("make-macro"), $2);}
     ;

variables : VARIABLE { $$ = scm_call_1(scheme_func("list"), $1); }
          | variables APPLICATION VARIABLE { $$ = scm_call_2(scheme_func("append"),$1, scm_call_1(scheme_func("list"), $3));}
