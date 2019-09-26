#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <readline/readline.h>
#include <readline/history.h>

#define car(p) ((p).value.pair->atom[0])
#define cdr(p) ((p).value.pair->atom[1])
#define nilp(atom) ((atom).type == AtomType_Nil)
#define symbolp(atom) ((atom).type == AtomType_Symbol)
#define pairp(atom) ((atom).type == AtomType_Pair)
#define integerp(atom) ((atom).type == AtomType_Integer)
#define builtinp(atom) ((atom).type == AtomType_Builtin)
#define closurep(atom) ((atom).type == AtomType_Closure)
#define macrop(atom) ((atom).type == AtomType_Macro)
#define tosymbol(p) (p).value.symbol
#define tointeger(p) (p).value.integer
#define tobuiltin(p) (p).value.builtin

struct Atom;

typedef int (*Builtin)(struct Atom args, struct Atom* result);

const char* const TypeNames[] = {
    "NIL",
    "PAIR",
    "SYMBOL",
    "INTEGER",
    "BUILTIN",
    "CLOSURE",
    "MACRO",
};

typedef struct Atom {
    enum {
        AtomType_Nil,
        AtomType_Pair,
        AtomType_Symbol,
        AtomType_Integer,
        AtomType_Builtin,
        AtomType_Closure,
        AtomType_Macro,
    } type;

    union {
        struct Pair* pair;
        const char*  symbol;
        long         integer;
        Builtin      builtin;
    } value;
} Atom;

const char* typename(Atom p) { return TypeNames[p.type]; }

struct Pair {
    struct Atom atom[2];
};

Atom F_IF;
Atom F_QUOTE;
Atom F_DEFINE;
Atom F_LAMBDA;
Atom F_DEFMACRO;
Atom F_QUASIQUOTE;
Atom F_UNQUOTE_SPLICING;
Atom F_UNQUOTE;
Atom F_AND;

typedef enum {
    Error_OK = 0,
    Error_Syntax,
    Error_Unbound,
    Error_Args,
    Error_Type,
} Error;

const char* ErrorNames[] = {
    "",
    "syntax error",
    "unbound symbol",
    "argument error",
    "type error",
};

const char* errormsg = NULL;
int         errorlinum = -1;

#define panic(fmt, ...) do { fprintf(stderr, "ERR: " fmt "\n", ##__VA_ARGS__); exit(1); } while(0)
#define SETERR(x, ...) seterr(__LINE__, "error(%d): " x, __LINE__, ##__VA_ARGS__)

#define THROW(err, msg, ...) do { \
    seterr(__LINE__, "%s [%d]:" msg, ErrorNames[err], __LINE__, ##__VA_ARGS__); \
    return err; \
} while(0)

#define CHECKTYPE(x, pred) do { if (!pred(x)) { panic("type error"); return Error_Type; } } while (0)
#define CHECKARGN(args, n) do { if (list_length(args) != n) { return Error_Args; } } while (0)

void seterr(int linum, const char *fmt, ...)
{
    int size = 0;
    char *p = NULL;
    va_list ap;

    if (errormsg != NULL)
        return;

    /* Determine required size */
    va_start(ap, fmt);
    size = vsnprintf(p, size, fmt, ap);
    va_end(ap);

    if (size < 0)
        panic("failed to determine size for error message");

    size++;             /* For '\0' */
    p = malloc(size); assert(p);
    va_start(ap, fmt);
    size = vsnprintf(p, size, fmt, ap);
    va_end(ap);
    if (size < 0) {
        panic("vsnprintf failed");
    }
    errormsg = p;
    errorlinum = linum;
}

#define TRY(err) do { int rc = (err); if (rc != Error_OK) return rc; } while (0)
const Atom nil = { .type = AtomType_Nil };

int listp(Atom expr)
{
    while (!nilp(expr)) {
        if (!pairp(expr) && !closurep(expr))
            return 0;
        expr = cdr(expr);
    }
    return 1;
}

int list_length(Atom list)
{
    int result = 0;
    while (!nilp(list)) {
        assert(pairp(list));
        ++result;
        list = cdr(list);
    }
    return result;
}

Atom cons(Atom carval, Atom cdrval)
{
    Atom p;
    p.type = AtomType_Pair;
    p.value.pair = malloc(sizeof(struct Pair));
    car(p) = carval;
    cdr(p) = cdrval;
    return p;
}

/*
 * make_* routines
 */
Atom make_int(long x)
{
    Atom a;
    a.type = AtomType_Integer;
    a.value.integer = x;
    return a;
}

Atom make_builtin(Builtin fn)
{
    Atom a;
    a.type = AtomType_Builtin;
    a.value.builtin = fn;
    return a;
}

Atom sym_table = { .type = AtomType_Nil };

Atom make_sym(const char* s)
{
    Atom a, p = sym_table;
    while (!nilp(p)) {
        assert(p.type == AtomType_Pair);
        a = car(p);
        if (!strcmp(tosymbol(a), s)) {
            return a;
        }
        p = cdr(p);
    }
    p.type = AtomType_Symbol;
    tosymbol(p) = strdup(s);
    sym_table = cons(p, sym_table);
    return p;
}

int make_closure(Atom env, Atom args, Atom body, Atom* result)
{
    Atom p;
    if (!listp(body)) {
        return Error_Syntax;
    }
    p = args;
    while (!nilp(p) && !symbolp(p)) {
        if (!pairp(p) || !symbolp(car(p))) {
            return Error_Type;
        }
        p = cdr(p);
    }
    *result = cons(env, cons(args, body));
    result->type = AtomType_Closure;
    return Error_OK;
}

Atom copy_list(Atom list)
{
    Atom a, p;
    if (nilp(list))
        return nil;

    a = cons(car(list), nil);
    p = a;
    list = cdr(list);
    while (!nilp(list)) {
        cdr(p) = cons(car(list), nil);
        p = cdr(p);
        list = cdr(list);
    }
    return a;
}

void print_expr(Atom atom)
{
    switch (atom.type) {
        case AtomType_Nil:
            printf("NIL");
            break;
        case AtomType_Pair:
            putchar('(');
            print_expr(car(atom));
            atom = cdr(atom);
            while (!nilp(atom)) {
                if (atom.type == AtomType_Pair) {
                    putchar(' ');
                    print_expr(car(atom));
                    atom = cdr(atom);
                } else {
                    printf(" . ");
                    print_expr(atom);
                    break;
                }
            }
            putchar(')');
            break;
        case AtomType_Symbol:
            printf("%s", tosymbol(atom));
            break;
        case AtomType_Integer:
            printf("%ld", tointeger(atom));
            break;
        case AtomType_Builtin:
            printf("#<BUILTIN:%p>", tobuiltin(atom));
            break;
        case AtomType_Closure:
            printf("#<CLOSURE:%p>", atom.value.pair);
            break;
        case AtomType_Macro:
            printf("#<MACRO>");
            break;
    }
}

int lex(const char* str, const char** start, const char** end)
{
    const char* const ws     = " \t\n";
    const char* const delim  = "() \t\n";
    const char* const prefix = "()\'`";

    str += strspn(str, ws);
    while (*str == ';') {
        str = strchr(str, '\n');
        if (str == NULL) {
            *start = *end = NULL;
            return Error_Syntax;
        }
        str += strspn(str, ws);
    }
    if (*str == '\0') {
        *start = *end = NULL;
        SETERR("found unexpected end-of-input");
        return Error_Syntax;
    }

    *start = str;
    if (strchr(prefix, *str) != NULL)
        *end = str + 1;
    else if (str[0] == ',')
        *end = str + (str[1] == '@' ? 2 : 1);
    else
        *end = str + strcspn(str, delim);

    return Error_OK;
}

int parse_simple(const char* start, const char* end, Atom* result)
{
    char* buf, *p;

    /* Try Integer */
    long val = strtol(start, &p, 10);
    if (p == end) {
        result->type = AtomType_Integer;
        result->value.integer = val;
        return Error_OK;
    }

    /* Try NIL or Symbol */
    buf = malloc(end - start + 1);
    p = buf;
    while (start != end)
        *p++ = toupper(*start++);
    *p = '\0';
    if (!strcmp(buf, "NIL"))
        *result = nil;
    else
        *result = make_sym(buf);
    free(buf);
    return Error_OK;
}

int read_expr(const char* input, const char** end, Atom* result);

int read_list(const char* start, const char** end, Atom* result)
{
    Atom p;
    *end = start;
    p = *result = nil;
    for (;;) {
        const char* token;
        Atom item;

        TRY(lex(*end, &token, end));
        if (token[0] == ')') {
            return Error_OK;
        } else if (token[0] == '.' && *end - token == 1) { // TODO: check this 2nd condition
            /* Improper list */
            if (nilp(p)) {
                SETERR("improper list found in list context");
                return Error_Syntax;
            }
            TRY(read_expr(*end, end, &item));
            cdr(p) = item;
            TRY(lex(*end, &token, end));
            if (token[0] != ')') {
                SETERR("list missing closing paren");
                return Error_Syntax;
            }
            return Error_OK;
        }

        TRY(read_expr(token, end, &item));
        if (nilp(p)) {
            *result = cons(item, nil);
            p = *result;
        } else {
            cdr(p) = cons(item, nil);
            p = cdr(p);
        }
    }
}

int read_expr(const char* input, const char** end, Atom* result)
{
    const char* token;
    TRY(lex(input, &token, end));
    if (token[0] == '(') {
        return read_list(*end, end, result);
    } else if (token[0] == ')') {
        SETERR("expression list missing closing paren");
        return Error_Syntax;
    } else if (token[0] == '\'') {
        *result = cons(F_QUOTE, cons(nil, nil));
        return read_expr(*end, end, &car(cdr(*result)));
    } else if (token[0] == '`') {
        *result = cons(F_QUASIQUOTE, cons(nil, nil));
        return read_expr(*end, end, &car(cdr(*result)));
    } else if (token[0] == ',') {
        Atom sym = token[1] == '@' ? F_UNQUOTE_SPLICING : F_UNQUOTE;
        *result = cons(sym, cons(nil, nil));
        return read_expr(*end, end, &car(cdr(*result)));
    } else {
        return parse_simple(token, *end, result);
    }
}

Atom env_create(Atom parent) {
    return cons(parent, nil);
}

int env_get(Atom env, Atom symbol, Atom* result)
{
    Atom parent = car(env);
    Atom entries = cdr(env);
    while (!nilp(entries)) {
        Atom entry = car(entries);
        if (tosymbol(car(entry)) == tosymbol(symbol)) {
            *result = cdr(entry);
            return Error_OK;
        }
        entries = cdr(entries);
    }
    if (nilp(parent)) {
        SETERR("%s", tosymbol(symbol));
        return Error_Unbound;
    }
    return env_get(parent, symbol, result);
}

// TODO: this function doesn't need a return code?
int env_set(Atom env, Atom symbol, Atom value)
{
    Atom entries = cdr(env);
    while (!nilp(entries)) {
        Atom entry = car(entries);
        if (tosymbol(car(entry)) == tosymbol(symbol)) {
            cdr(entry) = value;
            return Error_OK;
        }
        entries = cdr(entries);
    }
    Atom entry = cons(symbol, value);
    cdr(env) = cons(entry, cdr(env));
    return Error_OK;
}

int eval_expr(Atom expr, Atom env, Atom* result);

int apply(Atom fn, Atom args, Atom* result)
{
    Atom env, params, body;

    if (builtinp(fn))
        return (*tobuiltin(fn))(args, result);
    else if (!closurep(fn)) {
        SETERR("tried to apply object that is not builtin or closure: %s", typename(fn));
        return Error_Type;
    }

    env = env_create(car(fn));
    params = car(cdr(fn));
    body = cdr(cdr(fn));

    while (!nilp(params) && !nilp(args)) {
        if (symbolp(params)) {
            env_set(env, params, args);
            params = nil;
            args = nil;
            break;
        }
        env_set(env, car(params), car(args));
        params = cdr(params);
        args   = cdr(args);
    }
    if (!nilp(params) && !nilp(args))
        return Error_Args;

    while (!nilp(body)) {
        TRY(eval_expr(car(body), env, result));
        body = cdr(body);
    }

    return Error_OK;
}

int symcmp(Atom a, Atom b) {
    assert(symbolp(a));
    assert(symbolp(b));
    return tosymbol(a) == tosymbol(b);
}

int eval_expr(Atom expr, Atom env, Atom* result)
{
    Atom op, args, p;

    if (symbolp(expr)) {
        return env_get(env, expr, result);
    } else if (!pairp(expr)) {
        *result = expr;
        return Error_OK;
    } else if (!listp(expr) /* && !closurep(expr) */) {
        SETERR("expression is not symbol, pair, or list: %s", typename(expr));
        return Error_Syntax;
    }

    op = car(expr);
    args = cdr(expr);
    if (symbolp(op)) {
        if (symcmp(op, F_IF)) {
            // form ( <IF> . ( <TEST> . ( <TRUE-EXPR> . ( <FALSE-EXPRE> . NIL ) ) ) )
            Atom cond, val;
            CHECKARGN(args, 3);
            TRY(eval_expr(car(args), env, &cond));
            val = nilp(cond) ? car(cdr(cdr(args))) : car(cdr(args));
            return eval_expr(val, env, result);
        } else if (symcmp(op, F_AND)) {
            // form ( <AND> <ARGS>... )
            if (nilp(args))
                return Error_Args;
            Atom cond;
            while (!nilp(args)) {
                assert(pairp(args));
                TRY(eval_expr(car(args), env, &cond));
                if (nilp(cond)) {
                    *result = nil;
                    return Error_OK;
                }
                args = cdr(args);
            }
            *result = make_int(1);
            return Error_OK;
        } else if (symcmp(op, F_QUOTE)) {
            // form ( <QUOTE> . ( <ATOM> . NIL ) )
            CHECKARGN(args, 1);
            *result = car(args);
            return Error_OK;
        } else if (symcmp(op, F_DEFINE)) {
            // form #1: ( <DEFINE> . ( <SYMBOL> . ( <EXPR> . NIL ) ) )
            // form #2: ( <DEFINE> . ( ( <NAME> . <ARGS>... ) . ( <BODY> . NIL ) ) )
            Atom sym, val;
            if (nilp(args)) return Error_Syntax;
            sym = car(args);
            if (symbolp(sym)) { // form #1
                CHECKARGN(args, 2);
                sym = car(args);
                CHECKTYPE(sym, symbolp);
                TRY(eval_expr(car(cdr(args)), env, &val));
            } else if (pairp(sym)) { // form #2
                CHECKARGN(args, 2);
                TRY(make_closure(env, cdr(sym), cdr(args), &val));
                sym = car(sym);
                CHECKTYPE(sym, symbolp);
            } else {
                return Error_Syntax;
            }
            *result = sym;
            return env_set(env, sym, val);
        } else if (symcmp(op, F_LAMBDA)) {
            // form: ( <LAMBDA> . ( (ARGS...) . ( (BODY...) . NIL ) ) )
            if (nilp(args) || nilp(cdr(args)))
                return Error_Args;
            return make_closure(env, car(args), cdr(args), result);
        } else if (symcmp(op, F_DEFMACRO)) {
            Atom name, macro;

            if (nilp(args) || nilp(cdr(args)))
                return Error_Args;
            if (!pairp(car(args)))
                return Error_Syntax;
            name = car(car(args));
            if (!symbolp(name)) {
                SETERR("expected symbol for name of macro: %s", typename(name));
                return Error_Type;
            }
            TRY(make_closure(env, cdr(car(args)), cdr(args), &macro));
            macro.type = AtomType_Macro;
            *result = name;
            return env_set(env, name, macro);
        }
    }

    /* Evaluate operator */
    TRY(eval_expr(op, env, &op));

    /* Is it a macro? */
    if (macrop(op)) {
        Atom expansion;
        op.type = AtomType_Closure;
        TRY(apply(op, args, &expansion));
        return eval_expr(expansion, env, result);
    }

    /* Evaluate arguments */
    // TODO: why do I need to copy the list?
    args = copy_list(args);
    p = args;
    while (!nilp(p)) {
        TRY(eval_expr(car(p), env, &car(p)));
        p = cdr(p);
    }

    return apply(op, args, result);
}

int builtin_car(Atom args, Atom* result)
{
    if (nilp(args) || !nilp(cdr(args))) {
        return Error_Args;
    } else if (nilp(car(args))) {
        *result = nil;
    } else if (!pairp(car(args))) {
        return Error_Type;
    } else {
        *result = car(car(args));
    }
    return Error_OK;
}

int builtin_cdr(Atom args, Atom* result)
{
    if (nilp(args) || !nilp(cdr(args))) {
        return Error_Args;
    } else if (nilp(car(args))) {
        *result = nil;
    } else if (!pairp(car(args))) {
        return Error_Type;
    } else {
        *result = cdr(car(args));
    }
    return Error_OK;
}

int builtin_cons(Atom args, Atom* result)
{
    // if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
    if (nilp(args) || list_length(args) != 2)
        return Error_Args;
    *result = cons(car(args), car(cdr(args)));
    return Error_OK;
}

int builtin_add(Atom args, Atom* result)
{
    int val = 0;
    while (!nilp(args)) {
        Atom a = car(args);
        if (!integerp(a))
            return Error_Args;
        val += tointeger(a);
        args = cdr(args);
    }
    *result = make_int(val);
    return Error_OK;
}

int builtin_subtract(Atom args, Atom* result)
{
    if (nilp(args)) {
        *result = make_int(0);
        return Error_OK;
    }

    Atom a = car(args);
    if (!integerp(a)) return Error_Args;
    int val = tointeger(a);
    args = cdr(args);
    if (nilp(args)) {
        *result = make_int(-val);
        return Error_OK;
    }

    while (!nilp(args)) {
        a = car(args);
        if (!integerp(a)) return Error_Args;
        val -= tointeger(a);
        args = cdr(args);
    }

    *result = make_int(val);
    return Error_OK;
}

int builtin_multiply(Atom args, Atom* result)
{
    int val = 1;
    while (!nilp(args)) {
        Atom a = car(args);
        if (!integerp(a))
            return Error_Args;
        val *= tointeger(a);
        args = cdr(args);
    }
    *result = make_int(val);
    return Error_OK;
}

int builtin_divide(Atom args, Atom* result)
{
    if (nilp(args)) {
        *result = make_int(1);
        return Error_OK;
    }
    if (!integerp(car(args)))
        return Error_Args;
    int val = tointeger(car(args));
    args = cdr(args);
    while (!nilp(args)) {
        val /= tointeger(car(args));
        args = cdr(args);
    }
    *result = make_int(val);
    return Error_OK;
}

int builtin_numeq(Atom args, Atom* result)
{
    if (list_length(args) != 2)
        return Error_Args;
    Atom a = car(args);
    Atom b = car(cdr(args));
    if (!integerp(a) && !integerp(b)) {
        return Error_Type;
    }
    *result = (tointeger(a) == tointeger(b)) ? make_sym("T") : nil;
    return Error_OK;
}

int builtin_numlt(Atom args, Atom* result)
{
    if (list_length(args) != 2)
        return Error_Args;
    Atom a = car(args);
    Atom b = car(cdr(args));
    if (!integerp(a) && !integerp(b)) {
        return Error_Type;
    }
    *result = (tointeger(a) < tointeger(b)) ? make_sym("T") : nil;
    return Error_OK;
}

int builtin_numgt(Atom args, Atom* result)
{
    if (list_length(args) != 2)
        return Error_Args;
    Atom a = car(args);
    Atom b = car(cdr(args));
    if (!integerp(a) && !integerp(b)) {
        return Error_Type;
    }
    *result = (tointeger(a) > tointeger(b)) ? make_sym("T") : nil;
    return Error_OK;
}

int builtin_numlte(Atom args, Atom* result)
{
    if (list_length(args) != 2)
        return Error_Args;
    Atom a = car(args);
    Atom b = car(cdr(args));
    if (!integerp(a) && !integerp(b))
        return Error_Type;
    *result = (tointeger(a) <= tointeger(b)) ? make_sym("T") : nil;
    return Error_OK;
}

int builtin_numgte(Atom args, Atom* result)
{
    if (list_length(args) != 2)
        return Error_Args;
    Atom a = car(args);
    Atom b = car(cdr(args));
    if (!integerp(a) && !integerp(b))
        return Error_Type;
    *result = (tointeger(a) >= tointeger(b)) ? make_sym("T") : nil;
    return Error_OK;
}

int builtin_apply(Atom args, Atom* result)
{
    // form: (APPLY fn arg-list)
    Atom fn;
    if (list_length(args) != 2)
        return Error_Args;

    fn = car(args);
    args = car(cdr(args));
    CHECKTYPE(args, listp);
    return apply(fn, args, result);
}

int builtin_eq(Atom args, Atom* result)
{
    if (list_length(args) != 2)
        return Error_Args;
    int eq = 0;
    Atom a = car(args);
    Atom b = car(cdr(args));
    if (a.type == b.type) {
        switch (a.type) {
            case AtomType_Nil:
                eq = 1;
                break;
            case AtomType_Pair:
            case AtomType_Closure:
            case AtomType_Macro:
               eq = a.value.pair == b.value.pair;
               break;
            case AtomType_Symbol:
               eq = tosymbol(a) == tosymbol(b);
               break;
            case AtomType_Integer:
               eq = tointeger(a) == tointeger(b);
               break;
            case AtomType_Builtin:
               eq = tobuiltin(a) == tobuiltin(b);
               break;
        }
    }
    *result = eq ? make_sym("T") : nil;
    return Error_OK;
}

int builtin_pairp(Atom args, Atom* result)
{
    CHECKARGN(args, 1);
    *result = pairp(car(args)) ? make_sym("T") : nil;
    return Error_OK;
}

char* slurp(const char* filename)
{
    /* for simplicity just read entire file into one giant buffer */
    FILE* file;
    char* buf;
    long len;
    file = fopen(filename, "r");
    if (!file) return NULL;
    fseek(file, 0, SEEK_END);
    len = ftell(file);
    fseek(file, 0, SEEK_SET);
    buf = malloc(len);
    fread(buf, 1, len, file);
    fclose(file);
    return buf;
}

void print_error(const char* msg)
{
    if (errormsg) {
        printf("%s: %s (%d)\n", msg, errormsg, errorlinum);
    } else {
        printf("%s\n", msg);
    }
}

void load_file(Atom env, const char* filename)
{
    char* text = slurp(filename);
    if (text) {
        const char* p = text;
        Atom expr;
        while (read_expr(p, &p, &expr) == Error_OK) {
            Atom result;
            Error err = eval_expr(expr, env, &result);
            if (err != Error_OK) {
                switch (err) {
                    case Error_OK:
                        print_expr(result); putchar('\n');
                        break;
                    case Error_Syntax:
                        print_error("syntax error");
                        break;
                    case Error_Unbound:
                        print_error("unbound symbol error");
                        break;
                    case Error_Args:
                        print_error("argument error\n");
                        break;
                    case Error_Type:
                        print_error("incorrect type error");
                        break;
                }
                printf("Error in expression:\n\t");
                print_expr(expr); putchar('\n');
            } else {
                assert(err == Error_OK);
                print_expr(result); putchar('\n');
            }
        }
        errormsg   = NULL;
        errorlinum = -1;
    }
    free(text);
}

Atom init() {
    F_QUOTE            = make_sym("QUOTE");
    F_DEFINE           = make_sym("DEFINE");
    F_LAMBDA           = make_sym("LAMBDA");
    F_IF               = make_sym("IF");
    F_DEFMACRO         = make_sym("DEFMACRO");
    F_QUASIQUOTE       = make_sym("QUASIQUOTE");
    F_UNQUOTE_SPLICING = make_sym("UNQUOTE-SPLICING");
    F_UNQUOTE          = make_sym("UNQUOTE");
    F_AND              = make_sym("AND");

    Atom env = env_create(nil);

    env_set(env, make_sym("CAR"),   make_builtin(&builtin_car));
    env_set(env, make_sym("CDR"),   make_builtin(&builtin_cdr));
    env_set(env, make_sym("CONS"),  make_builtin(&builtin_cons));
    env_set(env, make_sym("APPLY"), make_builtin(&builtin_apply));
    env_set(env, make_sym("EQ?"),   make_builtin(&builtin_eq));
    env_set(env, make_sym("PAIR?"), make_builtin(&builtin_pairp));
    env_set(env, make_sym("+"),     make_builtin(&builtin_add));
    env_set(env, make_sym("-"),     make_builtin(&builtin_subtract));
    env_set(env, make_sym("*"),     make_builtin(&builtin_multiply));
    env_set(env, make_sym("/"),     make_builtin(&builtin_divide));
    env_set(env, make_sym("="),     make_builtin(&builtin_numeq));
    env_set(env, make_sym("<"),     make_builtin(&builtin_numlt));
    env_set(env, make_sym(">"),     make_builtin(&builtin_numgt));
    env_set(env, make_sym("<="),    make_builtin(&builtin_numlte));
    env_set(env, make_sym(">="),    make_builtin(&builtin_numgte));

    env_set(env, make_sym("T"), make_sym("T"));
    env_set(env, make_sym("F"), make_sym("F"));

    // TODO: make smarter about finding library
    load_file(env, "library.lisp");

    return env;
}

void execute(const char* p, Atom env)
{
    Error err;
    Atom expr, result;
    err = read_expr(p, &p, &expr);
    if (err == Error_OK) {
        err = eval_expr(expr, env, &result);
    }
    switch (err) {
        case Error_OK:
            print_expr(result); putchar('\n');
            break;
        case Error_Syntax:
            print_error("syntax error");
            break;
        case Error_Unbound:
            print_error("unbound symbol error");
            break;
        case Error_Args:
            print_error("argument error\n");
            break;
        case Error_Type:
            print_error("incorrect type error");
            break;
    }
}

void repl()
{
    Atom env = init();
    char* buf = NULL;
    while ((buf = readline("> ")) != NULL) {
        if (strlen(buf) > 0) {
            add_history(buf);
        }
        const char* input = buf + strspn(buf, " \t\n");
        if (input[0] != '\0') {
            execute(input, env);
        }
        free(buf);
    }
}

void from_file(const char* filename)
{
    FILE *stream;
    char *line = NULL;
    size_t len = 0;
    ssize_t nread;
    Atom env = init();
    stream = fopen(filename, "r");
    if (stream == NULL) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }
    while ((nread = getline(&line, &len, stream)) != -1) {
        line[nread-1] = '\0';
        const char* input = line + strspn(line, " \t\n");
        if (input[0] != '\0') {
            printf("%s\n> ", input);
            execute(input, env);
        }
    }
    free(line);
    fclose(stream);
}

int main(int argc, char** argv)
{
    if (argc > 2) {
        fprintf(stderr, "Usage: %s [FILE]\n", argv[0]);
        exit(0);
    } else if (argc == 2) {
        from_file(argv[1]);
    } else {
        repl();
    }
    return 0;
}
