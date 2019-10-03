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
    "nil",
    "pair",
    "symbol",
    "integer",
    "builtin",
    "closure",
    "macro",
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
Atom F_APPLY;

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

#define THROW(err, msg, ...) do { \
    seterr(__LINE__, "%s [%d]: " msg, ErrorNames[err], __LINE__, ##__VA_ARGS__); \
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

int list_length_safe(Atom list)
{
    return listp(list) ? list_length(list) : 0;
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

Atom builtinInfo = { .type = AtomType_Nil };
Atom make_builtin(Builtin fn, Atom name)
{
    Atom a;
    a.type = AtomType_Builtin;
    a.value.builtin = fn;
    Atom info = cons(a, name);
    builtinInfo = cons(info, builtinInfo);
    return a;
}

const char* get_builtin_name(Atom builtin)
{
    if (!builtinp(builtin)) {
        return "not builtin";
    }
    Atom p = builtinInfo;
    while (!nilp(p)) {
        assert(pairp(p));
        assert(pairp(car(p)));
        if (tobuiltin(car(car(p))) == tobuiltin(builtin))
            return tosymbol(cdr(car(p)));
        p = cdr(p);
    }
    return "unknown builtin";
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
            // printf("#<BUILTIN:%p>", tobuiltin(atom));
            printf("#<BUILTIN:%s>", get_builtin_name(atom));
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
        THROW(Error_Syntax, "found unexpected end-of-input");
        // return Error_Syntax;
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
                THROW(Error_Syntax, "improper list found in list context");
                // return Error_Syntax;
            }
            TRY(read_expr(*end, end, &item));
            cdr(p) = item;
            TRY(lex(*end, &token, end));
            if (token[0] != ')') {
                THROW(Error_Syntax, "list missing closing paren");
                // return Error_Syntax;
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
        THROW(Error_Syntax, "expression list missing closing paren");
        // return Error_Syntax;
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

int symcmp(Atom a, Atom b) {
    assert(symbolp(a));
    assert(symbolp(b));
    return tosymbol(a) == tosymbol(b);
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
        THROW(Error_Unbound, "%s", tosymbol(symbol));
        // return Error_Unbound;
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

Atom list_get(Atom list, int k)
{
    while (k--)
        list = cdr(list);
    return car(list);
}

void list_set(Atom list, int k, Atom value)
{
    while (k--)
        list = cdr(list);
    car(list) = value;
}

void list_reverse(Atom* list)
{
    Atom tail = nil;
    while (!nilp(*list)) {
        Atom p = cdr(*list);
        cdr(*list) = tail;
        tail = *list;
        *list = p;
    }
    *list = tail;
}

Atom make_frame(Atom parent, Atom env, Atom tail)
{
    return cons(parent,
            cons(env,
            cons(nil, /* op */
            cons(tail,
            cons(nil, /* args */
            cons(nil, /* body */
            nil))))));
}

enum {
    FRAME_PARENT = 0,
    FRAME_ENV,  // 1
    FRAME_OP,   // 2
    FRAME_TAIL, // 3
    FRAME_ARGS, // 4
    FRAME_BODY, // 5
};

int eval_do_exec(Atom* stack, Atom* expr, Atom* env)
{
    Atom body;
    *env = list_get(*stack, FRAME_ENV);
    body = list_get(*stack, FRAME_BODY);
    *expr = car(body);
    body = cdr(body);
    if (nilp(body)) {
        *stack = car(*stack); // pop stack bc function finished
    } else {
        list_set(*stack, FRAME_BODY, body);
    }
    return Error_OK;
}

int eval_do_bind(Atom* stack, Atom* expr, Atom* env)
{
    Atom op, args, params, body;
    body = list_get(*stack, FRAME_BODY);
    if (!nilp(body))
        return eval_do_exec(stack, expr, env);

    op = list_get(*stack, FRAME_OP);
    args = list_get(*stack, FRAME_ARGS);

    *env = env_create(car(op));
    params = car(cdr(op));
    body = cdr(cdr(op));
    list_set(*stack, FRAME_ENV, *env);
    list_set(*stack, FRAME_BODY, body);

    while (!nilp(params)) {
        // REVISIT: why does this ever happen
        if (symbolp(params)) {
            env_set(*env, params, args);
            args = nil;
            break;
        }
        if (nilp(args)) {
            THROW(Error_Args, "too few arguments passed to function");
            // return Error_Args;
        }
        env_set(*env, car(params), car(args));
        params = cdr(params);
        args = cdr(args);
    }

    if (!nilp(args)) {
        THROW(Error_Args, "too many arguments passed to function.");
        // return Error_Args;
    }

    list_set(*stack, FRAME_ARGS, nil);
    return eval_do_exec(stack, expr, env);
}

int eval_do_apply(Atom* stack, Atom* expr, Atom* env, Atom* result)
{
    Atom op = list_get(*stack, FRAME_OP);
    Atom args = list_get(*stack, FRAME_ARGS);

    if (!nilp(args)) {
        list_reverse(&args);
        list_set(*stack, FRAME_ARGS, args);
    }

    if (symbolp(op) && symcmp(op, F_APPLY)) {
        /* Replace the current frame */
        *stack = car(*stack);
        *stack = make_frame(*stack, *env, nil);
        op = car(args);
        args = car(cdr(args));
        if (!listp(args)) {
            THROW(Error_Syntax, "argument must be of type list");
            // return Error_Syntax;
        }
        list_set(*stack, FRAME_OP, op);
        list_set(*stack, FRAME_ARGS, args);
        printf("eval_do_apply STACK FRAME[%d]: op = ", __LINE__); print_expr(op); putchar('\n'); // TEMP TEMP
    }

    if (builtinp(op)) {
        *stack = car(*stack);
        *expr = cons(op, args);
        return Error_OK;
    } else if (closurep(op)) {
        return eval_do_bind(stack, expr, env);
    } else {
        printf("expr = "); print_expr(*expr); putchar('\n');
        printf("op   = "); print_expr(op); putchar('\n'); // TEMP TEMP
        THROW(Error_Type, "invalid type for operand passed to apply: %s", typename(op));
        // return Error_Type;
    }
}

int eval_do_return(Atom* stack, Atom* expr, Atom* env, Atom* result)
{
    Atom args, op, body;
    *env = list_get(*stack, FRAME_ENV);
    op = list_get(*stack, FRAME_OP);
    body = list_get(*stack, FRAME_BODY);

    if (!nilp(body)) {
        /* Still running a procedure; ignore the result */
        return eval_do_apply(stack, expr, env, result);
    }

    if (nilp(op)) {
        /* Finished evaluating the operator */
        op = *result;
        list_set(*stack, FRAME_OP, op);
        if (macrop(op)) {
            /* Don't evaluate macro arguments */
            args = list_get(*stack, FRAME_TAIL);
            *stack = make_frame(*stack, *env, nil);
            op.type = AtomType_Closure;
            list_set(*stack, FRAME_OP, op);
            list_set(*stack, FRAME_ARGS, args);
            printf("eval_do_return STACK FRAME[%d]: op = ", __LINE__); print_expr(op); putchar('\n'); // TEMP
            return eval_do_bind(stack, expr, env);
        }
    } else if (symbolp(op)) {
        /* Finished working on special form */
        if (symcmp(op, F_DEFINE)) {
            Atom sym = list_get(*stack, FRAME_ARGS);
            (void) env_set(*env, sym, *result);
            *stack = car(*stack);
            *expr = cons(F_QUOTE, cons(sym, nil));
            return Error_OK;
        } else if (symcmp(op, F_IF)) {
            Atom args = list_get(*stack, FRAME_TAIL);
            *expr = nilp(*result) ? car(cdr(args)) : car(args);
            *stack = car(*stack);
            return Error_OK;
        } else if (symcmp(op, F_AND)) {
            Atom args = list_get(*stack, FRAME_TAIL);
            printf("eval_do_return F_AND: "); print_expr(args); putchar('\n');
            exit(0);
        } else {
            goto store_arg;
        }
    } else if (macrop(op)) {
        /* Finished evaluating macro */
        *expr = *result;
        *stack = car(*stack);
        return Error_OK;
    } else {
store_arg:
        /* Store evaluated argument */
        args = list_get(*stack, FRAME_ARGS);
        list_set(*stack, FRAME_ARGS, cons(*result, args));
    }

    args = list_get(*stack, FRAME_TAIL);
    if (nilp(args)) {
        /* No more arguments left to evaluate */
        return eval_do_apply(stack, expr, env, result);
    }

    /* Evaluate next argument */
    *expr = car(args);
    list_set(*stack, FRAME_TAIL, cdr(args));
    return Error_OK;
}

int eval_expr(Atom expr, Atom env, Atom* result);

int apply(Atom fn, Atom args, Atom* result)
{
    Atom env, params, body;

    if (builtinp(fn))
        return (*tobuiltin(fn))(args, result);
    else if (!closurep(fn)) {
        THROW(Error_Type, "cannot `apply` object of type %s", typename(fn));
        // return Error_Type;
    }

    env = env_create(car(fn));
    params = car(cdr(fn));
    body = cdr(cdr(fn));

    // TODO: set unbound parameters to nil?
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
    if (!nilp(params) && !nilp(args)) {
        THROW(Error_Args, "incorrect number of arguments passed to function");
        // return Error_Args;
    }

    while (!nilp(body)) {
        TRY(eval_expr(car(body), env, result));
        body = cdr(body);
    }

    return Error_OK;
}

int eval_expr(Atom expr, Atom env, Atom* result)
{
    printf("eval_expr[%d]: ", __LINE__); print_expr(expr); putchar('\n');

    Error err = Error_OK;
    Atom stack = nil;
    do {
        printf("eval_expr do[%d]: ", __LINE__); print_expr(expr); putchar('\n');


        if (symbolp(expr)) {
            err = env_get(env, expr, result);
        } else if (!pairp(expr)) {
            *result = expr;
        } else if (!listp(expr)) {
            THROW(Error_Syntax, "cannot eval object of type %s", typename(expr));
            // return Error_Syntax;
        } else {
            Atom op = car(expr);
            Atom args = cdr(expr);
            if (symbolp(op)) {
                /* Handle Special Forms */
                if (symcmp(op, F_QUOTE)) {
                    if (nilp(args) || !nilp(cdr(args))) {
                        THROW(Error_Args, "incorrect number of arguments passed to QUOTE: %d", list_length_safe(args));
                        // return Error_Args;
                    }
                    *result = car(args);
                } else if (symcmp(op, F_DEFINE)) {
                    if (nilp(args) || nilp(cdr(args))) {
                        THROW(Error_Args, "incorrect number of arguments passed to DEFINE: %d", list_length_safe(args));
                        // return Error_Args;
                    }
                    Atom sym = car(args);
                    if (pairp(sym)) {
                        // handle special form of (define (func-name args...) body...)
                        err = make_closure(env, cdr(sym), cdr(args), result);
                        sym = car(sym);
                        if (!symbolp(sym)) {
                            THROW(Error_Type, "received object of type %s where expecting a symbol for lambda special form", typename(sym));
                            // return Error_Type;
                        }
                        (void) env_set(env, sym, *result);
                        *result = sym;
                    } else if (symbolp(sym)) {
                        if (!nilp(cdr(cdr(args)))) {
                            THROW(Error_Args, "incorrect number of arguments passed to DEFINE: %d", list_length_safe(args));
                            // return Error_Args;
                        }
                        stack = make_frame(stack, env, nil);
                        list_set(stack, FRAME_OP, op);
                        list_set(stack, FRAME_ARGS, sym);
                        printf("eval_expr STACK FRAME[%d]: op = ", __LINE__); print_expr(op); putchar('\n'); // TEMP TEMP
                        expr = car(cdr(args));
                        continue;
                    } else {
                        THROW(Error_Type, "cannot assign to object type of %s", typename(sym));
                        // return Error_Type;
                    }
                } else if (symcmp(op, F_LAMBDA)) {
                    if (nilp(args) || nilp(cdr(args))) {
                        THROW(Error_Type, "incorrect number of arguments passed to LAMBDA: %d", list_length_safe(args));
                        // return Error_Args;
                    }
                    err = make_closure(env, car(args), cdr(args), result);
                } else if (symcmp(op, F_IF)) {
                    if (nilp(args) || nilp(cdr(args)) || nilp(cdr(cdr(args)))
                            || !nilp(cdr(cdr(cdr(args))))) {
                        THROW(Error_Type, "incorrect number of arguments passed to IF: %d", list_length_safe(args));
                        // return Error_Args;
                    }
                    stack = make_frame(stack, env, cdr(args));
                    list_set(stack, FRAME_OP, op);
                    printf("eval_expr STACK FRAME[%d]: op = ", __LINE__); print_expr(op); putchar('\n'); // TEMP TEMP
                    expr = car(args);
                    continue;
                } else if (symcmp(op, F_AND)) {
                    if (nilp(args)) {
                        THROW(Error_Type, "incorrect number of arguments passed to AND: %d", list_length_safe(args));
                    }
                    stack = make_frame(stack, env, cdr(args));
                    list_set(stack, FRAME_OP, F_AND);
                    expr = car(args);
                    continue;
                } else if (symcmp(op, F_DEFMACRO)) {
                    if (nilp(args) || nilp(cdr(args))) {
                        THROW(Error_Args, "incorrect number of arguments passed to DEFMACRO: %d", list_length_safe(args));
                        // return Error_Args;
                    }
                    if (!pairp(car(args))) {
                        THROW(Error_Syntax, "first argument of DEFMACRO must be a pair, received %s", typename(car(args)));
                        // return Error_Syntax;
                    }
                    Atom name = car(car(args));
                    if (!symbolp(name)) {
                        THROW(Error_Type, "cannot name DEFMACRO with object of type: %s", typename(name));
                        // return Error_Type;
                    }
                    Atom macro;
                    err = make_closure(env, cdr(car(args)), cdr(args), &macro);
                    if (err == Error_OK) {
                        macro.type = AtomType_Macro;
                        *result = name;
                        (void) env_set(env, name, macro);
                    }
                } else if (symcmp(op, F_APPLY)) {
                    if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args)))) {
                        THROW(Error_Args, "incorrect number of arguments passed to APPLY: %d", typename(args));
                        // return Error_Args;
                    }
                    stack = make_frame(stack, env, cdr(args));
                    list_set(stack, FRAME_OP, op);
                    printf("eval_expr STACK FRAME[%d]: op = ", __LINE__); print_expr(op); putchar('\n'); // TEMP TEMP
                    expr = car(args);
                    continue;
                } else {
                    goto push;
                }
            } else if (builtinp(op)) {
                err = (*op.value.builtin)(args, result);
            } else {
push:
                /* Handle function application */
                stack = make_frame(stack, env, args);
                expr = op;
                printf("eval_expr STACK FRAME[%d]: op = ", __LINE__); print_expr(op); putchar('\n'); // TEMP
                continue;
            }
        }

        if(nilp(stack))
            break;

        if (err == Error_OK)
            err = eval_do_return(&stack, &expr, &env, result);

    } while (err == Error_OK);

    return err;
}

int builtin_car(Atom args, Atom* result)
{
    if (nilp(args) || !nilp(cdr(args))) {
        THROW(Error_Args, "invalid number of arguments to car, expected 2, received %d", list_length_safe(args));
        // return Error_Args;
    } else if (nilp(car(args))) {
        *result = nil;
    } else if (!pairp(car(args))) {
        THROW(Error_Type, "invalid argument type to car: %s", typename(car(args)));
        // return Error_Type;
    } else {
        *result = car(car(args));
    }
    return Error_OK;
}

int builtin_cdr(Atom args, Atom* result)
{
    if (nilp(args) || !nilp(cdr(args))) {
        THROW(Error_Args, "invalid number of arguments to CDR, expected 2, received %d", list_length_safe(args));
        // return Error_Args;
    } else if (nilp(car(args))) {
        *result = nil;
    } else if (!pairp(car(args))) {
        THROW(Error_Type, "invalid argument type to cdr: %s", typename(car(args)));
        // return Error_Type;
    } else {
        *result = cdr(car(args));
    }
    return Error_OK;
}

int builtin_cons(Atom args, Atom* result)
{
    // if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
    // if (nilp(args) || list_length(args) != 2) {
    if (list_length_safe(args) != 2) {
        THROW(Error_Args, "invalid number of arguments to CONS, expected 2, received %d", list_length_safe(args));
        // return Error_Args;
    }
    *result = cons(car(args), car(cdr(args)));
    return Error_OK;
}

int builtin_add(Atom args, Atom* result)
{
    Atom a, b;
    if (list_length(args) != 2)
        return Error_Args;
    a = car(args);
    b = car(cdr(args));
    if (!integerp(a) || !integerp(b))
        return Error_Args;

    *result = make_int(tointeger(a) + tointeger(b));
    return Error_OK;

    // int val = 0;
    // while (!nilp(args)) {
    //     Atom a = car(args);
    //     if (!integerp(a))
    //         return Error_Args;
    //     val += tointeger(a);
    //     args = cdr(args);
    // }
    // *result = make_int(val);
    // return Error_OK;
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
        // printf("%s: %s (%d)\n", msg, errormsg, errorlinum);
        printf("%s\n", errormsg);
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

void add_builtin(Atom env, Atom name, Builtin fn)
{
    env_set(env, name, make_builtin(fn, name));
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
    F_APPLY            = make_sym("APPLY");

    Atom env = env_create(nil);

    add_builtin(env, make_sym("CAR"),   &builtin_car);
    add_builtin(env, make_sym("CDR"),   &builtin_cdr);
    add_builtin(env, make_sym("CONS"),  &builtin_cons);
    add_builtin(env, make_sym("APPLY"), &builtin_apply);
    add_builtin(env, make_sym("EQ?"),   &builtin_eq);
    add_builtin(env, make_sym("PAIR?"), &builtin_pairp);
    add_builtin(env, make_sym("+"),     &builtin_add);
    add_builtin(env, make_sym("-"),     &builtin_subtract);
    add_builtin(env, make_sym("*"),     &builtin_multiply);
    add_builtin(env, make_sym("/"),     &builtin_divide);
    add_builtin(env, make_sym("="),     &builtin_numeq);
    add_builtin(env, make_sym("<"),     &builtin_numlt);
    add_builtin(env, make_sym(">"),     &builtin_numgt);
    add_builtin(env, make_sym("<="),    &builtin_numlte);
    add_builtin(env, make_sym(">="),    &builtin_numgte);

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
