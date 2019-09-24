#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <readline/readline.h>
#include <readline/history.h>

#define car(p) ((p).value.pair->atom[0])
#define cdr(p) ((p).value.pair->atom[1])
#define nilp(atom) ((atom).type == AtomType_Nil)
#define symp(atom) ((atom).type == AtomType_Symbol)
#define pairp(atom) ((atom).type == AtomType_Pair)
#define intp(atom) ((atom).type == AtomType_Integer)
#define builtinp(atom) ((atom).type == AtomType_Builtin)
#define tosym(p) (p).value.symbol
#define toint(p) (p).value.integer
#define tobuiltin(p) (p).value.builtin

struct Atom;

typedef int (*Builtin)(struct Atom args, struct Atom* result);

typedef struct Atom {
    enum {
        AtomType_Nil,
        AtomType_Pair,
        AtomType_Symbol,
        AtomType_Integer,
        AtomType_Builtin,
    } type;

    union {
        struct Pair* pair;
        const char*  symbol;
        long         integer;
        Builtin      builtin;
    } value;
} Atom;

struct Pair {
    struct Atom atom[2];
};

const Atom nil = { .type = AtomType_Nil };

Atom cons(Atom carval, Atom cdrval)
{
    Atom p;
    p.type = AtomType_Pair;
    p.value.pair = malloc(sizeof(struct Pair));
    car(p) = carval;
    cdr(p) = cdrval;
    return p;
}

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
        if (!strcmp(tosym(a), s))
            return a;
        p = cdr(p);
    }
    p.type = AtomType_Symbol;
    tosym(p) = strdup(s);
    sym_table = cons(p, sym_table);
    return p;
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
            printf("%s", tosym(atom));
            break;
        case AtomType_Integer:
            printf("%ld", toint(atom));
            break;
        case AtomType_Builtin:
            printf("#<BUILTIN:%p>", tobuiltin(atom));
            break;
    }
}

typedef enum {
    Error_OK = 0,
    Error_Syntax,
    Error_Unbound,
    Error_Args,
    Error_Type,
} Error;

#define TRY(err) do { int rc = (err); if (rc != Error_OK) return rc; } while (0)

int lex(const char* str, const char** start, const char** end);
int parse_simple(const char* start, const char* end, Atom* result);
int read_list(const char* start, const char** end, Atom* result);
int read_expr(const char* input, const char** end, Atom* result);

int lex(const char* str, const char** start, const char** end)
{
    const char* const ws     = " \t\n";
    const char* const delim  = "() \t\n";
    const char* const prefix = "()";

    str += strspn(str, ws);
    if (*str == '\0') {
        *start = *end = NULL;
        return Error_Syntax;
    }

    *start = str;
    if (strchr(prefix, *str) != NULL)
        *end = str + 1;
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
    if (!strcmp(buf, "NIL"))
        *result = nil;
    else
        *result = make_sym(buf);
    free(buf);
    return Error_OK;
}

int read_list(const char* start, const char** end, Atom* result)
{
    Atom p;
    *end = start;
    p = *result = nil;
    for (;;) {
        const char* token;
        Atom item;

        TRY(lex(*end, &token, end));
        if (token[0] == ')')
            return Error_OK;
        if (token[0] == '.' && *end - token == 1) { // TODO: check this 2nd condition
            /* Improper list */
            if (nilp(p))
                return Error_Syntax;
            TRY(read_expr(*end, end, &item));
            cdr(p) = item;
            TRY(lex(*end, &token, end));
            if (token[0] != ')')
                return Error_Syntax;
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
    Error err;

    err = lex(input, &token, end);
    if (err == Error_Syntax)
        return err;

    if (token[0] == '(')
        return read_list(*end, end, result);
    else if (token[0] == ')')
        return Error_Syntax;
    else
        return parse_simple(token, *end, result);
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
        if (tosym(car(entry)) == tosym(symbol)) {
            *result = cdr(entry);
            return Error_OK;
        }
        entries = cdr(entries);
    }

    if (nilp(parent))
        return Error_Unbound;

    return env_get(parent, symbol, result);
}

// TODO: this function doesn't need a return code?
int env_set(Atom env, Atom symbol, Atom value)
{
    Atom entries = cdr(env);
    while (!nilp(entries)) {
        Atom entry = car(entries);
        if (tosym(car(entry)) == tosym(symbol)) {
            cdr(entry) = value;
            return Error_OK;
        }
        entries = cdr(entries);
    }
    Atom entry = cons(symbol, value);
    cdr(env) = cons(entry, cdr(env));
    return Error_OK;
}

int apply(Atom fn, Atom args, Atom* result)
{
    if (!builtinp(fn))
        return Error_Type;
    return (*tobuiltin(fn))(args, result);
}

int listp(Atom expr)
{
    while (!nilp(expr)) {
        if (expr.type != AtomType_Pair)
            return 0;
        expr = cdr(expr);
    }
    return 1;
}

Atom F_QUOTE;
Atom F_DEFINE;

int symcmp(Atom a, Atom b) {
    assert(symp(a));
    assert(symp(b));
    return tosym(a) == tosym(b);
}

int eval_expr(Atom expr, Atom env, Atom* result)
{
    Atom op, args, p;

    if (symp(expr)) {
        return env_get(env, expr, result);
    } else if (!pairp(expr)) {
        *result = expr;
        return Error_OK;
    } else if (!listp(expr)) {
        return Error_Syntax;
    }

    op = car(expr);
    args = cdr(expr);

    if (symp(op)) {
        if (symcmp(op, F_QUOTE)) {
            // form ( <QUOTE> . ( <ATOM> . NIL ) )
            if (nilp(args) || !nilp(cdr(args)))
                return Error_Args;
            *result = car(args);
            return Error_OK;
        } else if (symcmp(op, F_DEFINE)) {
            // form: ( <DEFINE> . ( <SYMBOL> . ( <EXPR> . NIL ) ) )
            Atom sym, val;
            if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
                return Error_Args;
            sym = car(args);
            if (!symp(sym))
                return Error_Type;
            TRY(eval_expr(car(cdr(args)), env, &val));
            *result = sym;
            env_set(env, sym, val);
            return Error_OK;
        } else {
            /* Evaluate operator */
            TRY(eval_expr(op, env, &op));

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
    }

    return Error_Syntax;
}

int builtin_car(Atom args, Atom* result)
{
    if (nilp(args) || !nilp(cdr(args)))
        return Error_Args;

    if (nilp(car(args)))
        *result = nil;
    else if (!pairp(car(args)))
        return Error_Type;
    else
        *result = car(car(args));

    return Error_OK;
}

int builtin_cdr(Atom args, Atom* result)
{
    if (nilp(args) || !nilp(cdr(args)))
        return Error_Args;

    if (nilp(car(args)))
        *result = nil;
    else if (!pairp(car(args)))
        return Error_Type;
    else
        *result = cdr(car(args));

    return Error_OK;
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

int builtin_cons(Atom args, Atom* result)
{
    // if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
    if (nilp(args) || list_length(args) != 2)
        return Error_Args;
    *result = cons(car(args), car(cdr(args)));
    return Error_OK;
}

Atom init() {
    F_QUOTE = make_sym("QUOTE");
    F_DEFINE = make_sym("DEFINE");
    Atom env = env_create(nil);

    env_set(env, make_sym("CAR"),  make_builtin(&builtin_car));
    env_set(env, make_sym("CDR"),  make_builtin(&builtin_cdr));
    env_set(env, make_sym("CONS"), make_builtin(&builtin_cons));

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
            printf("syntax error\n");
            break;
        case Error_Unbound:
            printf("error: symbol not bound\n");
            break;
        case Error_Args:
            printf("error: incorrect number of arguments\n");
            break;
        case Error_Type:
            printf("error: incorrect type\n");
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
        execute(buf, env);
        free(buf);
    }
}

int main(int argc, char** argv)
{
#if RUN_TESTS
    print_expr(make_int(42)); putchar('\n');
    print_expr(make_sym("FOO")); putchar('\n');
    print_expr(cons(make_sym("X"), make_sym("Y"))); putchar('\n');
    print_expr(
        cons(
            make_int(1),
            cons(
                make_int(2),
                cons(
                    make_int(3),
                    nil
                )
            )
        )
    );
    putchar('\n');

    Atom a = make_sym("FOO");
    Atom b = make_sym("FOO");
    assert(tosym(a) == tosym(b));
    Atom c = make_sym("BAR");
    assert(tosym(a) != tosym(c));

    //------------------------------------------------------------
    // Environment Tests
    //------------------------------------------------------------
    Atom parent = cons(nil, nil);
    Atom env    = cons(parent, nil);
    Error err;
    Atom ret;

    // find unbound symbol
    err = env_get(env, make_sym("FOO"), &ret);
    assert(err == Error_Unbound);

    // set value in child + access it
    err = env_set(env, make_sym("FOO"), make_sym("BAR"));
    assert(err == Error_OK);
    err = env_get(env, make_sym("FOO"), &ret);
    assert(tosym(ret) == tosym(make_sym("BAR")));

    // set value in parent + access it from child
    err = env_set(parent, make_sym("HELLO"), make_sym("WORLD"));
    assert(err == Error_OK);
    err = env_get(env, make_sym("HELLO"), &ret);
    assert(err == Error_OK);
    assert(tosym(ret) == tosym(make_sym("WORLD")));

    // set another value in parent + access it from child
    err = env_set(parent, make_sym("GOODBYE"), make_sym("SOMEONE"));
    assert(err == Error_OK);
    err = env_get(env, make_sym("GOODBYE"), &ret);
    assert(err == Error_OK);
    assert(tosym(ret) == tosym(make_sym("SOMEONE")));

    // set value in child that shadows parent + access it from child
    err = env_set(env, make_sym("GOODBYE"), make_sym("SOMEONE-ELSE"));
    assert(err == Error_OK);
    err = env_get(env, make_sym("GOODBYE"), &ret);
    assert(err == Error_OK);
    assert(tosym(ret) == tosym(make_sym("SOMEONE-ELSE")));

    // replace value in parent + access it from child
    err = env_set(parent, make_sym("HELLO"), make_sym("VENUS"));
    assert(err == Error_OK);
    err = env_get(env, make_sym("HELLO"), &ret);
    assert(err == Error_OK);
    assert(tosym(ret) == tosym(make_sym("VENUS")));

    printf("Passed.\n");
#endif
    // return 0;

    if (argc > 2) {
        fprintf(stderr, "Usage: %s [FILE]\n", argv[0]);
        exit(0);
    } else if (argc == 2) {
        FILE *stream;
        char *line = NULL;
        size_t len = 0;
        ssize_t nread;
        Atom env = init();

        stream = fopen(argv[1], "r");
        if (stream == NULL) {
            perror("fopen");
            exit(EXIT_FAILURE);
        }

        while ((nread = getline(&line, &len, stream)) != -1) {
            execute(line, env);
        }

        free(line);
        fclose(stream);
    } else {
        repl();
    }

    return 0;
}
