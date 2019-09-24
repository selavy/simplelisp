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
#define tosym(p) (p).value.symbol
#define toint(p) (p).value.integer

typedef struct Atom {
    enum {
        AtomType_Nil,
        AtomType_Pair,
        AtomType_Symbol,
        AtomType_Integer
    } type;

    union {
        struct Pair* pair;
        const char*  symbol;
        long         integer;
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
            printf("%s", atom.value.symbol);
            break;
        case AtomType_Integer:
            printf("%ld", atom.value.integer);
            break;
    }
}

typedef enum {
    Error_OK = 0,
    Error_Syntax,
    Error_Unbound,
} Error;

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
        Error err;

        err = lex(*end, &token, end);
        if (err != Error_OK)
            return err;
        if (token[0] == ')')
            return Error_OK;
        if (token[0] == '.' && *end - token == 1) { // TODO: check this 2nd condition
            /* Improper list */
            if (nilp(p))
                return Error_Syntax;
            err = read_expr(*end, end, &item);
            if (err)
                return err;
            cdr(p) = item;
            err = lex(*end, &token, end);
            if (!err && token[0] != ')')
                return Error_Syntax;
            return err;
        }

        err = read_expr(token, end, &item);
        if (err)
            return err;
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

int main(int argc, char** argv)
{
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
    return 0;

    char* buf = NULL;
    while ((buf = readline("> ")) != NULL) {
        if (strlen(buf) > 0) {
            add_history(buf);
        }

        const char* p = buf;
        Error err;
        Atom expr;
        err = read_expr(p, &p, &expr);
        switch (err) {
        case Error_Syntax:
            printf("Syntax Error\n");
            break;
        case Error_OK:
            print_expr(expr); putchar('\n');
            break;
        case Error_Unbound:
            printf("Unbound symbol\n");
            break;
        }
    }
    free(buf);

    return 0;
}
