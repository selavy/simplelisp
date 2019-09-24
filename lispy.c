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
#define assym(p) (p).value.symbol
#define asint(p) (p).value.integer

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

static const Atom nil = { .type = AtomType_Nil };

static Atom cons(Atom carval, Atom cdrval)
{
    Atom p;
    p.type = AtomType_Pair;
    p.value.pair = malloc(sizeof(struct Pair));
    car(p) = carval;
    cdr(p) = cdrval;
    return p;
}

static Atom make_int(long x)
{
    Atom a;
    a.type = AtomType_Integer;
    a.value.integer = x;
    return a;
}

static Atom sym_table = { .type = AtomType_Nil };

static Atom make_sym(const char* s)
{
    Atom a, p = sym_table;
    while (!nilp(p)) {
        assert(p.type == AtomType_Pair);
        a = car(p);
        if (!strcmp(assym(a), s))
            return a;
        p = cdr(p);
    }
    p.type = AtomType_Symbol;
    assym(p) = strdup(s);
    sym_table = cons(p, sym_table);
    return p;
}

static void print_expr(Atom atom)
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
} Error;

static int lex(const char* str, const char** start, const char** end);
static int parse_simple(const char* start, const char* end, Atom* result);
static int read_list(const char* start, const char** end, Atom* result);
static int read_expr(const char* input, const char** end, Atom* result);

static int lex(const char* str, const char** start, const char** end)
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

static int parse_simple(const char* start, const char* end, Atom* result)
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

static int read_list(const char* start, const char** end, Atom* result)
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

static int read_expr(const char* input, const char** end, Atom* result)
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
    assert(assym(a) == assym(b));
    Atom c = make_sym("BAR");
    assert(assym(a) != assym(c));

    char* buf = NULL;
    while ((buf = readline("> ")) != NULL) {
        if (strlen(buf) > 0) {
            add_history(buf);
        }

        const char* p = buf;
        Error err;
        Atom expr;
        err = read_expr(p, &p, &expr);
        if (err != Error_OK) {
            printf("Syntax Error\n");
        } else {
            print_expr(expr); putchar('\n');
        }
    }
    free(buf);

    return 0;
}
