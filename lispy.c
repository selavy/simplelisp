#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

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

    return 0;
}
