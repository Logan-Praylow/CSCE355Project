#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum { NODE_EMPTY, NODE_CHAR, NODE_UNION, NODE_CONCAT, NODE_STAR } NodeType;

typedef struct RegexNode {
    NodeType type;
    char symbol;                // only for NODE_CHAR
    struct RegexNode *left;
    struct RegexNode *right;
} RegexNode;

// ─────────────────────────────────────────────────────────────────
// Helpers to build, clone, and free trees
// ─────────────────────────────────────────────────────────────────
RegexNode* make_node(NodeType type, char symbol, RegexNode* left, RegexNode* right) {
    RegexNode* node = malloc(sizeof(RegexNode));
    node->type   = type;
    node->symbol = symbol;
    node->left   = left;
    node->right  = right;
    return node;
}

void free_tree(RegexNode* node) {
    if (!node) return;
    free_tree(node->left);
    free_tree(node->right);
    free(node);
}

RegexNode* clone_tree(RegexNode* n) {
    if (!n) return NULL;
    return make_node(n->type,
                     n->symbol,
                     clone_tree(n->left),
                     clone_tree(n->right));
}

// ε is represented as "/" under a star
RegexNode* make_epsilon() {
    RegexNode* empty = make_node(NODE_EMPTY, 0, NULL, NULL);
    return make_node(NODE_STAR, '*', empty, NULL);
}

// ─────────────────────────────────────────────────────────────────
// Parse a postfix regex into a syntax tree
// ─────────────────────────────────────────────────────────────────
RegexNode* parse_postfix(const char* line) {
    RegexNode* stack[1000];
    int top = 0;
    for (int i = 0; line[i]; i++) {
        char c = line[i];
        if (isspace(c)) continue;
        if (c == '/') {
            stack[top++] = make_node(NODE_EMPTY, 0, NULL, NULL);
        } else if (isalnum(c)) {
            stack[top++] = make_node(NODE_CHAR, c, NULL, NULL);
        } else if (c == '*') {
            RegexNode* a = stack[--top];
            stack[top++] = make_node(NODE_STAR, '*', a, NULL);
        } else if (c == '+') {
            RegexNode* b = stack[--top];
            RegexNode* a = stack[--top];
            stack[top++] = make_node(NODE_UNION, '+', a, b);
        } else if (c == '.') {
            RegexNode* b = stack[--top];
            RegexNode* a = stack[--top];
            stack[top++] = make_node(NODE_CONCAT, '.', a, b);
        }
    }
    return top == 1 ? stack[0] : NULL;
}

// Print in prefix (Polish) notation
void print_prefix(RegexNode* node) {
    if (!node) return;
    switch (node->type) {
      case NODE_EMPTY:  printf("/");                          break;
      case NODE_CHAR:   printf("%c", node->symbol);           break;
      case NODE_STAR:   printf("*");    print_prefix(node->left);            break;
      case NODE_UNION:  printf("+");    print_prefix(node->left);  print_prefix(node->right); break;
      case NODE_CONCAT: printf(".");    print_prefix(node->left);  print_prefix(node->right); break;
    }
}

// Compare two trees for structural equality
int trees_equal(RegexNode* a, RegexNode* b) {
    if (!a || !b) return a == b;
    if (a->type != b->type || a->symbol != b->symbol) return 0;
    return trees_equal(a->left,  b->left)
        && trees_equal(a->right, b->right);
}

// ─────────────────────────────────────────────────────────────────
// --simplify: bottom‑up rewrite rules
// ─────────────────────────────────────────────────────────────────
static int is_exactly_empty(RegexNode *n) {
    return n && n->type == NODE_EMPTY;
}
static int is_empty_star(RegexNode *n) {
    return n && n->type == NODE_STAR && is_exactly_empty(n->left);
}

RegexNode* simplify(RegexNode* node) {
    if (!node) return NULL;
    node->left  = simplify(node->left);
    node->right = simplify(node->right);

    // 1) (s*)* → s*
    if (node->type == NODE_STAR && node->left->type == NODE_STAR) {
        RegexNode* keep = node->left;
        free(node);
        return keep;
    }

    // 5) (s+∅*)* → s*   and   (∅*+s)* → s*
    if (node->type == NODE_STAR && node->left->type == NODE_UNION) {
        RegexNode* U = node->left;
        RegexNode* s = NULL;
        if (is_empty_star(U->left))       s = U->right;
        else if (is_empty_star(U->right)) s = U->left;
        if (s) {
            RegexNode* s_clone = clone_tree(s);
            free_tree(U->left);
            free_tree(U->right);
            free(U);
            free(node);
            return make_node(NODE_STAR, '*', s_clone, NULL);
        }
    }

    // 2) ∅ + s → s   or   s + ∅ → s
    if (node->type == NODE_UNION) {
        if (is_exactly_empty(node->left)) {
            RegexNode* keep = node->right;
            free(node);
            return keep;
        }
        if (is_exactly_empty(node->right)) {
            RegexNode* keep = node->left;
            free(node);
            return keep;
        }
    }

    // 3+4) ∅·s → ∅, s·∅ → ∅, ∅*·s → s, s·∅* → s
    if (node->type == NODE_CONCAT) {
        if (is_exactly_empty(node->left) || is_exactly_empty(node->right)) {
            free_tree(node->left);
            free_tree(node->right);
            free(node);
            return make_node(NODE_EMPTY, 0, NULL, NULL);
        }
        if (is_empty_star(node->left)) {
            RegexNode* keep = node->right;
            free(node);
            return keep;
        }
        if (is_empty_star(node->right)) {
            RegexNode* keep = node->left;
            free(node);
            return keep;
        }
    }

    return node;
}



// ─────────────────────────────────────────────────────────────────
// Q0: “empty”: is L(r) = ∅ ?
// ─────────────────────────────────────────────────────────────────
int is_empty(RegexNode* node) {
    if (!node) return 1;
    switch (node->type) {
      case NODE_EMPTY:  return 1;
      case NODE_CHAR:   return 0;
      case NODE_STAR:   return 0;  // r* always has ε
      case NODE_UNION:  return is_empty(node->left) && is_empty(node->right);
      case NODE_CONCAT: return is_empty(node->left) || is_empty(node->right);
    }
    return 1;
}

// Q1: “has-epsilon”: ε ∈ L(r)?
int has_epsilon(RegexNode* node) {
    if (!node) return 0;
    switch (node->type) {
      case NODE_EMPTY:  return 0;
      case NODE_CHAR:   return 0;
      case NODE_STAR:   return 1;
      case NODE_UNION:  return has_epsilon(node->left) || has_epsilon(node->right);
      case NODE_CONCAT: return has_epsilon(node->left) && has_epsilon(node->right);
    }
    return 0;
}

int has_nonepsilon(RegexNode* node) {
    if (!node) return 0;

    switch (node->type) {
        case NODE_EMPTY:
            return 0;
        case NODE_CHAR:
            return 1;
        case NODE_STAR:
            return has_nonepsilon(node->left);
        case NODE_UNION:
            return has_nonepsilon(node->left) || has_nonepsilon(node->right);
        case NODE_CONCAT:
            return
                (has_nonepsilon(node->left) && !has_epsilon(node->left) && has_epsilon(node->right)) ||
                (has_nonepsilon(node->right) && !has_epsilon(node->right) && has_epsilon(node->left)) ||
                (has_nonepsilon(node->left) && has_nonepsilon(node->right));
    }
    return 0;
}

// Q3: “uses a”: ∃ w∈L(r) containing symbol `target` somewhere
int uses_symbol(RegexNode* node, char target) {
    if (!node) return 0;
    switch (node->type) {
      case NODE_EMPTY:  return 0;
      case NODE_CHAR:   return node->symbol == target;
      case NODE_STAR:   return uses_symbol(node->left, target);
      case NODE_UNION:  return uses_symbol(node->left, target) || uses_symbol(node->right, target);
      case NODE_CONCAT: {
        int L = uses_symbol(node->left, target);
        int R = uses_symbol(node->right, target);
        int Ln = !is_empty(node->left);
        int Rn = !is_empty(node->right);
        return (L && Rn) || (R && Ln);
      }
    }
    return 0;
}

// Q3b: “not-using a”: filter out any occurence of `target`
RegexNode* not_using(RegexNode* node, char target) {
    if (!node) return NULL;
    switch (node->type) {
      case NODE_EMPTY:
        return make_node(NODE_EMPTY, 0, NULL, NULL);
      case NODE_CHAR:
        if (node->symbol == target)
            return make_node(NODE_EMPTY, 0, NULL, NULL);
        return make_node(NODE_CHAR, node->symbol, NULL, NULL);
      case NODE_UNION: {
        RegexNode* L = not_using(node->left,  target);
        RegexNode* R = not_using(node->right, target);
        if (is_empty(L) && is_empty(R)) {
            free_tree(L); free_tree(R);
            return make_node(NODE_EMPTY,0,NULL,NULL);
        }
        if (is_empty(L)) {
            free_tree(L);
            return R;
        }
        if (is_empty(R)) {
            free_tree(R);
            return L;
        }
        return make_node(NODE_UNION, '+', L, R);
      }
      case NODE_CONCAT: {
        RegexNode* L = not_using(node->left,  target);
        RegexNode* R = not_using(node->right, target);
        if (is_empty(L) || is_empty(R)) {
            free_tree(L); free_tree(R);
            return make_node(NODE_EMPTY,0,NULL,NULL);
        }
        return make_node(NODE_CONCAT, '.', L, R);
      }
      case NODE_STAR: {
        RegexNode* C = not_using(node->left, target);
        return make_node(NODE_STAR, '*', C, NULL);
      }
    }
    return NULL;
}

int is_infinite(RegexNode* node) {
    if (!node) return 0;
    switch (node->type) {
        case NODE_EMPTY:
        case NODE_CHAR:
            return 0;
        case NODE_UNION:
            return is_infinite(node->left)  || is_infinite(node->right);
        case NODE_CONCAT:
            return (is_infinite(node->left) && !is_empty(node->right)) ||
                   (is_infinite(node->right) && !is_empty(node->left));
        
        case NODE_STAR:
            // star is infinite iff its body can produce some non-empty string
            return has_nonepsilon(node->left);
    }
    return 0;
}

// ─────────────────────────────────────────────────────────────────
// Q5: “starts-with a” via Brzozowski derivative
// ─────────────────────────────────────────────────────────────────
RegexNode* derivative(RegexNode* r, char a) {
    if (!r) return make_node(NODE_EMPTY,0,NULL,NULL);
    switch (r->type) {
      case NODE_EMPTY:
        return make_node(NODE_EMPTY,0,NULL,NULL);
      case NODE_CHAR:
        return (r->symbol == a)
            ? make_epsilon()
            : make_node(NODE_EMPTY,0,NULL,NULL);
      case NODE_UNION: {
        RegexNode* L = derivative(r->left,  a);
        RegexNode* R = derivative(r->right, a);
        return make_node(NODE_UNION,'+',L,R);
      }
      case NODE_CONCAT: {
        // D(a, st) = D(a,s)·t  [ + (if ε∈s) D(a,t) ]
        RegexNode* Dleft = derivative(r->left,a);
        RegexNode* leftCat = make_node(NODE_CONCAT,'.',Dleft,clone_tree(r->right));
        if (has_epsilon(r->left)) {
            RegexNode* Dright = derivative(r->right,a);
            return make_node(NODE_UNION,'+',leftCat,Dright);
        }
        return leftCat;
      }
      case NODE_STAR: {
        // D(a, s*) = D(a,s)·s*
        RegexNode* Ds = derivative(r->left,a);
        return make_node(NODE_CONCAT,'.', Ds, clone_tree(r));
      }
    }
    return make_node(NODE_EMPTY,0,NULL,NULL);
}

int starts_with(RegexNode* r, char a) {
    RegexNode* d = derivative(r, a);
    int ans = !is_empty(d);
    free_tree(d);
    return ans;
}

RegexNode* reverse_regex(RegexNode* node) {
    if (!node) return NULL;

    switch (node->type) {
        case NODE_EMPTY:
            return make_node(NODE_EMPTY, 0, NULL, NULL);
        case NODE_CHAR:
            return make_node(NODE_CHAR, node->symbol, NULL, NULL);
        case NODE_STAR: {
            RegexNode* inner = reverse_regex(node->left);
            return make_node(NODE_STAR, '*', inner, NULL);
        }
        case NODE_UNION: {
            RegexNode* left = reverse_regex(node->left);
            RegexNode* right = reverse_regex(node->right);
            return make_node(NODE_UNION, '+', left, right);
        }
        case NODE_CONCAT: {
            RegexNode* left = reverse_regex(node->left);
            RegexNode* right = reverse_regex(node->right);
            return make_node(NODE_CONCAT, '.', right, left);  // 🔁 swapped
        }
    }
    return NULL;
}

int ends_with(RegexNode* node, char target) {
    RegexNode* rev = reverse_regex(node);
    int result = starts_with(rev, target);
    free_tree(rev);
    return result;
}

RegexNode* prefixes(RegexNode* r) {
    if (!r) return make_node(NODE_EMPTY, 0, NULL, NULL);
    switch (r->type) {
      case NODE_EMPTY:
        // prefixes(∅) = ∅
        return make_node(NODE_EMPTY, 0, NULL, NULL);

      case NODE_CHAR: {
        // prefixes(c) = c + ∅*
        RegexNode* charN = make_node(NODE_CHAR, r->symbol, NULL, NULL);
        RegexNode* epsN  = make_epsilon();
        return make_node(NODE_UNION, '+', charN, epsN);
      }

      case NODE_UNION: {
        // prefixes(s + t) = prefixes(s) + prefixes(t)
        RegexNode* L = prefixes(r->left);
        RegexNode* R = prefixes(r->right);
        return make_node(NODE_UNION, '+', L, R);
      }

      case NODE_CONCAT: {
        // prefixes(st) = ∅      if L(t)=∅
        //               prefixes(s) + s · prefixes(t)   otherwise
        if (is_empty(r->right)) {
          return make_node(NODE_EMPTY, 0, NULL, NULL);
        } else {
          RegexNode* Ps = prefixes(r->left);
          RegexNode* Pt = prefixes(r->right);
          // clone s to build s·prefixes(t)
          RegexNode* sClone = clone_tree(r->left);
          RegexNode* sPt    = make_node(NODE_CONCAT, '.', sClone, Pt);
          return make_node(NODE_UNION, '+', Ps, sPt);
        }
      }

      case NODE_STAR: {
        // prefixes(s*) = ∅*      if L(s)=∅
        //                s* · prefixes(s)   otherwise
        if (is_empty(r->left)) {
          return make_epsilon();
        } else {
          // clone the whole star
          RegexNode* starClone = clone_tree(r);
          RegexNode* Ps        = prefixes(r->left);
          return make_node(NODE_CONCAT, '.', starClone, Ps);
        }
      }
    }
    
    return make_node(NODE_EMPTY, 0, NULL, NULL);
}

RegexNode *insert_sym(RegexNode *r, char a_sym)
{
    if (!r)                           /* defensive */
        return make_node(NODE_EMPTY,0,NULL,NULL);

    switch (r->type) {

    /* ----------- base cases ----------- */
    case NODE_EMPTY:                  /* ∅ → ∅ */
        return make_node(NODE_EMPTY,0,NULL,NULL);

    case NODE_CHAR: {                 /* c → ac + ca */
        RegexNode *ac = make_node(NODE_CONCAT,'.',
                          make_node(NODE_CHAR,a_sym,NULL,NULL),
                          make_node(NODE_CHAR,r->symbol,NULL,NULL));

        RegexNode *ca = make_node(NODE_CONCAT,'.',
                          make_node(NODE_CHAR,r->symbol,NULL,NULL),
                          make_node(NODE_CHAR,a_sym,NULL,NULL));

        return make_node(NODE_UNION,'+',ac,ca);
    }

    /* ----------- union ----------- */
    case NODE_UNION: {
        RegexNode *L = insert_sym(r->left , a_sym);
        RegexNode *R = insert_sym(r->right, a_sym);
        return make_node(NODE_UNION,'+', L, R);
    }

    /* ----------- concatenation ----------- */
    case NODE_CONCAT: {
        /* insert(s)·t  +  s·insert(t) */
        RegexNode *leftPart  = make_node(NODE_CONCAT,'.',
                               insert_sym(r->left, a_sym),
                               clone_tree(r->right));

        RegexNode *rightPart = make_node(NODE_CONCAT,'.',
                               clone_tree(r->left),
                               insert_sym(r->right, a_sym));

        return make_node(NODE_UNION,'+', leftPart, rightPart);
    }

    /* ----------- star ----------- */
    case NODE_STAR: {
        /* a  +  s*·insert(s)·s* */
        RegexNode *singleA = make_node(NODE_CHAR, a_sym, NULL, NULL);

        RegexNode *concat  = make_node(NODE_CONCAT,'.',
                               clone_tree(r),         /* s* (left)  */
                               make_node(NODE_CONCAT,'.',
                                   insert_sym(r->left, a_sym),
                                   clone_tree(r)));   /* ... s* (right) */

        return make_node(NODE_UNION,'+', singleA, concat);
    }
    }

    /* should never be reached */
    return make_node(NODE_EMPTY,0,NULL,NULL);
}

RegexNode* bs_for_a(RegexNode* node) {
    if (!node) return NULL;

    switch (node->type) {
        case NODE_EMPTY:
            return make_node(NODE_EMPTY, 0, NULL, NULL);
        case NODE_CHAR:
            if (node->symbol == 'a') {
                // replace 'a' with b* (i.e., zero or more b's)
                RegexNode* b = make_node(NODE_CHAR, 'b', NULL, NULL);
                return make_node(NODE_STAR, '*', b, NULL);
            } else {
                return make_node(NODE_CHAR, node->symbol, NULL, NULL);
            }
        case NODE_UNION: {
            RegexNode* L = bs_for_a(node->left);
            RegexNode* R = bs_for_a(node->right);
            return make_node(NODE_UNION, '+', L, R);
        }
        case NODE_CONCAT: {
            RegexNode* L = bs_for_a(node->left);
            RegexNode* R = bs_for_a(node->right);
            return make_node(NODE_CONCAT, '.', L, R);
        }
        case NODE_STAR: {
            RegexNode* body = bs_for_a(node->left);
            return make_node(NODE_STAR, '*', body, NULL);
        }
    }
    return NULL;
}


// Returns a new tree for r′ = strip off an initial 'a' from all strings in L(r)
RegexNode* strip_symbol(RegexNode* r, char a) {
    if (!r) {
        // ∅ → ∅
        return make_node(NODE_EMPTY, 0, NULL, NULL);
    }
    switch (r->type) {
      case NODE_EMPTY:
        // ∅ → ∅
        return make_node(NODE_EMPTY, 0, NULL, NULL);

      case NODE_CHAR:
        // c → ∅* if c==a, else ∅
        if (r->symbol == a) {
            // ∅* matches exactly {ε}
            return make_epsilon();
        } else {
            return make_node(NODE_EMPTY, 0, NULL, NULL);
        }

      case NODE_UNION: {
        // (s + t) → strip(s) + strip(t)
        RegexNode* Lp = strip_symbol(r->left,  a);
        RegexNode* Rp = strip_symbol(r->right, a);
        return make_node(NODE_UNION, '+', Lp, Rp);
      }

      case NODE_CONCAT: {
        // st → if ε∈L(s)
        //          then strip(s)·t  +  strip(t)
        //          else strip(s)·t
        RegexNode* sp = strip_symbol(r->left,  a);
        RegexNode* tp = strip_symbol(r->right, a);

        if (has_epsilon(r->left)) {
            // build (strip(s)·t)
            RegexNode* leftCat = make_node(
              NODE_CONCAT, '.',
              sp,
              clone_tree(r->right)
            );
            // then union it with strip(t)
            return make_node(NODE_UNION, '+', leftCat, tp);
        } else {
            // just strip(s)·t
            return make_node(
              NODE_CONCAT, '.',
              sp,
              clone_tree(r->right)
            );
        }
      }

      case NODE_STAR: {
        // s* → strip(s)·s*
        RegexNode* sp = strip_symbol(r->left, a);
        RegexNode* starClone = clone_tree(r);
        return make_node(NODE_CONCAT, '.', sp, starClone);
      }
    }

    // fallback (shouldn't happen)
    return make_node(NODE_EMPTY, 0, NULL, NULL);
}

/* ====================================================================== */
/*  Insert exactly one copy of the symbol `a` somewhere in every string
    of L(r).  Return a new syntax tree (caller owns it).                  */
/* ====================================================================== */
RegexNode *insert_symbol(RegexNode *r, char a)
{
    if (!r)                                   /* should not happen            */
        return make_node(NODE_EMPTY, 0, NULL, NULL);

    switch (r->type)
    {
/* ---------------------------------------------------------------------- */
/*  atomic regexes                                                        */
/* ---------------------------------------------------------------------- */
    case NODE_EMPTY:                          /* ∅ → ∅                       */
        return make_node(NODE_EMPTY, 0, NULL, NULL);

    case NODE_CHAR: {                         /* c → a·c  +  c·a             */
        RegexNode *left  = make_node(
            NODE_CONCAT, '.',
            make_node(NODE_CHAR, a,             NULL, NULL),
            make_node(NODE_CHAR, r->symbol,     NULL, NULL));

        RegexNode *right = make_node(
            NODE_CONCAT, '.',
            make_node(NODE_CHAR, r->symbol,     NULL, NULL),
            make_node(NODE_CHAR, a,             NULL, NULL));

        return make_node(NODE_UNION, '+', left, right);
    }

/* ---------------------------------------------------------------------- */
/*  union:  (s + t) → insert(s) + insert(t)                               */
/* ---------------------------------------------------------------------- */
    case NODE_UNION:
        return make_node(
            NODE_UNION, '+',
            insert_symbol(r->left,  a),
            insert_symbol(r->right, a));

/* ---------------------------------------------------------------------- */
/*  concatenation:                                                        */
/*      (s·t) → insert(s)·t   +   s·insert(t)                             */
/*  (the older piece – insert(s)·t – is placed first)                     */
/* ---------------------------------------------------------------------- */
    case NODE_CONCAT: {
        RegexNode *leftTerm = make_node(
            NODE_CONCAT, '.',
            insert_symbol(r->left, a),
            clone_tree(r->right));

        RegexNode *rightTerm = make_node(
            NODE_CONCAT, '.',
            clone_tree(r->left),
            insert_symbol(r->right, a));

        return make_node(NODE_UNION, '+', leftTerm, rightTerm);
    }

/* ---------------------------------------------------------------------- */
/*  star:                                                                 */
/*      s* →  s*·a·s*   +   s*·insert(s)·s*                               */
/*      (between copies  +  inside one copy)                              */
/* ---------------------------------------------------------------------- */
    case NODE_STAR: {
        /* between copies: s*·a·s* */
        RegexNode *between = make_node(
            NODE_CONCAT, '.',
            clone_tree(r),
            make_node(NODE_CONCAT, '.',
                make_node(NODE_CHAR, a, NULL, NULL),
                clone_tree(r)));

        /* inside one copy: s*·insert(s)·s* */
        RegexNode *inside  = make_node(
            NODE_CONCAT, '.',
            clone_tree(r),
            make_node(NODE_CONCAT, '.',
                insert_symbol(r->left, a),
                clone_tree(r)));

        return make_node(NODE_UNION, '+', between, inside);
    }
    }

    /* unreachable */
    return make_node(NODE_EMPTY, 0, NULL, NULL);
}


int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s --<option> [symbol]\n", argv[0]);
        return 1;
    }
    // Determine mode
    int noop_mode        = strcmp(argv[1], "--no-op")        == 0;
    int simplify_mode    = strcmp(argv[1], "--simplify")     == 0;
    int empty_mode       = strcmp(argv[1], "--empty")        == 0;
    int eps_mode         = strcmp(argv[1], "--has-epsilon")  == 0;
    int noneps_mode      = strcmp(argv[1], "--has-nonepsilon")== 0;
    int uses_mode        = strcmp(argv[1], "--uses")         == 0;
    int notusing_mode    = strcmp(argv[1], "--not-using")    == 0;
    int infinite_mode    = strcmp(argv[1], "--infinite")     == 0;
    int startswith_mode  = strcmp(argv[1], "--starts-with")  == 0;
    int reverse_mode     = strcmp(argv[1], "--reverse")      == 0;
    int endswith_mode    = strcmp(argv[1], "--ends-with")    == 0;
    int prefixes_mode    = strcmp(argv[1], "--prefixes")     == 0;
    int bsfora_mode      = strcmp(argv[1], "--bs-for-a")     == 0;
    int insert_mode      = strcmp(argv[1], "--insert")       == 0;
    int strip_mode       = strcmp(argv[1], "--strip")        == 0;
    char sym = 0;
    if (uses_mode|| notusing_mode || startswith_mode || endswith_mode || strip_mode || insert_mode) {
        if (argc<3 || strlen(argv[2])!=1) {
            fprintf(stderr,"Error: %s requires one symbol argument\n",argv[1]);
            return 1;
        }
        sym = argv[2][0];
    }

    char line[1024];
    while (fgets(line, sizeof line, stdin)) {
        RegexNode* tree = parse_postfix(line);
        if (!tree) continue;

        if (empty_mode) {
            printf(is_empty(tree) ? "yes\n":"no\n");
            free_tree(tree);
            continue;
        }
        if (eps_mode) {
            printf(has_epsilon(tree) ? "yes\n":"no\n");
            free_tree(tree);
            continue;
        }
        if (noneps_mode) {
            printf(has_nonepsilon(tree) ? "yes\n":"no\n");
            free_tree(tree);
            continue;
        }
        if (simplify_mode) {
            RegexNode* prev;
            do {
                prev = tree;
                tree = simplify(tree);
            } while (!trees_equal(tree, prev));
            print_prefix(tree);
            printf("\n");
            free_tree(tree);
            continue;
        }
        if (uses_mode) {
            printf(uses_symbol(tree,sym) ? "yes\n":"no\n");
            free_tree(tree);
            continue;
        }
        if (notusing_mode) {
            RegexNode* filt = not_using(tree,sym);
            print_prefix(filt);
            printf("\n");
            free_tree(tree);
            free_tree(filt);
            continue;
        }
        if (infinite_mode) {
            printf(is_infinite(tree) ? "yes\n":"no\n");
            free_tree(tree);
            continue;
        }
        if (startswith_mode) {
            printf(starts_with(tree,sym) ? "yes\n":"no\n");
            free_tree(tree);
            continue;
        }

        if (reverse_mode) {
            RegexNode* rev = reverse_regex(tree);
            print_prefix(rev);
            printf("\n");
            free_tree(tree);
            free_tree(rev);
            continue;
        }

        if (endswith_mode) {
            printf(ends_with(tree, sym) ? "yes\n" : "no\n");
            free_tree(tree);
            continue;
        }

        if (prefixes_mode) {
            RegexNode* pref = prefixes(tree);
            print_prefix(pref);
            printf("\n");
            free_tree(tree);
            free_tree(pref);
            continue;
        }

        if (bsfora_mode) {
            RegexNode* replaced = bs_for_a(tree);
            print_prefix(replaced);
            printf("\n");
            free_tree(tree);
            free_tree(replaced);
            continue;
        }

        if (strip_mode) {
            RegexNode* stripped = strip_symbol(tree, sym);
            print_prefix(stripped);
            printf("\n");
            free_tree(tree);
            free_tree(stripped);
            continue;
        }

        if (insert_mode) {
            RegexNode* ins = insert_symbol(tree, sym);
            print_prefix(ins);
            printf("\n");
            free_tree(tree);
            free_tree(ins);
            continue;
        }
     
        // Default: --no-op
        print_prefix(tree);
        printf("\n");
        free_tree(tree);
    }
    return 0;
}
