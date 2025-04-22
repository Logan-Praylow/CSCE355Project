#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum { NODE_EMPTY, NODE_CHAR, NODE_UNION, NODE_CONCAT, NODE_STAR } NodeType;

typedef struct RegexNode {
    NodeType type;
    char symbol;
    struct RegexNode *left;
    struct RegexNode *right;
} RegexNode;

RegexNode* make_node(NodeType type, char symbol, RegexNode* left, RegexNode* right) {
    RegexNode* node = malloc(sizeof(RegexNode));
    node->type = type;
    node->symbol = symbol;
    node->left = left;
    node->right = right;
    return node;
}

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

void print_prefix(RegexNode* node) {
    if (!node) return;
    switch (node->type) {
        case NODE_EMPTY: printf("/"); break;
        case NODE_CHAR: printf("%c", node->symbol); break;
        case NODE_STAR: printf("*"); print_prefix(node->left); break;
        case NODE_UNION: printf("+"); print_prefix(node->left); print_prefix(node->right); break;
        case NODE_CONCAT: printf("."); print_prefix(node->left); print_prefix(node->right); break;
    }
}

int trees_equal(RegexNode* a, RegexNode* b) {
    if (!a || !b) return a == b;
    if (a->type != b->type || a->symbol != b->symbol) return 0;
    return trees_equal(a->left, b->left) && trees_equal(a->right, b->right);
}

void free_tree(RegexNode* node) {
    if (!node) return;
    free_tree(node->left);
    free_tree(node->right);
    free(node);
}

// Helper used only in simplify() to detect NODE_EMPTY exactly
static int is_exactly_empty(RegexNode *n) {
    return n && n->type == NODE_EMPTY;
}

static int is_empty_star(RegexNode *n) {
    return n && n->type == NODE_STAR && is_exactly_empty(n->left);
}

// Simplification function
RegexNode* simplify(RegexNode* node) {
    if (!node) return NULL;

    node->left = simplify(node->left);
    node->right = simplify(node->right);

    if (node->type == NODE_STAR && node->left && node->left->type == NODE_STAR) {
        RegexNode* keep = node->left;
        free(node);
        return keep;
    }

    if (node->type == NODE_UNION) {
        if (is_exactly_empty(node->left)) {
            RegexNode* keep = node->right;
            free(node->left);
            free(node);
            return keep;
        }
        if (is_exactly_empty(node->right)) {
            RegexNode* keep = node->left;
            free(node->right);
            free(node);
            return keep;
        }
        if (trees_equal(node->left, node->right)) {
            free_tree(node->right);
            RegexNode* keep = node->left;
            free(node);
            return keep;
        }
    }

    if (node->type == NODE_CONCAT) {
        if (is_exactly_empty(node->left) || is_exactly_empty(node->right)) {
            free_tree(node->left);
            free_tree(node->right);
            RegexNode* empty = make_node(NODE_EMPTY, 0, NULL, NULL);
            free(node);
            return empty;
        }
        if (is_empty_star(node->left)) {
            RegexNode* keep = node->right;
            free_tree(node->left);
            free(node);
            return keep;
        }
        if (is_empty_star(node->right)) {
            RegexNode* keep = node->left;
            free_tree(node->right);
            free(node);
            return keep;
        }
    }

    return node;
}

// Recursive check if a regex denotes the empty language
int is_empty(RegexNode* node) {
    if (!node) return 1;

    switch (node->type) {
        case NODE_EMPTY:
            return 1;
        case NODE_CHAR:
            return 0;
        case NODE_STAR:
            return 0;
        case NODE_UNION:
            return is_empty(node->left) && is_empty(node->right);
        case NODE_CONCAT:
            return is_empty(node->left) || is_empty(node->right);
    }
    return 1;
}

int has_epsilon(RegexNode* node) {
    if (!node) return 0;

    switch (node->type) {
        case NODE_EMPTY:
            return 0;  // ∅ does not contain ε
        case NODE_CHAR:
            return 0;  // 'a' ≠ ε
        case NODE_STAR:
            return 1;  // R* always includes ε
        case NODE_UNION:
            return has_epsilon(node->left) || has_epsilon(node->right);
        case NODE_CONCAT:
            return has_epsilon(node->left) && has_epsilon(node->right);
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

int uses_symbol(RegexNode* node, char target) {
    if (!node) return 0;

    switch (node->type) {
        case NODE_EMPTY:
            return 0;
        case NODE_CHAR:
            return node->symbol == target;
        case NODE_STAR:
            return uses_symbol(node->left, target);
        case NODE_UNION:
            return uses_symbol(node->left, target) || uses_symbol(node->right, target);
        case NODE_CONCAT: {
            // Generate a string by combining: left + right
            int left_contains = uses_symbol(node->left, target);
            int right_contains = uses_symbol(node->right, target);
            int left_nonempty = !is_empty(node->left);
            int right_nonempty = !is_empty(node->right);

            // Only if both sides can generate strings, then we combine
            return (left_contains && right_nonempty) || (right_contains && left_nonempty);
        }
    }
    return 0;
}

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
            RegexNode* l = not_using(node->left, target);
            RegexNode* r = not_using(node->right, target);
            if (is_exactly_empty(l) && is_exactly_empty(r)) {
                free_tree(l); free_tree(r);
                return make_node(NODE_EMPTY, 0, NULL, NULL);
            } else if (is_exactly_empty(l)) {
                free_tree(l);
                return r;
            } else if (is_exactly_empty(r)) {
                free_tree(r);
                return l;
            }
            return make_node(NODE_UNION, '+', l, r);
        }
        case NODE_CONCAT: {
            RegexNode* l = not_using(node->left, target);
            RegexNode* r = not_using(node->right, target);
            if (is_exactly_empty(l) || is_exactly_empty(r)) {
                free_tree(l); free_tree(r);
                return make_node(NODE_EMPTY, 0, NULL, NULL);
            }
            return make_node(NODE_CONCAT, '.', l, r);
        }
        case NODE_STAR: {
            RegexNode* child = not_using(node->left, target);
            return make_node(NODE_STAR, '*', child, NULL);
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

int starts_with(RegexNode* node, char target) {
    if (!node) return 0;
    switch (node->type) {
        case NODE_EMPTY:
            // ∅ has no strings
            return 0;
        case NODE_CHAR:
            // single‐char language matches exactly that char
            return node->symbol == target;
        case NODE_STAR:
            // s* contains strings starting with `target` iff s does
            return starts_with(node->left, target);
        case NODE_UNION:
            // s+t contains those of s or those of t
            return starts_with(node->left,  target)
                || starts_with(node->right, target);
        case NODE_CONCAT:
            // st contains strings starting with a if either
            // 1) s does, or
            // 2) ε ∈ L(s) and t does
            return starts_with(node->left,  target)
                || (has_epsilon(node->left)
                    && starts_with(node->right, target));
    }
    return 0;
}




// Main
int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s --<option>\n", argv[0]);
        return 1;
    }
    int uses_mode = strcmp(argv[1], "--uses") == 0;
    int noneps_mode = strcmp(argv[1], "--has-nonepsilon") == 0;
    int eps_mode = strcmp(argv[1], "--has-epsilon") == 0;
    int noop_mode = strcmp(argv[1], "--no-op") == 0;
    int simplify_mode = strcmp(argv[1], "--simplify") == 0;
    int empty_mode = strcmp(argv[1], "--empty") == 0;
    int notusing_mode = strcmp(argv[1], "--not-using") == 0;
    int infinite_mode = strcmp(argv[1], "--infinite") == 0;
    int startswith_mode = strcmp(argv[1], "--starts-with") == 0;

    if (!noop_mode && !simplify_mode && !empty_mode 
        && !eps_mode && !noneps_mode && !uses_mode
        && !notusing_mode && !infinite_mode && !startswith_mode) {
        fprintf(stderr, "Unsupported option: %s\n", argv[1]);
        return 1;
    }

    char target_symbol = 0;
    if (uses_mode) {
        if (argc < 3 || strlen(argv[2]) != 1) {
            fprintf(stderr, "Error: --uses requires a single character argument.\n");
            return 1;
        }
        target_symbol = argv[2][0];
    }

    char notusing_symbol = 0;
    if (notusing_mode) {
        if (argc < 3 || strlen(argv[2]) != 1) {
            fprintf(stderr, "Error: --not-using requires a single character argument.\n");
            return 1;
        }
        notusing_symbol = argv[2][0];
    }

    char startswith_symbol = 0;
    if (startswith_mode) {
        if (argc < 3 || strlen(argv[2]) != 1) {
            fprintf(stderr, "Error: --starts-with requires a single character argument.\n");
            return 1;
        }
        startswith_symbol = argv[2][0];
    }

    char line[1024];
    while (fgets(line, sizeof(line), stdin)) {
        RegexNode* tree = parse_postfix(line);
        if (!tree) continue;

        if (empty_mode) {
            printf(is_empty(tree) ? "yes\n" : "no\n");
            free_tree(tree);
            continue;
        }

        if (noneps_mode) {
            printf(has_nonepsilon(tree) ? "yes\n" : "no\n");
            free_tree(tree);
            continue;
        }
        
        if (simplify_mode) {
            RegexNode* prev;
            do {
                prev = tree;
                tree = simplify(tree);
            } while (!trees_equal(tree, prev));
        }

        if (eps_mode) {
            printf(has_epsilon(tree) ? "yes\n" : "no\n");
            free_tree(tree);
            continue;
        }

        if (uses_mode) {
            printf(uses_symbol(tree, target_symbol) ? "yes\n" : "no\n");
            free_tree(tree);
            continue;
        }

        if (notusing_mode) {
            RegexNode* filtered = not_using(tree, notusing_symbol);
            print_prefix(filtered);
            printf("\n");
            free_tree(tree);
            free_tree(filtered);
            continue;
        }
        
        if (infinite_mode) {
            printf(is_infinite(tree) ? "yes\n" : "no\n");
            free_tree(tree);
            continue;
        }

        if (startswith_mode) {
            printf(starts_with(tree, startswith_symbol) ? "yes\n" : "no\n");
            free_tree(tree);
            continue;
        }
        

        print_prefix(tree);
        printf("\n");
        free_tree(tree);
    }

    return 0;
}
