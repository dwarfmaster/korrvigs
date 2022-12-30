#include <stdlib.h>
#include <string.h>
#include <SWI-Prolog.h>
#include <tree_sitter/api.h>

#define pl_assert(c) if(!(c)) { PL_fail; }

TSLanguage* tree_sitter_norg();

// Data
typedef struct {
  functor_t element;
  functor_t equal;
  const char* text;
} data;

// Recursively unify term with a representation of node
// At the end, the cursor must be brought back to the starting point
foreign_t term_of_node(data dat, TSTreeCursor* cursor, term_t term) {
  TSNode node = ts_tree_cursor_current_node(cursor);
  const char* type = ts_node_type(node);
  uint32_t start = ts_node_start_byte(node);
  uint32_t end = ts_node_end_byte(node);

  term_t head = PL_new_term_ref();
  term_t attrs = PL_new_term_ref();
  term_t childs = PL_new_term_ref();
  term_t arg;

  pl_assert(PL_unify_functor(term, dat.element));
  pl_assert(PL_unify_arg(1, term, head));
  pl_assert(PL_unify_arg(2, term, attrs));
  pl_assert(PL_unify_arg(3, term, childs));

  // Type
  pl_assert(PL_unify_atom_chars(head, type));

  // Attributes
  pl_assert(PL_unify_list(attrs, head, attrs));
  pl_assert(PL_unify_functor(head, dat.equal));
  arg = PL_new_term_ref();
  pl_assert(PL_unify_arg(1, head, arg));
  pl_assert(PL_unify_atom_chars(arg, "start"));
  arg = PL_new_term_ref();
  pl_assert(PL_unify_arg(2, head, arg));
  pl_assert(PL_unify_uint64(arg, start));
  pl_assert(PL_unify_list(attrs, head, attrs));
  pl_assert(PL_unify_functor(head, dat.equal));
  arg = PL_new_term_ref();
  pl_assert(PL_unify_arg(1, head, arg));
  pl_assert(PL_unify_atom_chars(arg, "end"));
  arg = PL_new_term_ref();
  pl_assert(PL_unify_arg(2, head, arg));
  pl_assert(PL_unify_uint64(arg, end));

  // field name
  const char* field = ts_tree_cursor_current_field_name(cursor);
  if(field) {
    pl_assert(PL_unify_list(attrs, head, attrs));
    pl_assert(PL_unify_functor(head, dat.equal));
    arg = PL_new_term_ref();
    pl_assert(PL_unify_arg(1, head, arg));
    pl_assert(PL_unify_atom_chars(arg, "field"));
    arg = PL_new_term_ref();
    pl_assert(PL_unify_arg(2, head, arg));
    pl_assert(PL_unify_string_chars(arg, field));
  }
  pl_assert(PL_unify_nil(attrs));

  // Children
  if(ts_tree_cursor_goto_first_child(cursor)) {
    do {
      pl_assert(PL_unify_list(childs, head, childs));
      TSNode child = ts_tree_cursor_current_node(cursor);
      if(ts_node_is_named(child)) {
        pl_assert(term_of_node(dat, cursor, head));
      } else {
        uint32_t cstart = ts_node_start_byte(child);
        uint32_t cend = ts_node_end_byte(child);
        pl_assert(PL_unify_chars(head, PL_STRING | REP_UTF8, cend-cstart, dat.text + cstart));
      }
    } while(ts_tree_cursor_goto_next_sibling(cursor));
    ts_tree_cursor_goto_parent(cursor);
  }
  pl_assert(PL_unify_nil(childs));

  PL_succeed;
}

static foreign_t pl_parse_norg(term_t pl_content, term_t pl_ast) {
  TSLanguage* norg = tree_sitter_norg();
  if(!norg) PL_fail;

  char* content = NULL;
  if(!PL_get_chars(pl_content, &content, CVT_ATOM | CVT_STRING)) PL_fail;

  TSParser* parser = ts_parser_new();
  if(!parser) PL_fail;
  ts_parser_set_language(parser, norg);

  TSTree* tree = ts_parser_parse_string(parser, NULL, content, strlen(content));
  if(!tree) PL_fail;

  data dat;
  dat.element = PL_new_functor(PL_new_atom("element"), 3);
  dat.equal = PL_new_functor(PL_new_atom("="), 2);
  dat.text = content;

  TSNode root = ts_tree_root_node(tree);
  if(ts_node_is_null(root)) PL_fail;
  TSTreeCursor cursor = ts_tree_cursor_new(root);
  return term_of_node(dat, &cursor, pl_ast);
}

install_t install_norg_parser() {
  PL_register_foreign("parse_norg_impl", 2, pl_parse_norg, 0);
}

