/* Copyright (c) 2010-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#ifndef UTILS_LINK_LIST_H_
#define UTILS_LINK_LIST_H_

/*
 * Simple doubly linked list functions.
 */
#include <stddef.h>

#if defined(WINDDK) || defined(NT_INST)
#define LINK_LIST_INLINE _inline
#elif defined(_WIN32)
#define LINK_LIST_INLINE __inline
#else
#define LINK_LIST_INLINE inline
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct link_s link_t;

struct link_s {
    link_t* next;
    link_t* prev;
};

#define STRUCT_CONTAINING_LINK(_link_ptr,_struct_type,_link_field_name) \
    ((_struct_type*)((char*)(_link_ptr) - (ptrdiff_t) &(((_struct_type*) 0)->_link_field_name)))

static LINK_LIST_INLINE link_t* link_insert_prev(link_t* base_link, link_t* new_link) {
    new_link->prev = base_link->prev;
    new_link->next = base_link;
    base_link->prev->next = new_link;
    base_link->prev = new_link;
    return new_link;
}   /* link_insert_prev() */

static LINK_LIST_INLINE link_t* link_insert_next(link_t* base_link, link_t* new_link) {
    new_link->next = base_link->next;
    new_link->prev = base_link;
    base_link->next->prev = new_link;
    base_link->next = new_link;
    return new_link;
}   /* link_insert_prev() */

static LINK_LIST_INLINE link_t* link_remove(link_t* link) {
    link->prev->next = link->next;
    link->next->prev = link->prev;
    link->prev = link;
    link->next = link;
    return link;
}   /* link_remove() */

/* ------------------------------------------------------------------------- */
/*
 * A list is just a link that's used as an anchor. The list link marks both
 * the beginning and the end of the chain of links. When the only link left
 * is the list link, then the list is empty.
 */
typedef link_t list_t;

/**
 * Intended usage:
 *
 *    list_t LIST_INIT(g_my_list);
 */
#define LIST_INIT(_variable_name) _variable_name = { &_variable_name, &_variable_name }

static LINK_LIST_INLINE list_t* list_init(list_t* list) {
    list->prev = list;
    list->next = list;
    return list;
}   /* list_init() */

static LINK_LIST_INLINE int list_is_empty(list_t* list) {
    return list->next == list;
}   /* list_is_empty() */

static LINK_LIST_INLINE link_t* list_insert_prev(list_t* list, link_t* link) {
    return link_insert_prev(list, link);
}   /* list_insert_prev() */

static LINK_LIST_INLINE link_t* list_insert_next(list_t* list, link_t* link) {
    return link_insert_next(list, link);
}   /* list_insert_next() */

static LINK_LIST_INLINE link_t* list_remove_prev(list_t* list) {
    return list_is_empty(list) ? NULL : link_remove(list->prev);
}   /* list_remove_next() */

static LINK_LIST_INLINE link_t* list_remove_next(list_t* list) {
    return list_is_empty(list) ? NULL : link_remove(list->next);
}   /* list_remove_next() */

/**
 * Remove the prev link from @a _listp, returning a structure of type @a
 * _struct_type that has a link_t field name @a _link_field_name.
 */
#define list_remove_prev_struct(_listp,_struct_type,_link_field_name) \
    STRUCT_CONTAINING_LINK(list_remove_prev(_listp), _struct_type, _link_field_name)

/**
 * Remove the next link from @a _listp, returning a structure of type @a
 * _struct_type that has a link_t field name @a _link_field_name.
 */
#define list_remove_next_struct(_listp,_struct_type,_link_field_name) \
    STRUCT_CONTAINING_LINK(list_remove_next(_listp), _struct_type, _link_field_name)

/**
 * A simple list appends at the previous node (next is first).
 */
#define list_append(_list,_link)  list_insert_prev((_list), (_link))

/**
 * To use as a stack, assuming no duplicate links will be pushed:
 */
#define list_push(_list,_link)  list_insert_next((_list), (_link))
#define list_pop(_list)         list_remove_next((_list))

/**
 * Pop a link pointer from _list then return a structure pointer of type
 * _struct_type that has the link at field _link_field_name.
 */
#define list_pop_struct(_list,_struct_type,_link_field_name)   \
    STRUCT_CONTAINING_LINK(list_pop(_list), _struct_type, _link_field_name)

/**
 * @return a _struct_type* pointer to the top item on the stack, or NULL if
 * it is empty, where _struct_type a link at field _link_field_name.
 */
#define list_top_struct(_list,_struct_type,_link_field_name)   \
    (list_is_empty(_list) ? NULL : STRUCT_CONTAINING_LINK((_list)->next, _struct_type, _link_field_name))

/**
 * Move @a src_list to the end of @a tgt_list quickly.
 */
static LINK_LIST_INLINE list_t* list_append_list(list_t* tgt_list, list_t* src_list) {
    if (!list_is_empty(src_list)) {
        tgt_list->prev->next = src_list->next;
        src_list->next->prev = tgt_list->prev;
        src_list->prev->next = tgt_list;
        tgt_list->prev = src_list->prev;
        list_init(src_list);
    }
    return tgt_list;
}   /* list_append_list() */

/**
 * @return 1 if @a link is found within the first @a max_count items in @a
 * list, starting with the list's next pointer.
 */
static LINK_LIST_INLINE int list_has_link(list_t* list, link_t* link, int max_count) {
    link_t* list_link = list->next;
    while ((max_count-- > 0) && (list_link != list)) {
        if (list_link == link) {
            return 1;
        }
        list_link = list_link->next;
    }
    return 0;
}

/**
 * For each link in @a _listp, run @a _code using variable name @a _linkp to
 * hold the current link pointer.
 *
 * @a _linkp may be safely removed from @a _listp in @a _code.
 */
#define list_foreach_link(_listp,_linkp,_code)                          \
    do {                                                                \
        link_t* _linkp = (_listp)->next;                                \
        link_t* __next = NULL;                                          \
        for (; _linkp != (_listp); _linkp = __next) {                   \
            __next = _linkp->next;                                      \
            _code ;                                                     \
        }                                                               \
    } while (0)

/**
 * Using @a _listp as a list of structures of type @a _struct_type that are
 * linked through link_t field name @a _link_field_name, for each struct
 * pointer @a _structp in the list, execute @a _code.
 *
 * @a _structp may be safely removed from @a _listp in @a _code.
 */
#define list_foreach_struct(_listp,_structp,_struct_type,_link_field_name,_code) \
    do {                                                                \
        link_t* __link = (_listp)->next;                                \
        link_t* __next = NULL;                                          \
        for (; __link != (_listp); __link = __next) {                   \
            __next = __link->next;                                      \
            _struct_type* _structp = STRUCT_CONTAINING_LINK(__link, _struct_type, _link_field_name); \
            _code ;                                                     \
        }                                                               \
    } while (0)

#ifdef __cplusplus
}
#endif

#endif  // UTILS_LINK_LIST_H_
