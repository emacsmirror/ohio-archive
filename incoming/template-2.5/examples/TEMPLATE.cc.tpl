/* @(#)TEMPLATE.cc.tpl
 * Header style 1, see `template-header-regexp-alist'.
 */

#ifndef _(>>>FILE_UPCASE<<<)_H
#define _(>>>FILE_UPCASE<<<)_H 1

// Single line comments with "M-x template-single-comment".

// How the comment style is derived:

// First: when called with prefix argument, here 4: (>>>single4<<<)

// Second: old comment style provided by      Level 1: -(>>>single<<<)
// last character in line.  For details, see  Level 3: =(>>>single<<<)
// `template-comment-specification-alist'     Level 4: #(>>>single<<<)

// Third,  not shown here: `template-comment-specification-special'.

// Last: line starts at the beginning	      Level 3: (>>>single<<<)
{
  // line does not start at the beginning     Level 2: (>>>single<<<)
}

// Block comments, level 4: A block comment consists of all neighboring lines
// which start with spaces and `comment-start'.  "M-x template-block-command"
// must be called with point in the block or a empty lines after it.
//-#
(>>>block<<<)
// Block comment, level 3: The comment style also determines the number of
// empty lines at the beginning and the minimum number of lines after the
// block.  A second "M-x template-block-command" without a prefix argument in
// an empty line directly after a sucessful invokation of this command deletes
// the additional empty lines.(>>>block<<<)
#define WHATEVER whatever
//-=--=#
//-=-(>>>block<<<)
// Block comment, level 1: The comment style is derived in the same way as for
// single line comments.  Lines which exists entirely of whitespaces, the
// comment start and chars from the comment style specification are deleted.
// The last character of the last line of these lines is considered to
// represent the old comment style.

/*-(>>>block<<<)
 * C-style comments are also supported.
 *(>>>single<<<)

// The reason for calling `normal-mode' before the expansion is the fact that
// the comment commands use `indent-according-to-mode'.

#endif /* _(>>>FILE_UPCASE<<<)_H */
>>>TEMPLATE-DEFINITION-SECTION<<<
nil
normal-mode
nil
("single4" template-single-comment 4)
("single" (y-or-n-p-minibuf "Continue? ") (template-single-comment))
("block" (y-or-n-p-minibuf "Continue? ") (template-block-comment))
