#lang scribble/manual
@require[@for-label[stencil-vector-utils
                    racket/base]]

@title{stencil-vector-utils}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

@defmodule[stencil-vector-utils]

Functions to make it more convenient to use the stencil vectors added in Racket 8.6.

Some terminology: @racketfont{index} refers to the virtual index used
by @racket{stencil-vector-ref} etc. @code{0} corresponds to the first
set bit, @code{1} to the second, etc. no matter where in the bitmask
they are. @racketfont{slot} refers to the element associated wtih a
given bit position. @code{0} corresponds to the first bit @code{#b1},
@code{1} to the second bit @code{#b10}, and so on, regardless of how
many bits are set before it in the mask.

@section{Stencil Vector contracts and predicates}

@defthing[stencil-vector-slot? contract?
          #:value (integer-in 0 (sub1 (stencil-vector-mask-width)))]{

Verifies that a value is a valid slot number.

}

@defthing[stencil-vector-bitmask? contract?
          #:value (integer-in 0 (sub1 (expt 2 (stencil-vector-mask-width))))]{

Verifies that a number is a valid bitmask value.

}

@defproc[(stencil-vector-slot-has-slot? [sv stencil-vector?] [slot stencil-vector-slot?])
         boolean?]{

Returns @code{#t} if a value is associated with the given slot, @code{#f} if not.

}

@defproc[(stencil-vector-empty? [sv stencil-vector?]) boolean?]{

Returns @code{#t} if the stencil vector has a length of 0.

}

@section{Stencil Vector Operations}

@defproc[(make-stencil-vector [bitmask stencil-vector-bitmask?] [v any/c])
         stencil-vector?]{

Create a new stencil vector using the given bitmask, with every element populated with @code{v}.

}

@defproc[(build-stencil-vector [bitmask stencil-vector-bitmask?] [proc (-> exact-nonnegative-integer? any/c)])
         stencil-vector?]{

Create a new stencil vector using the given bitmask, populated by the
results of calling @code{proc} on each index in turn.

}

@defproc[(stencil-vector-copy [sv stencil-vector?] [#:bitmask stencil-vector-bitmask? (stencil-vector-mask sv)])
         stencil-vector?]{

Returns a newly allocated copy of the stencil vector. Normally the new
one has the same bitmask, but you can specify a new one with the
@code{#:bitmask} keyword. It is an error if the new bitmask has a
different arity than the original.

}

@defproc[(stencil-vector-slot->index [sv stencil-vector?] [slot stencil-vector-slot?])
         (or/c exact-nonnegative-integer? #f)]{

Returns the index corresponding to the nth slot, or @code{#f} if that slot is unused.

}

@defproc[(stencil-vector-index->slot [sv stencil-vector?] [n exact-nonnegative-integer?])
         stencil-vector-slot?]{

Returns the slot corresponding to the nth index, which must be valid.

}

@defproc[(stencil-vector-slot-ref [sv stencil-vector?] [slot stencil-vector-slot?]  [default any/c (lambda () (error "Index not present: " i))])
         any]{

Returns the value associated with the given slot. If that slot is
unused, and @code{default} is a procedure, the result of invoking it
as a tail call is returned. Otherwise, @code{default} is returned.

}

@defproc[(stencil-vector->list [sv stencil-vector?]) list?]{

Returns a list of the stencil vector's elements.

}

@defproc[(stencil-vector->vector [sv stencil-vector?]) vector?]{

Returns a normal vector corresponding to the stencil vector.

}

@section{Stencil Vector Iteration}

@defproc[(in-stencil-vector [sv stencil-vector?]) sequence?]{

Returns a sequence that iterates through the elements of the stencil vector.

}

@defproc[(stencil-vector-for-each [proc (-> any/c any/c)] [sv stencil-vector?]) void?]{

Calls @code{proc} for each element of the stencil vector in turn.

}

@defproc[(stencil-vector-map! [proc (-> any/c any/c)] [sv stencil-vector?]) veoid?]{

Replaces each element of the stencil vector with the result of calling
@code{proc} on the old value.

}

@defproc[(stencil-vector-map [proc (-> any/c any/c)] [sv stencil-vector?] [#:bitmask bitmask stencil-vector-bitmask? (stencil-vector-mask sv)])
         stencil-vector?]{

Returns a new stencil vector created by applying @code{proc} to each
element of @code{sv}. Normally the new stencil vector has the same
bitmask as the original; a new one can be specified with the
@code{#:bitmask} keyword. It is an error if the new mask has a
different arity than the original vector's.

}

@defproc[(stencil-vector-fold [kons (-> any/c any/c any/c)] [knil any/c] [sv stencil-vector?]) any/c]{

Folds the elements of the stencil vector. The first argument to
@code{kons} is the current element, the second argument is the result
of the last call, initially @code{knil} for the first element.

}