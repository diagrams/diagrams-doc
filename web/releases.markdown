---
title: Releases
---



---- XXX

what to do with this text?


  Previously we had a primitive operation 'beside', which combined two
  diagrams into one, placing one next to the other, implemented in terms
  of 'Boundable'.  However, this actually conflated three different
  things:

    1. Positioning one thing next to another
    2. Combining things
    3. The specific implementation technique using Boundable

  1 is one of the original important design goals for diagrams.  2 is
  what Monoid is for, so the most primitive positioning primitive
  needn't do it as well.  And there are some things (such as animations)
  which are not Boundable but which it makes sense to place next to one
  another.

  Hence the 'Juxtaposable' class has a single method, 'juxtapose', which
  positions one thing next to another thing, without combining them.
  'beside' will now be implemented as a call to 'juxtapose' combined
  with a call to 'mappend' (see the corresponding diagrams-lib patch).
  A default implementation for 'juxtapose' is provided for those things
  which do have a Boundable instance.


    - Change semantics of 'beside' (hence also (|||) and (===)) so the
      result's origin is the same as that of the first argument.

  This has several advantages:
  
    1. beside v, (|||), and (===) are now associative
    2. getting the old behavior from the new is easy (just insert an extra align);
       getting the new behavior from old was difficult.
    3. fits better with behavior of cat.
  
  Of course, this may break diagrams which happened to rely on the old
  behavior (namely, to put the local origin of the combined diagram at
  the point of tangency between the two arguments).  But this is easy to
  fix, either by aligning the first argument in the same direction
  before using 'beside', or, if the arguments are the same width,
  centering the result.
