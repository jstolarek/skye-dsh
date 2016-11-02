Ideas on extending DSH with provenance tracking
===============================================

  * it should be possible to add provenance to DSH.  Transformation would by
    syntax directed, **but:**

  * DSH's internal AST contains explicit type information.  It should be
    possible to use that type information to guide transformations.  However, I
    doubt this approach will work for anything but trivial examples.  I fear
    that using type information provided in the AST will sooner or later require
    a full implementation of type checking and inference algorithm.  Needless to
    say, this will be far from trivial.

  * One idea how to add provenance: define a new version of `Externals` module
    with desugaring combinators that insert provenance information.  This is a
    half-baked idea: there must be a way to specify which field should be
    tracked for provenance.