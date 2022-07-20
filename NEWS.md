# New in 0.1.1

* Unclipping now works for files with an extension which is not `.tex`, which can happen when the `fig.ext` chunk option is used.
* `stringr` is now a mandatory dependency.

# New in 0.1.0

* Annotations relative to _data_ coordinates respect scale transformations with the `transform=TRUE` option to `ggtikzAnnotation` (new default).
* Automatically replace `Inf` and `-Inf` in TikZ coordinate specifications with the maximum and minimum for the annotation's reference frame, respectively. This is enabled by setting the `replace_inf=TRUE` option to `ggtikzAnnotation` (new default)
* Draw annotations in a viewport shrunk by the size of lines surrounding the plot, to prevent the annotations from clipping into these lines
* Plots can be automatically unclipped to accommodate annotations extending beyond the plot boundaries. See `unclip()` and `set_ggtikz_unclip_hook()`.
* Discrete scales are now handled.

# New in 0.0.1

Initial release.
