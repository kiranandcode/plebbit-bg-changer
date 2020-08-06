# Plebbit Bg Changer
Quick OCaml script to change your background using images from Plebbit.

```
Script to automatically set desktop backgrounds from Reddit images

  plebbit_slideshow.exe SUBCOMMAND

=== subcommands ===

  download   Download images from Reddit to a local folder.
  import     Import saved images into database.
  change-bg  Change background image using images in database.
  update-db  Update stored database to accurately track pictures.
  version    print version information
  help       explain a given subcommand (perhaps recursively)
```

Wrote this originally in Python, but Python is trash.

Have a look at `reddit.ml` for a rather nice typed wrapper around
Reddit's public and private APIs, using first-class modules to
implement authentication.
