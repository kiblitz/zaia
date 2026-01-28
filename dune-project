(lang dune 3.18)

(version 0.0.1)

(generate_opam_files)

(name zaia)

(source
 (github kiblitz/zaia))

(license MIT)

(maintainers kiblitz)

(authors kiblitz)

(package
 (name zaia)
 (synopsis
  "some synopsis")
 (description
  "some description")
 (depends
  "bignum"
  "cmdliner"
  "core"
  "core_kernel"
  ("dune"
   (>= "3.18.1"))
  ("ocaml"
   (>= "5.3.0"))
  "ocplib-endian"
  "ppx_jane"
  "ppx_yojson_conv"
  "yojson"))
