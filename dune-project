(lang dune 1.11)
(generate_opam_files true)

(name croquis)
(source (github pveber/croquis))
(license CeCILL-B)
(authors "Philippe Veber")
(maintainers "philippe.veber@gmail.com")

(package
  (name croquis)
  (synopsis "Vector graphics library")
  (description "")
  (depends
    core_kernel
    ocamlify
    otfm
    vg))
