(lang dune 3.25)

(name yuka_gotify)

(using melange 1.0)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))
