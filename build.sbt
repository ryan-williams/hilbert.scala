group("com.runsascoded")
name := "hilbert"
`2.12`.version := "2.12.8"
`2.12`.only
dep(
  iterators % "2.2.0",
  shapeless_utils % "1.5.1"
)
