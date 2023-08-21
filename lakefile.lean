import Lake
open Lake DSL

package «lean4stats» {
  -- add package configuration options here
}

lean_lib «Lean4stats» {
  -- add library configuration options here
}

require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "885480409e857a9bda3ca843d37ce9bfcd2c03d4"

@[default_target]
lean_exe countConstants where
  root := `Lean4Stats.CountConstants
  supportInterpreter := true

@[default_target]
lean_exe «lean4stats» {
  root := `Main
}
