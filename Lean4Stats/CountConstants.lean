import Mathlib

namespace Lean

open Std

unsafe def constsInExpr (e : Expr) (l : NameSet := .empty) : MetaM NameSet :=
  e.foldConsts (return l) (fun n l => do
    let l' ← l
    if l'.contains n
    then return l'
    else
      (do
        let d ← getConstInfo n
        constsInExpr (← d.value?) (l'.insert n)) <|> return l')

unsafe def printConstsInDecl (n : Name) : MetaM Unit := do
  let d ← getConstInfo n
  let l ← constsInExpr (← d.value?) .empty
  IO.println l.toList.length

--This is wrong
unsafe def constsInName
    (n : Name) (c : ConstantInfo) (f : NameMap ℕ := .empty)
    (l : NameSet := .empty) :
    MetaM (NameMap ℕ × ℕ × NameSet) :=
  if l.contains n then return (f, 0, l)
  else
  match f.find? n with
  | some x => return (f, x, l)
  | none => (do
      let v ← c.value?
      v.foldConsts (return (f, 1, l.insert n))
        (fun n x => do
          let (f, num, l) ← x
          let c ← getConstInfo n
          let (f2, num2, l2) ← constsInName n c f l
          return (f2.insert n num2, num + num2, l2))) <|>
      return (f, 1, l)
  -- e.foldConsts (return (0, l)) (fun n x => do
  --   let (num, l) ← x
  --   if l.contains n then return (num, l)
  --   else
  --   match f.find? n with
  --   | some x => return (num + x, l)
  --   | none =>
  --     (do
  --       let d ← getConstInfo n
  --       let v ← d.value?
  --       const_in_expr' v f (l.insert n)) <|>
  --       return (0, l))

unsafe def findBiggestConst : MetaM Unit := do
  let cs := (← getEnv).constants
  let n : (Name × ℕ × NameMap ℕ) ← cs.fold
    (fun old n c => do
      let (biggest, biggest_num, f) ← old
      (do
        let (f, num, _) ← constsInName n c f .empty 0
        if num > biggest_num
        then return (n, num, f.insert n num)
        else return (biggest, biggest_num, f.insert n num)) <|>
      return (biggest, biggest_num, f.insert n 0)

      -- (do
      --   let e ← c.value?
      --   let (num, l) ← const_in_expr' e f l
      --   if num > biggest_num
      --   then return (n, num, f.insert n num, l)
      --   else return (biggest, biggest_num, f.insert n num, l)) <|>
      --   return (biggest, biggest_num, f.insert n 0, l)

        )
    (return (default, 0, .empty))
  IO.println (n.1, n.2.1)
  return ()

unsafe def list_decls : MetaM Unit := do
  let cs := (← getEnv).constants
  let n : NameSet := cs.fold
    (fun old n _ => old.insert n)
    .empty
  IO.println n.toList.length

end Lean

open Lean Core Elab Command Std.Tactic.Lint Meta Std

elab "compileTimeSearchPath" : term =>
  return toExpr (← searchPathRef.get)

unsafe def main (l : List String) : IO UInt32 := do
  searchPathRef.set compileTimeSearchPath
  let module : Name := `Mathlib
  withImportModules [{module}] {} (trustLevel := 1024) fun env => do
    let ctx := {fileName := "", fileMap := default}
    let state := {env}
    let name := l.head!.toName
    let (num, _, _) ← MetaM.toIO (do constsInName name (← getConstInfo name)) ctx state
    IO.println num.2.1
    return 0
