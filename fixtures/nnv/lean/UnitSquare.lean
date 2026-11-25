-- Written by `maithic nnv verify`. The kernel runs a checker that is proved sound; nothing else is trusted.
import Maithic.NN.Cert.Property

open Maithic.NN

namespace Maithic.NNV.Job

def net : Network := [
  { neurons := [⟨[1/10, (-3/10)], 1/20⟩, ⟨[7/10, 1/5], (-3/20)⟩, ⟨[(-2/5), 3/5], 1/10⟩], activation := .relu },
  { neurons := [⟨[11/10, (-9/10), 1/2], 1/100⟩, ⟨[(-1/5), 4/5, 3/10], 0⟩], activation := .relu },
  { neurons := [⟨[1, (-1)], 0⟩], activation := .identity }]

def input : Box := [⟨0, 1⟩, ⟨0, 1⟩]

def claimed : Box := [⟨(-1), 1⟩]

def certificate : Certificate := [
  [⟨0, 2702159776422299/18014398509481984⟩, ⟨0, 6755399441055749/9007199254740992⟩, ⟨0, 3152519739159349/4503599627370496⟩],
  [⟨0, 4728779608739027/9007199254740992⟩, ⟨0, 3647915698170107/4503599627370496⟩],
  [⟨(-7295831396340217/9007199254740992), 4728779608739031/9007199254740992⟩]]

/-- Every point of the input box is mapped into the claimed box. -/
theorem property_holds : Holds net input claimed :=
  checkProperty_sound (cert := certificate) (by decide +kernel)

end Maithic.NNV.Job
