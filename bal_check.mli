type mobile = branch * branch
and branch =
    SimpleBranch of length * weight
  | CompoundBranch of length * mobile
and length = int
and weight = int

let rec compute_weight : mobile -> weight =
  fun m ->
    match m with
      SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> w1 + w2
    | SimpleBranch (l1, w1), CompoundBranch (l2, m2) -> w1 + compute_weight m2
    | CompoundBranch (l1, m1), SimpleBranch (l2, w2) -> compute_weight m1 + w2
    | CompoundBranch (l1, m1), CompoundBranch (l2, m2) ->
        compute_weight m1 + compute_weight m2
    
let rec balanced : mobile -> bool =
  fun m ->
    match m with
      SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> l1 * w1 = l2 * w2
    | SimpleBranch (l1, w1), CompoundBranch (l2, m2) ->
        if balanced m2 then l1 * w1 = l2 * compute_weight m2 else false
    | CompoundBranch (l1, m1), SimpleBranch (l2, w2) ->
        if balanced m1 then l1 * compute_weight m1 = l2 * w2 else false
    | CompoundBranch (l1, m1), CompoundBranch (l2, m2) ->
        if balanced m1 && balanced m2 then
          l1 * compute_weight m1 = l2 * compute_weight m2
        else false;;
