(declare-const x (_ BitVec 32))
(declare-const y (_ BitVec 32))
(declare-const z (_ BitVec 32))
(declare-const u (_ BitVec 65))
(declare-const v (_ BitVec 64))
(declare-const w (_ BitVec 64))
(assert (= x (bvnot (bvnot x))))
(assert (= (bvneg y) (bvneg y)))
(check-sat)
