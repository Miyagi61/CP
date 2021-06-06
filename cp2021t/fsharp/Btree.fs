

module BTree

open Cp
// (1) Datatype definition -----------------------------------------------------

type BTree<'a> = Empty | Node of ('a*(BTree<'a> * BTree<'a>))

let inBTree x = either (konst Empty) Node x

let outBTree x = match x with
                    | Empty -> i1 ()
                    | Node (a,(t1,t2)) -> i2 (a,(t1,t2))

// (2) Ana + cata + hylo -------------------------------------------------------

let baseBTree f g = id -|- (f >< (g >< g))

let recBTree f = baseBTree id f          // that is:  id -|- (id >< (f >< f))

let rec cataBTree a = a << (recBTree (cataBTree a)) << outBTree

let rec anaBTree f = inBTree << (recBTree (anaBTree f) ) << f

let hyloBTree a c = cataBTree a << anaBTree c

// (3) Map ---------------------------------------------------------------------

//instance Functor BTree
//         where fmap f = cataBTree ( inBTree . baseBTree f id )
let fmap f = cataBTree ( inBTree << baseBTree f id )

// (4) Examples ----------------------------------------------------------------

// (4.0) Inversion (mirror) ----------------------------------------------------

//let invBTree x = cataBTree (inBTree << (id -|- id >< swap)) x

// (4.1) Counting --------------------------------------------------------------

let countBTree x = cataBTree (either (konst 0) (succ << (uncurry (+)) << p2)) x

// (4.2) Serialization ---------------------------------------------------------

let inord x = let join (h,(l,r)) = l @ [h] @ r
              in (either (konst []) join) x

let inordt x = cataBTree inord x // in-order

let preord x = let f(h,(l,r))= h :: l @ r
                in (either (konst []) f) x

let preordt x = cataBTree preord x // pre-order

let postordt x = let f(h,(l,r))=l @ r @ [h]
                 in  cataBTree (either (konst []) f) // post-order

// (4.3) Quicksort ------------------------------------------------------

let rec part x y = match x with
                        | [] -> ([],[])
                        | (h::t) -> if h < y then let (s,l) = part t y in (h::s,l)
                                     else let (s,l) = part t y in (s,h::l)

let qsep x = match x with
                        | [] -> Left ()
                        | (h::t) -> let (s,l) = part t h
                                     in  Right (h,(s,l))

let qSort x = hyloBTree inord qsep x

// (4.4) Traces ------------------------------------------------

//let tunion (x,(l,r)) = union (map (conc a) l) (map (conc a) r)

//let traces = cataBTree (either (konst [[]]) tunion)

// (4.5) Towers of Hanoi ---------------------------------

let present = inord

let strategy x = match x with
                        | (d,0) -> Left ()
                        | (d,n) -> Right ((n-1,d),((not d,n-1),(not d,n-1)))

let hanoi = hyloBTree present strategy

// (5) Depth and balancing (using mutual recursion) --------------------------

let baldepth_h (a,((b1,b2),(d1,d2)))  = (b1 && b2 && abs(d1-d2)<=1,1+max d1 d2) 

let baldepth_f ((b1,d1),(b2,d2)) = ((b1,b2),(d1,d2))

let baldepth_g x = either (konst(true,1)) (baldepth_h << (id >< baldepth_f)) x

let baldepth x = cataBTree baldepth_g x

let balBTree x = p1 << baldepth

let depthBTree x = p2 << baldepth
