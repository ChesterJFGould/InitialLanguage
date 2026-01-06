(def s :
  (forall (d *)
    (forall (c *)
      (forall (e *)
        (->
          (-> e (-> d c))
          (->
            (-> e d)
            (-> e c))))))
  (fun f (fun a (fun e ((f e) (a e))))))

(def k : (forall (a *) (forall (b *) (-> a (-> b a))))
  (fun a (fun b a)))

(def i : (forall (a *) (-> a a))
  (fun x x))

